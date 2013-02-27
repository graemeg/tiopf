unit tiLstSrvr_Cli;

interface
uses
  IdSMTP,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdMessage,
  IdMessageClient,
  IdPOP3,
  Contnrs,
  Classes,
  SysUtils,
  tiLstSrvrCfg_BOM ;

const
  cParamNoDelete = 'NoDelete' ;

type

  TtiLstSrvr    = class ;
  TMessageTask  = class ;
  TMessageTasks = class ;

  TMessageTask = class( TObject )
  private
    FOwner: TMessageTasks;
  protected
    function IsListMaster(const pEMailAddress : string): boolean ;
    function ReplaceMacrosInMessage( const pConfig : TtiLstSrvrCfg ; const pMessage : string ) : string ;
  public
    function Execute( pMsg : TIDMessage ) : boolean ; virtual ; abstract ;
    property Owner : TMessageTasks read FOwner write FOwner ;
  end ;

  TMessageTaskClass = class of TMessageTask ;

  TMessageTaskRejectNonValidEMailAddress = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskBouncedMessage = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskRejectAutoResponder = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskJoin = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskTest = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskLeave = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskDelete = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskGetLog = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskRejectNotAuthorisedToPost = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskRejectAttachment = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskRejectEmptyMessage = class( TMessageTask )
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTaskForwardToAll = class( TMessageTask )
  private
    procedure ArchiveMessage( pSender : string ;
                              pSubject : string ;
                              pMessage : string ;
                              pDiscussionID : string ) ;
  public
    function Execute( pMsg : TIDMessage ) : boolean ; override ;
  end ;

  TMessageTasks = class( TObject )
  private
    FList : TObjectList ;
    FSMTP : TIDSMTP ;
    FOwner: TtiLstSrvr;
    procedure   RegisterMessageTask( pClass : TMessageTaskClass ) ;
    procedure   DoSendResponse( pMsg : TIDMessage ) ;
    function    FormatSubject( const pSubject : string ) : string ;
    procedure SetSenderAddresses(const pMsg: TIDMessage ; const pFrom : string );
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Owner : TtiLstSrvr read FOwner write FOwner ;
    procedure   ForEach( pMsg : TIDMessage ) ;
    procedure   SendResponseText( const pSubject : string ;
                                  const pText : string ;
                                  const pTo : string ;
                                  const pAttachments : TList = nil ;
                                  const pDiscussionID : string = '';
                                  const pFrom : string = '' ) ;
    function GetMessageText(pMsg: TIDMessage): string;
    function GetAttachmentCount(pMsg: TIDMessage): integer;
    function GetReplyToAddress(pMsg: TIDMessage): string;
    function GetSenderDebugDetails( pMsg : TIDMessage ) : string ;
  end ;

  TtiLstSrvrUpdateProgressEvent = procedure( const pTarget : integer ; const pProgress : integer ; const pMessage : string ) of object ;

  TtiLstSrvr = class( TObject )
  private
    FPOP: TIdPOP3;
    FMessageTasks : TMessageTasks ;
    FLstSrvrCfg : TtiLstSrvrCfg ;
    FOnUpdateProgress: TtiLstSrvrUpdateProgressEvent;
    procedure ProcessMessage( pIndex : integer ) ; overload ;
    procedure ProcessMessage( pMsg : TIDMessage ) ; overload ;
    procedure RejectOversizeMessage( pMsg : TIDMessage ) ;
  public
    constructor Create( const pLstSrvrCfg : TtiLstSrvrCfg ) ;
    destructor  Destroy ; override ;
    procedure   Execute ;
    property    Config : TtiLstSrvrCfg read FLstSrvrCfg ;
    property    OnUpdateProgress : TtiLstSrvrUpdateProgressEvent read FOnUpdateProgress write FOnUpdateProgress ;
  end ;

  TThrdLstSrvr = class( TThread )
  private
    FLstSrvrCfgs: TtiLstSrvrCfgs;
    FOnUpdateProgress: TtiLstSrvrUpdateProgressEvent;
  public
    constructor Create ;
    procedure   Execute ; override ;
    property    LstSrvrCfgs : TtiLstSrvrCfgs read FLstSrvrCfgs write FLstSrvrCfgs ;
    property    OnUpdateProgress : TtiLstSrvrUpdateProgressEvent read FOnUpdateProgress write FOnUpdateProgress ;
  end ;


implementation
uses
  tiUtils
  ,tiLog
  ,Windows
  ,tiXML
  ,FileCtrl
  ,tiCRC32
  ,tiDialogs
  ,ComObj
  ,ActiveX
  ,IdEMailAddress
  ,IdAttachment
  ,IdText
  ,tiCommandLineParams
  ;

const
  cDiscussionID = 'LstSrvr-Thrd-ID' ;


{ TtiLstSrvr }

constructor TtiLstSrvr.Create( const pLstSrvrCfg : TtiLstSrvrCfg ) ;

begin
  inherited Create ;
  FPOP := TIdPOP3.Create( nil ) ;
  FLstSrvrCfg := pLstSrvrCfg ;
  FMessageTasks := TMessageTasks.Create ;
  FMessageTasks.Owner := self ;
end;

destructor TtiLstSrvr.Destroy;
begin
  FPOP.Free ;
  FMessageTasks.Free ;
  inherited;
end;

procedure TtiLstSrvr.Execute;
var
  i : integer ;
begin
  FPop.Host     := FLstSrvrCfg.POPHost ;
  FPop.Username   := FLstSrvrCfg.POPUserID ;
  FPop.Password := FLstSrvrCfg.POPPassword ;
  try
    // ToDo: This value 5 should be data driven
    for i := 1 to 5 do
    begin
      FPop.Connect ;
      try
        Log( IntToStr( FPop.CheckMessages ) +
             ' messages on: ' +
             FPop.Host + '\' +
             FPop.Username + '\' +
             FPop.Password ) ;
        if FPop.CheckMessages > 0 then
        begin
          ProcessMessage( 1 ) ;
        end
        else
        begin
          Break ; //==>
        end ;
      finally
        FPop.Disconnect ;
      end ;
    end ;
  except
    on e:exception do
      LogError( 'Error processing messages. ' + Cr +
                'Host: '     + FPop.Host + Cr +
                'UserID: '   + FPop.Username + Cr +
                'Password: ' + FPop.Password + Cr( 2 ) +
                'Message:  ' + e.message ) ;
  end ;
end;

procedure TtiLstSrvr.ProcessMessage( pIndex : integer ) ;
var
  lMsg : TIDMessage ;
  lMessageSize : integer ;
  lSubject :string ;
begin
  lMsg := TIDMessage.Create( nil ) ;
  try
    FPop.RetrieveHeader( pIndex, lMsg ) ;
    lSubject := lMsg.Subject ;
    lMessageSize := FPOP.RetrieveMsgSize( pIndex );
    if lMessageSize > FLstSrvrCfg.MaxMessageSize * 1024 then
      RejectOverSizeMessage( lMsg )
    else
    begin
      FPop.Retrieve( pIndex, lMsg ) ;
      ProcessMessage( lMsg ) ;
    end ;
  finally
    lMsg.Free ;
  end ;
  if not gCommandLineParams.IsParam(cParamNoDelete) then
    FPop.Delete( pIndex ) ;
end ;

procedure TtiLstSrvr.ProcessMessage(pMsg: TIDMessage);
begin
  Log( 'Processing message' ) ;
  Log( '  Subject: ' + pMsg.Subject ) ;
  Log( '  ReplyTo: ' + pMsg.ReplyTo.EMailAddresses ) ; // peter_hinrichsen@techinsite.com.au
  Log( '  Sender:  ' + pMsg.Sender.Text ) ;            // mb10618a@server-mail.com
  Log( '  From:    ' + pMsg.From.Text ) ;              // Peter Hinrichsen <peter_hinrichsen@techinsite.com.au>
  // Require a hook here into the DoOnUpdateProgress event. Yuck.
  FMessageTasks.ForEach( pMsg ) ;
end;

procedure TtiLstSrvr.RejectOversizeMessage(pMsg: TIDMessage);
begin
  Log( 'Message being processed by: ' + ClassName ) ;
  FMessageTasks.SendResponseText(
                    'Sorry, message rejected',
                    FLstSrvrCfg.RejectMessageSizeMessage,
                    FMessageTasks.GetReplyToAddress( pMsg )
                   ) ;
  Log( 'Done' ) ;
end;

{ TMessageTasks }

constructor TMessageTasks.Create;
begin
  inherited ;
  FList := TObjectList.Create ;

  FSMTP := TIDSMTP.Create( nil ) ;
  RegisterMessageTask( TMessageTaskRejectNonValidEMailAddress ) ;
  RegisterMessageTask( TMessageTaskBouncedMessage ) ;
//  RegisterMessageTask( TMessageTaskRejectOversizeMessage ) ;
//  RegisterMessageTask( TMessageTaskRejectAttachment ) ;

  RegisterMessageTask( TMessageTaskJoin ) ;
  RegisterMessageTask( TMessageTaskTest ) ;
  RegisterMessageTask( TMessageTaskLeave ) ;
  RegisterMessageTask( TMessageTaskRejectNotAuthorisedToPost ) ;
  RegisterMessageTask( TMessageTaskDelete ) ;
  RegisterMessageTask( TMessageTaskGetLog ) ;
  RegisterMessageTask( TMessageTaskRejectAutoResponder ) ;
  RegisterMessageTask( TMessageTaskRejectEmptyMessage ) ;
  RegisterMessageTask( TMessageTaskForwardToAll ) ;
end;

destructor TMessageTasks.Destroy;
begin
  FList.Free ;
  FSMTP.Free ;
  inherited;
end;

procedure TMessageTasks.DoSendResponse(pMsg: TIDMessage);
begin
  try
    FSMTP.Host := Owner.Config.SMTPHost ;
    FSMTP.Connect ;
    try
      FSMTP.Send( pMsg ) ;
    finally
      FSMTP.Disconnect ;
    end ;
  except
    on e: exception do
      tiFmtException(e, ClassName, 'DoSendResponse' ) ;
  end ;
end;

procedure TMessageTasks.ForEach(pMsg: TIDMessage);
var
  i : integer ;
begin
  for i := 0 to FList.Count - 1 do
    if TMessageTask( FList.Items[i] ).Execute( pMsg ) then
      Break ;
end;

procedure TMessageTasks.RegisterMessageTask(pClass: TMessageTaskClass);
var
  lMessageTask : TMessageTask ;
begin
  lMessageTask := pClass.Create ;
  lMessageTask.Owner := self ;
  FList.Add( lMessageTask ) ;
end;

function TMessageTasks.GetMessageText(pMsg: TIDMessage): string;
var
  i : integer ;
begin
  result := pMsg.Body.Text ;
  for i := 0 to Pred(pMsg.MessageParts.Count) do
    if pMsg.MessageParts.Items[i] is TIdText then
      result := result + TIdText(pMsg.MessageParts.Items[i]).Body.Text ;
end;

{ TMessateTaskJoin }

function TMessageTaskJoin.Execute(pMsg: TIDMessage): boolean;
var
  lMsg : string ;
begin
  result := SameText( pMsg.Subject, 'join' ) or
            SameText( pMsg.Subject, 'subscribe' ) or
            SameText( pMsg.Subject, 'add' ) ;
  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
  Log( '  Adding: ' + Owner.GetReplyToAddress( pMsg )) ;
  // Add the new member to the list
  if Owner.Owner.Config.ListMembers.AddByEMailAddress(
       Owner.GetReplyToAddress( pMsg ) ) then
  begin
    Owner.Owner.Config.Save ;
    // Send a welcome message
    Owner.SendResponseText( 'Welcome',
                            Owner.Owner.Config.JoinMessage,
                            Owner.GetReplyToAddress( pMsg )
                          ) ;
    // Send a message to the list master
    lMsg :=
      'Join request from: ' + CrLf +
      Owner.GetSenderDebugDetails( pMsg ) + CrLf( 2 ) +
      Owner.GetMessageText( pMsg ) ;
      Owner.SendResponseText( 'JoinRequest: ' + Owner.GetReplyToAddress( pMsg ),
                              lMsg,
                              Owner.Owner.Config.ListMasterEMailAdrs
                            ) ;
  end else
    Log( '  Not added, already a members: ' + Owner.GetReplyToAddress( pMsg )) ;

  Log( 'Done' ) ;
end;

function TMessageTaskTest.Execute(pMsg: TIDMessage): boolean;
var
  lMsg : string ;
begin
  result := SameText( pMsg.Subject, 'test' ) or
            SameText( pMsg.Subject, 'testing' ) ;
  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
  lMsg :=
    'Test request received from: ' + CrLf +
    Owner.GetSenderDebugDetails( pMsg ) + CrLf( 2 ) +
    Owner.GetMessageText( pMsg ) ;
  Owner.SendResponseText( 'TestRequest',
                          lMsg,
                          Owner.Owner.Config.ListMasterEMailAdrs
                         ) ;
  lMsg := 'Your test of ' +
          Owner.Owner.Config.ListName +
          ' passed.' + CrLf( 2 ) +
          'Your EMail software was setup with the following settings.' + CrLf +
          Owner.GetSenderDebugDetails( pMsg ) ;
  Owner.SendResponseText( 'TestRequest',
                          lMsg,
                          Owner.GetReplyToAddress( pMsg )
                        ) ;
  Log( 'Done' ) ;
end;

procedure TMessageTasks.SetSenderAddresses(const pMsg : TIDMessage ; const pFrom : string ) ;
var
  lSenderAdrsType : TSenderAdrsType ;
  lName : string ;
  lAddress : string ;
begin

  lSenderAdrsType := Owner.Config.SenderAdrsType ;
  if ( not ( lSenderAdrsType in [satSenderAndListName, satListMaster] )) then
    tiFmtException( 'Invalid <SenderAdrsType> <' +
                    cSenderAdrsTypesDB[Owner.Config.SenderAdrsType] + '>',
                    ClassName, 'SetSenderAdrs' ) ;

  if ( lSenderAdrsType = satSenderAndListName ) and
     ( pFrom = '' ) then
  begin
    lName    := Owner.Config.ListNameLong ;
    lAddress := Owner.Config.ListEMailAdrs ;
  end else if ( lSenderAdrsType = satSenderAndListName ) and
              ( pFrom <> '' ) then
  begin
    lName    := pFrom ;
    lAddress := Owner.Config.ListEMailAdrs ;
  end else if ( lSenderAdrsType = satListMaster ) and
              ( pFrom = '' ) then
  begin
    lName    := Owner.Config.ListNameLong ;
    lAddress := Owner.Config.ListEMailAdrs ;
  end else if ( lSenderAdrsType = satListMaster ) and
              ( pFrom <> '' ) then
  begin
    lName    := Owner.Config.ListMasterName ;
    lAddress := Owner.Config.ListMasterEMailAdrs ;
  end else
    tiFmtException( 'Invalid combination of <SenderAdrsType> and <From>',
                    ClassName, 'SetSenderAdrs' ) ;

  pMsg.From.Name              := lName ;
  pMsg.From.Address           := lAddress ;
  pMsg.ReplyTo.EMailAddresses := lAddress ;
  pMsg.Headers.Values['Return-Path'] := lAddress ;

end ;

procedure TMessageTasks.SendResponseText(const pSubject, pText, pTo : string ;
                                         const pAttachments : TList = nil ;
                                         const pDiscussionID : string = '' ;
                                         const pFrom : string = '' );
var
  lMsg : TIDMessage ;
  i : integer ;
  lAttachmentTo : TIDAttachment ;
  lAttachmentFrom : TIDAttachment ;
  lFileName : TFileName ;
  lText : string ;
begin
  try
    lMsg := TIDMessage.Create( Nil ) ;
    try
      Log( '  Sending message to: ' + pTo ) ;
      lText := pText ;
      if pAttachments <> nil then
      begin
        if pAttachments.Count > 0 then
        begin
          lText := 'Warning: This message contains ' ;
          if pAttachments.Count = 1 then
            lText := lText + 'an attachment that has '
          else
            lText := lText + 'attachments that have ' ;
          lText :=
                   '* * * * * * * * * * * * * * * * * * * * * * * * * * * * ' + CrLf +
                   lText + CrLf + 'not been checked for viruses. ' + CrLf +
                   'Please exercise the necessary precautions when opening ' +
                   'an attachment.' + CrLf +
                   'Open this attachment at your own risk.' + CrLf +
                   '* * * * * * * * * * * * * * * * * * * * * * * * * * * * ' + CrLf( 2 ) +
                   pText ;
        end ;
        for i := 0 to pAttachments.Count - 1 do
        begin
          lAttachmentFrom := TIdAttachment( pAttachments.Items[i] ) ;
          lFileName :=
            tiAddTrailingSlash( tiGetTempDir ) +
            ExtractFileName( lAttachmentFrom.FileName ) ;
          // Warning: Not thread safe.
          lAttachmentFrom.SaveToFile( lFileName ) ;
          lAttachmentTo   := TIdAttachment.Create( lMsg.MessageParts ) ;
          lAttachmentTo.Assign( lAttachmentFrom ) ;
          lAttachmentTo.FileName := lFileName ;
        end ;
      end ;

      lMsg.Body.Text := lText ;
      lMsg.Recipients.EMailAddresses := pTo ;
      SetSenderAddresses( lMsg, pFrom ) ;
      lMsg.Subject := FormatSubject( pSubject ) ;
      if pDiscussionID <> '' then
        lMsg.Headers.Values[cDiscussionID] := pDiscussionID;
      DoSendResponse( lMsg ) ;
    finally
      lMsg.Free ;
    end ;
  except
    on e:exception do
      LogError( 'Error in ' + ClassName + '.SendResponseText. Subject: ' +
                pSubject + ' To: ' + pTo ) ;
  end ;
end;

function TMessageTasks.GetAttachmentCount(pMsg: TIDMessage): integer;
var
  i : integer ;
begin
  result := 0 ;
  for i := 0 to Pred(pMsg.MessageParts.Count) do
    if (pMsg.MessageParts.Items[i] is TIdAttachment) then
      inc( result ) ;
end;

function TMessageTasks.GetReplyToAddress(pMsg: TIDMessage): string;
begin
  result := pMsg.From.Address ;   // Peter Hinrichsen <peter_hinrichsen@techinsite.com.au>
  if result <> '' then
    exit ;  //==>

  result := pMsg.ReplyTo.EMailAddresses ;// peter_hinrichsen@techinsite.com.au
  if result <> '' then
    exit ;  //==>

  result := pMsg.Sender.Address ; // mb10618a@server-mail.com

end;

function TMessageTasks.FormatSubject(const pSubject: string): string;
var
  lListName : string ;
begin
  lListName := Owner.Config.ListName + '-' ;
  result := pSubject ;
  if Pos( lListName, result ) = 0 then
    result := lListName + result ;
end;

function TMessageTasks.GetSenderDebugDetails(pMsg: TIDMessage): string;
begin
  result :=
          '  Sender:   ' + pMsg.Sender.Text + CrLf +
          '  From:     ' + pMsg.From.Text + CrLf +
          '  Reply to: ' + pMsg.ReplyTo.EMailAddresses ;

end;

{ TListMembers }

{ TMessageTaskLeave }

function TMessageTaskLeave.Execute(pMsg: TIDMessage): boolean;
var
  lMsg : string ;
begin
  result := SameText( pMsg.Subject, 'leave' ) or
            SameText( pMsg.Subject, 'unsubscribe' ) or
            SameText( pMsg.Subject, 'remove' ) ;
  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
  if Owner.Owner.Config.ListMembers.DeleteByEMailAddress(
    Owner.GetReplyToAddress( pMsg )) then
  begin
    Owner.Owner.Config.Save ;
    Owner.SendResponseText( 'Goodbye',
                             Owner.Owner.Config.LeaveMessage,
                             Owner.GetReplyToAddress( pMsg )
                             ) ;
    lMsg :=
      'Leave request from: ' + CrLf +
      Owner.GetSenderDebugDetails( pMsg ) + CrLf( 2 ) +
      Owner.GetMessageText( pMsg ) ;
    Owner.SendResponseText( 'LeaveRequest: ' + Owner.GetReplyToAddress( pMsg ),
                            lMsg,
                            Owner.Owner.Config.ListMasterEMailAdrs ) ;
  end else
    Log( 'Not removed, not a member: ' + Owner.GetReplyToAddress( pMsg ));
  Log( 'Done' ) ;

end;

{ TMessageTaskForwardToAll }

procedure TMessageTaskForwardToAll.ArchiveMessage(pSender, pSubject, pMessage, pDiscussionID : string);
var
  lMessage : string ;
  lFileName : TFileName ;
  lMessageIndex : integer ;
  lsl : TStringList ;
  lXMLTrans : IXMLReservedCharsTranslator;
begin
  lXMLTrans := CreateXMLReservedCharsTranslator;

  lMessage := tiStrTran( pMessage,
                         Owner.Owner.Config.FooterMessage,
                         '' ) ;
  lMessage := tiTrimTrailingWhiteSpace( lMessage ) ;

  lMessage :=
    '<?xml version="1.0"?>' + CrLf +
    '<tiOPFEmailArchive>' + CrLf +
    '<subject>' +
    lXMLTrans.RemoveReserved( rcXML, pSubject) +
    '</subject>' + CrLf +
    '<sender>' +
    lXMLTrans.RemoveReserved( rcXML, pSender) +
    '</sender>' + CrLf +
    '<date>' +
    tiDateTimeToStr( Now ) +
    '</date>' + CrLf +
    '<discussionid>' +
    pDiscussionID +
    '</discussionid>' + CrLf +
    '<body>' +
    lXMLTrans.RemoveReserved( rcXML, lMessage) +
    '</body>' + CrLf +
    '</tiOPFEmailArchive>' ;

  lMessageIndex := Owner.Owner.Config.MessageIndex ;

  if not DirectoryExists( tiRemoveTrailingSlash( Owner.Owner.Config.ArchiveDir )) then
    ForceDirectories( Owner.Owner.Config.ArchiveDir ) ;

  lFileName :=
    tiAddTrailingSlash( Owner.Owner.Config.ArchiveDir ) +
    tiPad0( IntToStr( lMessageIndex ), 8 ) + '.xml' ;
  Owner.Owner.Config.IncMessageIndex ;
  tiStringToFile( lMessage, lFileName ) ;
  Log( 'Message archived to ' + lFileName ) ;

  lFileName :=
    tiAddTrailingSlash( Owner.Owner.Config.ArchiveDir ) +
    'Index.txt' ;

  lsl := TStringList.Create ;
  try
    if FileExists( lFileName ) then
      lsl.LoadFromFile( lFileName ) ;
    lsl.Add( tiPad0( IntToStr( lMessageIndex ), 8 ) + ',' +
             tiStrTran( lXMLTrans.RemoveReserved( rcXML, pSender), ',' ,'' ) + ',' +
             tiStrTran( lXMLTrans.RemoveReserved( rcXML, pSubject), ',' , '' )) ;
    lsl.SaveToFile( lFileName ) ;
  finally
    lsl.Free ;
  end ;

end;

function TMessageTaskForwardToAll.Execute(pMsg: TIDMessage): boolean;
var
  lSubject : string ;
  lMessage : string ;
  lFooterMessage : string ;
  i : integer ;
  lFrom : string ;
  lSendTo : string ;
  lAttachments : TList ;
  lGuid : TGUID;
  lDiscussionID : string ;
begin
  result := true ;

  Log( 'Message being processed by: ' + ClassName ) ;

  lSubject := pMsg.Subject ;
  lMessage := Owner.GetMessageText( pMsg ) ;
  lMessage := tiTrimTrailingWhiteSpace( lMessage ) ;
  lFrom    := Owner.GetReplyToAddress( pMsg ) ;
  lDiscussionID := pMsg.Headers.Values[cDiscussionID];
  if lDiscussionID = '' then
  begin
    CoCreateGuid(lGUID);
    lDiscussionID := GuidToString(lGUID);
  end ;

  // ToDo: Set a message size limit
  // ToDo: Send message back to sender if attachment

  lFooterMessage := Owner.Owner.Config.FooterMessage ;
  // ToDo: Better to strip out all instances of lFooterMessage, and add it to the end
  if Pos( lMessage, lFooterMessage ) = 0 then
    lMessage := lMessage + CrLf(2)+
                lFooterMessage ;

  lAttachments := TList.Create ;
  try
    for i := 0 to Pred(pMsg.MessageParts.Count) do
      if (pMsg.MessageParts.Items[i] is TIdAttachment) then
        lAttachments.Add( pMsg.MessageParts.Items[i] ) ;

    Log( 'Message being processed by: ' + ClassName ) ;
    Log( '  Forwarding message: ' + lSubject ) ;
    Log( '  Subject: ' + lSubject ) ;
    Log( '  Message: ' + tiAddEllipsis( lMessage, 100 )) ;

    for i := 0 to Owner.Owner.Config.ListMembers.Count - 1 do
    begin
      if Owner.Owner.Config.ListMembers.Items[i].Deleted then
        Continue ; //==>

      lSendTo := Owner.Owner.Config.ListMembers.Items[i].EMailAddress ;
      if lSendTo = '' then
        Continue ; //==>
      Owner.SendResponseText( lSubject,
                              lMessage,
                              lSendTo,
                              lAttachments,
                              lDiscussionID,
                              lFrom
                               ) ;
    end ;
  finally
    lAttachments.Free ;
  end ;
  ArchiveMessage( Owner.GetReplyToAddress( pMsg ),
                  lSubject,
                  lMessage,
                  lDiscussionID
                  ) ;
  Log( 'Done' ) ;

end;

{ TMessageTaskRejectAttachment }
function TMessageTaskRejectAttachment.Execute(pMsg: TIDMessage): boolean;
begin
  result := Owner.GetAttachmentCount( pMsg ) <> 0 ;
  if result then
  begin
    Log( 'Message being processed by: ' + ClassName ) ;
    Owner.SendResponseText( 'Sorry, post rejected',
                            Owner.Owner.Config.RejectAttachmentMessage,
                            Owner.GetReplyToAddress( pMsg )) ;
    Log( 'Done' ) ;
  end ;
end;

{ TThrdLstSrvr }

constructor TThrdLstSrvr.Create;
begin
  inherited Create( true ) ;
  FreeOnTerminate := true ;
end;

procedure TThrdLstSrvr.Execute ;
var
  lLstSrvr : TtiLstSrvr ;
  i : integer ;
begin
  for i := 0 to FLstSrvrCfgs.Count - 1 do
  begin
    if ( FLstSrvrCfgs.Items[i].Deleted ) or
       ( not FLstSrvrCfgs.Items[i].Active ) then
      Continue ; //==>
    lLstSrvr := TtiLstSrvr.Create( FLstSrvrCfgs.Items[i] ) ;
    try
      lLstSrvr.OnUpdateProgress := FOnUpdateProgress ;
      lLstSrvr.Execute ;
    finally
      lLstSrvr.Free ;
    end ;
  end ;
end;

{ TMessageTaskTest }

{ TMessageTaskDelete }

function TMessageTaskDelete.Execute(pMsg: TIDMessage): boolean;
var
  lAdrs : string ;
  lAdrses : TStringList ;
  lReplyToAddress : string ;
  i : integer ;
  lReplyText : string ;
begin
  result := SameText( pMsg.Subject, 'delete' ) ;
  if not result then
    Exit ; //==>

  lReplyToAddress := Owner.GetReplyToAddress( pMsg ) ;
  result := IsListMaster(lReplyToAddress);

  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
  lReplyText := '' ;
  lAdrses := TStringList.Create ;
  try
    lAdrses.Text := Owner.GetMessageText( pMsg ) ;
    for i := 0 to lAdrses.Count - 1 do
    begin
      lAdrs := Trim(lAdrses.Strings[i]);
      if Trim(lAdrs) = '' then
        Continue ; //==>
      if lReplyText <> '' then
        lReplyText := lReplyText + Cr ;
      if Owner.Owner.Config.ListMembers.FindByEMailAddress(lAdrs) <> nil then
      begin
        Owner.Owner.Config.ListMembers.DeleteByEMailAddress( lAdrs ) ;
        lReplyText := lReplyText + 'X  ' + lAdrs ;
      end else
        lReplyText := lReplyText + '   ' + lAdrs ;
    end ;
    Owner.Owner.Config.Save ;
    Owner.SendResponseText( 'Deleted by list master',
                            lReplyText,
                            Owner.Owner.Config.ListMasterEMailAdrs
                             ) ;
  finally
    lAdrses.Free ;
  end ;
  Log( 'Done' ) ;

end;

{ TMessageTaskRejectNonValidEMailAddress }

function TMessageTaskRejectNonValidEMailAddress.Execute( pMsg: TIDMessage): boolean;
var
  lEMailAddress : string ;
begin
  lEMailAddress := Owner.GetReplyToAddress( pMsg ) ;
  result := not tiIsEMailAddressValid( lEMailAddress ) ;
  if result then
  begin
    Log( 'Message being processed by: ' + ClassName ) ;
    Log( 'Invalid email address rejected: ' +
         ' ' + lEMailAddress + '>' ) ;
  end;
end;

{ TMessageTaskRejectAutoResponder }

function TMessageTaskRejectAutoResponder.Execute( pMsg: TIDMessage): boolean;
var
  lEMailAddress : string ;
  lMessage : string ;
  lThisCRC : string ;
  lLastCRC : string ;
begin
  lEMailAddress := Owner.GetReplyToAddress( pMsg ) ;
  lMessage      := Owner.GetMessageText( pMsg ) ;
  lThisCRC      := IntToStr( StringToCRC32( lMessage )) ;
  lLastCRC      := Owner.Owner.Config.LastMessageCRC.Values[ lEMailAddress ] ;
  result :=
    ( lThisCRC = lLastCRC ) and
    ( lThisCRC <> '' ) and
    ( lLastCRC <> '' ) ;
  Owner.Owner.Config.LastMessageCRC.Values[ lEMailAddress ] := lThisCRC ;
  if result then
  begin
    Log( 'Message being processed by: ' + ClassName ) ;
    Log( 'Repeat message not forwarded' ) ;
    Log( 'Sender: ' + Owner.GetReplyToAddress( pMsg )) ;
    Log( 'Subject: ' + pMsg.Subject ) ;
    Log( 'CRC: ' + lThisCRC ) ;
  end ;
end;

{ TMessageTask }


//Move this to the config object
function TMessageTask.IsListMaster(const pEMailAddress: string): boolean;
begin
  result := SameText( Owner.Owner.Config.ListMasterEMailAdrs, pEMailAddress ) ;
end;

function TMessageTask.ReplaceMacrosInMessage(const pConfig: TtiLstSrvrCfg; const pMessage: string): string;
var
  lMessageMacros : TMessageMacros ;
  i : integer ;
begin
  lMessageMacros := pConfig.Owner.MessageMacros ;
  result := pMessage ;
  for i := 0 to lMessageMacros.Count - 1 do
    result := tiCIStrTran( result,
                           lMessageMacros.Items[i].PlaceHolder,
                           lMessageMacros.Items[i].Value( Owner.Owner.Config )) ;
end;

{
1. Write code to evaluate these macros in the _Cli file
2. Write a preview button in the GUI
4. Write the code to insert a macro into the GUI
5. Modify the GUI with the extra two properties
6. Modify the XML file with the extra two properties (YUCK)
}

{ TMessageTaskBouncedMessage }

function TMessageTaskBouncedMessage.Execute(pMsg: TIDMessage): boolean;
var
  lSubject : string ;
begin
  lSubject := LowerCase( pMsg.Subject ) ;
  result := ( Pos( 'failure notice',         lSubject ) <> 0 ) or
            ( Pos( 'could not send message', lSubject ) <> 0 )  or
            ( Pos( 'return mail',            lSubject ) <> 0 )  or
            ( Pos( 'returned mail',          lSubject ) <> 0 )  or
            ( Pos( 'delivery failure',       lSubject ) <> 0 )  or
            ( Pos( 'delivery has failed',    lSubject ) <> 0 )  or
            ( Pos( 'message delayed',        lSubject ) <> 0 )  or
            ( Pos( 'mail delivery failed',   lSubject ) <> 0 )  or
            ( Pos( 'mail failed',            lSubject ) <> 0 )  or
            ( Pos( 'returning to sender',    lSubject ) <> 0 )  or
            ( Pos( 'undelivered mail',       lSubject ) <> 0 )  ;

  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
// Commented out while I look for a solution to bounced messages
// missing the sendto address (so we don't know who bounced them)  
{
  Log( 'Sending bounce message to: ' + Owner.Owner.Config.ListMasterEMailAdrs ) ;

  lMsg :=
    '  Subject: ' + pMsg.Subject + Cr +
    '  ReplyTo: ' + pMsg.ReplyTo.EMailAddresses + Cr +
    '  Sender:  ' + pMsg.Sender.Text + Cr +
    '  From:    ' + pMsg.From.Text + Cr +
    tiReplicate( '-', 60 ) + Cr +
    pMsg.Body.Text + Cr +
    tiReplicate( '-', 60 ) + Cr +
  pMsg.Headers.Text ;

  Owner.SendResponseText( 'Bounced message',
                          lMsg,
                          Owner.Owner.Config.ListMasterEMailAdrs
                        ) ;
}
  Log( 'Done' ) ;
end;

{ TMessageTaskRejectNotAuthorisedToPost }

function TMessageTaskRejectNotAuthorisedToPost.Execute(pMsg: TIDMessage): boolean;
var
  lIsListMember : boolean ;
  lAcceptPostsFrom : TAcceptPostsFrom ;
  lIsListMaster    : boolean ;
begin
  result := false ;
  lIsListMember    := Owner.Owner.Config.ListMembers.IsMember( Owner.GetReplyToAddress( pMsg ));
  lAcceptPostsFrom := Owner.Owner.Config.AcceptPostsFrom;
  lIsListMaster    := false ;

  case lAcceptPostsFrom of
  apfListMembers : begin
                     result := not lIsListMember ;
                   end ;
  apfListMaster  : begin
                     lIsListMaster := SameText( Owner.Owner.Config.ListMasterEMailAdrs,
                                                Owner.GetReplyToAddress( pMsg ));
                     result := not lIsListMaster  ;
                   end ;
  else
    tiFmtException( 'Invalid <AccpetPostsFrom> value <' +
                    cAcceptPostsFromDB[lAcceptPostsFrom] +
                    '>', ClassName, 'Execute' ) ;
  end ;
  if result then
  begin
    Log( 'Message processed by: ' + ClassName ) ;
    Log( '  Accept posts from: ' + cAcceptPostsFromDB[lAcceptPostsFrom]) ;
    Log( '  Is list member: ' + tiBoolToStr(lIsListMember));
    Log( '  Is list master: ' + tiBoolToStr(lIsListMaster));
    Log( '  Reject message: ' + tiBoolToStr(result));
  end ;
end;

{ TMessageTaskRejectEmptyMessage }

function TMessageTaskRejectEmptyMessage.Execute(pMsg: TIDMessage): boolean;
var
  lReplyText : string ;
begin
  result := Trim(Owner.GetMessageText( pMsg )) = '' ;
  if not result then
    Exit ; //==>
  Log( 'Message being processed by: ' + ClassName ) ;
  lReplyText :=
    'Empty message received from: ' +
    Owner.GetReplyToAddress( pMsg ) + Cr +
    'Subject line: ' + pMsg.Subject;
  Owner.SendResponseText( 'Empty message',
                          lReplyText,
                          Owner.Owner.Config.ListMasterEMailAdrs
                           ) ;
end;

{ TMessageTaskGetLog }

function TMessageTaskGetLog.Execute(pMsg: TIDMessage): boolean;
var
  lReplyToAddress : string ;
  lReplyText : string ;
begin
  result := SameText( pMsg.Subject, 'getlog' ) ;
  if not result then
    Exit ; //==>

  lReplyToAddress := Owner.GetReplyToAddress( pMsg ) ;
  result := SameText( Owner.Owner.Config.ListMasterEMailAdrs,
                   lReplyToAddress ) ;
  if not result then
    Exit ; //==>

  Log( 'Message being processed by: ' + ClassName ) ;
  lReplyText := tiFileToString( gLog.LogFileName ) ;
  Owner.SendResponseText( 'tiListServer Log at ' + DateTimeToStr(Now),
                          lReplyText,
                          Owner.Owner.Config.ListMasterEMailAdrs
                           ) ;
  Log( 'Done' ) ;

end;

end.

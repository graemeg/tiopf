unit tiLstSrvrCfg_BOM;

interface
uses
  tiPtnVisPerObj
  ,Classes
  ,SysUtils
  ,tiPerObjOIDAbs
  ;

type
  TSenderAdrsType = ( satUnknown, satSenderAndListName, satListMaster ) ;
const
  cSenderAdrsTypesDB : array[TSenderAdrsType] of string =
                    ( 'UNKNOWN', 'SENDER_AND_LIST_NAME', 'LIST_MASTER' ) ;

procedure AssignSenderAdrsTypes(const pStrings : TStrings);
function  DBStringToSenderAdrsType(const pValue : string): TSenderAdrsType;

type
  TAcceptPostsFrom = ( apfUnknown, apfListMembers, apfListMaster ) ;
const
  cAcceptPostsFromDB : array[TAcceptPostsFrom] of string =
                    ( 'UNKNOWN', 'LIST_MEMBERS', 'LIST_MASTER' ) ;

procedure AssignAcceptPostsFrom(const pStrings : TStrings);
function  DBStringToAcceptPostsFrom(const pValue : string): TAcceptPostsFrom;

type

  TtiLstSrvrCfgs = class ;
  TtiLstSrvrCfg  = class ;
  TListMembers   = class ;
  TListMember    = class ;
  TMessageMacros = class ;
  TMessageMacro  = class ;

  //----------------------------------------------------------------------------
  TtiLstSrvrCfgs = class( TPerObjList )
  private
    FMessageMacros: TMessageMacros;
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TtiLstSrvrCfg ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiLstSrvrCfg); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Items[i:integer] : TtiLstSrvrCfg read GetItems write SetItems ;
    procedure   Add( pObject : TtiLstSrvrCfg   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    MessageMacros : TMessageMacros read FMessageMacros write FMessageMacros ;
  published
  end ;

  //----------------------------------------------------------------------------
  TtiLstSrvrCfg = class( TPerObjAbs )
  private
    FListMembers : TListMembers ;
    FLastMessageCRC : TStringList ;
    FPOPHost: string;
    FPOPPassword: string;
    FPOPUserID: string;
    FMaxMessageSize: integer;
    FListMasterEMailAdrs: string;
    FSMTPHost: string;
    FListName: string;
    FListEMailAdrs: string;
    FRejectMessageSizeMessage: string;
    FJoinMessage: string;
    FLeaveMessage: string;
    FNonListMemberMessage: string;
    FRejectAttachmentMessage: string;
    FDataDir: string;
    FArchiveDir: string;
    FListDir: string;
    FFooterMessage: string;
    FMessageIndex : integer ;
    FListNameLong: string;
    FListMasterName: string;
    FSenderAdrsType: TSenderAdrsType;
    FAcceptPostsFrom: TAcceptPostsFrom;
    FActive: boolean;
    function    GetMessageIndex: integer;
    procedure   SetMessageIndex(const Value: integer);
    function    GetSenderAdrsTypeAsString: string;
    procedure   SetSenderAdrsTypeAsString(const Value: string);
    function    GetAcceptPostsFromAsString: string;
    procedure   SetAcceptPostsFromAsString(const Value: string);
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TtiLstSrvrCfgs; reintroduce ;
    procedure   SetOwner(const Value: TtiLstSrvrCfgs); reintroduce ;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TtiLstSrvrCfgs             read GetOwner      write SetOwner ;
    procedure   IncMessageIndex ;
    property    SenderAdrsType : TSenderAdrsType read FSenderAdrsType write FSenderAdrsType ;
    property    AcceptPostsFrom : TAcceptPostsFrom read FAcceptPostsFrom write FAcceptPostsFrom ;
  published             
    // General list setup
    property    ListName : string read FListName write FListName ;
    property    ListNameLong : string read FListNameLong write FListNameLong ;
    property    ListMasterName : string read FListMasterName write FListMasterName ;
    property    ListEMailAdrs : string read FListEMailAdrs write FListEMailAdrs ;
    property    ListMasterEMailAdrs : string read FListMasterEMailAdrs write FListMasterEMailAdrs ;
    property    MaxMessageSize : integer read FMaxMessageSize write FMaxMessageSize ;
    property    SenderAdrsTypeAsString : string  read GetSenderAdrsTypeAsString write SetSenderAdrsTypeAsString ;
    property    AcceptPostsFromAsString : string read GetAcceptPostsFromAsString write SetAcceptPostsFromAsString ;
    property    Active : boolean read FActive write FActive ;

    // Pop & SMTP server setup
    property    POPHost : string read FPOPHost write FPOPHost ;
    property    POPUserID : string read FPOPUserID write FPOPUserID ;
    property    POPPassword : string read FPOPPassword write FPOPPassword ;
    property    SMTPHost : string read FSMTPHost write FSMTPHost ;

    // Data directories
    property    ListDir : string read FListDir write FListDir ;
    property    DataDir : string read FDataDir write FDataDir ;
    property    ArchiveDir : string read FArchiveDir write FArchiveDir ;

    // Message text
    property    JoinMessage : string read FJoinMessage write FJoinMessage ;
    property    LeaveMessage : string read FLeaveMessage write FLeaveMessage ;
    property    RejectMessageSizeMessage : string read FRejectMessageSizeMessage write FRejectMessageSizeMessage ;
    property    NonListMemberMessage : string read FNonListMemberMessage write FNonListMemberMessage ;
    property    RejectAttachmentMessage : string read FRejectAttachmentMessage write FRejectAttachmentMessage ;
    property    FooterMessage : string read FFooterMessage write FFooterMessage ;

    // List members
    property    ListMembers : TListMembers read FListMembers ;

    // The index of the last message saved to the archive
    property    MessageIndex : integer read GetMessageIndex write SetMessageIndex ;
    property    LastMessageCRC : TStringList read FLastMessageCRC ;
    
  end ;

  //----------------------------------------------------------------------------
  TListMembers = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TListMember ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TListMember); reintroduce ;
    function    GetOwner: TtiLstSrvrCfg; reintroduce ;
    procedure   SetOwner(const Value: TtiLstSrvrCfg); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    property    Items[i:integer] : TListMember read GetItems write SetItems ;
    procedure   Add( pObject : TListMember   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TtiLstSrvrCfg read GetOwner      write SetOwner ;
    function    FindByEMailAddress( const pEMailAddress : string ) : TListMember ;
    function    AddByEMailAddress( const pEMailAddress : string ): boolean ;
    function    DeleteByEMailAddress( const pEMailAddress : string ): boolean ;
    function    IsMember( const pEMailAddress : string ) : boolean ;
    procedure   LoadFromFile( const pFileName : TFileName ) ;
  published
  end ;

  //----------------------------------------------------------------------------
  TListMember = class( TPerObjAbs )
  private
    FEMailAddress: string;
  protected
    function    GetOwner: TtiLstSrvrCfg; reintroduce ;
    procedure   SetOwner(const Value: TtiLstSrvrCfg); reintroduce ;
  public
    property    Owner       : TtiLstSrvrCfg             read GetOwner      write SetOwner ;
  published
    property    EMailAddress : string read FEMailAddress write FEMailAddress ;
    //property    Index ;
  end ;

  //----------------------------------------------------------------------------
  TMessageMacros = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TMessageMacro ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TMessageMacro); reintroduce ;
    function    GetOwner: TtiLstSrvrCfgs; reintroduce ;
    procedure   SetOwner(const Value: TtiLstSrvrCfgs); reintroduce ;
  public
    property    Items[i:integer] : TMessageMacro read GetItems write SetItems ;
    procedure   Add( pObject : TMessageMacro   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TtiLstSrvrCfgs read GetOwner      write SetOwner ;
    procedure   RegisterMacro( const pPlaceHolder, pPropertyName : string ) ;
  published
  end ;

  //----------------------------------------------------------------------------
  TMessageMacro = class( TPerObjAbs )
  private
    FPlaceHolder: string;
    FPropertyName: string;
  protected
    function    GetOwner: TMessageMacros; reintroduce ;
    procedure   SetOwner(const Value: TMessageMacros); reintroduce ;
  public
    property    Owner       : TMessageMacros             read GetOwner      write SetOwner ;
    function    Value( pLstSrvrCfg : TtiLstSrvrCfg ) : string ;
  published
    property    PlaceHolder  : string read FPlaceHolder write FPlaceHolder ;
    property    PropertyName : string read FPropertyName write FPropertyName ;
  end ;

implementation
uses
  tiUtils
  ,FileCtrl
  ,tiLog
  ,TypInfo
  ;

const
  cuEnterValue = '<Enter Value>' ;
  cuLIST_NAME_LONG            = '%LIST_NAME_LONG%' ;
  cuLIST_MASTER_NAME          = '%LIST_MASTER_NAME%' ;
  cuLIST_EMAIL_ADDRESS        = '%LIST_EMAIL_ADDRESS%' ;
  cuLIST_MASTER_EMAIL_ADDRESS = '%LIST_MASTER_EMAIL_ADDRESS%' ;
  cuMESSAGE_SIZE_LIMIT        = '%MESSAGE_SIZE_LIMIT%' ;

procedure AssignSenderAdrsTypes(const pStrings : TStrings);
var
  i : TSenderAdrsType ;
begin
  pStrings.Clear;
  for i := low( TSenderAdrsType ) to High( TSenderAdrsType ) do
    pStrings.Add(cSenderAdrsTypesDB[i]);
end ;

function  DBStringToSenderAdrsType(const pValue : string): TSenderAdrsType;
var
  i : TSenderAdrsType ;
begin
  result := satUnknown ;
  for i := low( TSenderAdrsType ) to High( TSenderAdrsType ) do
    if SameText( pValue, cSenderAdrsTypesDB[i] ) then
    begin
      result := i ;
      Exit ; //==>
    end ;
end ;

procedure AssignAcceptPostsFrom(const pStrings : TStrings);
var
  i : TAcceptPostsFrom ;
begin
  pStrings.Clear;
  for i := low( TAcceptPostsFrom ) to High( TAcceptPostsFrom ) do
    pStrings.Add(cAcceptPostsFromDB[i]);
end ;

function  DBStringToAcceptPostsFrom(const pValue : string): TAcceptPostsFrom;
var
  i : TAcceptPostsFrom ;
begin
  result := apfUnknown ;
  for i := low( TAcceptPostsFrom ) to High( TAcceptPostsFrom ) do
    if SameText( pValue, cAcceptPostsFromDB[i] ) then
    begin
      result := i ;
      Exit ; //==>
    end ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLstSrvrCfg
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLstSrvrCfg.Create;
begin
  inherited ;
  FListMembers := TListMembers.Create ;
  FListMembers.Owner := Self ;
  FListMembers.ItemOwner := Self ;

  ListName            := cuEnterValue ;
  ListEMailAdrs       := cuEnterValue ;
  ListMasterEMailAdrs := cuEnterValue ;
  MaxMessageSize      := 100 ;
  POPHost             := cuEnterValue ;
  POPUserID           := cuEnterValue ;
  POPPassword         := cuEnterValue ;
  SMTPHost            := cuEnterValue ;
  ArchiveDir          := '' ;
  MessageIndex        := 1 ;

  FLastMessageCRC := TStringList.Create ;

  JoinMessage              :=
    'Welcome to the %LIST_NAME_LONG% mailing list.'                     + Cr(2) +
    'To send a message to the list: mailto:%LIST_EMAIL_ADDRESS% '       + Cr +
    'To leave the list mailto:%LIST_EMAIL_ADDRESS%?subject=leave'       + Cr(2) +
    'If you have a problem with the list, you can contact me directly ' + Cr +
    'on mailto:%LIST_MASTER_EMAIL_ADDRESS%'                             + Cr(2) +
    'Please note, the list does not currently accept attachments and '  + Cr +
    'the message size limit is %MESSAGE_SIZE_LIMIT%kb'                  + Cr(2) +
    'We all look forward to hearing from you.'                          + Cr(2) +
    'Regards,'                                                          + Cr(2) +
    '%LIST_MASTER_NAME%.' ;

  LeaveMessage             :=
    'Thank you for being a member of the %LIST_NAME_LONG% ' + Cr +
    ' mailing list.'                                        + Cr(2) +
    'To re-join the list, send a message '                  + Cr +
    'to: mailto:%LIST_EMAIL_ADDRESS%?subject=join'          + Cr(2) +
    'Hope to hear from you again.'                          + Cr(2) +
    'Rgs,'                                                  + Cr(2) +
    '%LIST_MASTER_NAME%.' ;

  RejectMessageSizeMessage :=
    'Sorry, we where not able to send you message onto the list ' + Cr +
    'members because it is too big.'                              + Cr(2) +
    'You can send messages up to %MESSAGE_SIZE_LIMIT%kb in size.' + Cr(2) +
    'Please send the message again with the text reduced.'        + Cr(2) +
    'Sorry for the inconvenience.' ;

  NonListMemberMessage     :=
    'Sorry, you are not currently a member of the %LIST_NAME_LONG%' + Cr +
    ' mailing list and you post to the '                            + Cr +
    'list has been rejected.'                                       + Cr(2) +
    'If you would like to join the list, please send a message '    + Cr +
    'to mailto:%LIST_EMAIL_ADDRESS%?subject=join'                   + Cr(2) +
    'Once again, we apologise for the inconvenience and hope '      + Cr +
    'you will understand that this is the best way to keep the '    + Cr +
    'list membership up to date.'                                   + Cr(2) +
    'We look forward from hearing from you again.'                  + Cr(2) +
    'Rgs,'                                                          + Cr(2) +
    '%LIST_MASTER_NAME%.' ;

  RejectAttachmentMessage  :=
    'Sorry, you are not able to send a message with an attachment ' + Cr +
    'to this list.'                                                 + Cr(2) +
    'Please send the message again without the attachment.'         + Cr(2) +
    'You will ofter find that only a few list members will be '     + Cr +
    'wanting to receive an attachment. The way around this is to '  + Cr +
    'send a message asking for members who want the attachment to ' + Cr +
    'respond directly to you. You can then send the attachment '    + Cr +
    'straight to them.'                                             + Cr(2) +
    'Sorry for the inconvenience.'                                  + Cr(2) +
    '%LIST_MASTER_NAME%.' ;

  FooterMessage            :=
    'To send a message to the list '                                + Cr +
    'mailto:%LIST_EMAIL_ADDRESS%?body=your_message '             + Cr +
    'To leave the list mailto:%LIST_EMAIL_ADDRESS%?subject=leave';

end;

destructor TtiLstSrvrCfg.Destroy;
begin
  FListMembers.Free;
  FLastMessageCRC.Free ;
  inherited;
end;

function TtiLstSrvrCfg.GetAcceptPostsFromAsString: string;
begin
  result := cAcceptPostsFromDB[FAcceptPostsFrom];
end;

function TtiLstSrvrCfg.GetCaption: string;
begin
  result := ListName ;
  if Not Active then
    result := result + ' [Not active]' ;
end;

function TtiLstSrvrCfg.GetMessageIndex: integer;
begin
  result := FMessageIndex ;
end;

function TtiLstSrvrCfg.GetOwner: TtiLstSrvrCfgs;
begin
  result := TtiLstSrvrCfgs( inherited GetOwner ) ;
end;

function TtiLstSrvrCfg.GetSenderAdrsTypeAsString: string;
begin
  result := cSenderAdrsTypesDB[FSenderAdrsType];
end;

procedure TtiLstSrvrCfg.IncMessageIndex;
begin
  Inc( FMessageIndex ) ;
  Dirty := true ;
  Save ;
end;

procedure TtiLstSrvrCfg.SetAcceptPostsFromAsString(const Value: string);
begin
  FAcceptPostsFrom := DBStringToAcceptPostsFrom(Value);
end;

procedure TtiLstSrvrCfg.SetMessageIndex(const Value: integer);
begin
  FMessageIndex := Value ;
end;

procedure TtiLstSrvrCfg.SetOwner(const Value: TtiLstSrvrCfgs);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLstSrvrCfgs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiLstSrvrCfgs.Add(pObject: TtiLstSrvrCfg; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

constructor TtiLstSrvrCfgs.Create;
begin
  inherited;
  FMessageMacros := TMessageMacros.Create ;
  FMessageMacros.RegisterMacro( cuLIST_NAME_LONG, 'ListNameLong' ) ;
  FMessageMacros.RegisterMacro( cuLIST_MASTER_NAME, 'ListMasterName' ) ;
  FMessageMacros.RegisterMacro( cuLIST_EMAIL_ADDRESS, 'ListEMailAdrs' ) ;
  FMessageMacros.RegisterMacro( cuLIST_MASTER_EMAIL_ADDRESS, 'ListMasterEMailAdrs' ) ;
  FMessageMacros.RegisterMacro( cuMESSAGE_SIZE_LIMIT, 'MaxMessageSize' ) ;
end;

destructor TtiLstSrvrCfgs.Destroy;
begin
  FMessageMacros.Free ;
  inherited;
end;

function TtiLstSrvrCfgs.GetCaption: string;
begin
  result := 'TI Group Mail Accounts' ;
end;

function TtiLstSrvrCfgs.GetItems(i: integer): TtiLstSrvrCfg;
begin
  result := TtiLstSrvrCfg( inherited GetItems( i )) ;
end;

procedure TtiLstSrvrCfgs.SetItems(i: integer; const Value: TtiLstSrvrCfg);
begin
  inherited SetItems( i, Value ) ;
end;

{ TListMembers }

procedure TListMembers.Add(pObject: TListMember; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TListMembers.AddByEMailAddress(const pEMailAddress: string): boolean;
var
  lData : TListMember ;
begin
  result := false ;
  if FindByEMailAddress( pEMailAddress ) <> nil then
    Exit ; //==>
  result := true ;
  lData := TListMember.CreateNew ;
  lData.EMailAddress := pEMailAddress ;
  Add( lData ) ;
end;

function TListMembers.DeleteByEMailAddress(const pEMailAddress: string): boolean;
var
  lData : TListMember ;
begin
  lData := FindByEMailAddress( pEMailAddress ) ;
  if lData <> nil then
    lData.Deleted := true ;
  result := lData <> nil ;
end;

function TListMembers.FindByEMailAddress(const pEMailAddress: string): TListMember;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( pEMailAddress, Items[i].EMailAddress ) and
       ( not Items[i].Deleted ) then
    begin
      result := Items[i];
      Exit ; //==>
    end ;
end;

function TListMembers.GetItems(i: integer): TListMember;
begin
  result := TListMember( inherited GetItems( i )) ;
end;

function TListMembers.GetOID: TOID;
begin
  result := Owner.OID ;
end;

function TListMembers.GetOwner: TtiLstSrvrCfg;
begin
  result := TtiLstSrvrCfg( inherited GetOwner ) ;
end;

function TListMembers.IsMember(const pEMailAddress: string): boolean;
begin
  result := ( FindByEMailAddress( pEMailAddress ) <> nil ) ;
end;

procedure TListMembers.LoadFromFile(const pFileName: TFileName);
var
  lsl : TStringList ;
  i : integer ;
begin
  lsl := TStringList.Create ;
  try
    lsl.LoadFromFile( pFileName ) ;
    for i := 0 to lsl.Count - 1 do
      AddByEMailAddress( lsl.Strings[i] ) ;
  finally
    lsl.Free ;
  end ;
end;

procedure TListMembers.SetItems(i: integer; const Value: TListMember);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TListMembers.SetOwner(const Value: TtiLstSrvrCfg);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TListMember
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TListMember.GetOwner: TtiLstSrvrCfg;
begin
  result := TtiLstSrvrCfg( inherited GetOwner ) ;
end;

procedure TListMember.SetOwner(const Value: TtiLstSrvrCfg);
begin
  inherited SetOwner( Value ) ;
end;

{ TMessageMacros }

procedure TMessageMacros.Add(pObject: TMessageMacro; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TMessageMacros.GetItems(i: integer): TMessageMacro;
begin
  result := TMessageMacro( inherited GetItems( i )) ;
end;

function TMessageMacros.GetOwner: TtiLstSrvrCfgs;
begin
  result := TtiLstSrvrCfgs( inherited GetOwner ) ;
end;

procedure TMessageMacros.RegisterMacro(const pPlaceHolder, pPropertyName: string);
var
  lMessageMacro : TMessageMacro ;
begin
  lMessageMacro := TMessageMacro.Create ;
  lMessageMacro.PlaceHolder := pPlaceHolder ;
  lMessageMacro.PropertyName := pPropertyName ;
  lMessageMacro.ObjectState := posClean ;
  Add( lMessageMacro ) ;
end;

procedure TMessageMacros.SetItems(i: integer; const Value: TMessageMacro);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TMessageMacros.SetOwner(const Value: TtiLstSrvrCfgs);
begin
  inherited SetOwner( Value ) ;
end;

{ TMessageMacro }

function TMessageMacro.GetOwner: TMessageMacros;
begin
  result := TMessageMacros( inherited GetOwner ) ;
end;

procedure TMessageMacro.SetOwner(const Value: TMessageMacros);
begin
  inherited SetOwner( Value ) ;
end;

function TMessageMacro.Value(pLstSrvrCfg: TtiLstSrvrCfg): string;
begin
  result := GetStrProp( pLstSrvrCfg, PropertyName ) ;
end;

procedure TtiLstSrvrCfg.SetSenderAdrsTypeAsString(const Value: string);
begin
  FSenderAdrsType := DBStringToSenderAdrsType(Value);
end;

end.

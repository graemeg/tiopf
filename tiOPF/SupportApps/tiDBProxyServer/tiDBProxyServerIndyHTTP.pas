unit tiDBProxyServerIndyHTTP;

{$I tiDefines.inc}

interface
uses
   Classes
  ,SysUtils
  ,ExtCtrls
  ,Contnrs
  ,tiObjAbs
  ,IdHTTPServer
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPServer
  ,IdCustomHTTPServer
  ,IdContext
  ,tiQueryRemote_Svr
  ,tiXML
  ;

const
  cErrorInServerExtension = 'Error in server extension: %s '#13#13'%s' ;

type

  TtiDBProxyServerIndyHTTP = class ;

  TtiDBProxyServerIndyHTTPAction = class( TtiObjAbs )
  private
    FOwner: TtiDBProxyServerIndyHTTP;
  protected
    procedure GetReturnPage(const pResponseInfo: TIdHTTPResponseInfo ; const pPageFileName : string);
    function  StaticPageLocation : string ;
    function  CGIBinLocation : string ;
  public
    constructor Create(const pOwner : TtiDBProxyServerIndyHTTP ) ;
    function    CanExecute(const pDocument: string): boolean ; virtual ; abstract ;
    procedure   Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); virtual ; abstract ;
    property    Owner : TtiDBProxyServerIndyHTTP read FOwner write FOwner ;
    function    Clone : TtiDBProxyServerIndyHTTPAction ;
  end ;

  TtiDBPS_ExecuteRemoteXML = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  // For a legacy system. Use TtiDBPS_TestAlive1 in new applications
  TtiDBPS_TestAlive = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_TestAlive1 = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_TestHTML = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_TestXML = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_Default = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_CanNotFindPage = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_GetLogFile = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_ServerVersion = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBPS_CanFindPage = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  // ToDo: Not a true CGI interface, but a hack. Change this to support true CGI
  TtiDBPS_RunCGIExtension = class( TtiDBProxyServerIndyHTTPAction )
  public
    function  CanExecute(const pDocument: string): boolean ; override ;
    procedure Execute(const pDocument : string ; const pRequestInfo: TIdHTTPRequestInfo; const pResponseInfo: TIdHTTPResponseInfo); override ;
  end ;

  TtiDBProxyServerIndyHTTP = class( TtiObjAbs )
  private
    FIdHTTPServer: TIdHTTPServer;
    FServerActions: TObjectList;
    FTmrStartServer : TTimer ;
    FStaticPageLocation: string;
    FCGIBinLocation: string;
    FXMLTags: TtiXMLTags;
//    procedure DoIDHTTPServerCommandGet(pThread: TIdPeerThread;
//                                     pRequestInfo: TIdHTTPRequestInfo;
//                                     pResponseInfo: TIdHTTPResponseInfo);
    procedure DoIDHTTPServerCommandGet(pContext:TIdContext;
                                       pRequestInfo: TIdHTTPRequestInfo;
                                       pResponseInfo: TIdHTTPResponseInfo);
    procedure DoTimerStartServer(Sender: TObject);
    procedure DoLog(const pMessage : string);
    procedure SetStaticPageLocation(const Value: string);
    procedure SetCGIBinLocation(const Value: string);
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    StaticPageLocation : string read FStaticPageLocation write SetStaticPageLocation ;
    property    CGIBinLocation     : string read FCGIBinLocation     write SetCGIBinLocation ;
    property    XMLTags            : TtiXMLTags read FXMLTags ;
  end ;


implementation
uses
  tiDialogs
  ,tiUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiLog
  ,Windows
  ,tiPersist
  ,tiDBProxyServerStats
  ,tiDBConnectionPool
  ,mlConsoleApp
  ,cTIDBProxyServerConstants
  ,cTIPersist
  ;

const
  cRetryInterval = 10 ; // Try again to start the HTTP server in X seconds
  cDefaultPageName = 'default.htm' ;

{ TtiDBProxyServerIndyHTTP }

constructor TtiDBProxyServerIndyHTTP.Create;
begin
  inherited ;
  FXMLTags := TtiXMLTags.Create;
  FXMLTags.OptXMLDBSize := optDBSizeOn;

  FServerActions:= TObjectList.Create(true);
  FServerActions.Add(TtiDBPS_ExecuteRemoteXML.Create(Self));
  FServerActions.Add(TtiDBPS_TestAlive1.Create(Self));
  FServerActions.Add(TtiDBPS_TestHTML.Create(Self));
  FServerActions.Add(TtiDBPS_TestXML.Create(Self));
  FServerActions.Add(TtiDBPS_Default.Create(Self));
  FServerActions.Add(TtiDBPS_CanFindPage.Create(Self));
  FServerActions.Add(TtiDBPS_GetLogFile.Create(Self));
  FServerActions.Add(TtiDBPS_ServerVersion.Create(Self));
  FServerActions.Add(TtiDBPS_RunCGIExtension.Create(Self));
  FServerActions.Add(TtiDBPS_TestAlive.Create(Self));
  // This one must be last...
  FServerActions.Add(TtiDBPS_CanNotFindPage.Create(Self));

  StaticPageLocation := tiAddTrailingSlash(tiAddTrailingSlash(tiGetEXEPath) + cStaticPageDir) ;
  CGIBinLocation     := tiAddTrailingSlash(tiAddTrailingSlash(tiGetEXEPath) + cCGIBinDir) ;
  FIdHTTPServer := TIdHTTPServer.Create(Nil);
  FIdHTTPServer.OnCommandGet := DoIDHTTPServerCommandGet;
  FIdHTTPServer.KeepAlive := true ;
  FTmrStartServer := TTimer.Create(nil);
  FTmrStartServer.Interval := 100 ;
  FTmrStartServer.OnTimer := DoTimerStartServer ;
  FTmrStartServer.Enabled := true ;

end;

destructor TtiDBProxyServerIndyHTTP.Destroy;
begin
  FIdHTTPServer.Free;
  FTmrStartServer.Free;
  FServerActions.Free;
  FXMLTags.Free;
  inherited;
end;

procedure TtiDBProxyServerIndyHTTP.DoTimerStartServer(Sender : TObject);
begin
  FTmrStartServer.Interval := cRetryInterval * 1000 ;
  try
    if not DirectoryExists(FStaticPageLocation) then
      ForceDirectories(FStaticPageLocation);

    if not DirectoryExists(FStaticPageLocation) then
      raise exception.create('Unable to locate or create directory for static pages <' + FStaticPageLocation + '>');

    DoLog('Attempting to start HTTP server on port ' + IntToStr(FidHTTPServer.DefaultPort ));
    FIdHTTPServer.Active := true ;

    FTmrStartServer.Enabled := false ;
    DoLog('HTTP server started');
    DoLog('Static web pages will be read from <' + FStaticPageLocation + '>');

  except
    on e:exception do
    begin
      DoLog( 'Unable to start HTTP server (will try again in ' +
             IntToStr( cRetryInterval ) + ' seconds)' + Cr(2) +
             'Error message: ' + e.Message ) ;
    end ;
  end;
end ;

procedure TtiDBProxyServerIndyHTTP.DoIDHTTPServerCommandGet(
  pContext:TIdContext; pRequestInfo: TIdHTTPRequestInfo;
  pResponseInfo: TIdHTTPResponseInfo);
var
  lDocument : string ;
  i : integer ;
  lServerAction : TtiDBProxyServerIndyHTTPAction ;
begin
  try
    lDocument := pRequestInfo.Document ;
    if lDocument[1] = '/' then
      lDocument := Copy( lDocument, 2, Length(lDocument) - 1 ) ;
    for i := 0 to FServerActions.Count - 1 do
      if ( FServerActions.Items[i] as TtiDBProxyServerIndyHTTPAction ).CanExecute(lDocument) then
      begin
        lServerAction :=
          ( FServerActions.Items[i] as TtiDBProxyServerIndyHTTPAction ).Clone ;
        try
          lServerAction.Execute(lDocument, pRequestInfo, pResponseInfo);
        finally
          lServerAction.Free;
        end;
        Exit ; //==>
      end ;
    // Should not get here - unless can not find page.
    pResponseInfo.ContentText := Format(cErrorCanNotFindPage, [lDocument]);
  except
    on e:exception do
    begin
      LogError(e.message);
      pResponseInfo.ContentText := Format(cErrorOnServer,[e.Message]);
    end ;
  end;
end;

{
procedure TtiDBProxyServerIndyHTTP.DoIDHTTPServerCommandGet(
  pThread: TIdPeerThread; pRequestInfo: TIdHTTPRequestInfo;
  pResponseInfo: TIdHTTPResponseInfo);
var
  lDocument : string ;
  i : integer ;
  lServerAction : TtiDBProxyServerIndyHTTPAction ;
begin
  try
    lDocument := pRequestInfo.Document ;
    if lDocument[1] = '/' then
      lDocument := Copy( lDocument, 2, Length(lDocument) - 1 ) ;
    for i := 0 to FServerActions.Count - 1 do
      if ( FServerActions.Items[i] as TtiDBProxyServerIndyHTTPAction ).CanExecute(lDocument) then
      begin
        lServerAction :=
          ( FServerActions.Items[i] as TtiDBProxyServerIndyHTTPAction ).Clone ;
        try
          lServerAction.Execute(lDocument, pRequestInfo, pResponseInfo);
        finally
          lServerAction.Free;
        end;
        Exit ; //==>
      end ;
    // Should not get here - unless can not find page.
    pResponseInfo.ContentText := Format(cErrorCanNotFindPage, [lDocument]);
  except
    on e:exception do
    begin
      LogError(e.message);
      pResponseInfo.ContentText := Format(cErrorOnServer,[e.Message]);
    end ;
  end;
end;
}

procedure TtiDBProxyServerIndyHTTP.DoLog(const pMessage: string);
begin
  Log(pMessage);
//  if Assigned(FDoLogToMainForm) then
//    FDoLogToMainForm(pMessage,false,true);
end;

procedure TtiDBProxyServerIndyHTTP.SetStaticPageLocation(
  const Value: string);
begin
  FStaticPageLocation := Value;
  if FStaticPageLocation <> '' then
    FStaticPageLocation := tiAddTrailingSlash(FStaticPageLocation);
end;

procedure TtiDBProxyServerIndyHTTP.SetCGIBinLocation(const Value: string);
begin
  FCGIBinLocation := Value;
end;

{ TtiDBPS_ExecuteRemoteXML }

function TtiDBPS_ExecuteRemoteXML.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxy ) ;
end;

procedure TtiDBPS_ExecuteRemoteXML.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  pResponseInfo.ContentText := ExecuteRemoteXML( pRequestInfo.UnparsedParams);
end;

{ TtiDBPS_TestAlive1 }

function TtiDBPS_TestAlive1.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyTestAlive1 ) ;
end;

procedure TtiDBPS_TestAlive1.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
var
  lPooledDB : TPooledDB ;
  lResult   : string ;
  lDBConnectionName : string ;
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  lDBConnectionName := gTIPerMgr.DefaultPerLayer.DefaultDBConnectionName ;
  try
    lPooledDB := gTIPerMgr.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName ) ;
    try
      if lPooledDB.Database.Test then
        lResult := Owner.XMLTags.ProxyTestPassed
      else
        lResult := Owner.XMLTags.ProxyTestFailed;
    finally
      gTIPerMgr.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, lPooledDB);
    end ;
  except
    on e:exception do
      lResult := Owner.XMLTags.ProxyTestFailed;
  end ;
  pResponseInfo.ContentText := lResult;
end;

{ TtiDBPS_TestHTML }

function TtiDBPS_TestHTML.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyTestHTML );
end;

procedure TtiDBPS_TestHTML.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
var
  lDBProxyServerStats : TtiDBProxyServerStats;
begin
  // Don't log this. AutoRefresh will cause the log to become full 
  //Log('Processing document <' + pDocument +
  //    ' in <' + ClassName + '>' );
  lDBProxyServerStats := TtiDBProxyServerStats.Create;
  try
    // ToDo: Read RefreshRate from RequestInfo
    lDBProxyServerStats.TestRefreshRate := 1 ;
    lDBProxyServerStats.Execute;
    pResponseInfo.ContentText := lDBProxyServerStats.AsHTML;
  finally
    lDBProxyServerStats.Free;
  end;
end;

{ TtiDBPS_TestXML }

function TtiDBPS_TestXML.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyTestXML );
end;

procedure TtiDBPS_TestXML.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
var
  lDBProxyServerStats : TtiDBProxyServerStats;
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  lDBProxyServerStats := TtiDBProxyServerStats.Create;
  try
    // ToDo: Read RefreshRate from RequestInfo
    lDBProxyServerStats.TestRefreshRate := 1 ;
    lDBProxyServerStats.Execute;
    pResponseInfo.ContentText := lDBProxyServerStats.AsXML;
  finally
    lDBProxyServerStats.Free;
  end;
end;

{ TtiDBPS_Default }

function TtiDBPS_Default.CanExecute(const pDocument: string): boolean;
begin
  result := (( pDocument = '' ) and
             ( FileExists( StaticPageLocation + cDefaultPageName ))) or
            ( SameText( pDocument, cDefaultPageName ) and
             ( FileExists( StaticPageLocation + cDefaultPageName ))) ;
end;

procedure TtiDBPS_Default.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  GetReturnPage(pResponseInfo, StaticPageLocation + cDefaultPageName ) ;
end;

{ TtiDBProxyServerIndyHTTPAction }

function TtiDBProxyServerIndyHTTPAction.CGIBinLocation: string;
begin
  Assert(Owner <> nil, 'Owner not assigned' ) ;
  result := Owner.CGIBinLocation ;
end;

function TtiDBProxyServerIndyHTTPAction.Clone: TtiDBProxyServerIndyHTTPAction;
begin
  result := ( ClassType.Create as TtiDBProxyServerIndyHTTPAction );
  result.Owner := Owner ;
end;

constructor TtiDBProxyServerIndyHTTPAction.Create(
  const pOwner: TtiDBProxyServerIndyHTTP);
begin
  inherited Create;
  FOwner := pOwner ;
end;

procedure TtiDBProxyServerIndyHTTPAction.GetReturnPage(
  const pResponseInfo: TIdHTTPResponseInfo; const pPageFileName: string);
var
  lExt : string ;
begin
  lExt := LowerCase(tiExtractExtension(pPageFileName));
  // ToDo: TtiDBProxyServerIndyHTTP.GetReturnPage will only return text, gif,
  //       jpg, exe or zip pages. Extend this functionality
  if ( lExt = 'gif' ) or
     ( lExt = 'jpg' ) or
     ( lExt = 'exe' ) or
     ( lExt = 'zip' ) then
  begin
    pResponseInfo.FreeContentStream := true ;
    pResponseInfo.ContentStream := TFileStream.Create(pPageFileName, fmOpenRead or fmShareDenyNone );
    pResponseInfo.ContentEncoding := 'MIME'
  end else
    pResponseInfo.ContentText := tiFileToString(pPageFileName);
end;

function TtiDBProxyServerIndyHTTPAction.StaticPageLocation: string;
begin
  Assert(Owner <> nil, 'Owner not assigned' ) ;
  result := Owner.StaticPageLocation ;
end;

{ TtiDBPS_CanNotFindPage }

function TtiDBPS_CanNotFindPage.CanExecute(const pDocument: string): boolean;
var
  lFileNameStatic : string ;
  lFileNameCGI : string;
begin

  if ( pDocument = '' ) and ( not FileExists( StaticPageLocation + cDefaultPageName )) then
  begin
    result := true;
    Exit ; //==>
  end ;

  if ( pDocument = '' ) and ( FileExists( StaticPageLocation + cDefaultPageName )) then
  begin
    result := false;
    Exit ; //==>
  end ;

  lFileNameStatic := StaticPageLocation + pDocument;
  lFileNameCGI := CGIBinLocation + pDocument;

  if ( pDocument <> '' ) and
     ((( not FileExists( lFileNameStatic ))) and
      (( not FileExists( lFileNameCGI )))) then
  begin
    result := true ;
    Exit ; //==>
  end ;

  result := false ;

end;

procedure TtiDBPS_CanNotFindPage.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  pResponseInfo.ContentText := Format(cErrorCanNotFindPage, [pDocument]);
  pResponseInfo.ResponseNo := cHTTPResponseCodePageNotFound ;
end;

{ TtiDBPS_CanFindPage }

function TtiDBPS_CanFindPage.CanExecute(const pDocument: string): boolean;
begin
  result := ( pDocument <> '' ) and
            ( FileExists( StaticPageLocation + pDocument ));
end;

procedure TtiDBPS_CanFindPage.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  GetReturnPage(pResponseInfo, StaticPageLocation + pDocument );
end;

{ TtiDBPS_GetLogFile }

function TtiDBPS_GetLogFile.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyGetLog ) ;
end;

procedure TtiDBPS_GetLogFile.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  if FileExists(gLog.LogFileName) then
    pResponseInfo.ContentText :=
      '<HTML><PRE>' +
      tiFileToString(gLog.LogFileName) +
      '</PRE></HTML>'
  else
    pResponseInfo.ContentText := Format(cErrorCanNotFindLogFile, [gLog.LogFileName]);
end;

{ TtiDBPS_ServerVersion }

function TtiDBPS_ServerVersion.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyServerVersion ) ; 
end;

procedure TtiDBPS_ServerVersion.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  pResponseInfo.ContentText := Owner.XMLTags.XMLVersion ;
end;

{ TtiDBPS_RunCGIExtension }

function TtiDBPS_RunCGIExtension.CanExecute(const pDocument: string): boolean;
begin
  result := ( pDocument <> '' ) and
            ( FileExists( CGIBinLocation + pDocument ) and
            ( SameText( ExtractFileExt(pDocument), '.exe')));
end;

procedure TtiDBPS_RunCGIExtension.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
var
  lCGI : string ;
  lParams : string ;
  ls : string ;
  lExitCode : Integer ;
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  try
    lCGI := CGIBinLocation + pDocument ;
    lParams := pRequestInfo.UnparsedParams ;
    ls := '' ;
    Log('About to call ' + lCGI + ' ' + lParams);
    lExitCode := ExecConsoleApp(lCGI, lParams, ls, nil, false);
    if lExitCode = 0 then
      pResponseInfo.ContentText := ls
    else
    begin
      pResponseInfo.ResponseNo := cHTTPResponseCodeInternalError;
      pResponseInfo.ResponseText := Format(cErrorHTTPCGIExtension, [pDocument, lExitCode]);
      LogError(pResponseInfo.ResponseText, false);
    end ;
  except
    on e:exception do
    begin
      LogError(e.message, false);
      pResponseInfo.ResponseNo  := cHTTPResponseCodeInternalError  ;
      pResponseInfo.ResponseText :=
        Format( cErrorInServerExtension, [pDocument, e.message]); ;
    end ;
  end ;
end;

{ TtiDBPS_TestAlive }

function TtiDBPS_TestAlive.CanExecute(const pDocument: string): boolean;
begin
  result := SameText( pDocument, cgTIDBProxyTestAlive ) ;
end;

procedure TtiDBPS_TestAlive.Execute(const pDocument: string;
  const pRequestInfo: TIdHTTPRequestInfo;
  const pResponseInfo: TIdHTTPResponseInfo);
var
  lPooledDB : TPooledDB ;
  lResult   : string ;
  lDBConnectionName : string ;
begin
  Log('Processing document <' + pDocument +
      ' in <' + ClassName + '>' );
  lDBConnectionName := gTIPerMgr.DefaultPerLayer.DefaultDBConnectionName ;
  try
    lPooledDB := gTIPerMgr.DefaultPerLayer.DBConnectionPools.Lock(lDBConnectionName ) ;
    try
      if lPooledDB.Database.Test then
        lResult :=
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="passed" />' +
          '</a>'
      else
        lResult :=
          '<?xml version="1.0" ?>' +
          '<a>' +
          '<tidbproxytestalive status="failed" />' +
          '</a>'
    finally
      gTIPerMgr.DefaultPerLayer.DBConnectionPools.UnLock(lDBConnectionName, lPooledDB);
    end ;
  except
    on e:exception do
      lResult := Owner.XMLTags.ProxyTestFailed;
  end ;
  pResponseInfo.ContentText := lResult;
end;

end.

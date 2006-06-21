unit tiFileSyncReader_Remote_Svr;

interface
uses
  tiObjAbs
  ,Classes
  ,IdHTTPServer
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPServer
  ,IdCustomHTTPServer
  ;

const
  cErrorOnServer = 'Error on tiFileSyncHTTP remote server <%s>';

type

  TtiFileSyncRemoteServer = class(TtiObjAbs)
  private
    FHTTPServer : TidHTTPServer;
    FParams: TStringList;
    procedure DoIDHTTPServerCommandGet(pThread: TIdPeerThread;
                                       pRequestInfo: TIdHTTPRequestInfo;
                                       pResponseInfo: TIdHTTPResponseInfo);
    function    GetActive: Boolean;
    procedure   SetActive(const Value: Boolean);
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Active: Boolean read GetActive Write SetActive;
  end ;

implementation
uses
  Dialogs // For debugging
  ,SysUtils
  ,tiLog
  ,tiUtils
  ,tiFileSyncReader_DiskFiles
  ,cFileSync
  ;

{ TtiFileSyncRemoteServer }

constructor TtiFileSyncRemoteServer.Create;
begin
  inherited ;
  FHTTPServer := TidHTTPServer.Create(nil);
  FHTTPServer.OnCommandGet := DoIDHTTPServerCommandGet;
  FParams:= TStringList.Create;
end;

destructor TtiFileSyncRemoteServer.Destroy;
begin
  FParams.Free;
  FHTTPServer.Free;
  inherited;
end;

procedure TtiFileSyncRemoteServer.DoIDHTTPServerCommandGet(
  pThread: TIdPeerThread; pRequestInfo: TIdHTTPRequestInfo;
  pResponseInfo: TIdHTTPResponseInfo);
var
  lCommand : string ;
  lData: string ;
  lFSR : TFileSyncReaderDiskFiles;
  ls : string ;
begin
  try
    FParams.CommaText := pRequestInfo.UnparsedParams;
    lCommand:= FParams.Values[cHTTPParamNameCommand];
    lData:=    FParams.Values[cHTTPParamNameData];
    lFSR := TFileSyncReaderDiskFiles.Create;
    try
      ls := lFSR.Execute(lCommand, lData );
      pResponseInfo.ContentText := ls ;
    finally
      lFSR.Free;
    end;
  except
    on e:exception do
    begin
      LogError(e.message);
      pResponseInfo.ContentText := Format(cErrorOnServer,[e.Message]);
    end ;
  end;
end;

function TtiFileSyncRemoteServer.GetActive: Boolean;
begin
  Result := FHTTPServer.Active;
end;

procedure TtiFileSyncRemoteServer.SetActive(const Value: Boolean);
begin
  FHTTPServer.Active := Value;
end;

end.

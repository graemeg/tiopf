unit tiFileSyncReader_Remote_Svr;

interface
uses
  tiBaseObject
  ,Classes
  ,IdHTTPServer
  ,IdBaseComponent
  ,IdComponent
  ,IdTCPServer
  ,IdCustomHTTPServer
  ,IdContext
  ;

const
  cErrorOnServer = 'Error on tiFileSyncHTTP remote server <%s>';

type

  TtiFileSyncRemoteServer = class(TtiBaseObject)
  private
    FHTTPServer : TidHTTPServer;
    FParams: TStringList;
    procedure DoIDHTTPServerCommandGet(AContext:TIdContext;
                                       ARequestInfo: TIdHTTPRequestInfo;
                                       AResponseInfo: TIdHTTPResponseInfo);
    function    GetActive: Boolean;
    procedure   SetActive(const Value: Boolean);
  public
    constructor Create(APort: Integer);
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
  ,tiWebServerConstants
  ,cFileSync
  ;

{ TtiFileSyncRemoteServer }

constructor TtiFileSyncRemoteServer.Create(APort: Integer);
begin
  inherited Create;
  FHTTPServer := TidHTTPServer.Create(nil);
  FHTTPServer.DefaultPort:= APort;
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
  AContext:TIdContext;
  ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  lCommand : string ;
  lData: string ;
  lFSR : TFileSyncReaderDiskFiles;
  ls : string ;
begin
  try
    FParams.CommaText := ARequestInfo.UnparsedParams;
    lCommand:= FParams.Values[cHTTPParamNameCommand];
    lData:=    FParams.Values[cHTTPParamNameData];
    lFSR := TFileSyncReaderDiskFiles.Create;
    try
      ls := lFSR.Execute(lCommand, lData );
      AResponseInfo.ContentText := ls ;
      AResponseInfo.ContentType := cHTTPContentTypeTextHTML;
    finally
      lFSR.Free;
    end;
  except
    on e:exception do
    begin
      LogError(e.message);
      AResponseInfo.ContentText := Format(cErrorOnServer,[e.Message]);
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

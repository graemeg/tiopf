unit tiCGIDBConnection;

interface

const
  cErrorReadingDBConnectionFromINI = 'Database connection details not found in <%s> Looking for [%s] %s, %s, %s';

function  tiGetCGIAppINIFileName: string;
procedure tiCGIConnectToDB(const pINIFileName: string = '');
procedure tiCGIGetDBConnectionDetails(var pDatabaseName, pUserName, pPassword: string; const pINIFileName: string = '');

implementation
uses
   tiQueryXMLLight
  ,tiOPFManager
  ,SysUtils
  ,tiUtils
  ,tiConstants
  ,cOPDMS
  ,tiPersistenceLayers
  ,tiRegINI
  ,tiWebServerConstants
  ,tiDialogs
  ;

function tiGetCGIAppINIFileName: string;
begin
  Result := ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + '..\' + cTIDBProxyServerININame );
end;

function tiGetAppINIFileName(const pINIFilename: string = ''): string;
var
  lFileName: string;
  lPathName: string;
begin
  if pINIFilename = '' then
    Result := ExpandFileName( tiAddTrailingSlash(tiGetEXEPath) + cTIDBProxyServerININame )
  else begin
    lFileName := ExtractFileName(pINIFileName);
    lPathName := ExtractFilePath(pINIFileName);
    if lFileName = '' then
      lFileName := cTIDBProxyServerININame;
    if lPathName = '' then
      lPathName := tiAddTrailingSlash(tiGetEXEPath) + '..\'
    else
      lPathName := tiAddTrailingSlash(lPathName) ;
    Result := ExpandFileName( lPathName + lFileName );
  end;
end;

procedure tiCGIGetDBConnectionDetails(var pDatabaseName, pUserName, pPassword: string; const pINIFileName: string = '');
var
  lINI : TtiINIFile;
begin
  lINI := TtiINIFile.Create(tiGetAppINIFileName(pINIFileName));
  try
    pDatabaseName := lINI.ReadString(cTIDBProxyServerINIDBSection, cTIDBProxyServerINIDBNameIndent,     ''  ) ;
    pUserName     := lINI.ReadString(cTIDBProxyServerINIDBSection, cTIDBProxyServerINIDBUserIndent,     '' ) ;
    pPassword     := lINI.ReadString(cTIDBProxyServerINIDBSection, cTIDBProxyServerINIDBPasswordIndent, '' ) ;
  finally
    lINI.Free;
  end;
end ;

procedure tiCGIConnectToDB(const pINIFileName: string = '');
var
  lPerLayerName   : string ;
  lDatabaseName   : string ;
  lUserName       : string ;
  lPassword       : string ;
  lError          : string ;
  lINIFileName    : string ;
begin
  lPerLayerName := gTIOPFManager.DefaultPerLayerName ;
  Assert( lPerLayerName <> cTIPersistXMLLight, 'Default PerLayerName is ' +
          cTIPersistXMLLight + '. You must include tiQuery??? ahead of tiQueryXMLLight' ) ;

  gTIOPFManager.TerminateOnFailedDBConnection := False ;
  gTIOPFManager.VisMgr.BreakOnException := True ;
  if pINIFileName = '' then
    lINIFileName := tiGetCGIAppINIFileName
  else
    lINIFileName := pINIFileName;

  tiCGIGetDBConnectionDetails(lDatabaseName, lUserName, lPassword, lINIFileName );
  if (lDatabaseName = '' ) or
     (lUserName = '' ) or
     (lPassword = '' ) then
    raise Exception.CreateFmt( cErrorReadingDBConnectionFromINI, [pINIFileName, cTIDBProxyServerINIDBSection,
             cTIDBProxyServerINIDBNameIndent, cTIDBProxyServerINIDBUserIndent, cTIDBProxyServerINIDBPasswordIndent]);

  if gTIOPFManager.DefaultDBConnectionName = lDatabaseName then
    Exit ; //==>

  try
    gTIOPFManager.ConnectDatabase( lDatabaseName, lUserName, lPassword, '', lPerLayerName ) ;
    gTIOPFManager.DefaultDBConnectionName := lDatabaseName ;
    gTIOPFManager.DefaultPerLayerName := lPerLayerName;
  except
    on e:Exception do
    begin
      lError := e.message ;
      lError := tiStrTran(lError, #10, '');
      lError := tiStrTran(lError, #13, ' ');
      lError := tiStrTran(lError, '  ', ' ');
      lError := tiStrTran(lError, '  ', ' ');
      lError := tiStrTran(lError, '  ', ' ');
      raise Exception.Create(lError);
    end;
  end;
end ;

initialization

end.

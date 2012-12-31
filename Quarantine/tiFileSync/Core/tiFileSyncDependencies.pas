unit tiFileSyncDependencies;

interface
uses
   tiOIDGUID
  ,cFileSync
  ,tiFileSyncReader_Abs
  ,tiFileSyncReader_DiskFiles
  ,tiFileSyncReader_Remote
  ,tiFileSyncSetup_BOM
  ,tiFileName_BOM
  ,tiFileSync_Mgr

  ,tiCompressZLib
  ,tiHTTPIndy
 ;

procedure ConnectToDatabase;
procedure RegisterMappings;

implementation
uses
   tiUtils
  ,tiOPFManager
  ,tiConstants
 ;

procedure RegisterMappings;
begin
  tiFileSyncSetup_BOM.RegisterMappings;
end;

procedure ConnectToDatabase;
var
  lDB : string;
begin
  lDB := tiSwapExt(ParamStr(0), 'xml');
  if not gTIOPFManager.DefaultPerLayer.DatabaseExists(lDB,  'null', 'null') then
  begin
    gTIOPFManager.DefaultPerLayer.CreateDatabase(lDB,  'null', 'null');
    gTIOPFManager.ConnectDatabase(lDB,  'null', 'null', cTIPersistXMLLight);
    tiFileSyncSetup_BOM.CreateTables;
  end else
    gTIOPFManager.ConnectDatabase(lDB,  'null', 'null', cTIPersistXMLLight);
end;

end.

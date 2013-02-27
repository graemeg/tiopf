unit tiFileSyncDependencies;

interface
uses
//  tiQueryXMLLight
   tiPerObjOIDGUID
  ,cFileSync
  ,tiFileSyncReader_Abs
  ,tiFileSyncReader_DiskFiles
  ,tiFileSyncReader_Remote
  ,tiFileSyncSetup_BOM
  ,tiFileName_BOM
  ,tiFileSync_Mgr

  ,tiCompressZLib
  ;

procedure ConnectToDatabase ;
procedure RegisterMappings ;

implementation
uses
   tiUtils
  ,tiPersist
  ,cTIPersist
  ;

procedure RegisterMappings ;
begin
  tiFileSyncSetup_BOM.RegisterMappings;
end ;

procedure ConnectToDatabase ;
var
  lDB : string ;
begin
  lDB := tiSwapExt( ParamStr(0), 'xml' ) ;
  if not gTIPerMgr.DefaultPerLayer.DatabaseExists(lDB,  'null', 'null') then
  begin
    gTIPerMgr.DefaultPerLayer.CreateDatabase(lDB,  'null', 'null') ;
    gTIPerMgr.LoadDatabaseLayer(cTIPersistXMLLight, lDB,  'null', 'null' ) ;
    tiFileSyncSetup_BOM.CreateTables;
  end else
    gTIPerMgr.LoadDatabaseLayer(cTIPersistXMLLight, lDB,  'null', 'null' ) ;
end ;

end.

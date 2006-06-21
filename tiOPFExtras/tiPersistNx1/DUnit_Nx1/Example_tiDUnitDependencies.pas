unit tiDUnitDependencies;

interface
uses
   TestFramework
  ,tiDUnitINI
  ,tstPerFramework_BOM
  ,tiPersistAbs_TST         //  <<== Abstract test cases
  ,tiDBConnectionSetupAbs_TST  //  <<== DatabaseName, UserName and Password of test DBs
  ,tiPersist_TST
  ,tiUtils_TST
  ,tiPerObj_TST
  ,tiClassToDBMap_TST
  ,tiPtnVis_TST
  ,tiPtnVisMgr_TST
  ,tiPoolAbs_tst
  ,tiPtnVisSQL_TST
  ,tiPerObjOID_TST
  ,tiSQLMgr_TST
  ,tiEncrypt_TST
  ,tiCompress_TST
  ,tiQueue_TST
  ,tiDataSet_TST
  ,tiStreams_TST
  ,cTIPersist

  ,tiPerFrameworkAbs_TST
  ,tiPerFrameworkIBX_TST
  ,tiPerFrameworkXML_TST
  ,tiPerFrameworkBDEParadox_TST
  ,tiPerFrameworkADOAccess_TST
  ,tiPerFrameworkADOSQLServer_TST
  ,tiPerFrameworkCSV_TST
  ,tiPerFrameworkTab_TST
  ,tiPerFrameworkDOA_TST
  ,tiPerFrameworkIBO_TST
  ,tiPerFrameworkNx1_TST        // NexusDb 1
  ;


procedure RegisterTests;
function  gPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;
procedure RegisterDBTests( const pTestName : string ;
                           const pTestCaseClass : TtiTestCaseClass ) ;

implementation

var
  uPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;

procedure RegisterTests;
begin

  // See ..\Bin\DUnitTIOPF.ini for details on configuration
  tstPerFramework_BOM.RegisterMappings ; // Register the OO-DB Mappings to be tested

  tiUtils_TST.RegisterTests ;           // Completed
  tiCompress_TST.RegisterTests ;        // Completed
  tiEncrypt_TST.RegisterTests ;         // Completed
  tiPtnVis_TST.RegisterTests ;          // Completed
  tiPerObj_TST.RegisterTests ;          // Completed
  tiQueue_Tst.RegisterTests ;           // Completed
  tiPersist_TST.RegisterTests ;         // Completed
  tiPerObjOID_TST.RegisterTests ;       // Completed
  tiClassToDBMap_TST.RegisterTests ;    // Completed
  tiDataSet_TST.RegisterTests;          // Completed
  tiStreams_TST.RegisterTests;          // Completed

  tiPerFrameworkAbs_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistIBX ) then
    tiPerFrameworkIBX_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistXML ) then
    tiPerFrameworkXML_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistBDEParadox ) then
    tiPerFrameworkBDEParadox_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOAccess ) then
    tiPerFrameworkADOAccess_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOSQLServer ) then
    tiPerFrameworkADOSQLServer_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistCSV ) then
    tiPerFrameworkCSV_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistTab ) then
    tiPerFrameworkTAB_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistDOA ) then
    tiPerFrameworkDOA_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistNx1 ) then    // NexusDb 1
    tiPerFrameworkNx1_TST.RegisterTests ;                     // NexusDb 1

  //tiPoolAbs_TST.RegisterTests ;       // Under construction
  //tiPtnVisMgr_TST.RegisterTests ;     // Under construction
  //tiSQLMgr_TST.RegisterTests ;        // Under construction
  //tiPtnVisSQL_TST.RegisterTests ;     // Under construction

end ;

//------------------------------------------------------------------------------
function gPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;
begin
  if uPerFrameworkSetupFactory = nil then
  begin
    uPerFrameworkSetupFactory := TTestPerFrameworkSetupFactory.Create ;
    // These are core persistence frameworks that are tested regularly
    // as part of the build process
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistBDEParadox,   TPerFrameworkSetupBDEParadox ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistIBX,          TPerFrameworkSetupIBX ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistXML,          TPerFrameworkSetupXML ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistCSV,          TPerFrameworkSetupCSV ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistTAB,          TPerFrameworkSetupTAB ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistADOAccess,    TPerFrameworkSetupADOAccess ) ;
    // These are not tested as part of the build process as they require
    // SQLServer, Oracle or IBO to be purchased.
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistADOSQLServer, TPerFrameworkSetupADOSQLServer ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistDOA,          TPerFrameworkSetupDOA ) ;
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistIBO,          TPerFrameworkSetupIBO ) ;
    // These are not tested as part of the build process as they require
    // NexusDb to be purchased.
    uPerFrameworkSetupFactory.RegisterClass( cTIPersistNx1,   TPerFrameworkSetupNx1 ) ;
  end ;
  result := uPerFrameworkSetupFactory ;
end ;

procedure RegisterDBTests( const pTestName : string ;
                           const pTestCaseClass : TtiTestCaseClass ) ;
begin

  if gPerFrameworkSetupFactory.ToRun( cTIPersistIBX ) then
    RegisterTest( pTestName, TTestDBConnectionSetupIBX.Create( pTestCaseClass.Suite ));

  if gPerFrameworkSetupFactory.ToRun( cTIPersistXML ) then
    RegisterTest( pTestName, TTestDBConnectionSetupXML.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistCSV ) then
    RegisterTest( pTestName, TTestDBConnectionSetupCSV.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistTAB ) then
    RegisterTest( pTestName, TTestDBConnectionSetupTAB.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistBDEParadox ) then
    RegisterTest( pTestName, TTestDBConnectionSetupBDEParadox.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOAccess ) then
    RegisterTest( pTestName, TTestDBConnectionSetupADOAccess.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOSQLServer ) then
    RegisterTest( pTestName, TTestDBConnectionSetupADOSQLServer.Create( pTestCaseClass.Suite ) );

  if gPerFrameworkSetupFactory.ToRun( cTIPersistIBO ) then
    RegisterTest( pTestName, TTestDBConnectionSetupIBO.Create( pTestCaseClass.Suite ));

  if gPerFrameworkSetupFactory.ToRun( cTIPersistDOA ) then
    RegisterTest( pTestName, TTestDBConnectionSetupDOA.Create( pTestCaseClass.Suite ));

  // NexusDb 1
  if gPerFrameworkSetupFactory.ToRun( cTIPersistNx1 ) then
    RegisterTest( pTestName, TTestDBConnectionSetupNx1.Create( pTestCaseClass.Suite ) );

end ;

initialization

finalization
  uPerFrameworkSetupFactory.Free ;

end.

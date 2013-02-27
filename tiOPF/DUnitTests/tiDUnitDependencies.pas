unit tiDUnitDependencies;

interface
uses
   TestFramework
  ,tiDUnitINI
  ,tstPerFramework_BOM
  ,tiPersistAbs_TST            //  <<== Abstract test cases
  ,tiDBConnectionSetupAbs_TST  //  <<== Classes that manage the selection of persistence layers to test
  ,tiRegPerLayer_TST
  ,tiPersist_TST
  ,tiUtils_TST
  ,tiPerObj_TST
  ,tiClassToDBMap_TST
  ,tiPtnVis_TST
  ,tiPtnVisMgr_TST
  ,tiPoolAbs_tst
  ,tiPtnVisSQL_TST
  ,tiPerObjOID_TST
//  ,tiSQLMgr_TST
  ,tiEncrypt_TST
  ,tiCompress_TST
  ,tiQueue_TST
  ,tiDataSet_TST
  ,tiStreams_TST
  ,cTIPersist
  ,tiXML_TST
  ,tiXMLToTIDataSet_TST

  ,tiQueryAbs_TST

  ,tiQuerySQL_TST
  ,tiPerFrameworkIBX_TST
  ,tiPerFrameworkBDEParadox_TST
  ,tiPerFrameworkADOAccess_TST
  ,tiPerFrameworkADOSQLServer_TST
  ,tiPerFrameworkRemote_TST
  ,tiPerFrameworkDOA_TST
  ,tiPerFrameworkIBO_TST

  ,tiQueryNonSQL_TST
  ,tiPerFrameworkXML_TST
  ,tiPerFrameworkCSV_TST
  ,tiPerFrameworkTab_TST
  ,tiPerFrameworkXMLLight_TST

  {$IFDEF TEST_REMOTE}
  ,tiHTTP_TST
  ,tiQueryRemote
  {$ENDIF}

  ;


procedure RegisterTests;
function  gPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;
procedure RemoveUnSelectedPersistenceLayerSetups;
procedure RegisterDBTests( const pTestName : string ;
                           const pTestCaseClass : TtiTestCaseClass ) ;

implementation

var
  uPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;

procedure RegisterTests;
begin
  // See ..\Bin\DUnitTIOPF.ini for details on configuration
  tstPerFramework_BOM.RegisterMappings ; // Register the OO-DB Mappings to be tested

  // These are all the 'non persistence' tests
  tiUtils_TST.RegisterTests ;
  tiXML_TST.RegisterTests ;
  tiXMLToTIDataSet_TST.RegisterTests;
  tiCompress_TST.RegisterTests ;
  tiEncrypt_TST.RegisterTests ;
  tiPtnVis_TST.RegisterTests ;
  tiPerObj_TST.RegisterTests ;
  tiQueue_Tst.RegisterTests ;
  tiDataSet_TST.RegisterTests;
  tiStreams_TST.RegisterTests;
  tiPoolAbs_TST.RegisterTests ;
  tiRegPerLayer_TST.RegisterTests ;
  tiPersist_TST.RegisterTests ;

  {$IFDEF TEST_REMOTE}
  tiHTTP_TST.RegisterTests;
  {$ENDIF}

  tiQueryAbs_TST.RegisterTests ;

  if gPerFrameworkSetupFactory.ToRun( cTIPersistIBX ) then
    tiPerFrameworkIBX_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistBDEParadox ) then
    tiPerFrameworkBDEParadox_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOAccess ) then
    tiPerFrameworkADOAccess_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistADOSQLServer ) then
    tiPerFrameworkADOSQLServer_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistDOA ) then
    tiPerFrameworkDOA_TST.RegisterTests ;
  {$IFDEF TEST_REMOTE}
  if gPerFrameworkSetupFactory.ToRun( cTIPersistRemote ) then
    tiPerFrameworkRemote_TST.RegisterTests ;
  {$ENDIF}

  if gPerFrameworkSetupFactory.ToRun( cTIPersistXML ) then
    tiPerFrameworkXML_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistCSV ) then
    tiPerFrameworkCSV_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistTab ) then
    tiPerFrameworkTAB_TST.RegisterTests ;
  if gPerFrameworkSetupFactory.ToRun( cTIPersistXMLLight ) then
    tiPerFrameworkXMLLight_TST.RegisterTests ;

  tiPerObjOID_TST.RegisterTests ;       // Completed
  tiClassToDBMap_TST.RegisterTests ;    // Completed
//  tiSQLMgr_TST.RegisterTests ;          // Completed

  //tiPtnVisMgr_TST.RegisterTests ;     // Under construction
  //tiPtnVisSQL_TST.RegisterTests ;     // Under construction

  gPerFrameworkSetupFactory.RunAllOnce ;

end ;

//------------------------------------------------------------------------------
function gPerFrameworkSetupFactory : TTestPerFrameworkSetupFactory ;
begin
  if uPerFrameworkSetupFactory = nil then
  begin
    uPerFrameworkSetupFactory := TTestPerFrameworkSetupFactory.Create ;
    // These are core persistence frameworks that are tested regularly
    // as part of the build process
    // SQL based persistence layers
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupBDEParadox.Create ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupIBX.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupADOAccess.Create  ) ;

    // Non-SQL Persistence layers
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupXML.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupCSV.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupTAB.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupXMLLight.Create  ) ;
//    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupRemote.Create  ) ;

    // These are not tested as part of the build process as they require
    // SQLServer, Oracle or IBO to be purchased.
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupADOSQLServer.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupDOA.Create  ) ;
    uPerFrameworkSetupFactory.Add( TPerFrameworkSetupIBO.Create  ) ;
  end ;
  result := uPerFrameworkSetupFactory ;
end ;

procedure RemoveUnSelectedPersistenceLayerSetups;
var
  i : integer ;
begin
  for i := gPerFrameworkSetupFactory.Count - 1 downto 0 do
    if not gPerFrameworkSetupFactory.Items[i].Selected then
      gPerFrameworkSetupFactory.Delete(i);
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

  if gPerFrameworkSetupFactory.ToRun( cTIPersistXMLLight ) then
    RegisterTest( pTestName, TTestDBConnectionSetupXMLLight.Create( pTestCaseClass.Suite ) );

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

//  if gPerFrameworkSetupFactory.ToRun( cTIPersistRemote ) then
//    RegisterTest( pTestName, TTestDBConnectionSetupRemote.Create( pTestCaseClass.Suite ));



end ;

initialization

finalization
  uPerFrameworkSetupFactory.Free ;

end.

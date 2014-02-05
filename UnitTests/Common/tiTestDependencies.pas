unit tiTestDependencies;

{$I tiDefines.inc}

interface
uses
  TestExtensions,
  tiTestFramework,
  tiOPFTestCase;

procedure tiRegisterTests;
procedure tiRemoveUnSelectedPersistenceLayerSetups;
procedure tiRemoveXMLLightIfNotRegistered;
procedure tiRegisterPersistenceTest(const ATestCaseClass: TtiTestCaseWithPersistenceLayerClass);
procedure tiRegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass); overload;
procedure tiRegisterExpectedTIOPFMemoryLeaks;


implementation
uses
{$IFNDEF FPC}
  {$IFDEF FASTMM}
   FastMM4,
  {$ENDIF}
  IdThreadSafe,
  IdGlobal,
{$ENDIF}
  TestFramework
  ,SysUtils

  ,tiConstants
  ,tiOPFManager
  ,tiOPFTestManager
  ,tiLog // Confirm which of these must be referenced here
  ,tiLogToFile
  ,tiBOMsForTesting
  ,tiOIDForTesting
  ,tiUtils

  ,tiBaseObject_TST
  ,tiUtils_TST
  ,tiPersistenceLayers_TST
  ,tiObject_TST
  ,tiCompress_TST
  ,tiEncrypt_TST
  ,tiVisitor_TST
  ,tiVisitorDB_TST
  ,tiStreams_TST
  ,tiThread_TST
  ,tiPool_TST
  ,tiQueue_TST
  ,tiOID_TST
  ,tiLogToFile_TST
  {$IFDEF MSWINDOWS}
  ,tiWin32_TST
  ,tiGUIUtils_TST
    {$IFNDEF FPC}
  ,tiDataSet_TST  // tiDataset not FPC ready yet
    {$ENDIF FPC}
  {$ENDIF MSWINDOWS}
  ,tiOPFManager_TST
  ,tiDataBuffer_TST
  ,tiTextParser_TST
  ,tiTextParserStructCSV_TST
  ,tiSyncObjs_TST
  ,tiAutoMap_TST
  ,tiQuery_TST
  ,tiXML_TST
  ,tiCriteria_TST
  ,tiRTTI_TST
  ,tiTokenLibrary_TST
  ,tiBaseMediator_TST
  {$IFNDEF FPC}   // Windows + Delphi specific units
  ,tiXMLToTIDataSet_TST
  ,tiHTTP_TST
  ,tiWebServer_TST
  {$IFDEF LINK_REMOTE}
  ,tiApplicationServer_TST
  {$ENDIF}
  {$ENDIF}

  // Persistent test fixtures (in alpha order)
  ,tiOPFADOAccess_TST
  ,tiOPFBDEParadox_TST
  ,tiOPFADOSQLServer_TST
  ,tiOPFCrSdac_TST
  ,tiOPFCSV_TST
  ,tiOPFDOA_TST
  ,tiOPFFBL_TST
  ,tiOPFFIBP_TST
  ,tiOPFIBO_TST
  ,tiOPFIBX_TST
  {$IFDEF LINK_REMOTE}
  ,tiOPFRemote_TST
  {$ENDIF}
  ,tiOPFSQLDB_IB_TST
  ,tiOPFSQLDB_PQ_TST
//  ,tiOPFSQLDB_MY50_TST
//  ,tiOPFSQLDB_MY51_TST
  ,tiOPFSQLDB_MY55_TST
  ,tiOPFTAB_TST
  ,tiOPFXML_TST
  ,tiOPFXMLLight_TST
  ,tiOPFZeos_FB_TST
  ,tiOPFDBISASM4_TST
  ,tiOPFAsqlite3_TST
  //,tiOPFZeos_MySQL41_TST // No tests
  ,tiOPFZeos_MySQL50_TST
  ,tiOPFUIB_FB_TST
  ;

const
  cSuiteNameNonPersistentTests  = 'Non persistent tests';
  cSuiteNamePersistentTests     = 'Persistent tests for [%s]';


procedure tiRegisterTests;
begin
  gLog.RegisterLog(TtiLogToFile.CreateWithFileName(tiGetTempDir, '', True));
  (GLog.FindByLogClass(TtiLogToFile) as TtiLogToFile).EnableCaching:= False;

  GTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorForTesting.Create;

  // See ..\Bin\DUnitTIOPF.ini for details on configuration
  tiBOMsForTesting.RegisterMappings; // Register the OO-DB Mappings to be tested

  // These are all the 'non persistence' tests
  tiBaseObject_TST.RegisterTests;
  tiUtils_TST.RegisterTests;
  tiRTTI_TST.RegisterTests;

  tiPersistenceLayers_TST.RegisterTests;
  tiVisitor_TST.RegisterTests;
  tiVisitorDB_TST.RegisterTests;
  tiObject_TST.RegisterTests;
  tiCompress_TST.RegisterTests;
  tiEncrypt_TST.RegisterTests;
  tiStreams_TST.RegisterTests;
  tiThread_TST.RegisterTests;
  tiPool_TST.RegisterTests;
  tiQueue_Tst.RegisterTests;
  tiOID_TST.RegisterTests;
  {$IFDEF MSWINDOWS}
  tiWin32_TST.RegisterTests;
  tiGUIUtils_TST.RegisterTests;
  {$IFNDEF FPC}
  tiDataSet_TST.RegisterTests;
  {$ENDIF}
  {$ENDIF}
  tiOPFManager_TST.RegisterTests;
  tiDataBuffer_TST.RegisterTests;
  tiTextParser_TST.RegisterTests;
  tiTextParserStructCSV_TST.RegisterTests;
  tiQuery_TST.RegisterTests;
  tiSyncObjs_TST.RegisterTests;
  tiAutoMap_TST.RegisterTests;
  tiXML_TST.RegisterTests;
  tiCriteria_TST.RegisterTests;
  tiTokenLibrary_TST.RegisterTests;
  tiLogToFile_TST.RegisterTests;
  tiBaseMediator_TST.RegisterTests;
  {$IFNDEF FPC}
  tiXMLToTIDataSet_TST.RegisterTests;
  tiHTTP_TST.RegisterTests;
  tiWebServer_TST.RegisterTests;
  {$IFDEF LINK_REMOTE}
  tiApplicationServer_TST.RegisterTests;
  {$ENDIF}
  {$ENDIF}



  // Persistent test fixtures (in alpha order)
  tiOPFADOAccess_TST.RegisterTests;
  tiOPFBDEParadox_TST.RegisterTests;
  tiOPFADOSQLServer_TST.RegisterTests;
  tiOPFCrSdac_TST.RegisterTests;
  tiOPFCSV_TST.RegisterTests;
  tiOPFDOA_TST.RegisterTests;
  tiOPFFBL_TST.RegisterTests;
  tiOPFFIBP_TST.RegisterTests;
  tiOPFIBO_TST.RegisterTests;
  tiOPFIBX_TST.RegisterTests;
  {$IFDEF LINK_REMOTE}
  tiOPFRemote_TST.RegisterTests;
  {$ENDIF}
  tiOPFSQLDB_IB_TST.RegisterTests;
  tiOPFSQLDB_PQ_TST.RegisterTests;
//  tiOPFSQLDB_MY50_TST.RegisterTests;
//  tiOPFSQLDB_MY51_TST.RegisterTests;
  tiOPFSQLDB_MY55_TST.RegisterTests;
  tiOPFTAB_TST.RegisterTests;
  tiOPFXML_TST.RegisterTests;
  tiOPFXMLLight_TST.RegisterTests;
  tiOPFZeos_FB_TST.RegisterTests;
//  tiOPFZeos_MySQL41_TST.RegisterTests; // No tests
  tiOPFZeos_MySQL50_TST.RegisterTests;
  tiOPFDBISASM4_TST.RegisterTests;
  tiOPFAsqlite3_TST.RegisterTests;
  tiOPFUIB_FB_TST.RegisterTests;

end;

procedure tiRemoveUnSelectedPersistenceLayerSetups;
var
  i : integer;
begin
  for i := GTIOPFTestManager.Count - 1 downto 0 do
    if (not GTIOPFTestManager.Items[i].Selected) then
      GTIOPFTestManager.Delete(i);
end;

procedure tiRemoveXMLLightIfNotRegistered;
{$ifndef Link_XMLLight}
var
  LPLTest: TtiOPFTestSetupData;
  LIndex: integer;
{$endif}
begin
  {$ifndef Link_XMLLight}
  if GTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistXMLLight) then
  begin
    LPLTest:= GTIOPFTestManager.FindByPersistenceLayerName(cTIPersistXMLLight);
    if LPLTest<>nil then
    begin
      LIndex:= LPLTest.Index;
      GTIOPFTestManager.Delete(LIndex);
    end;
  end;
  {$endif}
end;

procedure tiRegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
begin
  if GTIOPFTestManager.TestNonPersistentClasses then
    RegisterTest(cSuiteNameNonPersistentTests, ATestCaseClass.Suite);
end;

procedure tiRegisterPersistenceTest(const ATestCaseClass: TtiTestCaseWithPersistenceLayerClass);
var
  LTestSuiteName: string;
  LPersistenceLayerName: string;
begin
  LPersistenceLayerName:= ATestCaseClass.PersistenceLayerName;
  if GTIOPFTestManager.ToRun(LPersistenceLayerName) then
  begin
    LTestSuiteName:= Format(cSuiteNamePersistentTests, [LPersistenceLayerName]);
    RegisterTest(LTestSuiteName, ATestCaseClass.Suite);
  end;
end;

procedure tiRegisterExpectedTIOPFMemoryLeaks;
begin
  {$IFNDEF FPC}
    {$IFDEF EnableMemoryLeakReporting}
      FastMM4.RegisterExpectedMemoryLeak(TidThreadSafeInteger, 1);
      FastMM4.RegisterExpectedMemoryLeak(TidCriticalSection, 2);
    {$ENDIF EnableMemoryLeakReporting}
  {$ENDIF FPC}
end;

end.








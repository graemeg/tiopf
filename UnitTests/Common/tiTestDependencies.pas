unit tiTestDependencies;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  tiFPCUnitUtils, // Helper functions to fake DUnit methods
  {$ENDIF}
  tiTestFramework
  ,tiOPFTestManager
  ,TestExtensions
  ,TestFrameworkIfaces
  ;

type
  TtiTestDecoratorClass = class of TTestSetup;

procedure RegisterTests;
function  GTIOPFTestManager: TtiOPFTestManager;
procedure RemoveUnSelectedPersistenceLayerSetups;
procedure RemoveXMLLightIfNotRegistered;
procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass); overload;
procedure RegisterNonPersistentTest(const ATestDecorator: TtiTestDecoratorClass;
                                    const ATestCaseClass: TtiTestCaseClass); overload;
function  PersistentSuiteName(APerLayerName: string): string;
procedure RegisterExpectedTIOPFMemoryLeaks;


implementation
uses
{$IFNDEF FPC}
   FastMM4
  ,IdThreadSafe
  ,IdGlobal
  ,TestFramework,
{$ENDIF}
  SysUtils

  ,tiLog // Confirm which of these must be referenced here
  ,tiLogToFile
  ,tiBOMsForTesting

  ,tiBaseObject_TST
  ,tiUtils_TST
  ,tiObject_TST
  ,tiCompress_TST
  ,tiEncrypt_TST
  ,tiVisitor_TST
  ,tiVisitorDB_TST
  ,tiStreams_TST
  ,tiPool_tst
  ,tiQueue_TST
  ,tiOID_TST
  ,tiLogToFile_TST
  {$IFDEF MSWINDOWS}
  ,tiWin32_TST
  ,tiGUIUtils_TST
  ,tiDataSet_TST
  {$ENDIF}
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
  {$IFNDEF FPC}
  ,tiXMLToTIDataSet_TST
  ,tiHTTP_TST
  ,tiWebServer_tst
  {$ENDIF}

  // Persistent test fixtures (in alpha order)
  ,tiOPFADOAccess_TST
  ,tiOPFBDEParadox_TST
  ,tiOPFADOSQLServer_TST
  ,tiOPFCSV_TST
  ,tiOPFDOA_TST
  ,tiOPFFBL_TST
  ,tiOPFIBO_TST
  ,tiOPFIBX_TST
  ,tiOPFRemote_TST
  //,tiOPFSQLDB_IB_TST  // no such test
  ,tiOPFTAB_TST
  ,tiOPFXML_TST
  ,tiOPFXMLLight_TST
  //,tiOPFZeos_FB10_TST  // No tests
  ,tiOPFZeos_FB15_TST
  ,tiOPFDBISASM4_TST
  ,tiOPFAsqlite3_TST   
  //,tiOPFZeos_MySQL41_TST // No tests
  //,tiOPFZeos_MySQL50_TST // No tests

  ;

var
  UTIOPFTestManager: TtiOPFTestManager;

const
  cSuiteNameNonPersistentTests  = 'Non persistent tests';
  cSuiteNamePersistentTests     = 'Persistent tests for [%s]';


procedure RegisterTests;
begin

//  if not IsConsole then
//    gLog.RegisterLog(TtiLogToGUI.Create);
  gLog.RegisterLog(TtiLogToFile.CreateWithFileName('', '', True));

  // See ..\Bin\DUnitTIOPF.ini for details on configuration
  tiBOMsForTesting.RegisterMappings; // Register the OO-DB Mappings to be tested

  // These are all the 'non persistence' tests
  tiBaseObject_tst.RegisterTests;
  tiUtils_TST.RegisterTests;
  tiRTTI_TST.RegisterTests;
  tiVisitor_TST.RegisterTests;
  tiVisitorDB_TST.RegisterTests;
  tiObject_tst.RegisterTests;
  tiCompress_TST.RegisterTests;
  tiEncrypt_TST.RegisterTests;
  tiStreams_TST.RegisterTests;
  tiPool_TST.RegisterTests;
  tiQueue_Tst.RegisterTests;
  tiOID_TST.RegisterTests;
  {$IFDEF MSWINDOWS}
  tiWin32_TST.RegisterTests;
  tiGUIUtils_TST.RegisterTests;
  tiDataSet_TST.RegisterTests;
  {$ENDIF}
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
  {$IFNDEF FPC}
  tiXMLToTIDataSet_TST.RegisterTests;
  tiHTTP_TST.RegisterTests;
  tiWebServer_tst.RegisterTests;
  {$IFDEF LINK_REMOTE} tiApplicationServer_tst.RegisterTests; {$ENDIF} // Work in progress
  {$ENDIF}

  // Persistent test fixtures (in alpha order)
  tiOPFADOAccess_TST.RegisterTests;
  tiOPFBDEParadox_TST.RegisterTests;
  tiOPFADOSQLServer_TST.RegisterTests;
  tiOPFCSV_TST.RegisterTests;
  tiOPFDOA_TST.RegisterTests;
  tiOPFFBL_TST.RegisterTests;
  tiOPFIBO_TST.RegisterTests;
  tiOPFIBX_TST.RegisterTests;
  tiOPFRemote_TST.RegisterTests;
  //tiOPFSQLDB_IB_TST.RegisterTests; // No tests
  tiOPFTAB_TST.RegisterTests;
  tiOPFXML_TST.RegisterTests;
  tiOPFXMLLight_TST.RegisterTests;
  //tiOPFZeos_FB10_TST.RegisterTests; // No tests
  tiOPFZeos_FB15_TST.RegisterTests;
  //tiOPFZeos_MySQL41_TST.RegisterTests; // No tests
  //tiOPFZeos_MySQL50_TST.RegisterTests; // No tests
  tiOPFDBISASM4_TST.RegisterTests;
  tiOPFAsqlite3_TST.RegisterTests;
end;


function GTIOPFTestManager: TtiOPFTestManager;
begin
  if UTIOPFTestManager = nil then
    UTIOPFTestManager := TtiOPFTestManager.Create;
  result := UTIOPFTestManager;
end;


procedure RemoveUnSelectedPersistenceLayerSetups;
var
  i : integer;
begin
  for i := GTIOPFTestManager.Count - 1 downto 0 do
    if (not GTIOPFTestManager.Items[i].Selected) then
      GTIOPFTestManager.Delete(i);
end;

procedure RemoveXMLLightIfNotRegistered;
{$ifndef Link_XMLLight}
var
  LPLTest: TtiOPFTestSetupData;
  LIndex: integer;
{$endif}
begin
  {$ifndef Link_XMLLight}
  if GTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistXMLLight) then
  begin
    LPLTest:= GTIOPFTestManager.FindByPerLayerName(cTIPersistXMLLight);
    if LPLTest<>nil then
    begin
      LIndex:= LPLTest.Index;
      GTIOPFTestManager.Delete(LIndex);
    end;
  end;
  {$endif}
end;

procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
begin
  if GTIOPFTestManager.TestNonPersistentClasses then
    RegisterTest(cSuiteNameNonPersistentTests, ATestCaseClass.Suite);
end;

procedure RegisterNonPersistentTest(const ATestDecorator: TtiTestDecoratorClass;
                                    const ATestCaseClass: TtiTestCaseClass);
begin
  if GTIOPFTestManager.TestNonPersistentClasses then
    RegisterTest(cSuiteNameNonPersistentTests, AtestDecorator.Suite(ATestCaseClass.Suite));
end;


function PersistentSuiteName(APerLayerName: string): string;
begin
  Result := Format(cSuiteNamePersistentTests, [APerLayerName]);
end;

procedure RegisterExpectedTIOPFMemoryLeaks;
begin
  {$IFNDEF FPC}
  FastMM4.RegisterExpectedMemoryLeak(TidThreadSafeInteger, 1);
  FastMM4.RegisterExpectedMemoryLeak(TidCriticalSection, 2);
  {$ENDIF FPC}
end;

initialization

finalization
  UTIOPFTestManager.Free;

end.







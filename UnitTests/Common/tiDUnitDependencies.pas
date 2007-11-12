unit tiDUnitDependencies;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  tiFPCUnitUtils, // Helper functions to fake DUnit methods
  {$ENDIF}
  tiTestFramework
  ,tiOPFTestManager
  ;


procedure RegisterTests;
function  GTIOPFTestManager: TtiOPFTestManager;
procedure RemoveUnSelectedPersistenceLayerSetups;
procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
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
  ,tiOPFManager
  ,tiDUnitINI
  ,tiConstants
  ,tstPerFramework_BOM

  {$IFDEF OBJECT_TRACKING}
  ,tiBaseObject_TST
  {$ENDIF}
  ,tiPersistenceLayers_TST
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
  ,tiQuerySQL_TST
  ,tiQueryNonSQL_TST
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

  {$IFNDEF FPC},tiOPFADOAccess_TST{$ENDIF}
  {$IFNDEF FPC},tiOPFBDEParadox_TST{$ENDIF}
  {$IFNDEF FPC},tiOPFADOSQLServer_TST{$ENDIF}
  ,tiOPFCSV_TST
  {$IFNDEF FPC},tiOPFDOA_TST{$ENDIF}
  ,tiOPFFBL_TST
  {$IFNDEF FPC},tiOPFIBO_TST{$ENDIF}
  {$IFNDEF FPC},tiOPFIBX_TST{$ENDIF}
  {$IFNDEF FPC},tiOPFRemote_TST{$ENDIF}
  //,tiOPFSQLDB_IB_TST  // no such test
  ,tiOPFTAB_TST
  {$IFNDEF FPC},tiOPFXML_TST{$ENDIF}
  ,tiOPFXMLLight_TST
  //,tiOPFZeos_FB10_TST  // No tests
  {$IFNDEF FPC},tiOPFZeos_FB15_TST{$ENDIF}
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
  tstPerFramework_BOM.RegisterMappings; // Register the OO-DB Mappings to be tested

  // These are all the 'non persistence' tests
  {$IFDEF OBJECT_TRACKING}
  tiBaseObject_tst.RegisterTests;
  {$ENDIF}
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
  tiPersistenceLayers_tst.RegisterTests;
  tiAutoMap_TST.RegisterTests;
  tiXML_TST.RegisterTests;
  tiCriteria_TST.RegisterTests;
  tiTokenLibrary_TST.RegisterTests;
  tiLogToFile_TST.RegisterTests;
  {$IFNDEF FPC}
  tiXMLToTIDataSet_TST.RegisterTests;
  tiHTTP_TST.RegisterTests;
  tiWebServer_tst.RegisterTests;
  {$ENDIF}

  // Persistent test fixtures (in alpha order)
  {$IFNDEF FPC}tiOPFADOAccess_TST.RegisterTests;{$ENDIF}
  {$IFNDEF FPC}tiOPFBDEParadox_TST.RegisterTests;{$ENDIF}
  {$IFNDEF FPC}tiOPFADOSQLServer_TST.RegisterTests;{$ENDIF}
  tiOPFCSV_TST.RegisterTests;
  {$IFNDEF FPC}tiOPFDOA_TST.RegisterTests;{$ENDIF}
  tiOPFFBL_TST.RegisterTests;
  {$IFNDEF FPC}tiOPFIBO_TST.RegisterTests;{$ENDIF}
  {$IFNDEF FPC}tiOPFIBX_TST.RegisterTests;{$ENDIF}
  {$IFNDEF FPC}tiOPFRemote_TST.RegisterTests;{$ENDIF}
  //tiOPFSQLDB_IB_TST.RegisterTests; // No tests
  tiOPFTAB_TST.RegisterTests;
  {$IFNDEF FPC}tiOPFXML_TST.RegisterTests;{$ENDIF}
  tiOPFXMLLight_TST.RegisterTests;
  //tiOPFZeos_FB10_TST.RegisterTests; // No tests
  {$IFNDEF FPC}tiOPFZeos_FB15_TST.RegisterTests;{$ENDIF}
  //tiOPFZeos_MySQL41_TST.RegisterTests; // No tests
  //tiOPFZeos_MySQL50_TST.RegisterTests; // No tests

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


procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
begin
  if GTIOPFTestManager.TestNonPersistentClasses then
    RegisterTest(cSuiteNameNonPersistentTests, ATestCaseClass.Suite);
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




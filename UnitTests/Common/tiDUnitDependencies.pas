unit tiDUnitDependencies;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTestFramework
  ,tiOPFTestManager
  ;


procedure RegisterTests;
function  gTIOPFTestManager: TtiOPFTestManager;
procedure RemoveUnSelectedPersistenceLayerSetups;
procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
function  PersistentSuiteName(APerLayerName: string): string;


implementation
uses
   SysUtils
  ,tiLog
  ,tiLogToGUI
  ,tiLogToFile
  ,tiBaseObject_TST
  ,tiOPFManager
  ,tiDUnitUtils
  ,tiDUnitINI
  ,tiConstants

  ,tstPerFramework_BOM
  ,tiPersistenceLayers_TST
  ,tiUtils_TST
  ,tiObject_TST
  ,tiCompress_TST
  ,tiEncrypt_TST
  ,tiVisitor_TST
  ,tiStreams_TST
  ,tiPool_tst
  ,tiQueue_TST
  ,tiOID_TST
  {$IFDEF MSWINDOWS}
  ,tiWin32_TST
  ,tiGUIUtils_TST
  {$ENDIF}
  ,tiDataSet_TST
  ,tiTextParser_TST
  ,tiTextParserStructCSV_TST
  ,tiSyncObjs_TST
  ,tiClassToDBMap_TST
  ,tiQuery_TST
  ,tiQuerySQL_TST
  ,tiQueryNonSQL_TST
  ,tiXML_TST
  ,tiCriteria_TST

  {$IFDEF FPC}
  ,tiOPFFBL_TST
//  ,tiOPFSQLDB_IB_TST
//  ,tiOPFZeos_FB15_TST
  {$ELSE}
  ,tiXMLToTIDataSet_TST
  ,tiHTTP_TST
  ,tiWebServer_tst

  ,tiOPFIBX_TST
  ,tiOPFBDEParadox_TST
  ,tiOPFADOAccess_TST
  ,tiOPFADOSQLServer_TST
  ,tiOPFRemote_TST
  ,tiOPFDOA_TST
  ,tiOPFIBO_TST
//  ,tiOPFZeos_FB15_TST

  ,tiOPFXML_TST
  {$ENDIF}
  ,tiOPFXMLLight_TST
  ,tiOPFCSV_TST
  ,tiOPFTAB_TST
  ;

var
  uTIOPFTestManager: TtiOPFTestManager;


const
  cSuiteNameNonPersistentTests  = 'Non persistent tests';
  cSuiteNamePersistentTests     = 'Persistent tests for [%s]';


procedure RegisterTests;
begin
  if not IsConsole then
    gLog.RegisterLog(TtiLogToGUI.Create);
  gLog.RegisterLog(TtiLogToFile.CreateWithFileName('', '', True));

  gTIOPFManager.VisitorManager.BreakOnException := True;

  // See ..\Bin\DUnitTIOPF.ini for details on configuration
  tstPerFramework_BOM.RegisterMappings; // Register the OO-DB Mappings to be tested

  // These are all the 'non persistence' tests
  tiBaseObject_tst.RegisterTests;
  tiUtils_TST.RegisterTests;
  tiObject_tst.RegisterTests;
  tiCompress_TST.RegisterTests;
  tiEncrypt_TST.RegisterTests;
  tiVisitor_TST.RegisterTests;
  tiStreams_TST.RegisterTests;
  tiPool_TST.RegisterTests;
  tiQueue_Tst.RegisterTests;
  tiOID_TST.RegisterTests;
  {$IFDEF MSWINDOWS}
  tiWin32_TST.RegisterTests;
  tiGUIUtils_TST.RegisterTests;
  {$ENDIF}
  tiDataSet_TST.RegisterTests;
  tiTextParser_TST.RegisterTests;
  tiTextParserStructCSV_TST.RegisterTests;
  tiQuery_TST.RegisterTests;
  tiSyncObjs_TST.RegisterTests;
  tiPersistenceLayers_tst.RegisterTests;
  tiClassToDBMap_TST.RegisterTests;
  tiXML_TST.RegisterTests;
  tiCriteria_TST.RegisterTests;

  {$IFDEF FPC}
  tiOPFFBL_TST.RegisterTests;
//  tiOPFSQLDB_IB_TST.RegisterTests;
//  tiOPFZeos_FB15_TST.RegisterTests;
  {$ELSE}
  tiXMLToTIDataSet_TST.RegisterTests;
  tiHTTP_TST.RegisterTests;
//  tiWebServer_tst.RegisterTests;

  tiOPFIBX_TST.RegisterTests;
  tiOPFBDEParadox_TST.RegisterTests;
  tiOPFADOAccess_TST.RegisterTests;
  tiOPFADOSQLServer_TST.RegisterTests;
  tiOPFDOA_TST.RegisterTests;
  tiOPFRemote_TST.RegisterTests;
  tiOPFXML_TST.RegisterTests;
//  tiOPFZeos_FB15_TST.RegisterTests;
  {$ENDIF FPC}
  tiOPFXMLLight_TST.RegisterTests;
  tiOPFCSV_TST.RegisterTests;
  tiOPFTAB_TST.RegisterTests;
end;


function gTIOPFTestManager: TtiOPFTestManager;
begin
  if uTIOPFTestManager = nil then
  begin
    uTIOPFTestManager := TtiOPFTestManager.Create;

    {$IFDEF FPC}
    // Non-SQL Persistence layers
    uTIOPFTestManager.Add(TtiOPFTestSetupDataCSV.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataTAB.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataXMLLight.Create);
    // SQL based persistence layers
    uTIOPFTestManager.Add(TtiOPFTestSetupDataFBL.Create);
//    uTIOPFTestManager.Add(TtiOPFTestSetupDataSQLDB_IB.Create);
//    uTIOPFTestManager.Add(TtiOPFTestSetupDataZeos.Create);
    {$ELSE}
    // Non-SQL Persistence layers
    uTIOPFTestManager.Add(TtiOPFTestSetupDataXML.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataCSV.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataTAB.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataXMLLight.Create);

    // These are core persistence frameworks that are tested regularly
    // as part of the build process
    // SQL based persistence layers
    uTIOPFTestManager.Add(TtiOPFTestSetupDataBDEParadox.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataIBX.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataRemote.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataADOAccess.Create);

    //uTIOPFTestManager.Add(TtiOPFTestSetupDataADOSQLServer.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataDOA.Create);
    uTIOPFTestManager.Add(TtiOPFTestSetupDataIBO.Create);
//    uTIOPFTestManager.Add(TtiOPFTestSetupDataZeos.Create);
    {$ENDIF FPC}
  end;
  result := uTIOPFTestManager;
end;


procedure RemoveUnSelectedPersistenceLayerSetups;
var
  i : integer;
begin
  for i := gTIOPFTestManager.Count - 1 downto 0 do
    if (not gTIOPFTestManager.Items[i].Selected) then
      gTIOPFTestManager.Delete(i);
end;


procedure RegisterNonPersistentTest(ATestCaseClass: TtiTestCaseClass);
begin
  if gTIOPFTestManager.TestNonPersistentClasses then
    {$IFDEF FPC}
    RegisterTest(ATestCaseClass);
    {$ELSE}
    RegisterTest(cSuiteNameNonPersistentTests, ATestCaseClass.Suite);
    {$ENDIF}
end;


function PersistentSuiteName(APerLayerName: string): string;
begin
  Result := Format(cSuiteNamePersistentTests, [APerLayerName]);
end;


initialization

finalization
  uTIOPFTestManager.Free;

end.


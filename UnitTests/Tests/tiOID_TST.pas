unit tiOID_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  {$IFDEF FPC}
  ,testregistry
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ,tiTestFramework
  ,tiOID
 ;

type

  TTestTIOIDManager = class(TtiOPFTestCase)
  public
    constructor Create{$IFNDEF FPC}(AMethodName: string){$ENDIF}; override;
  published
    procedure   OIDFactory_RegisterMapping;
    procedure   OIDFactory_CreateOID;
    procedure   OIDFactory_CreateNextOIDGenerator;

    procedure   NextOIDMgrInteger;
    procedure   NextOIDMgrString;
    procedure   NextOIDMgrGUID;

    procedure   NextOIDStringSetNextOIDChars;
    procedure   NextOIDInteger;
    procedure   NextOIDString;
    procedure   NextOIDGUID;

    procedure   tiObject_CreateNew;            
    procedure   tiObject_CreateNew_OIDGUID  ; // GUID OID
    procedure   tiObject_CreateNew_OIDInteger; // GUID Integer
  end;


  TTestTIOID = class(TtiTestCase)
  private
    FOIDClass : TOIDClass;
  published
    procedure AsString ; virtual; abstract;
    procedure AsVariant; virtual; abstract;
    procedure Null     ; virtual; abstract;
    procedure Assign   ; virtual; abstract;
    procedure Compare  ;
    procedure Equals;
    procedure Clone;

// Database dependant methods that do not have tests (yet)
//    procedure AssignToTIQuery;
//    procedure AssignToTIQuery;
//    procedure AssignFromTIQuery;
//    procedure AssignFromTIQuery;
//    procedure EqualsQueryField;
//    procedure GetNextValue;
  end;


  TTestTIOIDInteger = class(TTestTIOID)
  protected
    procedure SetUp; override;
  published
    procedure AsString; override;
    procedure AsVariant; override;
    procedure Null; override;
    procedure Assign; override;
  end;
  

  TTestTIOIDString = class(TTestTIOID)
  protected
    procedure SetUp; override;
  published
    procedure AsString; override;
    procedure AsVariant; override;
    procedure Null; override;
    procedure Assign; override;
  end;


  TTestTIOIDGUID = class(TTestTIOID)
  protected
    procedure SetUp; override;
  published
    procedure AsString; override;
    procedure AsVariant; override;
    procedure Null; override;
    procedure Assign; override;
  end;


  // A support class for testing TOIDFactory
  TOIDFactory_TST = class(TOIDFactory)
  private
    function GetItems(AIndex: Integer): TOIDClassMapping;
  public
    function CountMappings : integer;
    property Items[AIndex:Integer]: TOIDClassMapping read GetItems;
  end;


procedure RegisterTests;


implementation
uses
   tiOPFManager
  ,tiObject 
  ,SysUtils
  ,tiQuery
  ,tiOIDGUID    // Pull in the integer OID framework
  ,tiOIDInteger // Pull in the integer OID framework
  ,tiOIDString  // Pull in the string OID framework
  ,tiLog
  ,tiOPFTestManager
  ,Math
  ,tiDUnitDependencies
  {$IFDEF MSWINDOWS}
  ,tiWin32
  {$ENDIF}
  ,tiPersistenceLayers
  ,tiUtils
 ;
  

const
  // Number of times to repeat NextOID test
  // Set a high number for thorough testing (eg, 100000)
  // Set a low number for quick testing (eg, 100)
  cRepeatCount = 100;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIOIDInteger);
  RegisterNonPersistentTest(TTestTIOIDString);
  RegisterNonPersistentTest(TTestTIOIDGUID);
end;


{ TTestTIOID }

procedure TTestTIOID.Clone;
var
  lOID1 : TOID;
  lOID2 : TOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID1.AsVariant := 1;
    lOID2 := lOID1.Clone;
    try
      CheckIs(lOID2, FOIDClass, 'Failed on OID.Clone.ClassType');
      CheckEquals(lOID1.AsString, lOID2.AsString, 'Failed on lOID1.AsVariant = lOID2.AsVariant');
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOID.Compare;
var
  lOID1 : TOID;
  lOID2 : TOID;
  lCompare : integer;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 2;
      lOID2.AsVariant := 2;
      lCompare := lOID1.Compare(lOID2);
      CheckEquals(0, lCompare, 'Failed on compare = 0');

      lOID1.AsVariant := 2;
      lOID2.AsVariant := 1;
      lCompare := lOID1.Compare(lOID2);
      CheckEquals(1, lCompare, 'Failed on compare = 1');

      lOID1.AsVariant := 1;
      lOID2.AsVariant := 2;
      lCompare := lOID1.Compare(lOID2);
      CheckEquals(-1, lCompare, 'Failed on compare = -1');
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOID.Equals;
var
  lOID1 : TOID;
  lOID2 : TOID;
  lEquals : boolean;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 1;
      lOID2.AsVariant := 1;
      lEquals := lOID1.Equals(lOID2);
      Check(lEquals, 'Failed on Equals');

      lOID1.AsVariant := 1;
      lOID2.AsVariant := 2;
      lEquals := lOID1.Equals(lOID2);
      Check(not lEquals, 'Failed on not Equals');

    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


{ TTestTIOIDInteger }

procedure TTestTIOIDInteger.Assign;
var
  lOID1 : TOID;
  lOID2 : TOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 1;
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDInteger.AsString;
var
  lOID : TOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := '1';
    CheckEquals('1', lOID.AsString, 'Failed on AsString');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDInteger.AsVariant;
var
  lOID : TOID;
  lInt : integer;
  lStr : string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := '1';
    CheckEquals('1', lOID.AsString, 'Failed on AsString');
    lStr := lOID.AsVariant;
    CheckEquals('1', lStr, 'Failed on AsVariant #1');
    lInt := lOID.AsVariant;
    CheckEquals( 1, lInt, 'Failed on AsVariant #2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDInteger.Null;
var
  lOID : TOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on IsNull');
    lOID.AsVariant := 1;
    Check(not lOID.IsNull, 'Failed on not IsNull');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on SetToNull');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDInteger.SetUp;
begin
  inherited;
  FOIDClass := TOIDInteger;
end;


{ TTestTIOIDGUID }

procedure TTestTIOIDGUID.Assign;
var
  lOID1: TOID;
  lOID2: TOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := tiCreateGUIDString;
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDGUID.AsString;
var
  lOID: TOID;
  lValue: string;
begin
  lValue := tiCreateGUIDString;
  lOID   := FOIDClass.Create;
  try
    lOID.AsString := lValue;
    CheckEquals(lValue, lOID.AsString, 'Failed on 1');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.AsVariant;
var
  lOID: TOID;
  lValue: string;
  lStr: string;
begin
  lValue := tiCreateGUIDString;
  lOID   := FOIDClass.Create;
  try
    lOID.AsVariant := lValue;
    CheckEquals(lValue, lOID.AsString, 'Failed on 1');
    lStr := lOID.AsVariant;
    CheckEquals(lValue, lStr,'Failed on 2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.Null;
var
  lOID: TOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on 1');
    lOID.AsVariant := tiCreateGUIDString;
    Check(not lOID.IsNull, 'Failed on 2');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on 3');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.SetUp;
begin
  inherited;
  FOIDClass := TOIDGUID;
end;


{ TTestTIOIDString }

procedure TTestTIOIDString.Assign;
var
  lOID1 : TOID;
  lOID2 : TOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 'test';
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDString.AsString;
var
  lOID : TOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := 'test';
    CheckEquals('test', lOID.AsString, 'Failed on 1');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.AsVariant;
var
  lOID : TOID;
  lStr : string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := 'test';
    CheckEquals('test', lOID.AsString, 'Failed on 1');
    lStr := lOID.AsVariant;
    CheckEquals('test', lStr, 'Failed on 2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.Null;
var
  lOID : TOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on 1');
    lOID.AsVariant := 'test';
    Check(not lOID.IsNull, 'Failed on 2');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on 3');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.SetUp;
begin
  inherited;
  FOIDClass := TOIDString;
end;


procedure TTestTIOIDManager.NextOIDInteger;
var
  i : integer;
  lOIDStart : TOID;
  lOIDCurrent : TOID;
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);
  gTIOPFManager.DefaultOIDClassName := cOIDClassNameInteger;

  lRegPerLayer.NextOIDMgr.Clear;
  lOIDStart  := gTIOPFManager.OIDFactory.CreateOID;
  lOIDCurrent := gTIOPFManager.OIDFactory.CreateOID;
  CheckIs(lOIDStart, TOIDInteger);
  try
    CheckIs(lOIDCurrent, TOIDInteger);
    CreateNextOIDIntTable;
    try
      lRegPerLayer.NextOIDMgr.AssignNextOID(lOIDStart, DatabaseName);
      lOIDCurrent.Assign(lOIDStart);
      CheckEquals('100000000', lOIDStart.AsString, 'lOIDStart.AsString');
      CheckEquals('100000000', lOIDCurrent.AsString, 'lOIDCurrent.AsString');
      for i := 0 to cRepeatCount do
      begin
        CheckEquals(
          StrToInt(lOIDStart.AsString) + i,
          StrToInt(lOIDCurrent.AsString),
          'Failed on iteration ' + IntToStr(i));
        lRegPerLayer.NextOIDMgr.AssignNextOID(lOIDCurrent, DatabaseName);
      end;
    finally
      DropNextOIDTable;
    end;
  finally
    lOIDStart.Free;
    lOIDCurrent.Free;
  end;
end;


procedure TTestTIOIDManager.OIDFactory_CreateNextOIDGenerator;
var
  lFactory : TOIDFactory;
  lNextOIDGenerator : TNextOIDGenerator;
begin
  lFactory := TOIDFactory.Create;
  try
    lFactory.RegisterMapping(cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger) ;
    lFactory.RegisterMapping(cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID) ;

    lNextOIDGenerator := lFactory.CreateNextOIDGenerator(cOIDClassNameInteger);
    try
      CheckIs(lNextOIDGenerator, TNextOIDGeneratorInteger);
    finally
      lNextOIDGenerator.Free;
    end;

    lNextOIDGenerator := lFactory.CreateNextOIDGenerator(cOIDClassNameGUID);
    try
      CheckIs(lNextOIDGenerator, TNextOIDGeneratorGUID);
    finally
      lNextOIDGenerator.Free;
    end;

  finally
    lFactory.Free;
  end;
end;


procedure TTestTIOIDManager.OIDFactory_CreateOID;
var
  lFactory : TOIDFactory;
  lOID : TOID;
begin
  lFactory := TOIDFactory.Create;
  try
    lFactory.RegisterMapping(cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger) ;
    lFactory.RegisterMapping(cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID) ;

    lOID := lFactory.CreateOID(cOIDClassNameInteger);
    try
      CheckIs(lOID, TOIDInteger);
    finally
      lOID.Free;
    end;

    lOID := lFactory.CreateOID(cOIDClassNameGUID);
    try
      CheckIs(lOID, TOIDGUID);
    finally
      lOID.Free;
    end;

  finally
    lFactory.Free;
  end;
end;


procedure TTestTIOIDManager.OIDFactory_RegisterMapping;
var
  lFactory : TOIDFactory_TST;
  lOIDClassMapping : TOIDClassMapping;
begin
  lFactory := TOIDFactory_TST.Create;
  try
    lFactory.RegisterMapping(cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger) ;
    CheckEquals(1, lFactory.CountMappings);
    lFactory.RegisterMapping(cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID) ;
    CheckEquals(2, lFactory.CountMappings);
    lOIDClassMapping := lFactory.Items[0];
    CheckEquals(TOIDInteger, lOIDClassMapping.OIDClass);
    CheckEquals(TNextOIDGeneratorInteger, lOIDClassMapping.NextOIDGeneratorClass);

    lOIDClassMapping := lFactory.Items[1];
    CheckEquals(TOIDGUID, lOIDClassMapping.OIDClass);
    CheckEquals(TNextOIDGeneratorGUID, lOIDClassMapping.NextOIDGeneratorClass);

  finally
    lFactory.Free;
  end;
end;


procedure TTestTIOIDManager.NextOIDMgrInteger;
var
  lOID : TOID;
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  CreateNextOIDIntTable;
  try
    // This required tidying up. At the time of writing, only
    // one OID class type is possible for an application instance
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameInteger;
    lRegPerLayer.NextOIDMgr.Clear;
    lOID := TOIDInteger.Create;
    try
      Check(lOID.IsNull, 'not lOID.IsNull');
      lRegPerLayer.NextOIDMgr.AssignNextOID(lOID, DatabaseName);
      Check(not lOID.IsNull, 'lOID.IsNull');
    finally
      lOID.Free;
    end;
  finally
    DropNextOIDTable;
  end;
end;


procedure TTestTIOIDManager.NextOIDMgrString;
var
  lOID : TOID;
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  CreateNextOIDStrTable;
  try
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameString;
    lRegPerLayer.NextOIDMgr.Clear;
    lOID := TOIDString.Create;
    try
      Check(lOID.IsNull, 'not lOID.IsNull');
      lRegPerLayer.NextOIDMgr.AssignNextOID(lOID, DatabaseName);
      Check(not lOID.IsNull, 'lOID.IsNull');
    finally
      lOID.Free;
    end;
  finally
    DropNextOIDTable;
  end;
end;


procedure TTestTIOIDManager.NextOIDGUID;
var
  i : integer;
  lOID : TOID;
  lsl : TStringList;
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  // This line should not be necessary and can be removed once we sort out
  // the relationship between:
  // Persistence layer->OIDClass->Database
  gTIOPFManager.DefaultOIDClassName := cOIDClassNameGUID;
  lRegPerLayer.NextOIDMgr.Clear;
  lOID  := gTIOPFManager.OIDFactory.CreateOID;
  try
    CheckIs(lOID, TOIDGUID, 'OID not a TOIDGUID');
    try
      lsl := TStringList.Create;
      try
        for i := 1 to cRepeatCount do
        begin
          lRegPerLayer.NextOIDMgr.AssignNextOID(lOID, DatabaseName);
          CheckEquals(-1, lsl.IndexOf(lOID.AsString), 'Non unique GUID');
          CheckEquals(36, Length(lOID.AsString), 'GUID length incorrect');
          lsl.Add(lOID.AsString);
          Check(not lOID.IsNull, 'lOID.IsNull');
          //Log(lOID.AsString);
        end;
      finally
        lsl.Free;
      end;
    finally
      lOID.Free;
    end;
  finally
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator(PerFrameworkSetup.DBName);
  end;
end;


constructor TTestTIOIDManager.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  SetupTasks := [sutPerLayer, sutDBConnection{, sutTables} ];
end;


procedure TTestTIOIDManager.NextOIDString;
var
  i : integer;
  lOID : TOID;
  lsl : TStringList;
  lNextOIDGenerator : TNextOIDGeneratorString;
  lRegPerLayer : TtiPersistenceLayer;
const
  cOIDLength   = 10;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  // This line should not be necessary and can be removed once we sort out
  // the relationship between:
  // Persistence layer->OIDClass->Database
  gTIOPFManager.DefaultOIDClassName := cOIDClassNameString;
  lRegPerLayer.NextOIDMgr.Clear;
  lNextOIDGenerator := (lRegPerLayer.NextOIDMgr.FindCreateByDatabaseName(PerFrameworkSetup.DBName) as TNextOIDGeneratorString);
  try
    lNextOIDGenerator.OIDChars := '0123456789';
    lNextOIDGenerator.OIDLength  := cOIDLength;
    lNextOIDGenerator.OIDPrefix  := '1';
    lOID  := gTIOPFManager.OIDFactory.CreateOID;
    try
      CheckIs(lOID, TOIDString, 'OID not a TOIDString');
      CreateNextOIDStrTable;

      lsl := TStringList.Create;
      try
        for i := 0 to cRepeatCount do
        begin
          lRegPerLayer.NextOIDMgr.AssignNextOID(lOID, DatabaseName);
          CheckEquals(-1, lsl.IndexOf(lOID.AsString), 'Non unique OID');
          CheckEquals(10, Length(lOID.AsString), 'OIDAsString length incorrect <' + lOID.AsString + '>');
          lsl.Add(lOID.AsString);
          Check(not lOID.IsNull, 'lOID.IsNull');
          //Log(lOID.AsString);
          CheckEquals(Power(10, cOIDLength-1)+ i, StrToInt(lOID.AsString), 'Failed on ' + IntToStr(i));
        end;
      finally
        lsl.Free;
      end;
    finally
      lOID.Free;
    end;
  finally
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator(PerFrameworkSetup.DBName);
  end;
end;


procedure TTestTIOIDManager.NextOIDStringSetNextOIDChars;
var
  lNextOIDGenerator : TNextOIDGeneratorString;
begin
  lNextOIDGenerator := TNextOIDGeneratorString.Create;
  try
    lNextOIDGenerator.OIDChars := 'abcd';
    CheckEquals('abcd', lNextOIDGenerator.OIDChars);
  finally
    lNextOIDGenerator.Free;
  end;
end;


procedure TTestTIOIDManager.NextOIDMgrGUID;
var
  lOID : TOID;
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  gTIOPFManager.DefaultOIDClassName := cOIDClassNameGUID;
  lOID := TOIDGUID.Create;
  try
    Check(lOID.IsNull, 'not lOID.IsNull');
    lRegPerLayer.NextOIDMgr.AssignNextOID(lOID, DatabaseName);
    Check(not lOID.IsNull, 'lOID.IsNull');
  finally
    lOID.Free;
  end;
end;


{ TOIDFactory_TST }

function TOIDFactory_TST.CountMappings: integer;
begin
  result := FList.Count;
end;


function TOIDFactory_TST.GetItems(AIndex: Integer): TOIDClassMapping;
begin
  result := TOIDClassMapping(FList.Items[AIndex]);
end;


procedure TTestTIOIDManager.tiObject_CreateNew;
var
  lData : TtiObjectList;
  lItem : TtiObject;
begin
  lData := TtiObjectList.CreateNew;
  try
    Check(lData.ObjectState = posCreate, 'Failed on ObjectState = posCreate');
    lItem := TtiObject.CreateNew(lData);
    lData.Add(lItem);
    Check(lItem.ObjectState = posCreate, 'Failed on ObjectState = posCreate');
    Check(lItem.Owner = lData, 'Failed on lItem.Owner = lData');
  finally
    lData.Free;
  end;
end;


procedure TTestTIOIDManager.tiObject_CreateNew_OIDGUID;
var
  lItem : TtiObject;
begin
  gTIOPFManager.DefaultOIDClassName := cOIDClassNameGUID;
  lItem := TtiObject.CreateNew;
  try
    CheckIs(lItem.OID, TOIDGUID, 'OID not a TOIDGUID');
    CheckEquals(36, Length(lItem.OID.AsString), 'OID does not look like a GUID');
  finally
    lItem.Free;
  end;
end;


procedure TTestTIOIDManager.tiObject_CreateNew_OIDInteger;
var
  lItem : TtiObject;
  lSavedPerLayerName : string;
  lSavedDatabaseName : string;
begin
  CreateNextOIDIntTable;
  try
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameInteger;
    lItem := TtiObject.CreateNew(DatabaseName, PerLayerName);
    try
      CheckIs(lItem.OID, TOIDInteger, 'OID not a TOIDInteger');
      CheckNotEquals('', lItem.OID.AsString, 'OID not assigned');
    finally
      lItem.Free;
    end;

    lSavedPerLayerName := gTIOPFManager.DefaultPerLayerName;
    lSavedDatabaseName := gTIOPFManager.DefaultDBConnectionName;
    gTIOPFManager.DefaultPerLayerName := PerLayerName;
    try
      gTIOPFManager.DefaultDBConnectionName := DatabaseName;
      try
        lItem := TtiObject.CreateNew;
        try
          CheckIs(lItem.OID, TOIDInteger, 'OID not a TOIDInteger');
          CheckNotEquals('', lItem.OID.AsString, 'OID not assigned');
        finally
          lItem.Free;
        end;
      finally
        gTIOPFManager.DefaultPerLayerName := lSavedPerLayerName;
      end;
    finally
      gTIOPFManager.DefaultDBConnectionName := lSavedDatabaseName;
    end;
  finally
    DropNextOIDTable;
  end;
end;


end.


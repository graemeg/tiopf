{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerObjOID_TST;

interface
uses
  Classes  // needed for TStringList
  ,tiPersistAbs_TST
  ,TestFrameWork
  ,tiPerObjOIDAbs
  ;

type

  // A support class for testing TOIDFactory
  TOIDFactory_TST = class( TOIDFactory )
  private
    function GetItems(pIndex: Integer): TOIDClassMapping;
  public
    function CountMappings : integer ;
    property Items[pIndex:Integer] : TOIDClassMapping read GetItems ;
  end ;

  TTestPerObjOIDMgr = class( TtiPerTestCase )
  published
    procedure OIDFactory_RegisterMapping ;
    procedure OIDFactory_CreateOID ;
    procedure OIDFactory_CreateNextOIDGenerator ;

    procedure NextOIDMgrInteger ;
    procedure NextOIDMgrString ;
    procedure NextOIDMgrGUID ;

    procedure NextOIDStringSetNextOIDChars;
    procedure NextOIDInteger;
    procedure NextOIDString;
    procedure NextOIDGUID;
  public
    constructor Create(MethodName: string); override ;
  end ;

  TTestPerObjOIDAbs = class( TTestCase )
  private
    FOIDClass : TOIDClass ;
  published
    procedure AsString  ; virtual ; abstract ;
    procedure AsVariant ; virtual ; abstract ;
    procedure Null      ; virtual ; abstract ;
    procedure Assign    ; virtual ; abstract ;
    procedure Compare   ;
    procedure Equals;
    procedure Clone ;

// Database dependant methods that do not have tests (yet)
//    procedure AssignToTIQuery;
//    procedure AssignToTIQuery;
//    procedure AssignFromTIQuery;
//    procedure AssignFromTIQuery;
//    procedure EqualsQueryField;
//    procedure GetNextValue;

  end ;

  TTestPerObjOIDInteger = class(TTestPerObjOIDAbs)
  protected
    procedure Setup ; override ;
  published
    procedure AsString ; override ;
    procedure AsVariant ; override ;
    procedure Null ; override ;
    procedure Assign; override;
  end ;

  TTestPerObjOIDString = class(TTestPerObjOIDAbs)
  protected
    procedure Setup ; override ;
  published
    procedure AsString ; override ;
    procedure AsVariant ; override ;
    procedure Null ; override ;
    procedure Assign; override;
  end ;

  TTestPerObjOIDGUID = class(TTestPerObjOIDAbs)
  protected
    procedure Setup ; override ;
  published
    procedure AsString ; override ;
    procedure AsVariant ; override ;
    procedure Null ; override ;
    procedure Assign; override;
  end ;

procedure RegisterTests ;

implementation
uses
   tiPersist
  ,SysUtils
  ,tiQuery
  ,tiPerObjOIDGUID    // Pull in the integer OID framework
  ,tiPerObjOIDInteger // Pull in the integer OID framework
  ,tiPerObjOIDString  // Pull in the string OID framework
  ,tiLog
  ,tiDBConnectionSetupAbs_TST
  ,Math
  ,tiDUnitDependencies
  ,tiWin32
  ,tiRegPerLayer
  ;

const
  // Number of times to repeat NextOID test
  // Set a high number for thorough testing (eg, 100000)
  // Set a low number for quick testing (eg, 100)
  cRepeatCount = 100 ;

procedure RegisterTests ;
begin
  tiDUnitDependencies.RegisterDBTests( 'NextOID', TTestPerObjOIDMgr );
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
  begin
    RegisterTest( TTestPerObjOIDInteger.Suite );
    RegisterTest( TTestPerObjOIDString.Suite );
    RegisterTest( TTestPerObjOIDGUID.Suite );
  end ;
end ;

procedure TTestPerObjOIDMgr.NextOIDInteger;
var
  i : integer ;
  lOIDStart : TOID ;
  lOIDCurrent : TOID ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);
  gTIPerMgr.DefaultOIDClassName := cOIDClassNameInteger ;

  lRegPerLayer.NextOIDMgr.Clear ;
  lOIDStart   := gTIPerMgr.OIDFactory.CreateOID ;
  lOIDCurrent := gTIPerMgr.OIDFactory.CreateOID ;
  CheckIs( lOIDStart, TOIDInteger ) ;
  try
    CheckIs( lOIDCurrent, TOIDInteger ) ;
    CreateNextOIDIntTable ;
    try
      lRegPerLayer.NextOIDMgr.AssignNextOID( lOIDStart, DatabaseName ) ;
      lOIDCurrent.Assign( lOIDStart ) ;
      CheckEquals( '100000000', lOIDStart.AsString, 'lOIDStart.AsString' ) ;
      CheckEquals( '100000000', lOIDCurrent.AsString, 'lOIDCurrent.AsString' ) ;
      for i := 0 to cRepeatCount do
      begin
        CheckEquals(
          StrToInt( lOIDStart.AsString ) + i,
          StrToInt( lOIDCurrent.AsString ),
          'Failed on iteration ' + IntToStr( i ));
        lRegPerLayer.NextOIDMgr.AssignNextOID( lOIDCurrent, DatabaseName ) ;
      end ;
    finally
      DropNextOIDTable ;
    end ;
  finally
    lOIDStart.Free ;
    lOIDCurrent.Free ;
  end ;
end;

(*
{ TTestPerObjOIDIBX }

procedure TTestPerObjOIDIBX.Setup;
begin
  FPerLayerID := cTIPersistIBX ;
  inherited;
end;

{ TTestPerObjOIDBDEParadox }

procedure TTestPerObjOIDBDEParadox.Setup;
begin
  FPerLayerID := cTIPersistBDEParadox ;
  inherited;
end;

{ TTestPerObjOIDADOAccess }

procedure TTestPerObjOIDADOAccess.Setup;
begin
  FPerLayerID := cTIPersistADOAccess ;
  inherited;
end;

{ TTestPerObjOIDADOSQLServer }
procedure TTestPerObjOIDADOSQLServer.Setup;
begin
  FPerLayerID := cTIPersistADOSQLServer ;
  inherited;
end;

{ TTestPerObjOIDDOA }

procedure TTestPerObjOIDDOA.Setup;
begin
  FPerLayerID := cTIPersistDOA ;
  inherited;
end;

{ TTestPerObjOIDXML }

procedure TTestPerObjOIDXML.Setup;
begin
  FPerLayerID := cTIPersistXML ;
  inherited;
end;

*)

procedure TTestPerObjOIDMgr.OIDFactory_CreateNextOIDGenerator;
var
  lFactory : TOIDFactory ;
  lNextOIDGenerator : TNextOIDGenerator ;
begin
  lFactory := TOIDFactory.Create ;
  try
    lFactory.RegisterMapping( cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger )  ;
    lFactory.RegisterMapping( cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID )  ;

    lNextOIDGenerator := lFactory.CreateNextOIDGenerator( cOIDClassNameInteger ) ;
    try
      CheckIs( lNextOIDGenerator, TNextOIDGeneratorInteger ) ;
    finally
      lNextOIDGenerator.Free ;
    end ;

    lNextOIDGenerator := lFactory.CreateNextOIDGenerator( cOIDClassNameGUID ) ;
    try
      CheckIs( lNextOIDGenerator, TNextOIDGeneratorGUID ) ;
    finally
      lNextOIDGenerator.Free ;
    end ;

  finally
    lFactory.Free ;
  end ;
end;

procedure TTestPerObjOIDMgr.OIDFactory_CreateOID;
var
  lFactory : TOIDFactory ;
  lOID : TOID ;
begin
  lFactory := TOIDFactory.Create ;
  try
    lFactory.RegisterMapping( cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger )  ;
    lFactory.RegisterMapping( cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID )  ;

    lOID := lFactory.CreateOID( cOIDClassNameInteger ) ;
    try
      CheckIs( lOID, TOIDInteger ) ;
    finally
      lOID.Free ;
    end ;

    lOID := lFactory.CreateOID( cOIDClassNameGUID ) ;
    try
      CheckIs( lOID, TOIDGUID ) ;
    finally
      lOID.Free ;
    end ;

  finally
    lFactory.Free ;
  end ;
end;

procedure TTestPerObjOIDMgr.OIDFactory_RegisterMapping;
var
  lFactory : TOIDFactory_TST ;
  lOIDClassMapping : TOIDClassMapping ;
begin
  lFactory := TOIDFactory_TST.Create ;
  try
    lFactory.RegisterMapping( cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger )  ;
    CheckEquals( 1, lFactory.CountMappings ) ;
    lFactory.RegisterMapping( cOIDClassNameGUID, TOIDGUID, TNextOIDGeneratorGUID )  ;
    CheckEquals( 2, lFactory.CountMappings ) ;
    lOIDClassMapping := lFactory.Items[0];
    CheckEquals( TOIDInteger, lOIDClassMapping.OIDClass ) ;
    CheckEquals( TNextOIDGeneratorInteger, lOIDClassMapping.NextOIDGeneratorClass ) ;

    lOIDClassMapping := lFactory.Items[1];
    CheckEquals( TOIDGUID, lOIDClassMapping.OIDClass ) ;
    CheckEquals( TNextOIDGeneratorGUID, lOIDClassMapping.NextOIDGeneratorClass ) ;

  finally
    lFactory.Free ;
  end ;
end;

{ TOIDFactory_TST }

function TOIDFactory_TST.CountMappings: integer;
begin
  result := FList.Count ;
end;

function TOIDFactory_TST.GetItems(pIndex: Integer): TOIDClassMapping;
begin
  result := TOIDClassMapping(FList.Items[pIndex]);
end;

procedure TTestPerObjOIDMgr.NextOIDMgrInteger;
var
  lOID : TOID ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  CreateNextOIDIntTable ;
  try
    // This required tidying up. At the time of writing, only
    // one OID class type is possible for an application instance
    gTIPerMgr.DefaultOIDClassName := cOIDClassNameInteger ;
    lRegPerLayer.NextOIDMgr.Clear ;
    lOID := TOIDInteger.Create ;
    try
      Check( lOID.IsNull, 'not lOID.IsNull' ) ;
      lRegPerLayer.NextOIDMgr.AssignNextOID( lOID, DatabaseName ) ;
      Check( not lOID.IsNull, 'lOID.IsNull' ) ;
    finally
      lOID.Free ;
    end ;
  finally
    DropNextOIDTable ;
  end ;
end;

procedure TTestPerObjOIDMgr.NextOIDMgrString;
var
  lOID : TOID ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  CreateNextOIDStrTable ;
  try
    gTIPerMgr.DefaultOIDClassName := cOIDClassNameString ;
    lRegPerLayer.NextOIDMgr.Clear ;
    lOID := TOIDString.Create ;
    try
      Check( lOID.IsNull, 'not lOID.IsNull' ) ;
      lRegPerLayer.NextOIDMgr.AssignNextOID( lOID, DatabaseName ) ;
      Check( not lOID.IsNull, 'lOID.IsNull' ) ;
    finally
      lOID.Free ;
    end ;
  finally
    DropNextOIDTable ;
  end ;
end;

procedure TTestPerObjOIDMgr.NextOIDGUID;
var
  i : integer ;
  lOID : TOID ;
  lsl : TStringList ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  // This line should not be necessary and can be removed once we sort out
  // the relationship between:
  // Persistence layer->OIDClass->Database
  gTIPerMgr.DefaultOIDClassName := cOIDClassNameGUID ;
  lRegPerLayer.NextOIDMgr.Clear ;
  lOID   := gTIPerMgr.OIDFactory.CreateOID ;
  try
    CheckIs( lOID, TOIDGUID, 'OID not a TOIDGUID');
    try
      lsl := TStringList.Create ;
      try
        for i := 1 to cRepeatCount do
        begin
          lRegPerLayer.NextOIDMgr.AssignNextOID( lOID, DatabaseName ) ;
          CheckEquals( -1, lsl.IndexOf( lOID.AsString ), 'Non unique GUID' ) ;
          CheckEquals( 36, Length( lOID.AsString ), 'GUID length incorrect' ) ;
          lsl.Add( lOID.AsString ) ;
          Check( not lOID.IsNull, 'lOID.IsNull' ) ;
          Log( lOID.AsString ) ;
        end ;
      finally
        lsl.Free ;
      end ;
    finally
      lOID.Free ;
    end ;
  finally
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator( PerFrameworkSetup.DBName ) ;
  end ;
end;

{ TTestPerObjOIDAbs }

procedure TTestPerObjOIDAbs.Clone;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID1.AsVariant := 1 ;
    lOID2 := lOID1.Clone ;
    try
      CheckIs( lOID2, FOIDClass, 'Failed on OID.Clone.ClassType' ) ;
      CheckEquals( lOID1.AsString, lOID2.AsString, 'Failed on lOID1.AsVariant = lOID2.AsVariant' ) ;
    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

procedure TTestPerObjOIDAbs.Compare;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
  lCompare : integer ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID2 := FOIDClass.Create ;
    try
      lOID1.AsVariant := 2 ;
      lOID2.AsVariant := 2 ;
      lCompare := lOID1.Compare( lOID2 );
      CheckEquals( 0, lCompare, 'Failed on compare = 0' ) ;

      lOID1.AsVariant := 2 ;
      lOID2.AsVariant := 1 ;
      lCompare := lOID1.Compare( lOID2 );
      CheckEquals( 1, lCompare, 'Failed on compare = 1' ) ;

      lOID1.AsVariant := 1 ;
      lOID2.AsVariant := 2 ;
      lCompare := lOID1.Compare( lOID2 );
      CheckEquals( -1, lCompare, 'Failed on compare = -1' ) ;
    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

procedure TTestPerObjOIDAbs.Equals;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
  lEquals : boolean ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID2 := FOIDClass.Create ;
    try
      lOID1.AsVariant := 1 ;
      lOID2.AsVariant := 1 ;
      lEquals := lOID1.Equals( lOID2 );
      Check( lEquals, 'Failed on Equals' ) ;

      lOID1.AsVariant := 1 ;
      lOID2.AsVariant := 2 ;
      lEquals := lOID1.Equals( lOID2 );
      Check( not lEquals, 'Failed on not Equals' ) ;

    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

{ TTestPerObjOIDInteger }

procedure TTestPerObjOIDInteger.Assign;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID2 := FOIDClass.Create ;
    try
      lOID1.AsVariant := 1 ;
      lOID2.Assign( lOID1 ) ;
      CheckEquals( lOID1.AsString, lOID2.AsString ) ;
    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

procedure TTestPerObjOIDInteger.AsString;
var
  lOID : TOID ;
begin
  lOID := FOIDClass.Create ;
  try
    lOID.AsString := '1' ;
    CheckEquals( '1', lOID.AsString, 'Failed on AsString' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDInteger.AsVariant;
var
  lOID : TOID ;
  lInt : integer ;
  lStr : string ;
begin
  lOID := FOIDClass.Create ;
  try
    lOID.AsVariant := '1' ;
    CheckEquals( '1', lOID.AsString, 'Failed on AsString' ) ;
    lStr := lOID.AsVariant ;
    CheckEquals( '1', lStr, 'Failed on AsVariant #1' ) ;
    lInt := lOID.AsVariant ;
    CheckEquals(  1, lInt, 'Failed on AsVariant #2' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDInteger.Null;
var
  lOID : TOID ;
begin
  lOID := FOIDClass.Create ;
  try
    Check( lOID.IsNull, 'Failed on IsNull' ) ;
    lOID.AsVariant := 1 ;
    Check( not lOID.IsNull, 'Failed on not IsNull' ) ;
    lOID.SetToNull ;
    Check( lOID.IsNull, 'Failed on SetToNull' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDInteger.Setup;
begin
  inherited;
  FOIDClass := TOIDInteger ;
end;

{ TTestPerObjOIDGUID }

procedure TTestPerObjOIDGUID.Assign;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID2 := FOIDClass.Create ;
    try
      lOID1.AsVariant := tiWin32CoCreateGUID;
      lOID2.Assign( lOID1 ) ;
      CheckEquals( lOID1.AsString, lOID2.AsString ) ;
    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

procedure TTestPerObjOIDGUID.AsString;
var
  lOID : TOID ;
  lValue : string ;
begin
  lValue := tiWin32CoCreateGUID;
  lOID := FOIDClass.Create ;
  try
    lOID.AsString := lValue ;
    CheckEquals( lValue, lOID.AsString, 'Failed on AsString' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDGUID.AsVariant;
var
  lOID : TOID ;
  lValue : string ;
  lStr : string ;
begin
  lValue := tiWin32CoCreateGUID;;
  lOID := FOIDClass.Create ;
  try
    lOID.AsVariant := lValue ;
    CheckEquals( lValue, lOID.AsString, 'Failed on AsString' ) ;
    lStr := lOID.AsVariant ;
    CheckEquals( lValue, lStr, 'Failed on AsVariant' ) ;
  finally
    lOID.Free ;
  end ;  
end;

procedure TTestPerObjOIDGUID.Null;
var
  lOID : TOID ;
begin
  lOID := FOIDClass.Create ;
  try
    Check( lOID.IsNull, 'Failed on IsNull' ) ;
    lOID.AsVariant := tiWin32CoCreateGUID; ;
    Check( not lOID.IsNull, 'Failed on not IsNull' ) ;
    lOID.SetToNull ;
    Check( lOID.IsNull, 'Failed on SetToNull' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDGUID.Setup;
begin
  inherited;
  FOIDClass := TOIDGUID ;
end;

constructor TTestPerObjOIDMgr.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [sutPerLayer, sutDBConnection{, sutTables} ];
end;
{ TTestPerObjOIDString }

procedure TTestPerObjOIDString.Assign;
var
  lOID1 : TOID ;
  lOID2 : TOID ;
begin
  lOID1 := FOIDClass.Create ;
  try
    lOID2 := FOIDClass.Create ;
    try
      lOID1.AsVariant := 'test' ;
      lOID2.Assign( lOID1 ) ;
      CheckEquals( lOID1.AsString, lOID2.AsString ) ;
    finally
      lOID2.Free ;
    end ;
  finally
    lOID1.Free ;
  end ;
end;

procedure TTestPerObjOIDString.AsString;
var
  lOID : TOID ;
begin
  lOID := FOIDClass.Create ;
  try
    lOID.AsString := 'test' ;
    CheckEquals( 'test', lOID.AsString, 'Failed on AsString' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDString.AsVariant;
var
  lOID : TOID ;
  lStr : string ;
begin
  lOID := FOIDClass.Create ;
  try
    lOID.AsVariant := 'test' ;
    CheckEquals( 'test', lOID.AsString, 'Failed on AsString' ) ;
    lStr := lOID.AsVariant ;
    CheckEquals( 'test', lStr, 'Failed on AsVariant #1' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDString.Null;
var
  lOID : TOID ;
begin
  lOID := FOIDClass.Create ;
  try
    Check( lOID.IsNull, 'Failed on IsNull' ) ;
    lOID.AsVariant := 'test' ;
    Check( not lOID.IsNull, 'Failed on not IsNull' ) ;
    lOID.SetToNull ;
    Check( lOID.IsNull, 'Failed on SetToNull' ) ;
  finally
    lOID.Free ;
  end ;
end;

procedure TTestPerObjOIDString.Setup;
begin
  inherited;
  FOIDClass := TOIDString ;
end;

procedure TTestPerObjOIDMgr.NextOIDString;
var
  i : integer ;
  lOID : TOID ;
  lsl : TStringList ;
  lNextOIDGenerator : TNextOIDGeneratorString ;
  lRegPerLayer : TtiRegPerLayer ;
const
  cOIDLength   = 10 ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  // This line should not be necessary and can be removed once we sort out
  // the relationship between:
  // Persistence layer->OIDClass->Database
  gTIPerMgr.DefaultOIDClassName := cOIDClassNameString ;
  lRegPerLayer.NextOIDMgr.Clear ;
  lNextOIDGenerator := ( lRegPerLayer.NextOIDMgr.FindCreateByDatabaseName(PerFrameworkSetup.DBName) as TNextOIDGeneratorString );
  try
    lNextOIDGenerator.OIDChars := '0123456789';
    lNextOIDGenerator.OIDLength   := cOIDLength ;
    lNextOIDGenerator.OIDPrefix   := '1' ;
    lOID   := gTIPerMgr.OIDFactory.CreateOID ;
    try
      CheckIs( lOID, TOIDString, 'OID not a TOIDString');
      CreateNextOIDStrTable ;

      lsl := TStringList.Create ;
      try
        for i := 0 to cRepeatCount do
        begin
          lRegPerLayer.NextOIDMgr.AssignNextOID( lOID, DatabaseName ) ;
          CheckEquals( -1, lsl.IndexOf( lOID.AsString ), 'Non unique OID' ) ;
          CheckEquals( 10, Length( lOID.AsString ), 'OIDAsString length incorrect <' + lOID.AsString + '>' ) ;
          lsl.Add( lOID.AsString ) ;
          Check( not lOID.IsNull, 'lOID.IsNull' ) ;
          Log( lOID.AsString ) ;
          CheckEquals( Power( 10, cOIDLength-1 )+ i, StrToInt( lOID.AsString ), 'Failed on ' + IntToStr(i));
        end ;
      finally
        lsl.Free ;
      end ;
    finally
      lOID.Free ;
    end ;
  finally
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator( PerFrameworkSetup.DBName ) ;
  end ;
end;

procedure TTestPerObjOIDMgr.NextOIDStringSetNextOIDChars;
var
  lNextOIDGenerator : TNextOIDGeneratorString ;
begin
  lNextOIDGenerator := TNextOIDGeneratorString.Create;
  try
    lNextOIDGenerator.OIDChars := 'abcd' ;
    CheckEquals( 'abcd', lNextOIDGenerator.OIDChars ) ;
  finally
    lNextOIDGenerator.Free ;
  end ;
end;

procedure TTestPerObjOIDMgr.NextOIDMgrGUID;
var
  lOID : TOID ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  CheckNotNull(lRegPerLayer, 'RegPerLayer not found <' + PerLayerName);

  gTIPerMgr.DefaultOIDClassName := cOIDClassNameGUID ;
  lOID := TOIDGUID.Create ;
  try
    Check( lOID.IsNull, 'not lOID.IsNull' ) ;
    lRegPerLayer.NextOIDMgr.AssignNextOID( lOID, DatabaseName ) ;
    Check( not lOID.IsNull, 'lOID.IsNull' ) ;
  finally
    lOID.Free ;
  end ;
end;

end.

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
    June, 2002, Peter Hinrichsen, Created
  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiSQLMgr_TST;

{$IFDEF OPTIMISE_XMLDB_SIZE}
  SQLMgr tests will not work with OPTIMISE_XMLDB_SIZE
{$ENDIF}

interface
uses
   tiPersistAbs_TST
  ,TestFramework
  ,tiSQLMgr_BOM
  ,tiPtnVisPerObj
  ,tiQuery
  ,tiXML
  ;

{$I tiDefines.inc}

type

  TTestTISQLMgr = class( TTestCase )
  published
    procedure SQLMgrsAddDatabase;
    procedure SQLMgrsFindByDatabaseName;
    procedure SQLMgrFindQueriesByName;
    procedure SQLMgrIsGroupUnique;
    procedure SQLMgrIsQueryUnique;
    procedure SQLMgrIsParamUnique;
    procedure SQLMgrParamAssign;
    procedure SQLMgrParamClone;
    procedure SQLMgrQueryAssign;
    procedure SQLMgrQueryClone;
//    procedure SQLMgrWriteConstants;
    // There ares some find visitors in tiSQLMgr_Cli that should also be tested
  end ;

  TTestTISQLMgrPersistence = class( TtiPerTestCase )
  private
    FXMLTags : TtiXMLTags ;
    procedure DeleteSQLMgrFile;
    procedure DoCreateTables;
    procedure DoDropTables;
  protected
    procedure Setup; override;
    procedure TearDown; override;

    // SQLMgrGroup
    procedure InsertTestSQLMgrGroup(const pOID: string);
    procedure CheckTestSQLMgrGroup(const pSQLMgrGroup : TSQLMgrGroup ; const pOID : string );

    // SQLMgrQuery
    procedure InsertTestSQLMgrQuery(const pOIDGroup, pOIDQuery: string);
    procedure CheckTestSQLMgrQuery(const pSQLMgrQuery : TSQLMgrQuery ; const pOID : string ; pObjectState : TPerObjectState);

    // SQLMgrParam
    procedure InsertTestSQLMgrParam(const pOIDQuery, pOIDParam: string; pParamType : TtiQueryFieldKind);
    procedure CheckTestSQLMgrParam(const  pSQLMgrParam : TSQLMgrParam ; const pOID : string; pParamType : TtiQueryFieldKind ) ;

  public
    constructor Create(MethodName: string); override ;
    destructor  Destroy ; override ;
  published
    procedure SQLMgrCreateFile;
    procedure SQLMgrCreateTables;
    procedure SQLMgrDropTables;

    procedure SQLMgrReadPK;

    procedure SQLMgrGroupCreate;
    procedure SQLMgrGroupUpdate;
    procedure SQLMgrGroupDelete;

    procedure SQLMgrQueryRead;
    procedure SQLMgrQueryReadByQueryName;
    procedure SQLMgrFindCreateQueryByName;

    procedure SQLMgrQueryCreate;
    procedure SQLMgrQueryUpdate;
    procedure SQLMgrQueryDelete;

    procedure SQLMgrParamCreate;
    procedure SQLMgrParamUpdate;
    procedure SQLMgrParamDelete;

  end ;

procedure RegisterTests ;

function  CreateTestSQLMgrGroup(const pOID : string ) : TSQLMgrGroup ;
procedure AssignTestSQLMgrGroup(pSQLMgrGroup : TSQLMgrGroup; const pOID : string ) ;
function  CreateTestSQLMgrQuery(const pOID : string): TSQLMgrQuery;
procedure AssignTestSQLMgrQuery(const pSQLMgrQuery : TSQLMgrQuery ; const pOID : string ) ;
function  CreateTestSQLMgrParam(const pOID : string; pParamType : TtiQueryFieldKind ): TSQLMgrParam;
procedure AssignTestSQLMgrParam(const pSQLMgrParam : TSQLMgrParam ; const pOID : string; pParamType : TtiQueryFieldKind ) ;

implementation
uses
  tiDUnitDependencies
  ,tiSQLMgr_Svr
  ,tiSQLMgr_Cli
  ,tiPersist
  ,cTIPersist
  ,SysUtils
  ,tiExcept
  ,Classes
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,tiDBConnectionSetupAbs_TST
  ,tiDialogs
  ,tiPerFrameworkXMLLight_TST
  ;

const
  cOIDSQLMgrGroup = '1000' ;
  cOIDSQLMgrQuery = '1100' ;
  cOIDSQLMgrParam = '1200' ;

procedure RegisterTests ;      
begin

  tiSQLMgr_BOM.RegisterMappings;

  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTISQLMgr.Suite ) ;

  if gPerFrameworkSetupFactory.ToRun( cTIPersistXMLLight ) then
    RegisterTest( 'SQLManager Persistence',
                  TTestDBConnectionSetupXMLLight.Create( TTestTISQLMgrPersistence.Suite ) );

//  tiDUnitDependencies.RegisterDBTests(
//    'SQLManager Persistence',
//    TTestTISQLMgrPersistence);

end ;

procedure AssignTestSQLMgrGroup(
  pSQLMgrGroup: TSQLMgrGroup; const pOID: string);
begin
  Assert( pSQLMgrGroup.TestValid(TSQLMgrGroup), cTIInvalidObjectError);
  pSQLMgrGroup.GroupName    := pOID ;
  pSQLMgrGroup.DispOrder    := StrToInt(pOID) ;
end;

procedure AssignTestSQLMgrParam(
  const pSQLMgrParam: TSQLMgrParam; const pOID: string; pParamType : TtiQueryFieldKind);
var
  lInt : integer ;
  lBool : boolean ;
  lType : string ;
begin
  Assert( pSQLMgrParam.TestValid(TSQLMgrParam), cTIInvalidObjectError);
  lInt  := StrToInt(pOID) ;
  lBool := lInt mod 2 = 1;
  lType := QueryFieldKindToString(pParamType);
  pSQLMgrParam.ParamName    := pOID;
  pSQLMgrParam.DispOrder    := lInt ;
  pSQLMgrParam.ParamValue   := pOID ;
  pSQLMgrParam.IsNull       := lBool;
  pSQLMgrParam.ParamTypeStr := lType ;
end;

procedure AssignTestSQLMgrQuery(
  const pSQLMgrQuery: TSQLMgrQuery; const pOID: string);
var
  lInt : integer ;
  lBool : boolean ;
begin
  Assert(pSQLMgrQuery.TestValid(TSQLMgrQuery),cTIInvalidObjectError);
  lInt  := StrToInt(pOID);
  lBool := lInt mod 2 = 1;
  pSQLMgrQuery.QueryName    := pOID;
  pSQLMgrQuery.DispOrder    := lInt ;
  pSQLMgrQuery.QueryDesc    := pOID;
  pSQLMgrQuery.QueryLocked  := lBool;
  pSQLMgrQuery.TestInclude  := lBool;
  pSQLMgrQuery.SQL          := pOID;
end;

function CreateTestSQLMgrGroup(
  const pOID: string): TSQLMgrGroup;
begin
  result := TSQLMgrGroup.Create;
  result.OID.AsString := pOID ;
  AssignTestSQLMgrGroup(result, pOID);
end;

function CreateTestSQLMgrParam(
  const pOID: string; pParamType : TtiQueryFieldKind ): TSQLMgrParam;
begin
  result := TSQLMgrParam.Create;
  result.OID.AsString := pOID ;
  AssignTestSQLMgrParam(result, pOID, pParamType);
end;

function CreateTestSQLMgrQuery(
  const pOID: string): TSQLMgrQuery;
begin
  result := TSQLMgrQuery.Create;
  result.OID.AsString := pOID ;
  AssignTestSQLMgrQuery(result, pOID);
end;

{ TTestTISQLMgr }

procedure TTestTISQLMgr.SQLMgrFindQueriesByName;
var
  lSQLMgr  : TSQLMgr ;
  lGroup1  : TSQLMgrGroup ;
  lQuery11 : TSQLMgrQuery ;
  lQuery12 : TSQLMgrQuery ;
  lGroup2  : TSQLMgrGroup ;
  lQuery21 : TSQLMgrQuery ;
  lQuery22 : TSQLMgrQuery ;
  lList    : TList ;
begin
  lSQLMgr := TSQLMgr.Create ;
  try
    lGroup1 := TSQLMgrGroup.Create;
    lGroup1.GroupName := 'Group1' ;
    lSQLMgr.Add(lGroup1);
    lQuery11 := TSQLMgrQuery.create;
    lQuery11.QueryName := 'Query11';
    lGroup1.Add(lQuery11);
    lQuery12 := TSQLMgrQuery.create;
    lQuery12.QueryName := 'Query12';
    lGroup1.Add(lQuery12);

    lGroup2 := TSQLMgrGroup.Create;
    lGroup2.GroupName := 'Group2' ;
    lSQLMgr.Add(lGroup2);
    lQuery21 := TSQLMgrQuery.create;
    lQuery21.QueryName := 'Query21';
    lGroup2.Add(lQuery21);
    lQuery22 := TSQLMgrQuery.create;
    lQuery22.QueryName := 'Query22';
    lGroup2.Add(lQuery22);

    lList    := TList.Create ;
    try
      lSQLMgr.FindQueriesByName('Query2?', lList);
      CheckEquals(2, lList.Count, 'Query2?');

      lSQLMgr.FindQueriesByName('Query1?', lList);
      CheckEquals(2, lList.Count, 'Query1?');

      lSQLMgr.FindQueriesByName('Query?1', lList);
      CheckEquals(2, lList.Count, 'Query?1');

      lSQLMgr.FindQueriesByName('Query11', lList);
      CheckEquals(1, lList.Count, 'Query11');

    finally
      lList.Free;
    end;


  finally
    lSQLMgr.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrIsGroupUnique;
var
  lSQLMgr  : TSQLMgr ;
  lGroup1  : TSQLMgrGroup ;
  lGroup2  : TSQLMgrGroup ;
  lGroup3  : TSQLMgrGroup ;
  lGroup4  : TSQLMgrGroup ;
begin
  lSQLMgr := TSQLMgr.Create ;
  try
    lGroup1 := TSQLMgrGroup.Create;
    lGroup1.OID.AsString := '1';
    lGroup1.GroupName := 'Group1' ;
    lSQLMgr.Add(lGroup1);

    lGroup2 := TSQLMgrGroup.Create;
    lGroup2.OID.AsString := '2';
    lGroup2.GroupName := 'Group2' ;
    lSQLMgr.Add(lGroup2);

    lGroup3 := TSQLMgrGroup.Create;
    lGroup3.OID.AsString := '3';
    lGroup3.GroupName := 'Group3' ;
    lSQLMgr.Add(lGroup3);

    Check(lSQLMgr.IsGroupNameUnique(lGroup1), 'Group1');
    Check(lSQLMgr.IsGroupNameUnique(lGroup2), 'Group2');
    Check(lSQLMgr.IsGroupNameUnique(lGroup3), 'Group3');

    lGroup3.GroupName := 'Group2' ;
    Check(not lSQLMgr.IsGroupNameUnique(lGroup2), 'Group2');
    lGroup3.GroupName := 'Group3' ;

    lGroup4 := TSQLMgrGroup.Create;
    lGroup4.OID.AsString := '4';
    lGroup4.GroupName := 'Group3' ;
    Check(not lSQLMgr.IsGroupNameUnique(lGroup4), 'Group4');
    lGroup4.GroupName := 'Group4' ;
    Check(lSQLMgr.IsGroupNameUnique(lGroup4), 'Group4');


    lSQLMgr.Add(lGroup4);

  finally
    lSQLMgr.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrParamClone;
var
  lParam1 : TSQLMgrParam ;
  lParam2 : TSQLMgrParam ;
begin
  lParam1 := CreateTestSQLMgrParam(cOIDSQLMgrQuery,qfkString);
  try
    lParam2 := lParam1.Clone;
    try
      CheckEquals(lParam1.ParamName, lParam2.ParamName, 'ParamName');
      CheckEquals(lParam1.ParamTypeStr, lParam2.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam1.ParamValue, lParam2.ParamValue, 'ParamValue');
      CheckEquals(lParam1.IsNull, lParam2.IsNull, 'IsNull');
    finally
      lParam2.Free;
    end;
  finally
    lParam1.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrIsParamUnique;
var
  lQuery : TSQLMgrQuery;
  lParam1 : TSQLMgrParam;
  lParam2 : TSQLMgrParam;
  lParam3 : TSQLMgrParam;
begin
  lQuery := TSQLMgrQuery.Create;
  try
    lParam1 := TSQLMgrParam.Create;
    lParam1.OID.AsString := '1';
    lParam1.ParamName := 'Param1';
    lQuery.Params.Add(lParam1);

    lParam2 := TSQLMgrParam.Create;
    lParam2.OID.AsString := '2';
    lParam2.ParamName := 'Param2';
    lQuery.Params.Add(lParam2);

    lParam3 := TSQLMgrParam.Create;
    lParam3.OID.AsString := '3';
    lParam3.ParamName := 'Param3';
    lQuery.Params.Add(lParam3);

    Check(lQuery.IsParamNameUnique(lParam1), 'Param1');
    Check(lQuery.IsParamNameUnique(lParam2), 'Param2');
    Check(lQuery.IsParamNameUnique(lParam3), 'Param3');

    lParam2.ParamName := 'Param3';
    Check(not lQuery.IsParamNameUnique(lParam2), 'Param2');

    lParam2.ParamName := UpperCase('Param3');
    Check(not lQuery.IsParamNameUnique(lParam2), 'Param2');

  finally
    lQuery.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrQueryAssign;
var
  lQuery1  : TSQLMgrQuery ;
  lParam11 : TSQLMgrParam ;
  lParam12 : TSQLMgrParam ;
  lQuery2  : TSQLMgrQuery;
  lParam21 : TSQLMgrParam ;
  lParam22 : TSQLMgrParam ;
begin
  lQuery1  := CreateTestSQLMgrQuery(cOIDSQLMgrQuery);
  try
    lParam11 := CreateTestSQLMgrParam(cOIDSQLMgrParam, qfkString);
    lQuery1.Params.Add(lParam11);
    lParam12 := CreateTestSQLMgrParam(cOIDSQLMgrParam+'1', qfkInteger);
    lQuery1.Params.Add(lParam12);
    lQuery2  := TSQLMgrQuery.create;
    try
      lQuery2.Assign(lQuery1);
      CheckEquals( lQuery1.QueryName,   lQuery2.QueryName, 'QueryName');
      CheckEquals( lQuery1.QueryDesc,   lQuery2.QueryDesc, 'QueryDesc');
      CheckEquals( lQuery1.QueryLocked, lQuery2.QueryLocked, 'QueryLocked');
      CheckEquals( lQuery1.TestInclude, lQuery2.TestInclude, 'TestInclude');
      CheckEquals( lQuery1.SQL, lQuery2.SQL, 'SQL');

      CheckEquals( 2, lQuery2.Params.Count, 'Params.Count');
      lParam21 := lQuery2.Params.Items[0] ;
      CheckEquals(lParam11.ParamName,    lParam21.ParamName, 'ParamName');
      CheckEquals(lParam11.ParamTypeStr, lParam21.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam11.ParamValue,   lParam21.ParamValue, 'ParamValue');
      CheckEquals(lParam11.IsNull,       lParam21.IsNull, 'IsNull');

      lParam22 := lQuery2.Params.Items[1] ;
      CheckEquals(lParam12.ParamName,    lParam22.ParamName, 'ParamName');
      CheckEquals(lParam12.ParamTypeStr, lParam22.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam12.ParamValue,   lParam22.ParamValue, 'ParamValue');
      CheckEquals(lParam12.IsNull,       lParam22.IsNull, 'IsNull');

    finally
      lQuery2.Free;
    end;
  finally
    lQuery1.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrQueryClone;
var
  lQuery1  : TSQLMgrQuery ;
  lParam11 : TSQLMgrParam ;
  lParam12 : TSQLMgrParam ;
  lQuery2  : TSQLMgrQuery;
  lParam21 : TSQLMgrParam ;
  lParam22 : TSQLMgrParam ;
begin
  lQuery1  := CreateTestSQLMgrQuery(cOIDSQLMgrQuery);
  try
    lParam11 := CreateTestSQLMgrParam(cOIDSQLMgrParam, qfkString);
    lQuery1.Params.Add(lParam11);
    lParam12 := CreateTestSQLMgrParam(cOIDSQLMgrParam+'1', qfkInteger);
    lQuery1.Params.Add(lParam12);
    lQuery2  := lQuery1.Clone;
    try
      CheckEquals( lQuery1.QueryName,   lQuery2.QueryName, 'QueryName');
      CheckEquals( lQuery1.QueryDesc,   lQuery2.QueryDesc, 'QueryDesc');
      CheckEquals( lQuery1.QueryLocked, lQuery2.QueryLocked, 'QueryLocked');
      CheckEquals( lQuery1.TestInclude, lQuery2.TestInclude, 'TestInclude');
      CheckEquals( lQuery1.SQL, lQuery2.SQL, 'SQL');

      CheckEquals( 2, lQuery2.Params.Count, 'Params.Count');
      lParam21 := lQuery2.Params.Items[0] ;
      CheckEquals(lParam11.ParamName,    lParam21.ParamName, 'ParamName');
      CheckEquals(lParam11.ParamTypeStr, lParam21.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam11.ParamValue,   lParam21.ParamValue, 'ParamValue');
      CheckEquals(lParam11.IsNull,       lParam21.IsNull, 'IsNull');

      lParam22 := lQuery2.Params.Items[1] ;
      CheckEquals(lParam12.ParamName,    lParam22.ParamName, 'ParamName');
      CheckEquals(lParam12.ParamTypeStr, lParam22.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam12.ParamValue,   lParam22.ParamValue, 'ParamValue');
      CheckEquals(lParam12.IsNull,       lParam22.IsNull, 'IsNull');

    finally
      lQuery2.Free;
    end;
  finally
    lQuery1.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrIsQueryUnique;
var
  lSQLMgr  : TSQLMgr ;
  lGroup1  : TSQLMgrGroup ;
  lQuery11 : TSQLMgrQuery ;
  lQuery12 : TSQLMgrQuery ;
  lGroup2  : TSQLMgrGroup ;
  lQuery21 : TSQLMgrQuery ;
  lQuery22 : TSQLMgrQuery ;
begin
  lSQLMgr := TSQLMgr.Create ;
  try
    lGroup1 := TSQLMgrGroup.Create;
    lGroup1.GroupName := 'Group1' ;
    lSQLMgr.Add(lGroup1);
    lQuery11 := TSQLMgrQuery.create;
    lQuery11.OID.AsString := '11';
    lQuery11.QueryName := 'Query11';
    lGroup1.Add(lQuery11);
    lQuery12 := TSQLMgrQuery.create;
    lQuery11.OID.AsString := '12';
    lQuery12.QueryName := 'Query12';
    lGroup1.Add(lQuery12);

    lGroup2 := TSQLMgrGroup.Create;
    lGroup2.GroupName := 'Group2' ;
    lSQLMgr.Add(lGroup2);
    lQuery21 := TSQLMgrQuery.create;
    lQuery11.OID.AsString := '21';
    lQuery21.QueryName := 'Query21';
    lGroup2.Add(lQuery21);
    lQuery22 := TSQLMgrQuery.create;
    lQuery11.OID.AsString := '22';
    lQuery22.QueryName := 'Query22';
    lGroup2.Add(lQuery22);

    Check(lSQLMgr.IsQueryNameUnique(lQuery11), 'Query11');
    Check(lSQLMgr.IsQueryNameUnique(lQuery12), 'Query12');
    Check(lSQLMgr.IsQueryNameUnique(lQuery21), 'Query21');
    Check(lSQLMgr.IsQueryNameUnique(lQuery22), 'Query22');

    lQuery12.QueryName := 'Query11' ;
    Check(not lSQLMgr.IsQueryNameUnique(lQuery12), 'Query12');

    lQuery12.QueryName := UpperCase('Query11') ;
    Check(not lSQLMgr.IsQueryNameUnique(lQuery12), 'Query12');

  finally
    lSQLMgr.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrsAddDatabase;
var
  lSQLMgrs : TSQLMgrs ;
begin
  lSQLMgrs := TSQLMgrs.Create ;
  try
    lSQLMgrs.AddDatabase('TestDatabaseName');
    CheckEquals(1, lSQLMgrs.Count, 'Count');
    CheckEquals('TestDatabaseName', lSQLMgrs.Items[0].DatabaseName, 'DatabaseName');
  finally
    lSQLMgrs.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrsFindByDatabaseName;
var
  lSQLMgrs : TSQLMgrs ;
  lSQLMgr  : TSQLMgr ;
begin
  lSQLMgrs := TSQLMgrs.Create ;
  try
    lSQLMgrs.AddDatabase('TestDatabaseName_1');
    lSQLMgrs.AddDatabase('TestDatabaseName_2');
    lSQLMgrs.AddDatabase('TestDatabaseName_3');
    CheckEquals(3, lSQLMgrs.Count, 'Count');
    lSQLMgr := lSQLMgrs.FindByDatabaseName('TestDatabaseName_2');
    CheckNotNull(lSQLMgr, 'lSQLMgr');
    CheckEquals('TestDatabaseName_2', lSQLMgr.DatabaseName, 'DatabaseName');
    lSQLMgr := lSQLMgrs.FindByDatabaseName('TestDatabaseName_4');
    CheckNotNull(lSQLMgr, 'lSQLMgr');
    CheckEquals('TestDatabaseName_4', lSQLMgr.DatabaseName, 'DatabaseName');
  finally
    lSQLMgrs.Free;
  end;
end;

procedure TTestTISQLMgr.SQLMgrParamAssign;
var
  lParam1 : TSQLMgrParam ;
  lParam2 : TSQLMgrParam ;
begin
  lParam1 := CreateTestSQLMgrParam(cOIDSQLMgrQuery,qfkString);
  try
    lParam2 := TSQLMgrParam.Create;
    try
      lParam2.Assign(lParam1);
      CheckEquals(lParam1.ParamName, lParam2.ParamName, 'ParamName');
      CheckEquals(lParam1.ParamTypeStr, lParam2.ParamTypeStr, 'ParamTypeStr');
      CheckEquals(lParam1.ParamValue, lParam2.ParamValue, 'ParamValue');
      CheckEquals(lParam1.IsNull, lParam2.IsNull, 'IsNull');
    finally
      lParam2.Free;
    end;
  finally
    lParam1.Free;
  end;
end;

{ TTestTISQLMgrPersistence }

procedure TTestTISQLMgrPersistence.CheckTestSQLMgrGroup(
  const pSQLMgrGroup: TSQLMgrGroup; const pOID: string);
var
  lData : TSQLMgrGroup ;
begin
  Assert(pSQLMgrGroup.TestValid(TSQLMgrGroup),cTIInvalidObjectError);
  lData := CreateTestSQLMgrGroup(pOID);
  try
    CheckEquals(pOID, pSQLMgrGroup.GroupName, 'GroupName');
    CheckEquals(StrToInt(pOID), pSQLMgrGroup.DispOrder, 'DispOrder');
  finally
    lData.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.CheckTestSQLMgrParam(
  const pSQLMgrParam: TSQLMgrParam; const pOID: string;
  pParamType : TtiQueryFieldKind ) ;
var
  lData : TSQLMgrParam;
begin
  Assert(pSQLMgrParam.TestValid(TSQLMgrParam),cTIInvalidObjectError);
  lData := CreateTestSQLMgrParam(pOID, pParamType);
  try
    CheckEquals( lData.ParamName, pSQLMgrParam.ParamName, 'ParamName');
    CheckEquals( lData.DispOrder, pSQLMgrParam.DispOrder, 'DispOrder');
    CheckEquals( lData.ParamValue, pSQLMgrParam.ParamValue, 'ParamValue');
    CheckEquals( lData.IsNull, pSQLMgrParam.IsNull, 'IsNull');
    CheckEquals( lData.ParamTypeStr, pSQLMgrParam.ParamTypeStr, 'ParamTypeStr');
  finally
    lData.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.CheckTestSQLMgrQuery(
  const pSQLMgrQuery: TSQLMgrQuery; const pOID: string;
  pObjectState : TPerObjectState);
var
  lData : TSQLMgrQuery;
begin
  Assert(pSQLMgrQuery.TestValid(TSQLMgrQuery),cTIInvalidObjectError);
  lData := CreateTestSQLMgrQuery(pOID);
  try
    CheckEquals(lData.QueryName, pSQLMgrQuery.QueryName,    'Name');
    CheckEquals(lData.DispOrder, pSQLMgrQuery.DispOrder,    'DispOrder');
    if pObjectState = posPK then
    begin
      CheckEquals('',    pSQLMgrQuery.QueryDesc,   'Desc');
      CheckEquals(False, pSQLMgrQuery.QueryLocked, 'Locked');
      CheckEquals(False, pSQLMgrQuery.TestInclude, 'TestInclude');
      CheckEquals('',    pSQLMgrQuery.SQL,         'SQL');
    end else
    begin
      CheckEquals(lData.QueryDesc,  pSQLMgrQuery.QueryDesc,   'Desc');
      CheckEquals(lData.QueryLocked, pSQLMgrQuery.QueryLocked, 'Locked');
      CheckEquals(lData.TestInclude, pSQLMgrQuery.TestInclude, 'TestInclude');
      CheckEquals(lData.SQL,  pSQLMgrQuery.SQL,         'SQL');
    end ;
  finally
    lData.Free;
  end;
end;

constructor TTestTISQLMgrPersistence.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [sutPerLayer, sutDBConnection];
  FXMLTags := TtiXMLTags.Create ;
end;

procedure TTestTISQLMgrPersistence.DeleteSQLMgrFile;
begin
{
  if FileExists(FSQLMgrFileName) then
    DeleteFile(FSQLMgrFileName);
  Check(not FileExists(FSQLMgrFileName), 'Failed deleting pre-existing SQLMgr file');
}
end;

destructor TTestTISQLMgrPersistence.Destroy;
begin
  FXMLTags.Free;
  inherited;
end;

procedure TTestTISQLMgrPersistence.DoCreateTables;
var
  lSQLMgr : TSQLMgr ;
begin
  lSQLMgr  := TSQLMgr.Create;
  try
    lSQLMgr.FileName := DatabaseName;
    lSQLMgr.CreateTables;
  finally
    lSQLMgr.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.DoDropTables;
var
  lSQLMgr : TSQLMgr ;
begin
  lSQLMgr  := TSQLMgr.Create;
  try
    lSQLMgr.FileName := DatabaseName;
    lSQLMgr.DropTables;
  finally
    lSQLMgr.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.InsertTestSQLMgrGroup(const pOID: string);
var
  lParams : TtiQueryParams;
  lData   : TSQLMgrGroup ;
begin
  lParams := TtiQueryParams.Create;
  try
    lData := CreateTestSQLMgrGroup(pOID);
    try
      lParams.SetValueAsString(cFieldNameGroupOID,        lData.OID.AsString);
      lParams.SetValueAsInteger(cFieldNameGroupDispOrder, lData.DispOrder);
      lParams.SetValueAsString(cFieldNameGroupName,      lData.GroupName);
    finally
      lData.Free;
    end;
    gTIPerMgr.InsertRow(cTableNameSQLManGroup,lParams, DatabaseName, PerLayerName);
  finally
    lParams.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.InsertTestSQLMgrParam(const pOIDQuery, pOIDParam: string; pParamType : TtiQueryFieldKind);
var
  lParams : TtiQueryParams;
  lData   : TSQLMgrParam;
begin
  lData   := CreateTestSQLMgrParam(pOIDParam, pParamType);
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString(  cFieldNameParamOID,       lData.OID.AsString);
    lParams.SetValueAsString(  cFieldNameParamOIDSQL,    pOIDQuery);
    lParams.SetValueAsString(  cFieldNameParamName,      lData.ParamName);
    lParams.SetValueAsInteger( cFieldNameParamDispOrder, lData.DispOrder);
    lParams.SetValueAsString(  cFieldNameParamValue,     lData.ParamValue);
    lParams.SetValueAsBoolean( cFieldNameParamIsNull,    ldata.IsNull);
    lParams.SetValueAsString(  cFieldNameParamType,      lData.ParamTypeStr);
    gTIPerMgr.InsertRow(cTableNameSQLManParam,lParams, DatabaseName, PerLayerName);
  finally
    lParams.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.InsertTestSQLMgrQuery(const pOIDGroup, pOIDQuery: string);
var
  lParams : TtiQueryParams;
  lData : TSQLMgrQuery;
begin
  lParams := TtiQueryParams.Create;
  try
    lData := CreateTestSQLMgrQuery(pOIDQuery);
    try
      lParams.SetValueAsString(  cFieldNameSQLOID,         lData.OID.AsString);
      lParams.SetValueAsString(  cFieldNameSQLOIDGroup,    pOIDGroup);
      lParams.SetValueAsInteger( cFieldNameSQLDispOrder,   lData.DispOrder);
      lParams.SetValueAsInteger( cFieldNameSQLVersion,     lData.DispOrder);
      lParams.SetValueAsString(  cFieldNameSQLName,        lData.QueryName);
      lParams.SetValueAsString(  cFieldNameSQLDesc,        lData.QueryDesc);
      lParams.SetValueAsBoolean( cFieldNameSQLLocked,      lData.QueryLocked);
      lParams.SetValueAsBoolean( cFieldNameSQLTestInclude, lData.TestInclude);
      lParams.SetValueAsString(  cFieldNameSQLSQL,         pOIDQuery);
    finally
      lData.Free;
    end;
    gTIPerMgr.InsertRow(cTableNameSQLManSQL,lParams, DatabaseName, PerLayerName);
  finally
    lParams.Free;
  end;
end;

procedure TTestTISQLMgrPersistence.Setup;
begin
  DeleteSQLMgrFile;
  inherited;
end;

procedure TTestTISQLMgrPersistence.SQLMgrCreateFile;
var
  ls : string ;
begin
  TSQLMgr.CreateFile(DatabaseName);
  try
    Check(FileExists(DatabaseName), 'File not Create');
    ls := tiFileToString(DatabaseName);
    CheckEquals(FXMLTags.MakeXMLDatabase, ls);
  finally
    DeleteFile(DatabaseName);
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrCreateTables;
var
  lMDTable : TtiDBMetaDataTable;
  lMDField : TtiDBMetaDataField;
begin

  DoCreateTables;

  Check(gTIPerMgr.TableExists(cTableNameSQLManGroup, DatabaseName),
        'Table ' + cTableNameSQLManGroup + ' does not exist');
  Check(gTIPerMgr.TableExists(cTableNameSQLManSQL, DatabaseName),
        'Table ' + cTableNameSQLManSQL + ' does not exist');
  Check(gTIPerMgr.TableExists(cTableNameSQLManParam, DatabaseName),
        'Table ' + cTableNameSQLManParam + ' does not exist');

  // ToDo: Also check field kind and field size
  lMDTable := TtiDBMetaDataTable.Create;
  try
    lMDTable.Name := cTableNameSQLManGroup;
    gTIPerMgr.ReadMetaDataFields(lMDTable, DatabaseName, PerLayerName);
    CheckEquals( 3, lMDTable.Count, 'FieldCount cTableNameSQLManGroup');
    lMDField := lMDTable.FindByFieldName(cFieldNameGroupOID);
    CheckNotNull(lMDField, 'Field not found: ' + cFieldNameGroupOID);
    lMDField := lMDTable.FindByFieldName(cFieldNameGroupDispOrder);
    CheckNotNull(lMDField, 'Field not found: ' + cFieldNameGroupDispOrder);
    lMDField := lMDTable.FindByFieldName(cFieldNameGroupName);
    CheckNotNull(lMDField, 'Field not found: ' + cFieldNameGroupName);
  finally
    lMDTable.Free;
  end;

  lMDTable := TtiDBMetaDataTable.Create;
  try
    lMDTable.Name := cTableNameSQLManSQL;
    gTIPerMgr.ReadMetaDataFields(lMDTable, DatabaseName);
    CheckEquals( 9, lMDTable.Count, 'FieldCount cTableNameSQLManSQL');

  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLOID);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLOID);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLOIDGroup);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLOIDGroup);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLDispOrder);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLDispOrder);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLVersion);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLVersion);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLName);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLName);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLDesc);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLDesc);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLLocked);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLLocked);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLTestInclude);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLTestInclude);
  lMDField := lMDTable.FindByFieldName(  cFieldNameSQLSQL);
  CheckNotNull(lMDField, 'Field not found: ' + cFieldNameSQLSQL);

  finally
    lMDTable.Free;
  end;

  lMDTable := TtiDBMetaDataTable.Create;
  try
    lMDTable.Name := cTableNameSQLManParam;
    gTIPerMgr.ReadMetaDataFields(lMDTable, DatabaseName, PerLayerName);
    CheckEquals( 7, lMDTable.Count, 'FieldCount cTableNameSQLManParam');

  lMDField := lMDTable.FindByFieldName(    cFieldNameParamOID);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamOID);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamOIDSQL);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamOIDSQL);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamDispOrder);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamDispOrder);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamName);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamName);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamType);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamType);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamValue);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamValue);
  lMDField := lMDTable.FindByFieldName(    cFieldNameParamIsNull);
  CheckNotNull(lMDField, 'Field not found: ' +   cFieldNameParamIsNull);

  finally
    lMDTable.Free;
  end;

end;

procedure TTestTISQLMgrPersistence.SQLMgrDropTables;
var
  lSQLMgr  : TSQLMgr;
begin
  try gTIPerMgr.DropTable(cTableNameSQLManParam,DatabaseName, PerLayerName) except end ;
  try gTIPerMgr.DropTable(cTableNameSQLManSQL,DatabaseName, PerLayerName) except end ;
  try gTIPerMgr.DropTable(cTableNameSQLManGroup,DatabaseName, PerLayerName) except end ;

  lSQLMgr  := TSQLMgr.Create;
  try
    lSQLMgr.FileName := DatabaseName;
    lSQLMgr.CreateTables;
  finally
    lSQLMgr.Free;
  end;

  Check(gTIPerMgr.TableExists(cTableNameSQLManGroup, DatabaseName),
        'Table ' + cTableNameSQLManGroup + ' does not exist');
  Check(gTIPerMgr.TableExists(cTableNameSQLManSQL, DatabaseName),
        'Table ' + cTableNameSQLManSQL + ' does not exist');
  Check(gTIPerMgr.TableExists(cTableNameSQLManParam, DatabaseName),
        'Table ' + cTableNameSQLManParam + ' does not exist');

  lSQLMgr  := TSQLMgr.Create;
  try
    lSQLMgr.FileName := DatabaseName;
    lSQLMgr.DropTables;
  finally
    lSQLMgr.Free;
  end;

  Check(not gTIPerMgr.TableExists(cTableNameSQLManGroup, DatabaseName),
        'Table ' + cTableNameSQLManGroup + ' exist when it shoul not');
  Check(not gTIPerMgr.TableExists(cTableNameSQLManSQL, DatabaseName),
        'Table ' + cTableNameSQLManSQL + ' exist when it shoul not');
  Check(not gTIPerMgr.TableExists(cTableNameSQLManParam, DatabaseName),
        'Table ' + cTableNameSQLManParam + ' exist when it shoul not');

end;

procedure TTestTISQLMgrPersistence.SQLMgrFindCreateQueryByName;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin

  DoCreateTables;
  try
    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgrQuery := lSQLMgr.FindCreateQueryByName(cOIDSQLMgrQuery);
      CheckNull(lSQLMgrQuery);
    finally
      lSQLMgr.Free;
    end;

    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgrQuery := lSQLMgr.FindCreateQueryByName(cOIDSQLMgrQuery);
      CheckNotNull(lSQLMgrQuery);
      CheckObjectState(posClean, lSQLMgrQuery);
      CheckEquals(0, lSQLMgrQuery.Params.Count, 'Params.Count');
      CheckEquals('',    lSQLMgrQuery.QueryDesc,   'Desc');
      CheckEquals(False, lSQLMgrQuery.QueryLocked, 'Locked');
      CheckEquals(False, lSQLMgrQuery.TestInclude, 'TestInclude');
      CheckEquals(cOIDSQLMgrQuery, lSQLMgrQuery.SQL,         'SQL');
      CheckEquals(0, lSQLMgr.Count, 'lSQLMgr.Count');
      CheckEquals(1, lSQLMgr.Queries.Count, 'lSQLMgr.Queries.Count');
      CheckSame( lSQLMgrQuery, lSQLMgr.Queries.Items[0]);
    finally
      lSQLMgr.Free;
    end;
  finally
    DoDropTables;
  end ;

end;

procedure TTestTISQLMgrPersistence.SQLMgrGroupCreate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
begin
  DoCreateTables;
  try
    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgrGroup := CreateTestSQLMgrGroup(cOIDSQLMgrGroup);
      lSQLMgrGroup.ObjectState := posCreate ;
      lSQLMgr.Add(lSQLMgrGroup);
      lSQLMgr.Save;
      CheckObjectState(posClean,lSQLMgrGroup);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup);
      CheckObjectState(posClean, lSQLMgrGroup);
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrGroupDelete;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);
    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrGroup.Items[0].Read(DatabaseName,PerLayerName);
      lSQLMgrGroup.Deleted := true ;
      lSQLMgrGroup.Save;
      CheckObjectState(posDeleted, lSQLMgrGroup);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(0, lSQLMgr.Count, 'Count');
    finally
      lSQLMgr.Free;
    end;
  finally
    DoDropTables;
  end ;
end;


procedure TTestTISQLMgrPersistence.SQLMgrGroupUpdate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      AssignTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup + '1');
      lSQLMgrGroup.Dirty := true ;
      lSQLMgrGroup.Save;
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup+'1');
      CheckObjectState(posClean, lSQLMgrGroup);
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrParamCreate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
  lSQLMgrParam : TSQLMgrParam ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];

      lSQLMgrParam := CreateTestSQLMgrParam(cOIDSQLMgrParam, qfkString);
      lSQLMgrParam.ObjectState := posCreate ;
      lSQLMgrQuery.Params.Add(lSQLMgrParam, false);
      lSQLMgr.Save;
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];

      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posClean);
      CheckObjectState(posClean, lSQLMgrQuery);
      CheckEquals(1, lSQLMgrQuery.Params.Count, 'Params.Count');
      CheckTestSQLMgrParam(lSQLMgrQuery.Params.Items[0], cOIDSQLMgrParam, qfkString);

    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrParamDelete;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
  lSQLMgrParam : TSQLMgrParam ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckEquals(1, lSQLMgrQuery.Params.Count, 'Params.Count');
      lSQLMgrParam := lSQLMgrQuery.Params.Items[0];

      CheckTestSQLMgrParam(lSQLMgrParam, cOIDSQLMgrParam, qfkString);
      AssignTestSQLMgrParam(lSQLMgrParam, cOIDSQLMgrParam+'1', qfkInteger);
      lSQLMgrParam.Deleted := true ;
      lSQLMgr.Save;
      CheckObjectState(posDeleted, lSQLMgrParam);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckEquals(0, lSQLMgrQuery.Params.Count, 'Params.Count');
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrParamUpdate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
  lSQLMgrParam : TSQLMgrParam ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckEquals(1, lSQLMgrQuery.Params.Count, 'Params.Count');
      lSQLMgrParam := lSQLMgrQuery.Params.Items[0];

      CheckTestSQLMgrParam(lSQLMgrParam, cOIDSQLMgrParam, qfkString);
      AssignTestSQLMgrParam(lSQLMgrParam, cOIDSQLMgrParam+'1', qfkInteger);
      lSQLMgrParam.Dirty := true ;
      lSQLMgr.Save;
      CheckObjectState(posClean, lSQLMgrParam);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckEquals(1, lSQLMgrQuery.Params.Count, 'Params.Count');
      lSQLMgrParam := lSQLMgrQuery.Params.Items[0];
      CheckTestSQLMgrParam(lSQLMgrParam, cOIDSQLMgrParam+'1', qfkInteger);
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrQueryCreate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup);
      CheckObjectState(posClean, lSQLMgrGroup);

      lSQLMgrQuery := CreateTestSQLMgrQuery(cOIDSQLMgrQuery);
      lSQLMgrQuery.ObjectState := posCreate;
      lSQLMgrGroup.Add(lSQLMgrQuery, false);
      lSQLMgr.Save;
      CheckObjectState(posClean, lSQLMgrQuery);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posPK);
      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posClean);
      CheckObjectState(posClean, lSQLMgrQuery);
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrQueryDelete;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      try
        lSQLMgrQuery.Deleted := true ;
        Fail('Exception not raised when it should have been');
      except
        on e:exception do
          CheckExceptionMessage(cExcCanNotSetObjectStateDeleteWhenPK, e);
      end ;
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrQueryRead;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup);
      CheckObjectState(posClean, lSQLMgrGroup);

      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posPK);
      CheckObjectState(posPK, lSQLMgrQuery);
      CheckEquals(0, lSQLMgrQuery.Params.Count, 'Params.Count');

      lSQLMgrQuery.Read(DatabaseName,PerLayerName) ;
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posClean);
      CheckObjectState(posClean, lSQLMgrQuery);
      CheckEquals(1, lSQLMgrQuery.Params.Count, 'Params.Count');
      CheckTestSQLMgrParam(lSQLMgrQuery.Params.Items[0], cOIDSQLMgrParam, qfkString);

    finally
      lSQLMgr.Free;
    end;
  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrQueryReadByQueryName;
var
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgrQuery := TSQLMgrQuery.Create ;
    try
      lSQLMgrQuery.ReadByQueryName(cOIDSQLMgrQuery, DatabaseName);
      CheckObjectState(posClean, lSQLMgrQuery);
      CheckEquals(0, lSQLMgrQuery.Params.Count, 'Params.Count');

      CheckEquals('',    lSQLMgrQuery.QueryDesc,   'Desc');
      CheckEquals(False, lSQLMgrQuery.QueryLocked, 'Locked');
      CheckEquals(False, lSQLMgrQuery.TestInclude, 'TestInclude');
      CheckEquals(cOIDSQLMgrQuery, lSQLMgrQuery.SQL,         'SQL');

    finally
      lSQLMgrQuery.Free;
    end;
  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrQueryUpdate;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      AssignTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery + '1');
      lSQLMgrQuery.ObjectState := posUpdate;
      lSQLMgr.Save;
      CheckObjectState(posClean,lSQLMgrQuery);
    finally
      lSQLMgr.Free;
    end;

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      lSQLMgrQuery.Read(DatabaseName,PerLayerName);
      CheckTestSQLMgrQuery(lSQLMgrQuery,cOIDSQLMgrQuery + '1',posClean);
    finally
      lSQLMgr.Free;
    end;

  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.SQLMgrReadPK;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  DoCreateTables;
  try
    InsertTestSQLMgrGroup(cOIDSQLMgrGroup);
    InsertTestSQLMgrQuery(cOIDSQLMgrGroup, cOIDSQLMgrQuery);
    InsertTestSQLMgrParam(cOIDSQLMgrQuery, cOIDSQLMgrParam, qfkString);

    lSQLMgr := TSQLMgr.Create ;
    try
      lSQLMgr.FileName := DatabaseName;
      lSQLMgr.ReadPK;
      CheckEquals(1, lSQLMgr.Count, 'Count');
      lSQLMgrGroup := lSQLMgr.Items[0];
      CheckTestSQLMgrGroup(lSQLMgrGroup, cOIDSQLMgrGroup);
      CheckObjectState(posClean, lSQLMgrGroup);

      CheckEquals(1, lSQLMgrGroup.Count, 'Count');
      lSQLMgrQuery := lSQLMgrGroup.Items[0];
      CheckTestSQLMgrQuery(lSQLMgrQuery, cOIDSQLMgrQuery, posPK);
      CheckObjectState(posPK, lSQLMgrQuery);
      CheckEquals(0, lSQLMgrQuery.Params.Count, 'Params.Count');

      CheckEquals(1, lSQLMgr.Queries.Count, 'lSQLMgr.Queries' );
      CheckNotNull(lSQLMgr.FindQueryByName(cOIDSQLMgrQuery), 'FindQueryByName');

    finally
      lSQLMgr.Free;
    end;
  finally
    DoDropTables;
  end ;
end;

procedure TTestTISQLMgrPersistence.TearDown;
begin
  DeleteSQLMgrFile;
  inherited;
end;

end.


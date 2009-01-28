{
  For testing SQL based database: IBX, BDEParadox, DOA, ADO
}
unit tiQuerySQL_TST;

{$I tiDefines.inc}

interface
uses
  tiQuery_TST
 ;


type
  TTestTIQuerySQL = class(TTestTIQueryAbs)
  published
    procedure GetSetSQL; override;
    procedure QueryType; override;
    {$IFNDEF FPC}   { Temporary disabled for FPC - will fix soon (Graeme) }
    procedure ParamName; override;
    procedure ParamCount; override;
    procedure ParamsAsString; override;
    procedure ParamAsString; override;
    procedure ParamAsInteger; override;
    procedure ParamAsFloat; override;
    procedure ParamAsBoolean; override;
    procedure ParamAsDateTime; override;
    procedure ParamAsStream; override;
    procedure ParamAsMacro; override;
    procedure ParamIsNull; override;
    {$ENDIF}
    procedure OpenCloseActive; override;
    procedure ExecSQL; override;
    procedure RowsAffected; override;
  end;


implementation
uses
  Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,Contnrs
  ,SysUtils
  ,tiUtils
  ,TypInfo
  {$IFNDEF DELPHI6ORABOVE}
  ,FileCtrl
  {$ENDIF}
  ,tiTestFramework
  ,tiOPFTestCase
  ,tiQuery
 ;


procedure TTestTIQuerySQL.ExecSQL;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'select count(*) from test_group';
    Query.Open;
    Check(Query.FieldAsInteger[ Query.FieldName(0) ] = 1, 'FQuery.ExecSQL failed');
    Query.Close;
    Query.SQLText := 'delete from test_group';
    Query.ExecSQL;
    Query.SQLText := 'select count(*) from test_group';
    Query.Open;
    Check(Query.FieldAsInteger[Query.FieldName(0)] = 0, 'FQuery.ExecSQL failed');
    Query.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQuerySQL.OpenCloseActive;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'select * from Test_Group';
    Check(not Query.Active, 'FQuery.Active = false failed');
    Query.Open;
    Check(Query.Active, 'FQuery.Open failed');
    Query.Close;
    Check(not Query.Active, 'FQuery.Close failed');
    Query.Active := true;
    Check(Query.Active, 'FQuery.Active := true failed');
    Query.Active := false;
    Check(not Query.Active, 'FQuery.Active := false failed');
    Database.DeleteRow('test_group', nil);
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQuerySQL.GetSetSQL;
var
  lsl : TStringList;
const
  cSQL1 = 'select * from test_group';
  cSQL2 = 'delete from test_group';
begin

    Query.SQLText := cSQL1;

    Check(SameText(Trim(Query.SQLText),
                     Trim(cSQL1)),
           'Error with SQL test #1a');

    Query.SQLText := cSQL2;
    Check(SameText(Trim(Query.SQLText),
                     Trim(cSQL2)),
           'Error with SQL test #2a');

    lsl := TStringList.Create;
    try
      lsl.Text := cSQL1;
      Query.SQL.Assign(lsl);
      Check(SameText(Trim(Query.SQLText),
                       Trim(cSQL1)),
             'Error with SQL test #1b');

      lsl.Clear;
      lsl.Text := cSQL2;
      Query.SQL.Assign(lsl);
      Check(SameText(Trim(Query.SQLText),
                       Trim(cSQL2)),
             'Error with SQL test #2b');

    finally
      lsl.Free;
    end;

    Query.SQL.Clear;
    Check(SameText(Trim(Query.SQLText),
                     ''),
           'Error clearing SQL');

    Query.SQL.Add(cSQL1);
    Check(SameText(Trim(Query.SQLText),
                     Trim(cSQL1)),
           'Error with SQL test #1c');

    Query.SQL.Clear;
    Query.SQL.Add(cSQL2);
    Check(SameText(Trim(Query.SQLText),
                     Trim( cSQL2)),
           'Error with SQL test #2c');
end;


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsBoolean;
begin
  CreateTableInteger(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update ' + cTIQueryTableName +
                       ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
    Query.ParamAsBoolean[ cTIQueryColName ]:= False;
    CheckEquals(False, Query.ParamAsBoolean[ cTIQueryColName ], 'False');
    Query.ParamAsBoolean[ cTIQueryColName ]:= True;
    CheckEquals(True, Query.ParamAsBoolean[ cTIQueryColName ],  'True');
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsDateTime;
var
  lDate : TDateTime;
begin
  lDate := Now;
  CreateTableDateTime(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update ' + cTIQueryTableName +
                       ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
    Query.ParamAsDateTime[ cTIQueryColName ]:= lDate;
    CheckEquals(lDate, Query.ParamAsDateTime[ cTIQueryColName ], 0.0001);
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsFloat;
const
  cValue = 12345.6789;
begin
  CreateTableFloat(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update ' + cTIQueryTableName +
                       ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
    Query.ParamAsFloat[ cTIQueryColName ]:= cValue;
    CheckEquals(cValue, Query.ParamAsFloat[ cTIQueryColName ], 0.00001);
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsInteger;
begin
  CreateTableInteger(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update ' + cTIQueryTableName +
                       ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
    Query.ParamAsInteger[ cTIQueryColName ]:= 123456;
    CheckEquals(123456, Query.ParamAsInteger[ cTIQueryColName ]);
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsMacro;
const
  cFields = 'Item_Str_Field';
  cTable  = 'Test_Item';
begin
  Query.SQLText := 'select &fields from &table';
  Query.ParamAsMacro[ 'fields' ]:= cFields;
  Query.ParamAsMacro[ 'table' ] := cTable;
  Check(SameText(Trim(Query.SQLText),
                   'select ' + cFields + ' from ' + cTable),
         'ParamAsMacro failed');
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsString;
begin
  CreateTableString(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update ' + cTIQueryTableName +
                       ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
    Query.ParamAsString[ cTIQueryColName ]:= 'mickymouse';
    CheckEquals('mickymouse', Query.ParamAsString[ cTIQueryColName ]);
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamCount;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = ''Test''';

    Check(Query.ParamCount = 0, 'ParamCount failed on 0');

    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = :Group_Str_Field ';
    Query.ParamAsString[  'Group_Str_Field' ]  := 'test';
    Check(Query.ParamCount = 1, 'ParamCount failed on 1');

    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = :Group_Str_Field,  ' +
        'Group_Int_FIELD   = :Group_Int_Field  ';
    Query.ParamAsString[  'Group_Str_Field' ]  := 'test';
    Query.ParamAsInteger[ 'Group_Int_Field' ]  := 123;
    Check(Query.ParamCount = 2, 'ParamCount failed on 2');

    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = :Group_Str_Field,  ' +
        'Group_Int_FIELD   = :Group_Int_Field,  ' +
        'Group_Float_FIELD = :Group_Float_Field ';
    Query.ParamAsString[  'Group_Str_Field' ]  := 'test';
    Query.ParamAsInteger[ 'Group_Int_Field' ]  := 123;
    Query.ParamAsFloat[   'Group_Float_Field' ]:= 3.3;
    Check(Query.ParamCount = 3, 'ParamCount failed on 3');

  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamIsNull;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'Update Test_Group set Group_STR_FIELD = :Group_Str_Field';
    Query.ParamAsString[ 'Group_Str_Field' ]:= 'mickymouse';
    Check(Query.ParamIsNull[ 'Group_Str_Field' ] = false, 'Error checking ParamIsNull (false)');
    Query.ParamIsNull[ 'Group_Str_Field' ]:= true;
    Check(Query.ParamIsNull[ 'Group_Str_Field' ] = true, 'Error checking ParamIsNull (true)');
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamName;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_Str_Field   = :Group_Str_Field,  ' +
        'Group_Int_FIELD   = :Group_Int_Field,  ' +
        'Group_Float_FIELD = :Group_Float_Field ';

    // Not necessary to set parameter for ParmaName to work for most DBs, but
    // is required for DOA
    Query.ParamAsString[  'Group_Str_Field'   ]:= 'test';
    Query.ParamAsInteger[ 'Group_Int_Field'   ]:= 123;
    Query.ParamAsFloat[   'Group_Float_Field' ]:= 3.3;

    Check(SameText(Query.ParamName(0), 'Group_Str_Field'  ),   'ParamName failed on 0');
    Check(SameText(Query.ParamName(1), 'Group_Int_Field'  ),   'ParamName failed on 1');
    Check(SameText(Query.ParamName(2), 'Group_Float_Field'), 'ParamName failed on 2');
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamsAsString;
var
  lResult : string;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = ''Test''';
    lResult := Query.ParamsAsString;
    Check(SameText(
           lResult,
           ''),
           'ParamsAsStr failed with 0 params. Returned values was:' + CrLf +
           Query.ParamsAsString);

    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = :Group_Str_Field ';
    Query.ParamAsString[ 'Group_Str_Field' ]:= 'test';
    lResult := Query.ParamsAsString;
    Check(SameText(
           lResult,
           'Group_STR_FIELD := test'),
           'ParamsAsStr failed with 1 param. Returned values was:' + CrLf +
           Query.ParamsAsString);

    Query.SQLText :=
      'Update Test_Group set ' +
        'Group_STR_FIELD   = :Group_Str_Field,  ' +
        'Group_Int_FIELD   = :Group_Int_Field  ';
    Query.ParamAsString[ 'Group_Str_Field' ]:= 'test';
    Query.ParamAsInteger[ 'Group_Int_Field' ]:= 123;
    lResult := Query.ParamsAsString;
    Check(SameText(
           lResult,
           'Group_STR_FIELD := test' + CrLf +
           'Group_INT_FIELD := 123'),
           'ParamsAsStr failed with 2 params. Returned values was:' + CrLf +
           Query.ParamsAsString);
  finally
    Database.Commit;
  end;
end;
{$ENDIF}


procedure TTestTIQuerySQL.QueryType;
begin
  // This should be extended to:
  // a) Manage the removal of comments
  // b) Manage DML SQL
  Query.SQLText := 'select * from test_group';
  Check(Query.QueryType = qtSelect, 'Error testing QueryType with SELECT');

  Query.SQLText := 'update test_group set group_int_field = 1';
  Check(Query.QueryType = qtUpdate, 'Error testing QueryType with UPDATE');

  Query.SQLText := 'insert into test_group (group_int_field) values (1)';
  Check(Query.QueryType = qtInsert, 'Error testing QueryType with INSERT');

  Query.SQLText := 'delete from test_group where group_int_field = 1';
  Check(Query.QueryType = qtDelete, 'Error testing QueryType with DELETE');
end;


{$IFNDEF FPC}
procedure TTestTIQuerySQL.ParamAsStream;
var
  lStreamFrom : TStringStream;
  lStreamTo  : TMemoryStream;
begin
  CreateTableStream(Database);
  lStreamFrom := TStringStream.Create(LongString);
  try
    Database.StartTransaction;
    try
      lStreamTo  := TMemoryStream.Create;
      try
        Query.SQLText := 'Update ' + cTIQueryTableName +
                           ' set ' + cTIQueryColName + ' =:' + cTIQueryColName;
        Query.AssignParamFromStream(cTIQueryColName, lStreamFrom);
        Query.AssignParamToStream(cTIQueryColName, lStreamTo);
        CheckStreamContentsSame(lStreamFrom, lStreamTo);
      finally
        lStreamTo.Free;
      end;
    finally
      Database.Commit;
    end;
  finally
    lStreamFrom.Free;
  end;
end;
{$ENDIF}

procedure TTestTIQuerySQL.RowsAffected;
const
  NewOID='B9B61BB1-52E3-43F5-9143-A5B96149310E';
var
  LCountRowsAffected:integer;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    Query.SQLText := 'insert into test_group (OID,Group_Int_Field) VALUES ('+QuotedStr(NewOID)+',1)';
    LCountRowsAffected:=Query.ExecSQL;
    if Query.SupportsRowsAffected then
    begin
      CheckEquals(1,LCountRowsAffected,'Error testing ExecSQL WITH INSERT');

      Query.SQLText := 'update test_group SET Group_Int_Field=3 '
                      +' WHERE OID='+QuotedStr(NewOID)+' AND Group_Int_Field=2';
      LCountRowsAffected:=Query.ExecSQL;
      CheckEquals(0,LCountRowsAffected,'Error testing ExecSQL WITH UPDATE');

      Query.SQLText := 'update test_group SET Group_Int_Field=3 '
                      +' WHERE OID='+QuotedStr(NewOID)+' AND Group_Int_Field=1';
      LCountRowsAffected:=Query.ExecSQL;
      CheckEquals(1,LCountRowsAffected,'Error testing ExecSQL WITH UPDATE');

      Query.SQLText := 'delete from test_group';
      LCountRowsAffected:=Query.ExecSQL;
      CheckEquals(2,LCountRowsAffected,'Error testing ExecSQL WITH DELETE');
    end else
      CheckEquals(-1,LCountRowsAffected,'ExecSQL should return -1 if SupportsRowsAffected is false!');
    Query.Close;
  finally
    Database.Commit;
  end;
end;

end.







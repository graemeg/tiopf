{
  For testing Non SQL databases: XML, CSV
}
unit tiQueryNonSQL_TST;

{$I tiDefines.inc}

interface
uses
  tiQuery_TST;


type
  TTestTIQueryNonSQL = class(TTestTIQueryAbs)
  protected
    // These don't get run as they only have meaning for SQL databases
    procedure GetSetSQL;       override;
    procedure QueryType;       override;
    procedure OpenCloseActive; override;
    procedure ExecSQL;         override;
  published
    // SQL management
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
  end;


implementation
uses
  tiLog,
  tiTestFramework,
  tiOPFTestCase,
  tiUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  TypInfo,
  SysUtils,
  Classes;


procedure TTestTIQueryNonSQL.QueryType;
begin
  Assert(false, 'Test not implemented');
end;


procedure TTestTIQueryNonSQL.ExecSQL;
begin
  Check(True);
  LogWarning( 'Test not implemented');
end;


procedure TTestTIQueryNonSQL.OpenCloseActive;
begin
  Assert(false, 'Test not implemented');
end;


procedure TTestTIQueryNonSQL.ParamAsBoolean;
begin
  Query.ParamAsBoolean[ cTIQueryColName ]:= True;
  CheckEquals(True, Query.ParamAsBoolean[ cTIQueryColName ], 'True');
  Query.ParamAsBoolean[ cTIQueryColName ]:= False;
  CheckEquals(False, Query.ParamAsBoolean[ cTIQueryColName ], 'False');
end;


procedure TTestTIQueryNonSQL.ParamAsDateTime;
var
  lDate : TDateTime;
begin
  lDate := now;
  Query.ParamAsDateTime[ cTIQueryColName ]:= lDate;
  CheckEquals(lDate, Query.ParamAsDateTime[ cTIQueryColName ], 0.00001);
end;


procedure TTestTIQueryNonSQL.ParamAsFloat;
const
  cValue = 12345.6789;
begin
  Query.ParamAsFloat[ cTIQueryColName ]:= cValue;
  CheckEquals(cValue, Query.ParamAsFloat[ cTIQueryColName ], 0.00001);
end;


procedure TTestTIQueryNonSQL.ParamAsInteger;
begin
  Query.ParamAsInteger[ cTIQueryColName ]:= 123456;
  CheckEquals(123456, Query.ParamAsInteger[ cTIQueryColName ]);
end;


procedure TTestTIQueryNonSQL.ParamAsMacro;
begin
  Check(True);
  LogWarning( 'Test not implemented');
end;


procedure TTestTIQueryNonSQL.ParamAsString;
begin
  Query.ParamAsString[ cTIQueryColName ]:= 'mickymouse';
  CheckEquals('mickymouse', Query.ParamAsString[ cTIQueryColName ]);
end;


procedure TTestTIQueryNonSQL.ParamCount;
begin
  Check(Query.ParamCount = 0, 'ParamCount failed on 0');
  Query.ParamAsString[ 'Item_Str_Field' ]:= 'test';
  Check(Query.ParamCount = 1, 'ParamCount failed on 1');

  Query.ParamAsInteger[ 'Item_Int_Field' ]:= 123;
  Check(Query.ParamCount = 2, 'ParamCount failed on 2');

  Query.ParamAsFloat[   'Item_Float_Field' ]:= 3.3;
  Check(Query.ParamCount = 3, 'ParamCount failed on 3');
end;


procedure TTestTIQueryNonSQL.ParamIsNull;
begin
  Query.ParamAsString[ 'Item_Str_Field' ]:= 'micky mouse';
  Check(Query.ParamIsNull[ 'Item_Str_Field' ] = false, 'Error checking ParamIsNull (false)');
  Query.ParamIsNull[ 'Item_Str_Field' ]:= true;
  Check(Query.ParamIsNull[ 'Item_Str_Field' ] = true, 'Error checking ParamIsNull (true)');
end;


procedure TTestTIQueryNonSQL.ParamName;
begin
  Query.ParamAsString[ 'Item_Str_Field' ]:= 'test';
  Query.ParamAsInteger[ 'Item_Int_Field' ]:= 123;
  Query.ParamAsFloat[   'Item_Float_Field' ]:= 3.3;
  Check(SameText(Query.ParamName(0), 'Item_Str_Field'),   'ParamName failed on 0');
  Check(SameText(Query.ParamName(1), 'Item_Int_Field'),   'ParamName failed on 1');
  Check(SameText(Query.ParamName(2), 'Item_Float_Field'), 'ParamName failed on 2');
end;


procedure TTestTIQueryNonSQL.ParamsAsString;
const
  cAns1 = 'ITEM_STR_FIELD:= test';
  cAns2 = 'ITEM_INT_FIELD:= 123';
  cAns3 = 'ITEM_FLOAT_FIELD:= 3.3';
var
  LActual : string;
begin
  LActual := Query.ParamsAsString;
  CheckEquals('', LActual);

  Query.ParamAsString[ 'ITEM_STR_FIELD' ]:= 'test';
  LActual := Query.ParamsAsString;
  CheckEquals(cAns1, LActual);

  Query.ParamAsInteger[ 'ITEM_INT_FIELD' ]:= 123;
  LActual := Query.ParamsAsString;
  CheckEquals(cAns1 + CrLf + cAns2, LActual);

  Query.ParamAsFloat[   'ITEM_FLOAT_FIELD' ]:= 3.3;
  LActual := Query.ParamsAsString;
  CheckEquals( cAns1 + CrLf +
         cAns2 + CrLf +
         cAns3, LActual);
end;


{ EMethodNotImplemented }

procedure TTestTIQueryNonSQL.GetSetSQL;
begin
  Assert(false, 'Test not implemented');
end;


procedure TTestTIQueryNonSQL.ParamAsStream;
var
  lStreamFrom : TStringStream;
  lStreamTo  : TMemoryStream;
begin
  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo  := TMemoryStream.Create;
    try
      Query.AssignParamFromStream('Item_Binary_Field', lStreamFrom);
      Query.AssignParamToStream('Item_Binary_Field', lStreamTo);
      CheckStreamContentsSame(lStreamFrom, lStreamTo);
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;


end.

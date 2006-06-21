unit tiCriteria_TST;

interface
uses
  TestFrameWork
  ;

type
  TTestTICriteria = class( TTestCase )
  published

    // There will be some tests on the data structure -
    // Not sure exactly what will be required. I tend to
    // test even trivial code. Look inside tiDataSet_TST
    // for some ideas.
    procedure TestPerColumn;
    procedure TestPerColumns;
    procedure TestPerCriteria;
    procedure TestPerCriteriaList;
    procedure TestPerSelectionCriteriaAbs;
    procedure TestPerSelectionCriteriaList;

    // Test SQL generation - these will have to be cloned
    // for SQL, XPath and TPerObjList
    procedure TestPerEqualToCriteria_SQL;
    procedure TestPerEqualToFieldCriteria_SQL;
    procedure TestPerExistsCriteria_SQL;
    procedure TestPerGreatherThanCriteria_SQL;
    procedure TestPerGreatherThanFieldCriteria_SQL;
    procedure TestPerInCriteria_SQL;
    procedure TestPerLessThanCriteria_SQL;
    procedure TestPerLessThanFieldCriteria_SQL;
    procedure TestPerLikeCriteria_SQL;
    procedure TestPerNullCriteria_SQL;
    procedure TestPerBetweenCriteria_SQL;
    procedure TestPerSQLCriteria_SQL;

  end ;

procedure RegisterTests ;

implementation
uses
  tiCriteria
  ,tiPtnVisCriteria
  ,SysUtils
  ;

procedure RegisterTests ;
begin
  RegisterTest( TTestTICriteria.Suite );
end ;

{ TTestTICriteria }

procedure TTestTICriteria.TestPerBetweenCriteria_SQL;
var
  lCriteria : TPerCriteria ;
  lSQL : string ;
begin
  lCriteria := TPerCriteria.Create('test') ;
  try
    lCriteria.AddBetween('FIELD_1', '1', '2');
    lSQL := Trim(tiPerCriteriaAsSQL(lCriteria));
    CheckEquals( '(FIELD_1 BETWEEN 1 AND 2)', lSQL ) ;
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerColumn;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerColumns;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerCriteria;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerCriteriaList;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerEqualToCriteria_SQL;
var
  lCriteria : TPerCriteria ;
  lSQL : string ;
begin
  lCriteria := TPerCriteria.Create('test') ;
  try
    lCriteria.AddEqualTo('OID', '1');
    lSQL := Trim(tiPerCriteriaAsSQL(lCriteria));
    CheckEquals( '(OID = ''1'')', lSQL ) ;
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerEqualToFieldCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerExistsCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerGreatherThanCriteria_SQL;
var
  lCriteria : TPerCriteria ;
  lSQL : string ;
begin
  lCriteria := TPerCriteria.Create('test') ;
  try
    lCriteria.AddGreaterThan('FIELD_1', '1');
    lSQL := Trim(tiPerCriteriaAsSQL(lCriteria));
    CheckEquals( '(FIELD_1 > ''1'')', lSQL ) ;
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerGreatherThanFieldCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerInCriteria_SQL;
var
  lCriteria : TPerCriteria ;
  lSQL : string ;
begin
  lCriteria := TPerCriteria.Create('test') ;
  try
    lCriteria.AddIn('OID', 'SELECT OID FROM SUB_TABLE');
    lSQL := Trim(tiPerCriteriaAsSQL(lCriteria));
    CheckEquals( '(OID IN (SELECT OID FROM SUB_TABLE))', lSQL ) ;
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLessThanCriteria_SQL;
var
  lCriteria : TPerCriteria ;
  lSQL : string ;
begin
  lCriteria := TPerCriteria.Create('test') ;
  try
    lCriteria.AddLessThan('OID', '1');
    lSQL := Trim(tiPerCriteriaAsSQL(lCriteria));
    CheckEquals( '(OID < ''1''', lSQL ) ;
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLessThanFieldCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerLikeCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerNullCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerSelectionCriteriaAbs;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerSelectionCriteriaList;
begin
  Assert(false,'Under construction');
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL;
begin
  Assert(false,'Under construction');
end;

end.




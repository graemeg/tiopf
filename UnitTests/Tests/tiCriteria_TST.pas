unit tiCriteria_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTestFramework
  ;

type
  TTestTICriteria = class(TtiTestCase)
  published
    // Here are some trivial tests
    procedure TestPerColumns;
    procedure TestPerCriteriaList;
    procedure TestPerCriteria_IsEmbraced;
    procedure TestPerCriteria_ClearAll;
    procedure TestPerSelectionCriteriaList;

    // Testing SQL generation
    procedure TestPerEqualToCriteria_SQL;
    procedure TestPerExistsCriteria_SQL;
    procedure TestPerGreaterThanCriteria_SQL;
    procedure TestPerGreaterOrEqualThanCriteria_SQL;
    procedure TestPerInCriteria_SQL;
    procedure TestPerLessThanCriteria_SQL;
    procedure TestPerLikeCriteria_SQL;
    procedure TestPerNullCriteria_SQL;
    procedure TestPerBetweenCriteria_SQL;
    procedure TestPerSQLCriteria_SQL;

    // Testing XPath generation
    // The SQL tests will need to be clone for XPath at some point
  end;

procedure RegisterTests;

implementation
uses
  tiCriteria
  ,tiVisitorCriteria
  ,SysUtils
  ,tiDUnitDependencies
  ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTICriteria);
end;

{ TTestTICriteria }

procedure TTestTICriteria.TestPerColumns;
var
  lColumns: TPerColumns;
  lCol: TPerColumn;
begin
  lColumns := TPerColumns.Create;
  try
    lCol := TPerColumn.Create;
    lCol.Name       := 'Field1';
    lCol.Ascending  := True;
    lColumns.Add(lCol);
    CheckEquals(1, lColumns.Count, 'Failed on 1');
    CheckSame(lCol, lColumns.Items[0], 'Failed on 2');
    CheckSame(lColumns, lCol.Owner, 'Failed on 3');
    CheckEquals(True, lCol.Ascending, 'Failed on 4');
    CheckEquals('Field1', lCol.Name, 'Failed on 5');

    lCol := TPerColumn.Create;
    lCol.Name       := 'Field2';
    lCol.Ascending  := False;
    lColumns.Add(lCol);
    CheckEquals(2, lColumns.Count, 'Failed on 6');
    CheckSame(lCol, lColumns.Items[1], 'Failed on 7');
    CheckSame(lColumns, lCol.Owner, 'Failed on 8');
    CheckEquals(False, lCol.Ascending, 'Failed on 9');
    CheckEquals('Field2', lCol.Name, 'Failed on 10');
    
    lColumns.Clear;
    CheckEquals(0, lColumns.Count, 'Failed on 11');
  finally
    lColumns.Free;
  end;
end;

procedure TTestTICriteria.TestPerCriteriaList;
var
  lList: TPerCriteriaList;
  lItem: TPerCriteria;
begin
  lList := TPerCriteriaList.Create;
  try
    lItem := TPerCriteria.Create('Criteria1');
    lList.Add(lItem);
    CheckEquals(1, lList.Count, 'Failed on 1');
    CheckSame(lItem, lList.Items[0], 'Failed on 2');
    CheckSame(lList, lItem.Owner, 'Failed on 3');
    CheckEquals('Criteria1', lItem.Name, 'Failed on 4');

    lItem := TPerCriteria.Create('Criteria2');
    lList.Add(lItem);
    CheckEquals(2, lList.Count, 'Failed on 5');
    CheckSame(lItem, lList.Items[1], 'Failed on 6');
    CheckSame(lList, lItem.Owner, 'Failed on 7');
    CheckEquals('Criteria2', lItem.Name, 'Failed on 8');

    lList.Clear;
    CheckEquals(0, lList.Count, 'Failed on 9');
  finally
    lList.Free;
  end;
end;

procedure TTestTICriteria.TestPerCriteria_IsEmbraced;
var
  lList: TPerCriteriaList;
  lItem1: TPerCriteria;
  lItem2: TPerCriteria;
  lItem3: TPerCriteria;
begin
  lList := TPerCriteriaList.Create;
  try
    lItem1 := TPerCriteria.Create('Criteria1');
    CheckEquals(False, lItem1.isEmbraced, 'Failed on 1');
    lList.Add(lItem1);
    CheckEquals(False, lItem1.isEmbraced, 'Failed on 2');

    lItem2 := TPerCriteria.Create('Criteria2');
    CheckEquals(False, lItem2.isEmbraced, 'Failed on 3');
    lItem1.AddAndCriteria(lItem2);
    CheckEquals(True, lItem1.isEmbraced, 'Failed on 4');
    CheckEquals(False, lItem2.isEmbraced, 'Failed on 5');

    lItem3 := TPerCriteria.Create('Criteria3');
    CheckEquals(False, lItem3.isEmbraced, 'Failed on 6');
    lItem2.AddOrCriteria(lItem3);
    CheckEquals(True, lItem2.isEmbraced, 'Failed on 7');
    CheckEquals(False, lItem3.isEmbraced, 'Failed on 8');

    lList.Clear;
    CheckEquals(0, lList.Count, 'Failed on 9');
  finally
    lList.Free;
  end;
end;

procedure TTestTICriteria.TestPerCriteria_ClearAll;
var
  lCriteria: TPerCriteria;
begin
  lCriteria := TPerCriteria.Create('Criteria1');
  try
    CheckEquals(0, lCriteria.Criterias.Count, 'Failed on 1');
    CheckEquals(0, lCriteria.GetGroupByList.Count, 'Failed on 2');
    CheckEquals(0, lCriteria.GetOrderByList.Count, 'Failed on 3');
    
    lCriteria.AddOrCriteria(TPerCriteria.Create('A'));
    CheckEquals(1, lCriteria.Criterias.Count, 'Failed on 4');

    lCriteria.AddGroupBy('FieldA');
    CheckEquals(1, lCriteria.GetGroupByList.Count, 'Failed on 5');

    lCriteria.AddOrderBy('FieldB');
    CheckEquals(1, lCriteria.GetOrderByList.Count, 'Failed on 6');

    lCriteria.ClearAll;
    CheckEquals(0, lCriteria.Criterias.Count, 'Failed on 7');
    CheckEquals(0, lCriteria.GetGroupByList.Count, 'Failed on 8');
    CheckEquals(0, lCriteria.GetOrderByList.Count, 'Failed on 9');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerSelectionCriteriaList;
var
  lList: TPerSelectionCriteriaList;
  lItem: TPerEqualToCriteria;
begin
  lList := TPerSelectionCriteriaList.Create;
  try
    lItem := TPerEqualToCriteria.Create('A', 'B');
    lList.Add(lItem);
    CheckEquals(1, lList.Count, 'Failed on 1');
    CheckSame(lItem, lList.Items[0], 'Failed on 2');
    CheckSame(lList, lItem.Owner, 'Failed on 3');
    CheckEquals('A', lItem.Attribute, 'Failed on 4');
    CheckEquals('B', lItem.Value, 'Failed on 5');

    lItem := TPerEqualToCriteria.Create('C', 'D');
    lList.Add(lItem);
    CheckEquals(2, lList.Count, 'Failed on 6');
    CheckSame(lItem, lList.Items[1], 'Failed on 7');
    CheckSame(lList, lItem.Owner, 'Failed on 8');
    CheckEquals('C', lItem.Attribute, 'Failed on 9');
    CheckEquals('D', lItem.Value, 'Failed on 10');

    lList.Clear;
    CheckEquals(0, lList.Count, 'Failed on 11');
  finally
    lList.Free;
  end;
end;

procedure TTestTICriteria.TestPerEqualToCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddEqualTo('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID = ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddEqualTo('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID = 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
  
  // Negative criteria
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddNotEqualTo('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <> ''1'')', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;

  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddNotEqualTo('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <> 1)', lSQL, 'Failed on 4');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerExistsCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddExists('Select * from Order where Client_ID = 1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('( EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  // Negative criteria
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddNotExists('Select * from Order where Client_ID = 1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('( NOT EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterThanCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddGreaterThan('FIELD_1', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 > ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddGreaterThan('FIELD_1', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 > 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterOrEqualThanCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddGreaterOrEqualThan('FIELD_1', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 >= ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddGreaterOrEqualThan('FIELD_1', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 >= 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerInCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
  myStrArray: array[0..1] of String;
  myIntArray: array[0..4] of Integer;
begin
  { IN with a SQL statement }
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddIn('OID', 'SELECT OID FROM SUB_TABLE');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID IN (SELECT OID FROM SUB_TABLE))', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  { IN with string array elements }
  lCriteria := TPerCriteria.Create('test');
  try
    myStrArray[0] := 'Value1';
    myStrArray[1] := 'Value2';
//    lCriteria.AddIn('OID', ['Value1', 'Value2']);   { This also works in FPC, but not Delphi }
    lCriteria.AddIn('OID', myStrArray);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID IN (''Value1'', ''Value2''))', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  { IN with integer array elements }
  lCriteria := TPerCriteria.Create('test');
  try
    myIntArray[0] := 1;
    myIntArray[1] := 2;
    myIntArray[2] := 3;
    myIntArray[3] := 4;
    myIntArray[4] := 5;
//    lCriteria.AddIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
    lCriteria.AddIn('OID', myIntArray);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID IN (1, 2, 3, 4, 5))', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLessThanCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddLessThan('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID < ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddLessThan('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID < 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLikeCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddLike('FIELD_1', 'A%');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 LIKE ''A%'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  // Negative criteria
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddNotLike('FIELD_1', 'A%');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 NOT LIKE ''A%'')', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerNullCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddNull('FIELD_1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 IS NULL)', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerBetweenCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddBetween('FIELD_1', '1', '2');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 BETWEEN 1 AND 2)', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL;
var
  lCriteria: TPerCriteria;
  lSQL: string;
begin
  lCriteria := TPerCriteria.Create('test');
  try
    lCriteria.AddSQL('Upper(Field_1) LIKE ''A%''');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(Upper(Field_1) LIKE ''A%'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
end;

end.




unit tiCriteria_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework
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
    procedure TestHasCriteria;
    procedure TestHasOrderBy;

    // Testing SQL generation
    procedure TestPerEqualToCriteria_SQL;
    procedure TestPerExistsCriteria_SQL;
    procedure TestPerGreaterThanCriteria_SQL;
    procedure TestPerGreaterOrEqualThanCriteria_SQL;
    procedure TestPerInCriteria_SQL;
    procedure TestPerNotInCriteria_SQL;
    procedure TestPerLessThanCriteria_SQL;
    procedure TestPerLessOrEqualThanCriteria_SQL;
    procedure TestPerLikeCriteria_SQL;
    procedure TestPerNullCriteria_SQL;
    procedure TestPerBetweenCriteria_SQL;
    procedure TestPerSQLCriteria_SQL;
    procedure TestPerSQLCriteria_SQL_IgnoreEmptyCritera;
    procedure TestPerSQLCriteria_SQL_Or;
    procedure TestFieldName;

    // Testing order by sql generation
    procedure TestOrderByAscending;
    procedure TestOrderByDescending;
    procedure TestOrderByArrayAscending;
    procedure TestOrderByArrayDescending;
    procedure TestOrderByMixed;

    // Testing SQL generation with params
    procedure TestPerEqualToCriteria_SQL_WithParams;
    procedure TestPerExistsCriteria_SQL_WithParams;
    procedure TestPerGreaterThanCriteria_SQL_WithParams;
    procedure TestPerGreaterOrEqualThanCriteria_SQL_WithParams;
    procedure TestPerInCriteria_SQL_WithParams;
    procedure TestPerNotInCriteria_SQL_WithParams;
    procedure TestPerLessThanCriteria_SQL_WithParams;
    procedure TestPerLessOrEqualThanCriteria_SQL_WithParams;
    procedure TestPerLikeCriteria_SQL_WithParams;
    procedure TestPerNullCriteria_SQL_WithParams;
    procedure TestPerBetweenCriteria_SQL_WithParams;
    procedure TestPerSQLCriteria_SQL_WithParams;
    procedure TestPerSQLCriteria_SQL_IgnoreEmptyCritera_WithParams;


    // Testing XPath generation
    // The SQL tests will need to be clone for XPath at some point
  end;

procedure RegisterTests;

implementation
uses
  tiCriteria
  ,tiVisitorCriteria
  ,SysUtils
  ,tiTestDependencies
  ,tiQuery
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTICriteria);
end;

{ TTestTICriteria }

procedure TTestTICriteria.TestPerColumns;
var
  lColumns: TtiColumns;
  lCol: TtiColumn;
begin
  lColumns := TtiColumns.Create;
  try
    lCol := TtiColumn.Create;
    lCol.Name       := 'Field1';
    lCol.Ascending  := True;
    lColumns.Add(lCol);
    CheckEquals(1, lColumns.Count, 'Failed on 1');
    CheckSame(lCol, lColumns.Items[0], 'Failed on 2');
    CheckSame(lColumns, lCol.Owner, 'Failed on 3');
    CheckEquals(True, lCol.Ascending, 'Failed on 4');
    CheckEquals('Field1', lCol.Name, 'Failed on 5');

    lCol := TtiColumn.Create;
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
  lList: TtiCriteriaList;
  lItem: TtiCriteria;
begin
  lList := TtiCriteriaList.Create;
  try
    lItem := TtiCriteria.Create('Criteria1');
    lList.Add(lItem);
    CheckEquals(1, lList.Count, 'Failed on 1');
    CheckSame(lItem, lList.Items[0], 'Failed on 2');
    CheckSame(lList, lItem.Owner, 'Failed on 3');
    CheckEquals('Criteria1', lItem.Name, 'Failed on 4');

    lItem := TtiCriteria.Create('Criteria2');
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
  lList: TtiCriteriaList;
  lItem1: TtiCriteria;
  lItem2: TtiCriteria;
  lItem3: TtiCriteria;
begin
  lList := TtiCriteriaList.Create;
  try
    lItem1 := TtiCriteria.Create('Criteria1');
    CheckEquals(False, lItem1.isEmbraced, 'Failed on 1');
    lList.Add(lItem1);
    CheckEquals(False, lItem1.isEmbraced, 'Failed on 2');

    lItem2 := TtiCriteria.Create('Criteria2');
    CheckEquals(False, lItem2.isEmbraced, 'Failed on 3');
    lItem1.AddAndCriteria(lItem2);
    CheckEquals(True, lItem1.isEmbraced, 'Failed on 4');
    CheckEquals(False, lItem2.isEmbraced, 'Failed on 5');

    lItem3 := TtiCriteria.Create('Criteria3');
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
  lCriteria: TtiCriteria;
begin
  lCriteria := TtiCriteria.Create('Criteria1');
  try
    CheckEquals(0, lCriteria.Criterias.Count, 'Failed on 1');
    CheckEquals(0, lCriteria.GetGroupByList.Count, 'Failed on 2');
    CheckEquals(0, lCriteria.GetOrderByList.Count, 'Failed on 3');
    CheckEquals(0, lCriteria.SelectionCriterias.Count, 'Failed on 4');

    lCriteria.AddEqualTo('Field1', '2a');
    CheckEquals(1, lCriteria.SelectionCriterias.Count, 'Failed on 5');

    lCriteria.AddOrCriteria(TtiCriteria.Create('A'));
    CheckEquals(1, lCriteria.Criterias.Count, 'Failed on 6');

    lCriteria.AddGroupBy('FieldA');
    CheckEquals(1, lCriteria.GetGroupByList.Count, 'Failed on 7');

    lCriteria.AddOrderBy('FieldB');
    CheckEquals(1, lCriteria.GetOrderByList.Count, 'Failed on 8');

    lCriteria.ClearAll;
    CheckEquals(0, lCriteria.Criterias.Count, 'Failed on 9');
    CheckEquals(0, lCriteria.GetGroupByList.Count, 'Failed on 10');
    CheckEquals(0, lCriteria.GetOrderByList.Count, 'Failed on 11');
    CheckEquals(0, lCriteria.SelectionCriterias.Count, 'Failed on 12');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerSelectionCriteriaList;
var
  lList: TtiSelectionCriteriaList;
  lItem: TtiEqualToCriteria;
begin
  lList := TtiSelectionCriteriaList.Create;
  try
    lItem := TtiEqualToCriteria.Create('A', 'B');
    lList.Add(lItem);
    CheckEquals(1, lList.Count, 'Failed on 1');
    CheckSame(lItem, lList.Items[0], 'Failed on 2');
    CheckSame(lList, lItem.Owner, 'Failed on 3');
    CheckEquals('A', lItem.Attribute, 'Failed on 4');
    CheckEquals('B', lItem.Value, 'Failed on 5');

    lItem := TtiEqualToCriteria.Create('C', 'D');
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
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddEqualTo('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID = ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddEqualTo('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID = 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
  
  // Negative criteria
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNotEqualTo('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <> ''1'')', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNotEqualTo('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <> 1)', lSQL, 'Failed on 4');
  finally
    lCriteria.Free;
  end;

  // field names
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddEqualTo('OID', '1');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';

    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo = ''1'')', lSQL, 'Failed on 5');
  finally
    lCriteria.Free;
  end;

end;

procedure TTestTICriteria.TestPerEqualToCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddEqualTo('OID', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID = :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;
    lParams.Clear;
    
    lCriteria := TtiCriteria.Create('test');
    try
      CheckEquals(0, lParams.Count, 'Failed on 10');
      lCriteria.AddEqualTo('OID', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID = :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 14');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
    lParams.Clear;

    // Negative criteria
    lCriteria := TtiCriteria.Create('test');
    try
      CheckEquals(0, lParams.Count, 'Failed on 20');
      lCriteria.AddNotEqualTo('OID', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID <> :Criteria_1)', lSQL, 'Failed on 21');
      CheckEquals(1, lParams.Count, 'Failed on 22');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 23');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 24');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 25');
    finally
      lCriteria.Free;
    end;
    lParams.Clear;

    lCriteria := TtiCriteria.Create('test');
    try
      CheckEquals(0, lParams.Count, 'Failed on 30');
      lCriteria.AddNotEqualTo('OID', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID <> :Criteria_1)', lSQL, 'Failed on 31');
      CheckEquals(1, lParams.Count, 'Failed on 32');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 33');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 34');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 35');
    finally
      lCriteria.Free;
    end;
    lParams.Clear;

    // field names
    lCriteria := TtiCriteria.Create('test');
    try
      CheckEquals(0, lParams.Count, 'Failed on 40');
      lCriteria.AddEqualTo('OID', '1');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';

      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo = :Criteria_1)', lSQL, 'Failed on 41');
      CheckEquals(1, lParams.Count, 'Failed on 42');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 43');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 44');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 45');
    finally
      lCriteria.Free;
    end;
    lParams.Clear;
  finally
    lParams.Free;
  end;
end;

procedure TTestTICriteria.TestPerExistsCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddExists('Select * from Order where Client_ID = 1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('( EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
  
  // Negative criteria
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNotExists('Select * from Order where Client_ID = 1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('( NOT EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerExistsCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddExists('Select * from Order where Client_ID = 1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('( EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 1');
      CheckEquals(0, lParams.Count);
    finally
      lCriteria.Free;
    end;

    // Negative criteria
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNotExists('Select * from Order where Client_ID = 1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('( NOT EXISTS (Select * from Order where Client_ID = 1))', lSQL, 'Failed on 2');
      CheckEquals(0, lParams.Count);      
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterThanCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterThan('FIELD_1', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 > ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterThan('FIELD_1', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 > 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  // field names
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterThan('FIELD_1', '1');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo > ''1'')', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterThanCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterThan('FIELD_1', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 > :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterThan('FIELD_1', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 > :Criteria_1)', lSQL, 'Failed on 6');
      CheckEquals(1, lParams.Count, 'Failed on 7');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 8');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 9');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 10');
    finally
      lCriteria.Free;
    end;

    // field names
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterThan('FIELD_1', '1');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo > :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 14');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterOrEqualThanCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterOrEqualThan('FIELD_1', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 >= ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterOrEqualThan('FIELD_1', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 >= 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddGreaterOrEqualThan('FIELD_1', '1');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo >= ''1'')', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerGreaterOrEqualThanCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterOrEqualThan('FIELD_1', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 >= :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterOrEqualThan('FIELD_1', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 >= :Criteria_1)', lSQL, 'Failed on 6');
      CheckEquals(1, lParams.Count, 'Failed on 7');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 8');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 9');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 10');
    finally
      lCriteria.Free;
    end;

    // field names
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddGreaterOrEqualThan('FIELD_1', '1');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo >= :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 14');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerInCriteria_SQL;
var
    lCriteria: TtiCriteria;
  lSQL: string;
  myStrArray: array[0..1] of variant;
  myIntArray: array[0..4] of variant;
begin
  { IN with a SQL statement }
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddIn('OID', 'SELECT OID FROM SUB_TABLE');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID IN (SELECT OID FROM SUB_TABLE))', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  { IN with string array elements }
  lCriteria := TtiCriteria.Create('test');
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
  lCriteria := TtiCriteria.Create('test');
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

  lCriteria := TtiCriteria.Create('test');
  try
    myIntArray[0] := 1;
    myIntArray[1] := 2;
    myIntArray[2] := 3;
    myIntArray[3] := 4;
    myIntArray[4] := 5;
//    lCriteria.AddIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
    lCriteria.AddIn('OID', myIntArray);
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo IN (1, 2, 3, 4, 5))', lSQL, 'Failed on 4');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerInCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  myStrArray: array[0..1] of variant;
  myIntArray: array[0..4] of variant;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    { IN with a SQL statement }
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddIn('OID', 'SELECT OID FROM SUB_TABLE');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID IN (SELECT OID FROM SUB_TABLE))', lSQL, 'Failed on 1');
      CheckEquals(0, lParams.Count);
    finally
      lCriteria.Free;
    end;

    { IN with string array elements }
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      myStrArray[0] := 'Value1';
      myStrArray[1] := 'Value2';
  //    lCriteria.AddIn('OID', ['Value1', 'Value2']);   { This also works in FPC, but not Delphi }
      lCriteria.AddIn('OID', myStrArray);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID IN (:Criteria_1, :Criteria_2))', lSQL);
      CheckEquals(2, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamString);
      CheckEquals('Value1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamString);
      CheckEquals('Value2', lParams.Items[1].ValueAsString);
    finally
      lCriteria.Free;
    end;

    { IN with integer array elements }
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddIn('OID', [1, 2, 3, 4, 5]);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID IN (:Criteria_1, :Criteria_2, :Criteria_3, :Criteria_4, :Criteria_5))', lSQL, 'Failed on 3');
      CheckEquals(5, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamInteger);
      CheckEquals('2', lParams.Items[1].ValueAsString);
      CheckEquals('Criteria_3', lParams.Items[2].Name);
      CheckTrue(lParams.Items[3] is TtiQueryParamInteger);
      CheckEquals('3', lParams.Items[2].ValueAsString);
      CheckEquals('Criteria_4', lParams.Items[3].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('4', lParams.Items[3].ValueAsString);
      CheckEquals('Criteria_5', lParams.Items[4].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('5', lParams.Items[4].ValueAsString);
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      myIntArray[0] := 1;
      myIntArray[1] := 2;
      myIntArray[2] := 3;
      myIntArray[3] := 4;
      myIntArray[4] := 5;
  //    lCriteria.AddIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
      lCriteria.AddIn('OID', myIntArray);
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo IN (:Criteria_1, :Criteria_2, :Criteria_3, :Criteria_4, :Criteria_5))', lSQL, 'Failed on 3');
      CheckEquals(5, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamInteger);
      CheckEquals('2', lParams.Items[1].ValueAsString);
      CheckEquals('Criteria_3', lParams.Items[2].Name);
      CheckTrue(lParams.Items[3] is TtiQueryParamInteger);
      CheckEquals('3', lParams.Items[2].ValueAsString);
      CheckEquals('Criteria_4', lParams.Items[3].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('4', lParams.Items[3].ValueAsString);
      CheckEquals('Criteria_5', lParams.Items[4].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('5', lParams.Items[4].ValueAsString);
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;

end;

procedure TTestTICriteria.TestPerLessOrEqualThanCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessOrEqualThan('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <= ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessOrEqualThan('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID <= 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessOrEqualThan('OID', 1);
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo <= 1)', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLessOrEqualThanCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessOrEqualThan('OID', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID <= :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessOrEqualThan('OID', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID <= :Criteria_1)', lSQL, 'Failed on 6');
      CheckEquals(1, lParams.Count, 'Failed on 7');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 8');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 9');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 10');
    finally
      lCriteria.Free;
    end;

    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessOrEqualThan('OID', 1);
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo <= :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 14');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerLessThanCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessThan('OID', '1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID < ''1'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessThan('OID', 1);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID < 1)', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLessThan('OID', 1);
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo < 1)', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLessThanCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessThan('OID', '1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID < :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessThan('OID', 1);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID < :Criteria_1)', lSQL, 'Failed on 6');
      CheckEquals(1, lParams.Count, 'Failed on 7');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 8');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 9');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 10');
    finally
      lCriteria.Free;
    end;

    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLessThan('OID', 1);
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo < :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger, 'Failed on 14');
      CheckEquals('1', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerLikeCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLike('FIELD_1', 'A%');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 LIKE ''A%'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  // Negative criteria
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNotLike('FIELD_1', 'A%');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 NOT LIKE ''A%'')', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddLike('FIELD_1', 'A%');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo LIKE ''A%'')', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerLikeCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLike('FIELD_1', 'A%');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 LIKE :Criteria_1)', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('A%', lParams.Items[0].ValueAsString, 'Failed on 5');
    finally
      lCriteria.Free;
    end;

    // Negative criteria
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNotLike('FIELD_1', 'A%');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 NOT LIKE :Criteria_1)', lSQL, 'Failed on 6');
      CheckEquals(1, lParams.Count, 'Failed on 7');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 8');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 9');
      CheckEquals('A%', lParams.Items[0].ValueAsString, 'Failed on 10');
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddLike('FIELD_1', 'A%');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo LIKE :Criteria_1)', lSQL, 'Failed on 11');
      CheckEquals(1, lParams.Count, 'Failed on 12');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 13');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 14');
      CheckEquals('A%', lParams.Items[0].ValueAsString, 'Failed on 15');
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerNotInCriteria_SQL;
var
    lCriteria: TtiCriteria;
  lSQL: string;
  myStrArray: array[0..1] of variant;
  myIntArray: array[0..4] of variant;
begin
  { IN with a SQL statement }
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNotIn('OID', 'SELECT OID FROM SUB_TABLE');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID NOT IN (SELECT OID FROM SUB_TABLE))', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;

  { IN with string array elements }
  lCriteria := TtiCriteria.Create('test');
  try
    myStrArray[0] := 'Value1';
    myStrArray[1] := 'Value2';
//    lCriteria.AddNotIn('OID', ['Value1', 'Value2']);   { This also works in FPC, but not Delphi }
    lCriteria.AddNotIn('OID', myStrArray);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID NOT IN (''Value1'', ''Value2''))', lSQL, 'Failed on 2');
  finally
    lCriteria.Free;
  end;

  { IN with integer array elements }
  lCriteria := TtiCriteria.Create('test');
  try
    myIntArray[0] := 1;
    myIntArray[1] := 2;
    myIntArray[2] := 3;
    myIntArray[3] := 4;
    myIntArray[4] := 5;
//    lCriteria.AddNotIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
    lCriteria.AddNotIn('OID', myIntArray);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(OID NOT IN (1, 2, 3, 4, 5))', lSQL, 'Failed on 3');
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    myIntArray[0] := 1;
    myIntArray[1] := 2;
    myIntArray[2] := 3;
    myIntArray[3] := 4;
    myIntArray[4] := 5;
//    lCriteria.AddNotIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
    lCriteria.AddNotIn('OID', myIntArray);
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo NOT IN (1, 2, 3, 4, 5))', lSQL, 'Failed on 4');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerNotInCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  myStrArray: array[0..1] of variant;
  myIntArray: array[0..4] of variant;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    { IN with a SQL statement }
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNotIn('OID', 'SELECT OID FROM SUB_TABLE');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID NOT IN (SELECT OID FROM SUB_TABLE))', lSQL, 'Failed on 1');
      CheckEquals(0, lParams.Count);
    finally
      lCriteria.Free;
    end;

    { IN with string array elements }
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      myStrArray[0] := 'Value1';
      myStrArray[1] := 'Value2';
  //    lCriteria.AddNotIn('OID', ['Value1', 'Value2']);   { This also works in FPC, but not Delphi }
      lCriteria.AddNotIn('OID', myStrArray);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID NOT IN (:Criteria_1, :Criteria_2))', lSQL);
      CheckEquals(2, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamString);
      CheckEquals('Value1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamString);
      CheckEquals('Value2', lParams.Items[1].ValueAsString);
    finally
      lCriteria.Free;
    end;

    { IN with integer array elements }
    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNotIn('OID', [1, 2, 3, 4, 5]);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(OID NOT IN (:Criteria_1, :Criteria_2, :Criteria_3, :Criteria_4, :Criteria_5))', lSQL, 'Failed on 3');
      CheckEquals(5, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamInteger);
      CheckEquals('2', lParams.Items[1].ValueAsString);
      CheckEquals('Criteria_3', lParams.Items[2].Name);
      CheckTrue(lParams.Items[3] is TtiQueryParamInteger);
      CheckEquals('3', lParams.Items[2].ValueAsString);
      CheckEquals('Criteria_4', lParams.Items[3].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('4', lParams.Items[3].ValueAsString);
      CheckEquals('Criteria_5', lParams.Items[4].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('5', lParams.Items[4].ValueAsString);
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      myIntArray[0] := 1;
      myIntArray[1] := 2;
      myIntArray[2] := 3;
      myIntArray[3] := 4;
      myIntArray[4] := 5;
  //    lCriteria.AddNotIn('OID', [1, 2, 3, 4, 5]);  { This also works in FPC, but not Delphi }
      lCriteria.AddNotIn('OID', myIntArray);
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo NOT IN (:Criteria_1, :Criteria_2, :Criteria_3, :Criteria_4, :Criteria_5))', lSQL, 'Failed on 3');
      CheckEquals(5, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamInteger);
      CheckEquals('2', lParams.Items[1].ValueAsString);
      CheckEquals('Criteria_3', lParams.Items[2].Name);
      CheckTrue(lParams.Items[3] is TtiQueryParamInteger);
      CheckEquals('3', lParams.Items[2].ValueAsString);
      CheckEquals('Criteria_4', lParams.Items[3].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('4', lParams.Items[3].ValueAsString);
      CheckEquals('Criteria_5', lParams.Items[4].Name);
      CheckTrue(lParams.Items[4] is TtiQueryParamInteger);
      CheckEquals('5', lParams.Items[4].ValueAsString);
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;

end;

procedure TTestTICriteria.TestPerNullCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNull('FIELD_1');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 IS NULL)', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddNull('FIELD_1');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo IS NULL)', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerNullCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNull('FIELD_1');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria));
      CheckEquals('(FIELD_1 IS NULL)', lSQL);
    finally
      lCriteria.Free;
    end;

    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddNull('FIELD_1');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria));
      CheckEquals('(CustNo IS NULL)', lSQL);
    finally
      lCriteria.Free;
    end;
  finally
    lParams.Free;
  end;
end;

procedure TTestTICriteria.TestFieldName;
var
  lSelectionCriteria: TtiSelectionCriteriaAbs;
begin
  // use TtiEqualToCriteria so we don't get a warning about using an abstract class
  lSelectionCriteria := TtiEqualToCriteria.Create('Field1', '');
  try
    CheckEquals('Field1', lSelectionCriteria.Attribute);
    CheckEquals('Field1', lSelectionCriteria.FieldName);

    lSelectionCriteria.FieldName:= 'Field_1';
    CheckEquals('Field1', lSelectionCriteria.Attribute);
    CheckEquals('Field_1', lSelectionCriteria.FieldName);

  finally
    lSelectionCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestHasCriteria;
var
  lCriteria: TtiCriteria;
  lCriteriaAnd: TtiCriteria;
begin
  lCriteria := TtiCriteria.Create('test');
  lCriteriaAnd := TtiCriteria.Create('test and');
  try
    CheckFalse(lCriteria.HasCriteria, 'Failed at 1');
    lCriteria.AddBetween('FIELD_1', '1', '2');
    CheckTrue(lCriteria.HasCriteria, 'Failed at 2');
    lCriteria.AddBetween('FIELD_1', '1', '2');
    CheckTrue(lCriteria.HasCriteria, 'Failed at 3');

    // order by is no longer in HasCriteria, but in HasOrderBy
    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasCriteria, 'Failed at 4');
    lCriteria.AddOrderBy('FIELD_1');
    CheckFalse(lCriteria.HasCriteria, 'Failed at 5.1');
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 5.2');

    // group by is no longer in HasCriteria as it doesn't work yet
    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasCriteria, 'Failed at 6');
    lCriteria.AddGroupBy('FIELD_1');
    CheckFalse(lCriteria.HasCriteria, 'Failed at 7');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasCriteria, 'Failed at 8');
    lCriteria.AddAndCriteria(lCriteriaAnd);
    CheckTrue(lCriteria.HasCriteria, 'Failed at 9');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestHasOrderBy;
var
  lCriteria: TtiCriteria;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 1');
    lCriteria.AddOrderBy('FIELD_1');
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 2');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 3');
    lCriteria.AddOrderBy('FIELD_1', true);
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 4');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 5');
    lCriteria.AddOrderBy('FIELD_1', false);
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 6');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 7');
    lCriteria.AddOrderByAscending('FIELD_1');
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 8');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 9');
    lCriteria.AddOrderByDescending('FIELD_1');
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 10');

    lCriteria.ClearAll;
    CheckFalse(lCriteria.HasOrderBy, 'Failed at 11');
    lCriteria.AddOrderBy(['FIELD_1', 'FIELD_2']);
    CheckTrue(lCriteria.HasOrderBy, 'Failed at 12');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestOrderByArrayAscending;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy(['FIELD_1', 'FIELD_2']);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1, FIELD_2', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy(['FIELD_1', 'FIELD_2']);
    lCriteria.AddOrderBy(['FIELD_3', 'FIELD_4']);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1, FIELD_2, FIELD_3, FIELD_4', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestOrderByArrayDescending;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderByDescending(['FIELD_1', 'FIELD_2']);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1 DESC, FIELD_2 DESC', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderByDescending(['FIELD_1', 'FIELD_2']);
    lCriteria.AddOrderByDescending(['FIELD_3', 'FIELD_4']);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1 DESC, FIELD_2 DESC, FIELD_3 DESC, FIELD_4 DESC', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestOrderByAscending;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy('FIELD_1');
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderByAscending('FIELD_1');
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy('FIELD_1');
    lCriteria.AddOrderBy('FIELD_2');
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1, FIELD_2', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestOrderByDescending;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy('FIELD_1', false);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1 DESC', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderByDescending('FIELD_1');
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1 DESC', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy('FIELD_1', false);
    lCriteria.AddOrderBy('FIELD_2', false);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1 DESC, FIELD_2 DESC', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestOrderByMixed;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddOrderBy('FIELD_1');
    lCriteria.AddOrderBy('FIELD_2', false);
    lCriteria.AddOrderBy('FIELD_3');
    lCriteria.AddOrderBy('FIELD_4', false);
    lSQL := Trim(tiCriteriaOrderByAsSQL(lCriteria));
    CheckEquals('ORDER BY FIELD_1, FIELD_2 DESC, FIELD_3, FIELD_4 DESC', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerBetweenCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddBetween('FIELD_1', 1, 2);
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(FIELD_1 BETWEEN 1 AND 2)', lSQL);
  finally
    lCriteria.Free;
  end;

  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddBetween('FIELD_1', '1', '2');
    lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(CustNo BETWEEN ''1'' AND ''2'')', lSQL);
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerBetweenCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddBetween('FIELD_1', 1, 2);
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(FIELD_1 BETWEEN :Criteria_1 AND :Criteria_2)', lSQL);
      CheckEquals(2, lParams.Count);
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamInteger);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamInteger);
      CheckEquals('2', lParams.Items[1].ValueAsString);
    finally
      lCriteria.Free;
    end;

    lParams.Clear;
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddBetween('FIELD_1', '1', '2');
      lCriteria.SelectionCriterias[0].FieldName:= 'CustNo';
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(CustNo BETWEEN :Criteria_1 AND :Criteria_2)', lSQL);
      CheckEquals(2, lParams.Count);
      CheckEquals('Criteria_1', lParams.Items[0].Name);
      CheckTrue(lParams.Items[0] is TtiQueryParamString);
      CheckEquals('1', lParams.Items[0].ValueAsString);
      CheckEquals('Criteria_2', lParams.Items[1].Name);
      CheckTrue(lParams.Items[1] is TtiQueryParamString);
      CheckEquals('2', lParams.Items[1].ValueAsString);
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL;
var
  lCriteria: TtiCriteria;
  lSQL: string;
begin
  lCriteria := TtiCriteria.Create('test');
  try
    lCriteria.AddSQL('Upper(Field_1) LIKE ''A%''');
    lSQL := Trim(tiCriteriaAsSQL(lCriteria));
    CheckEquals('(Upper(Field_1) LIKE ''A%'')', lSQL, 'Failed on 1');
  finally
    lCriteria.Free;
  end;
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL_IgnoreEmptyCritera;
var
  lCriteriaBase, lCriteriaOr: TtiCriteria;
  lSQL: string;
begin
  lCriteriaBase := TtiCriteria.Create('test');
  try
    lCriteriaOr := TtiCriteria.Create('test');

    lCriteriaOr.AddEqualTo('OID', '2');
    lCriteriaBase.AddOrCriteria(lCriteriaOr);

    lSQL := Trim(tiCriteriaAsSQL(lCriteriaBase));
    CheckEquals('((OID = ''2''))', lSQL, 'Failed on 1');

    lCriteriaBase.Criterias.Clear;
  finally
    lCriteriaBase.Free;
  end;
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL_IgnoreEmptyCritera_WithParams;
var
  lCriteriaBase, lCriteriaOr: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteriaBase := TtiCriteria.Create('test');
    try
      lCriteriaOr := TtiCriteria.Create('test');

      lCriteriaOr.AddEqualTo('OID', '2');
      lCriteriaBase.AddOrCriteria(lCriteriaOr);

      lSQL := Trim(tiCriteriaAsSQL(lCriteriaBase, lParams));
      CheckEquals('((OID = :Criteria_1))', lSQL, 'Failed on 1');
      CheckEquals(1, lParams.Count, 'Failed on 2');
      CheckEquals('Criteria_1', lParams.Items[0].Name, 'Failed on 3');
      CheckTrue(lParams.Items[0] is TtiQueryParamString, 'Failed on 4');
      CheckEquals('2', lParams.Items[0].ValueAsString, 'Failed on 5');

      lCriteriaBase.Criterias.Clear;
    finally
      lCriteriaBase.Free;
    end;
  finally
    lParams.free;
  end;
end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL_Or;
var
  lCriteriaBase, lCriteriaOr: TtiCriteria;
  lSQL: string;
begin
  lCriteriaBase := TtiCriteria.Create('test');
  try
    lCriteriaBase.AddEqualTo('OID', '1') ;
    lCriteriaOr := TtiCriteria.Create('test');
    lCriteriaOr.AddEqualTo('OID', '2');
    lCriteriaBase.AddOrCriteria(lCriteriaOr);

    lSQL := Trim(tiCriteriaAsSQL(lCriteriaBase));
    CheckEquals('((OID = ''1'') OR '#$D#$A'(OID = ''2''))', lSQL, 'Failed on 1');

    lCriteriaBase.Criterias.Clear;
  finally
    lCriteriaBase.Free;
  end;

end;

procedure TTestTICriteria.TestPerSQLCriteria_SQL_WithParams;
var
  lCriteria: TtiCriteria;
  lParams: TtiQueryParams;
  lSQL: string;
begin
  lParams:= TtiQueryParams.Create;
  try
    lCriteria := TtiCriteria.Create('test');
    try
      lCriteria.AddSQL('Upper(Field_1) LIKE ''A%''');
      lSQL := Trim(tiCriteriaAsSQL(lCriteria, lParams));
      CheckEquals('(Upper(Field_1) LIKE ''A%'')', lSQL, 'Failed on 1');
      CheckEquals(0, lParams.Count);
    finally
      lCriteria.Free;
    end;
  finally
    lParams.free;
  end;

end;

end.







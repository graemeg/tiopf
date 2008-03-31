unit AdrsType_TST;

interface
uses
  tiTestFramework,
  AdrsType_BOM,
  Adrs_TST;

type

  TTestAdrsType = class(TAdrsTestCase)
  published
    procedure AdrsTypeList_Read;

    procedure TestLookupList_Save       ;
    procedure TestLookupList_Update     ;
    procedure TestLookupList_Delete     ;

    procedure TestLookupListItem_Read   ;
    procedure TestLookupListItem_Save   ;
    procedure TestLookupListItem_Update ;
    procedure TestLookupListItem_Delete ;

    procedure TestLookupLists_Read      ;

  end;


procedure RegisterTests;

implementation
uses
  TestFramework,
  SysUtils,
  tiObject,
  tiOPFManager,
  tiQuery,
  AdrsUnitTestConstants,

  tiDialogs ;

{ TTestAdrs }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestAdrsType.Suite);
end;

{ TTestAdrs }

procedure TTestAdrsType.TestLookupList_Delete;
//var
//  lLookupLists: TLookupLists;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    lLookupLists.Items[0].ObjectState:= posDelete;
//    lLookupLists.Save;
//    Check(posDeleted = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//  finally
//    lLookupLists.Free;
//  end;
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(0, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//  finally
//    lLookupLists.Free;
//  end;
end;

procedure TTestAdrsType.AdrsTypeList_Read;
var
  LList: TEAdrsTypeList;
begin
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType2);
  AdrsTypeSetup.EAdrsTypeInsert(cOIDEAdrsType1);
  LList:= TEAdrsTypeList.Create;
  try
    LList.Read;
    CheckEquals(2, LList.Count);
    Check(posClean = LList.ObjectState);
    Check(posClean = LList.Items[0].ObjectState);
    Check(posClean = LList.Items[1].ObjectState);
    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[0], cOIDEAdrsType1);
    AdrsTypeSetup.EAdrsTypeCheck(LList.Items[1], cOIDEAdrsType2);
  finally
    LList.Free;
  end;
end;

procedure TTestAdrsType.TestLookupList_Save;
//var
//  lLookupLists: TLookupLists;
//  lLookupList : TLookupList;
begin
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupList := TLookupList.Create;
//    lLookupList.ObjectState:= posCreate;
////    lLookupList.OID.AsString:= IntToStr(cOIDLookupListName);
////    lLookupList.ListName:= IntToStr(cOIDLookupListName);
//    lLookupLists.Add(lLookupList);
//    lLookupLists.Save;
//    Check(posClean = lLookupList.ObjectState, 'Failed on lLookupList.ObjectState = posClean');
//  finally
//    lLookupLists.Free;
//  end;
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, IntToStr(cOIDLookupListName));
//  finally
//    lLookupLists.Free;
//  end;
end;

procedure TTestAdrsType.TestLookupList_Update;
//var
//  lLookupLists: TLookupLists;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, IntToStr(cOIDLookupListName));
//    lLookupLists.Items[0].ListName:= cUpdateValue;
//    lLookupLists.Items[0].ObjectState:= posUpdate;
//    lLookupLists.Save;
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//  finally
//    lLookupLists.Free;
//  end;
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, cUpdateValue);
//  finally
//    lLookupLists.Free;
//  end;
end;

procedure TTestAdrsType.TestLookupListItem_Delete;
//var
//  lLookupLists: TLookupLists;
//  lLookupListItem : TLookupListItem;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
////  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, IntToStr(cOIDLookupListName));
//
//    CheckEquals(1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count');
//    lLookupListItem := lLookupLists.Items[0].Items[0];
//    lLookupListItem.ObjectState:= posDelete;
//    lLookupLists.Save;
//    Check(posDeleted = lLookupListItem.ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//  finally
//    lLookupLists.Free;
//  end;
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    CheckEquals(0, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count');
//  finally
//    lLookupLists.Free;
//  end;
//
end;

procedure TTestAdrsType.TestLookupListItem_Read;
//var
//  lLookupLists: TLookupLists;
//  lLookupList : TLookupList;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
////  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, IntToStr(cOIDLookupListName));
//
//    CheckEquals(1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count');
//    lLookupList := lLookupLists.Items[0];
//    Check(posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListItem(lLookupList.Items[0], cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//  finally
//    lLookupLists.Free;
//  end;
end;

procedure TTestAdrsType.TestLookupListItem_Save;
//var
//  lLookupList : TLookupList;
//  lLookupListItem: TLookupListItem;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
//
//  lLookupList:= TLookupList.Create;
//  try
//    lLookupList.ObjectState:= posClean;
////    lLookupList.OID.AsString:= IntToStr(cOIDLookupListName);
//    lLookupListItem:= TLookupListItem.Create;
//    lLookupListItem.ObjectState:= posCreate;
////    lLookupListItem.OID.AsString:= IntToStr(cOIDLookupListItem);
////    lLookupListItem.Text:= IntToStr(cOIDLookupListItem);
//    lLookupList.Add(lLookupListItem);
//Assert(False, 'Under construction');
////    lLookupList.Save;
//    Check(posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//  finally
//    lLookupList.Free;
//  end;
end;

procedure TTestAdrsType.TestLookupListItem_Update;
//var
//  lLookupLists: TLookupLists;
//  lLookupList : TLookupList;
//  lLookupListItem: TLookupListItem;
begin
////  InsertTestLookupListName(cOIDLookupListName, IntToStr(cOIDLookupListName));
////  InsertTestLookupListItem(cOIDLookupListName, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    CheckEquals(1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count');
//    lLookupListItem:= lLookupLists.Items[0].Items[0];
////    CheckLookupListItem(lLookupListItem, cOIDLookupListItem, IntToStr(cOIDLookupListItem));
//
//    lLookupListItem.Text:= cUpdateValue;
//    lLookupListItem.ObjectState:= posUpdate;
//    lLookupLists.Save;
//  finally
//    lLookupLists.Free;
//  end;
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(1, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupLists.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListName(lLookupLists.Items[0], cOIDLookupListName, IntToStr(cOIDLookupListName));
//
//    CheckEquals(1, lLookupLists.Items[0].Count, 'Failed on lLookupLists.Items[0].count');
//    lLookupList := lLookupLists.Items[0];
//    Check(posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    Check(posClean = lLookupList.Items[0].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
////    CheckLookupListItem(lLookupList.Items[0], cOIDLookupListItem, cUpdateValue);
//  finally
//    lLookupLists.Free;
//  end;
end;

procedure TTestAdrsType.TestLookupLists_Read;
//var
//  lLookupLists    : TLookupLists;
//  lLookupList     : TLookupList;
//  i, j: integer;
begin
//  for i:= 1 to 9 do
//  begin
////    InsertTestLookupListName(i*10, IntToStr(i*10));
//    for j:= 1 to 9 do
////      InsertTestLookupListItem(i*10, i*10+j, IntToStr(i*10+j));
//  end;
//
//  lLookupLists:= TLookupLists.Create;
//  try
//    lLookupLists.Read;
//    CheckEquals(9, lLookupLists.Count, 'Failed on count');
//    Check(posClean = lLookupLists.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//    for i:= 0 to 8 do
//    begin
//      Check(posClean = lLookupLists.Items[i].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//      AdrsTypeSetup.CheckLookupListName(lLookupLists.Items[i], (i+1)*10, IntToStr((i+1)*10));
//      lLookupList := lLookupLists.Items[i];
//      CheckEquals(9, lLookupList.Count, 'Failed on lLookupList.count');
//      for j:= 0 to 8 do
//      begin
//        Check(posClean = lLookupList.ObjectState, 'Failed on lLookupLists.ObjectState = posClean');
//        Check(posClean = lLookupList.Items[j].ObjectState, 'Failed on lLookupLists.Items[0].ObjectState = posClean');
//        AdrsTypeSetup.CheckLookupListItem(lLookupList.Items[j], (i+1)*10+j+1, IntToStr((i+1)*10+j+1));
//      end;
//    end;
//  finally
//    lLookupLists.Free;
//  end;
end;

end.

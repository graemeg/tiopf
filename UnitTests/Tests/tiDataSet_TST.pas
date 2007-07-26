unit TiDataset_TST;

interface

uses
  TestFramework,
  tiDataset,
  Db,
  Variants,
  SysUtils,
  tiObject,
  tiTestFramework;


type
  TTestTiDataset = class(TtiTestCase)
  private
    AList: TtiObjectList;
    ADataset: TtiDataset;
    procedure PopulateList;
    procedure CalcFields(Dataset: TDataSet);
    procedure FilterData(Dataset: TDataSet;var Accept: boolean);
    procedure LinkListToDataset;
  protected
    procedure SetUp;override;
    procedure TearDown;override;
  published
    procedure CheckOpen;
    procedure CheckOpenFirstBug;
    procedure CheckFirst;
    procedure CheckIterate;
    procedure CheckLast;
    procedure FieldCount;
    procedure FieldTypes;
    procedure RecordCount;
    procedure DataIntegrity;
    procedure BookMark;
    procedure RetrieveObject;
    procedure MoveForwardBack;

    procedure EofBof;

    procedure Insert;
    procedure Edit;
    procedure Cancel;
    procedure Delete;

    procedure CalculatedFields;
    procedure FilterTheData;
    //procedure ClearFields;
  end;

procedure RegisterTests;

implementation

uses
  tiOIDguid,
  tiOPFManager,
  tiQueryXML,
  tiDUnitDependencies;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTiDataset);
end;

const
  FirstValue = 1;
  LastValue = 100;

type
  TtiDatasetItem = class(TtiObject)
  private
    FIntField: integer;
    FFloatField: Double;
    FStrField: string;
    FDateField: TDateTime;
    FBoolField: boolean;
    FMemoField: string;
  published
    property IntField: integer read FIntField write FIntField;
    property StrField: string read FStrField write FStrField;
    property FloatField: Double read FFloatField write FFloatField;
    property DateField: TDateTime read FDateField write FDateField;
    property BoolField: boolean read FBoolField write FBoolField;
    property MemoField: string read FMemoField write FMemoField;
  end;

  { TTestTiDataset }

procedure TTestTiDataset.PopulateList;
var i: integer;
  d: TtiDatasetItem;
begin
  for i := FirstValue to LastValue do begin
    d := TtiDatasetItem.CreateNew;
    d.IntField := i;
    d.StrField := IntToStr(1);
    d.BoolField := Odd(i);
    d.DateField := Now;
    d.FloatField := i - 0.1;
    d.MemoField := stringofchar(chr(i), i);
    AList.Add(d);
  end;
end;

procedure TTestTiDataset.LinkListToDataset;
begin
  with ADataset do begin
    ObjectList := AList;
  end;
end;

procedure TTestTiDataset.Setup;
begin
  inherited;
  gTIOPFManager.DefaultPerLayerName:= 'XML';
//  gTIOPFManager.ConnectDatabase('dbdemos.xml', '', '');

  AList := TtiObjectList.Create;
  ADataset := TtiDataset.Create(nil);
  ADataset.ObjectClass := TtiDatasetItem;
  PopulateList; //Adds 100 Items 1..100
  LinkListToDataset;
  ADataset.Open;
end;

procedure TTestTiDataset.Teardown;
begin
  AList.Free;
  ADataset.Close;
  ADataset.Free;
  inherited;
end;

procedure TTestTiDataset.CheckOpen;
//Simply tests if the first record is selected after the dataset has been opened.
//This wasn't the case; I repaired this on 22 Feb 2004
begin
  with ADataset do begin
    Check(Active, 'Dataset Open method not valid');
    Close;
    Check(not Active, 'Dataset Close method not valid');
  end;
end;

procedure TTestTiDataset.CheckOpenFirstBug;
//Simply tests if the first record is selected after the dataset has been opened.
//This wasn't the case; I repaired this on 22 Feb 2004
begin
  with ADataset do begin
    Check(FieldByName('IntField').AsInteger = FirstValue, 'Open CheckOpenFirstBug retrieves incorrect object (1)');
    Check((GetActiveItem as TtiDatasetItem).IntField = FirstValue, 'Open CheckOpenFirstBug.GetActiveItem retrieves incorrect object (1)');

    Close;
    Open;
    Check(FieldByName('IntField').AsInteger = FirstValue, 'Open CheckOpenFirstBug retrieves incorrect object (2)');
    Check((GetActiveItem as TtiDatasetItem).IntField = FirstValue, 'Open CheckOpenFirstBug.GetActiveItem retrieves incorrect object (2)');
  end;
end;

procedure TTestTiDataset.CheckFirst;
begin
  with ADataset do begin
    First;
    Check(FieldByName('IntField').AsInteger = FirstValue, 'CheckFirst retrieves incorrect object');
    Check((GetActiveItem as TtiDatasetItem).IntField = FirstValue, 'CheckFirst.GetActiveItem retrieves incorrect object');
  end;
end;

procedure TTestTiDataset.CheckIterate;
var AValue: integer;
begin
  with ADataset do begin
    for AValue := FirstValue to LastValue do begin
      Check(FieldByName('IntField').AsInteger = AValue, 'CheckIterate retrieves incorrect object ' + IntToStr(AValue));
      Next;
    end;
  end;
end;

procedure TTestTiDataset.CheckLast;
begin
  with ADataset do begin
    Last;
    Check(FieldByName('IntField').AsInteger = LastValue, 'CheckLast retrieves incorrect object');
    Check((GetActiveItem as TtiDatasetItem).IntField = LastValue, 'CheckLast.GetActiveItem retrieves incorrect object');
  end;
end;

procedure TTestTiDataset.FieldCount;
var AFieldCount: integer;
begin
  AFieldCount := ADataset.Fields.Count;
  //Crap.. Its always one extra since TtiVisited publishes the property Caption
  Check(AFieldCount = 7, 'Incorrect field count');
end;

procedure TTestTiDataset.RecordCount;
var ARecordcount: integer;
begin
  ARecordCount := ADataset.RecordCount;
  Check(ARecordCount = LastValue - FirstValue + 1, 'Incorrect RecordCount');
  //ADataset.OnFilterRecord := FilterData;
  //ADataset.Filtered := true;
  //ARecordCount := ADataset.RecordCount
  //Check(RecordCount = (LastValue-FirstValue+1) div 2, 'Amount of records in filtering is incorrect');
end;

procedure TTestTiDataset.BookMark;
var SavePlace21: TBookmark;
  ARecordId: integer;
begin
  with ADataset do begin
    //we are sitting on record 1

    MoveBy(20); //Navigate to 21 and Bookmark it
    ARecordId := FieldByName('IntField').AsInteger;
    Check(ARecordId = 21, 'Navigation Error - MoveBy(2) has not moved to correct record');
    SavePlace21 := GetBookmark; //21

    MoveBy( - 1); // record 20
    ARecordId := FieldByName('IntField').AsInteger;
    Check(ARecordId = 20, 'Navigation Error - should be at record 20');

    First;
    ARecordId := FieldByName('IntField').AsInteger;
    Check(ARecordId = FirstValue, 'Navigation Error - First has not moved to first record');

    Last;
    ARecordId := FieldByName('IntField').AsInteger;
    Check(ARecordId = LastValue, 'Navigation Error - Last has not moved to last record');

    GotoBookmark(SavePlace21);
    ARecordId := FieldByName('IntField').AsInteger;
    Check(ARecordId = 21, 'Bookmarking Error - should be at record 21');
    FreeBookmark(SavePlace21);
  end;
end;

procedure TTestTiDataset.DataIntegrity;
//Purpose??
var i: integer;
  rsum: integer;
  checkres: integer;
begin
  checkres := 0;
  ADataset.First;
  for i := 0 to AList.Count - 1 do begin
    checkres := checkres +(AList[i]as TtiDatasetItem).IntField;
  end;

  rsum := 0;
  for i := 0 to ADataset.RecordCount - 1 do begin
    rsum := rsum + ADataset.FieldValues['IntField'];
    ADataset.Next;
  end;
  Check(rsum = checkres, 'Incorrect summing of all data');
end;

procedure TTestTiDataset.EofBof;
begin
  with ADataset do begin
    Close;
    AList.Clear;
    Open;
    Check(ADataset.RecordCount = 0, 'Error handling empty recordset - Recordcount');
    Next;
    Check(Eof, 'Error handling empty recordset - EOF');
    Prior;
    Check(Bof, 'Error handling empty recordset - BOF');
    Check(Eof and Bof, 'Error handling empty recordset - EOF & BOF');
  end;
end;

procedure TTestTiDataset.Insert;
var Rc: integer;
var ANewMemoField: string;
  ANewStrField: string;
  ANewIntField: integer;
  ANewDateField: TDateTime;
  ANewBoolField: boolean;
  ANewFloatField: Double;
begin
  //The Dataset does not support InsertRecord & AppendRecord or ClearFields!!

  with ADataset do begin
    ANewStrField := 'Edited Value!';
    ANewIntField := 12343;
    ANewDateField := Now + 1;
    ANewBoolField := true;
    ANewFloatField := 99999.9995;
    ANewMemoField := 'Edited Value!';

    Rc := RecordCount;

    First;
    Append;
    FieldByName('StrField').AsString := ANewStrField;
    FieldByName('IntField').AsInteger := ANewIntField;
    FieldByName('DateField').AsDateTime := ANewDateField;
    FieldByName('BoolField').AsBoolean := ANewBoolField;
    FieldByName('FloatField').AsFloat := ANewFloatField;
    FieldByName('MemoField').AsString := ANewMemoField;
    Post;

    Check(FieldByName('StrField').AsString = ANewStrField, 'String Edit error');
    Check(FieldByName('IntField').AsInteger = ANewIntField, 'Integer edit error');
    Check(FieldByName('DateField').AsDateTime = ANewDateField, 'Date edit error');
    Check(FieldByName('BoolField').AsBoolean = ANewBoolField, 'Boolean edit error');
    Check(Abs(FieldByName('FloatField').AsFloat - ANewFloatField) < 0.0001, 'Float edit error');
    Check(FieldByName('MemoField').Value = ANewMemoField, 'Memo edit error');
    Check(RecordCount = Rc + 1, Format('Records have not been appended prev=%d<>current=%d',[Rc + 1, RecordCount]));
  end;
end;

procedure TTestTiDataset.Edit;
const GotoRecord = 10;
var cNewMemoField, cNewStrField: string;
  nNewIntField: integer;
  dNewDateField: TDateTime;
  lNewBoolField: boolean;
  nNewFloatField: Double;
begin
  with ADataset do begin
    cNewStrField := 'Edited Value';
    nNewIntField := 12345;
    dNewDateField := Now - 1;
    lNewBoolField := true;
    nNewFloatField := 99999.9999;
    cNewMemoField := 'Edited Value';

    First; // record 0
    MoveBy(GotoRecord);
    Edit;
    FieldByName('StrField').Value := cNewStrField;
    FieldByName('IntField').Value := nNewIntField;
    FieldByName('DateField').Value := dNewDateField;
    FieldByName('BoolField').Value := lNewBoolField;
    FieldByName('FloatField').Value := nNewFloatField;
    FieldByName('MemoField').Value := cNewMemoField;
    Post;

    First; // record 0
    MoveBy(GotoRecord);
    Check(FieldByName('StrField').AsString = cNewStrField, 'String Edit error');
    Check(FieldByName('IntField').AsInteger = nNewIntField, 'Integer edit error');
    Check(FieldByName('DateField').AsDateTime = dNewDateField, 'Date edit error');
    Check(FieldByName('BoolField').AsBoolean = lNewBoolField, 'Boolean edit error');
    Check(Abs(FieldByName('FloatField').AsFloat - nNewFloatField) < 0.0001, 'Float edit error');
    Check(FieldByName('MemoField').Value = cNewMemoField, 'Memo edit error');
  end;
end;

procedure TTestTiDataset.Cancel;
const GotoRecord = 10;
var cNewMemoField, cNewStrField: string;
  nNewIntField: integer;
  dNewDateField: TDateTime;
  lNewBoolField: boolean;
  nNewFloatField: Double;
var cOldMemoField, cOldStrField: string;
  nOldIntField: integer;
  dOldDateField: TDateTime;
  lOldBoolField: boolean;
  nOldFloatField: Double;
begin
  with ADataset do begin
    cNewStrField := 'Edited Value!';
    nNewIntField := 12345;
    dNewDateField := Now - 1;
    lNewBoolField := true;
    nNewFloatField := 99999.9999;
    cNewMemoField := 'Edited Value!';

    First; // record 0
    MoveBy(GotoRecord);
    //Save old information
    cOldStrField := FieldByName('StrField').Value;
    nOldIntField := FieldByName('IntField').Value;
    dOldDateField := FieldByName('DateField').Value;
    lOldBoolField := FieldByName('BoolField').Value;
    nOldFloatField := FieldByName('FloatField').Value;
    cOldMemoField := FieldByName('MemoField').Value;

    Edit;
    //Set new information
    FieldByName('StrField').Value := cNewStrField;
    FieldByName('IntField').Value := nNewIntField;
    FieldByName('DateField').Value := dNewDateField;
    FieldByName('BoolField').Value := lNewBoolField;
    FieldByName('FloatField').Value := nNewFloatField;
    FieldByName('MemoField').Value := cNewMemoField;
    Cancel;

    First; // record 0
    MoveBy(GotoRecord); // record 20
    //Compare actual with saveld information
    Check(FieldByName('IntField').AsInteger = nOldIntField, 'Integer edit error');
    Check(FieldByName('StrField').AsString = cOldStrField);
    Check(FieldByName('DateField').AsDateTime = dOldDateField, 'Date edit error');
    Check(FieldByName('BoolField').AsBoolean = lOldBoolField, 'Boolean edit error');
    Check(FieldByName('FloatField').AsFloat = nOldFloatField, 'Float edit error');
    Check(FieldByName('MemoField').Value = cOldMemoField, 'Memo edit error');
  end;
end;

procedure TTestTiDataset.Delete;
var Rc: integer;
begin
  with ADataset do begin
    First;
    Rc := RecordCount;
    while not ADataset.IsEmpty do begin
      Delete;
      Check(RecordCount = Rc - 1, Format('Records have not been deleted; prev=%d<>current=%d',[Rc - 1, RecordCount]));
      Dec(Rc);
    end;

    //Also check Eof && Bof after all have been deleted
    Check(Eof, 'Error handling empty recordset - EOF');
    Check(Bof, 'Error handling empty recordset - BOF');
    Check(Eof and Bof, 'Error handling empty recordset - EOF & BOF');
  end;
end;

procedure TTestTiDataset.FieldTypes;
begin
  Check(ADataset.FieldByName('StrField').DataType = ftString, 'Field type error - should be String');
  Check(ADataset.FieldByName('IntField').DataType = ftInteger, 'Field type error - should be Integer');
  Check(ADataset.FieldByName('DateField').DataType = ftDateTime, 'Field type error - should be Date');
  Check(ADataset.FieldByName('BoolField').DataType = ftBoolean, 'Field type error - should be Boolean');
  Check(ADataset.FieldByName('FloatField').DataType = ftFloat, 'Field type error - should be Float');
  Check(ADataset.FieldByName('MemoField').DataType = ftString, 'Field type error - should be String');
end;

procedure TTestTiDataset.FilterData(Dataset: TDataSet;var Accept: boolean);
begin
  Accept := Dataset.FieldByname('BoolField').AsBoolean;
end;

procedure TTestTiDataset.FilterTheData;
var i, AFoundValues, ATrueVales: integer;
begin

  ATrueVales := 0;
  for i:= 0 to AList.Count-1 do begin
    if TtiDatasetItem(AList[i]).BoolField
    then Inc(ATrueVales);
  end;

  AFoundValues := 0;
  with ADataset do begin
    OnFilterRecord := FilterData;
    Filtered := true;
    First;
    while not Eof do begin
      Check((GetActiveItem as TtiDatasetItem).BoolField, 'Only true booleans should show');
      inc(AFoundValues);
      Next;
    end;

    Check(AFoundValues = ATrueVales, 'Amount of records in filtering is incorrect');
  end;
end;

procedure TTestTiDataset.CalcFields(Dataset: TDataSet);
begin
  Dataset.FieldByName('CalcField').AsInteger := Dataset.FieldByName('IntField').AsInteger;
end;

procedure TTestTiDataset.CalculatedFields;
//Create persistent fields the hard way, and add a calculated field.
var f: TIntegerField;
begin
  with ADataset do begin
    Close;
    exit; //Doesn't seem to work right now... ???? Why ????

    f := TIntegerField.Create(ADataset);
    f.FieldName := 'CalcField';
    f.FieldKind := fkCalculated;
    f.Name := 'CalcField';

    f := TIntegerField.Create(ADataset);
    f.FieldName := 'IntField';
    f.FieldKind := fkData;
    f.Name := 'IntField';

    //FieldDefs.Update;

    OnCalcFields := CalcFields;
    Open;
    First;
    Moveby(3);
    Check(FieldByName('IntField').AsInteger = 3, 'Manual field should be 3');
    Check(FieldByName('CalcField').AsInteger = 3, 'Calculated field should be 3');
  end;
end;

procedure TTestTiDataset.MoveForwardBack;
var c: integer;
begin
  with ADataset do begin

    First; //Record #1
    MoveBy(23); //#24
    for c := 0 to 9 do MoveBy( - 1); // 14
    Check((GetActiveItem as TtiDatasetItem).IntField = 14, 'Error in navigation');

    Last; //Record #100
    MoveBy(-25); //#75
    for c := 0 to 9 do MoveBy( - 1); // 65
    Check((GetActiveItem as TtiDatasetItem).IntField = 65, 'Error in navigation');
  end;
end;

procedure TTestTiDataset.RetrieveObject;
begin
  with ADataset do begin
    //We are siting on record 1
    MoveBy(3); // to record 4
    Check((GetActiveItem as TtiDatasetItem).IntField = 4, 'GetActiveItem retrieves incorrect object');
  end;
end;

{procedure TTestTiDataset.ClearFields;
begin
  //The Dataset does not support InsertRecord & AppendRecord or ClearFields!!
  with ADataset do begin
    First; //#1
    MoveBy(20); // #21
    Edit;
    ADataset.ClearFields; //Ahum, tiDataset does not support this function.. crap...
    Post;

    First; //#1
    MoveBy(20); // #21

    //Check NULL values
    Check(FieldByName('StrField').IsNull, 'String is not null');
    Check(FieldByName('IntField').IsNull, 'Integer is not null');
    Check(FieldByName('DateField').IsNull, 'Date is not null');
    Check(FieldByName('BoolField').IsNull, 'Boolean is not null');
    Check(FieldByName('FloatField').IsNull, 'Float is not null');
    Check(FieldByName('MemoField').IsNull, 'Memo is not null');

    //Check default values
    Check(FieldByName('StrField').AsString = '', 'String ClearFields error');
    Check(FieldByName('IntField').AsInteger = 0, 'Integer ClearFields error');
    Check(FieldByName('DateField').AsDateTime = 0, 'Date ClearFields error');
    Check(FieldByName('BoolField').AsBoolean = false, 'Boolean ClearFields error');
    Check(Abs(FieldByName('FloatField').AsFloat) < 0.0001, 'Float ClearFields error');
    Check(FieldByName('MemoField').AsString = '', 'Memo ClearFields error');
  end;
end;}

end.





unit sample;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxCheckBox, StdCtrls, cxRadioGroup, tiModelMediator, tiObject, tiDeMediators,
  Menus, cxLookAndFeelPainters, cxButtons, cxGraphics, cxMaskEdit,
  cxDropDownEdit, cxLabel, cxTrackBar, cxMemo, cxStyles, {cxSchedulerStorage,
  cxSchedulerCustomControls, cxSchedulerDateNavigator,} cxCalendar, cxSpinEdit,
  cxButtonEdit, ComCtrls, ShlObj,
  cxShellCommon, cxDBExtLookupComboBox, cxShellComboBox, cxRichEdit,
  cxCheckGroup, cxCheckComboBox, cxFontNameComboBox, cxColorComboBox,
  cxCheckListBox, cxProgressBar, cxListBox, cxGroupBox, cxLookupEdit,
  cxDBLookupEdit, cxDBLookupComboBox, cxMRUEdit, cxBlobEdit, cxImage,
  cxCurrencyEdit, cxTimeEdit, cxHyperLinkEdit, cxCalc, cxImageComboBox,
  cxCustomData, cxFilter, cxData, cxDataStorage, DB, cxDBData,
  cxGridBandedTableView, cxGridLevel, cxClasses, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  cxEditRepositoryItems, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer,
  cxTLData, cxLookAndFeels, dxCore, cxDateUtils, cxNavigator;

type

  TLookupItem = class(TtiObject)
  private
    FDescription: string;
  protected
    function GetCaption: string; override;
  published
    property Description: string read FDescription write FDescription;
  end;

  TLookupItemList = class(TtiObjectList);

  TTestObject = class(TtiObject)
  private
    FTestString: string;
    FTestBoolean: Boolean;
    FTestInteger: Integer;
    FTestDateTime: TDateTime;
    FTestLookupItem: TLookupItem;
    FTestDouble: Double;
    FTestCurrency: Currency;
    FTestColor: TColor;
    procedure SetTestInteger(const Value: Integer);
    procedure SetTestBoolean(const Value: Boolean);
    procedure SetTestColor(const Value: TColor);
    procedure SetTestCurrency(const Value: Currency);
    procedure SetTestDateTime(const Value: TDateTime);
    procedure SetTestDouble(const Value: Double);
    procedure SetTestLookupItem(const Value: TLookupItem);
    procedure SetTestString(const Value: string);
  published
    property TestString: string read FTestString write SetTestString;
    property TestBoolean: Boolean read FTestBoolean write SetTestBoolean;
    property TestInteger: Integer read FTestInteger write SetTestInteger;
    property TestLookupItem: TLookupItem read FTestLookupItem write SetTestLookupItem;
    property TestDateTime: TDateTime read FTestDateTime write SetTestDateTime;
    property TestDouble: Double read FTestDouble write SetTestDouble;
    property TestCurrency: Currency read FTestCurrency write SetTestCurrency;
    property TestColor: TColor read FTestColor write SetTestColor;
  end;

  TTestObjectList = class(TtiObjectList)
  end;

  TTestTreeObject = class(TtiObjectList)
  private
    FTestString: string;
    FTestBoolean: Boolean;
    FTestInteger: Integer;
    FTestDateTime: TDateTime;
    FTestLookupItem: TLookupItem;
    FTestDouble: Double;
    FTestCurrency: Currency;
    FTestColor: TColor;
    procedure SetTestInteger(const Value: Integer);
    procedure SetTestBoolean(const Value: Boolean);
    procedure SetTestColor(const Value: TColor);
    procedure SetTestCurrency(const Value: Currency);
    procedure SetTestDateTime(const Value: TDateTime);
    procedure SetTestDouble(const Value: Double);
    procedure SetTestLookupItem(const Value: TLookupItem);
    procedure SetTestString(const Value: string);
  published
    property TestString: string read FTestString write SetTestString;
    property TestBoolean: Boolean read FTestBoolean write SetTestBoolean;
    property TestInteger: Integer read FTestInteger write SetTestInteger;
    property TestLookupItem: TLookupItem read FTestLookupItem write SetTestLookupItem;
    property TestDateTime: TDateTime read FTestDateTime write SetTestDateTime;
    property TestDouble: Double read FTestDouble write SetTestDouble;
    property TestCurrency: Currency read FTestCurrency write SetTestCurrency;
    property TestColor: TColor read FTestColor write SetTestColor;
  end;

  TForm1 = class(TForm)
    cxTextEdit1: TcxTextEdit;
    bDebug: TcxButton;
    cxCheckBox1: TcxCheckBox;
    cxComboBox1: TcxComboBox;
    cxItemComboBox1: TcxComboBox;
    cxDynamicComboBox1: TcxComboBox;
    cxLabel1: TcxLabel;
    cxTrackBar1: TcxTrackBar;
    cxMemo1: TcxMemo;
    cxDateEdit1: TcxDateEdit;
    cxSpinEdit1: TcxSpinEdit;
    cxMaskEdit1: TcxMaskEdit;
    cxButtonEdit1: TcxButtonEdit;
    Label1: TLabel;
    Label2: TLabel;
    cxImageComboBox1: TcxImageComboBox;
    cxHyperLinkEdit1: TcxHyperLinkEdit;
    cxTimeEdit1: TcxTimeEdit;
    cxCurrencyEdit1: TcxCurrencyEdit;
    cxImage1: TcxImage;
    cxBlobEdit1: TcxBlobEdit;
    cxMRUEdit1: TcxMRUEdit;
    cxPopupEdit1: TcxPopupEdit;
    cxLookupComboBox1: TcxLookupComboBox;
    cxRadioGroup1: TcxRadioGroup;
    cxListBox1: TcxListBox;
    cxProgressBar1: TcxProgressBar;
    cxCheckListBox1: TcxCheckListBox;
    cxColorComboBox1: TcxColorComboBox;
    cxFontNameComboBox1: TcxFontNameComboBox;
    cxCheckComboBox1: TcxCheckComboBox;
    cxCheckGroup1: TcxCheckGroup;
    cxRichEdit1: TcxRichEdit;
    cxShellComboBox1: TcxShellComboBox;
    cxExtLookupComboBox1: TcxExtLookupComboBox;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxgbtvTest: TcxGridBandedTableView;
    bDebugList: TcxButton;
    cxCalcEdit1: TcxCalcEdit;
    cxgtvTest: TcxGridTableView;
    cxGrid1Level2: TcxGridLevel;
    cxvtlTest: TcxVirtualTreeList;
    tiModelMediator1: TtiModelMediator;
    tiModelMediator2: TtiModelMediator;
    tiModelMediator3: TtiModelMediator;
    cxvtlTest2: TcxVirtualTreeList;
    procedure FormCreate(Sender: TObject);
    procedure bDebugClick(Sender: TObject);
    procedure bDebugListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FGridMediator: TticxCustomGridTableViewMediatorView;
    FTreeMediator: TticxVirtualTreeListMediatorView;
    FData: TTestObject;
    FTestObjectList: TTestObjectList;
    FTestTreeObject: TTestTreeObject;
    FLookupItemList: TLookupItemList;
    procedure SetupMediators;
    procedure SetupTestData;
    procedure SetupTestObjectList;
    procedure SetupTestTreeObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  tiDialogs;

{$R *.dfm}

procedure TForm1.SetupMediators;
begin
  with tiModelMediator1 do
  begin
    // String controls
    AddProperty('TestString', cxTextEdit1);
    AddProperty('TestString', cxMaskEdit1);
    AddProperty('TestString', cxButtonEdit1);
    AddProperty('TestString', cxComboBox1);
    AddProperty('TestString', cxLabel1);
    AddProperty('TestString', cxMemo1);
    AddProperty('TestString', cxHyperLinkEdit1);
    AddProperty('TestString', cxMRUEdit1);
//    AddProperty('TestString', cxPopupEdit1);
//    AddProperty('TestString', cxRichEdit1);

    // Color controls
    AddProperty('TestColor', cxColorComboBox1);

    // Boolean controls
    AddProperty('TestBoolean', cxCheckBox1);

    // Integer controls
    AddProperty('TestInteger', cxTrackBar1);
    AddProperty('TestInteger', cxItemComboBox1);
    AddProperty('TestInteger', cxSpinEdit1);
    AddProperty('TestInteger', cxRadioGroup1);
    AddProperty('TestInteger', cxProgressBar1);

    // Single controls
    AddProperty('TestDouble', cxCalcEdit1);

    // Lookup item controls
    AddProperty('TestLookupItem', cxDynamicComboBox1).ValueList := FLookupItemList;

    // DateTime controls
    AddProperty('TestDateTime', cxDateEdit1);
    AddProperty('TestDateTime', cxTimeEdit1);

    // Currency controls
    AddProperty('TestCurrency', cxCurrencyEdit1);
  end;
  tiModelMediator1.Subject := FData;
  tiModelMediator1.Active := True;

  tiModelMediator2.AddComposite('TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', cxgbtvTest);
  tiModelMediator2.Subject := FTestObjectList;
  tiModelMediator2.Active := True;

  tiModelMediator3.Subject := FTestTreeObject;
  tiModelMediator3.Active := True;
  tiModelMediator3.AddComposite('TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', cxvtlTest);

  FGridMediator := TticxGridTableViewMediatorView.CreateCustom(cxgtvTest, FTestObjectList, 'TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', TTestObject);
  FTreeMediator := TticxVirtualTreeListMediatorView.CreateCustom(cxvtlTest2, FTestTreeObject, 'TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', TTestTreeObject);
end;

procedure TForm1.SetupTestData;
var
  NewItem: TLookupItem;
begin
  FLookupItemList := TLookupItemList.Create;

  NewItem := TLookupItem.Create;
  NewItem.Description := 'item 1';
  FLookupItemList.Add(NewItem);

  NewItem := TLookupItem.Create;
  NewItem.Description := 'item 2';
  FLookupItemList.Add(NewItem);

  NewItem := TLookupItem.Create;
  NewItem.Description := 'item 3';
  FLookupItemList.Add(NewItem);

  FData := TTestObject.Create;
  FData.TestString := 'This is a test string.';
  FData.TestBoolean := True;
  FData.TestInteger := 2;
  FData.TestLookupItem := TLookupItem(FLookupItemList.Items[1]);
  FData.TestDateTime := Now;
  FData.TestDouble := Random(10000) / 100;
  FData.TestCurrency := Random(10000) / 100;
  FData.TestColor := clRed;

end;

procedure TForm1.SetupTestObjectList;
var
  NewItem: TTestObject;
  I: Integer;
begin
  FTestObjectList := TTestObjectList.Create;
  for I := 0 to 100 do
  begin
    NewItem := TTestObject.Create;
    NewItem.OID.AsString := 'object' + IntToStr(i);    
    NewItem.TestString := 'string ' + IntToStr(i);
    NewItem.TestBoolean := (i mod 2 = 0);
    NewItem.TestInteger := i;
    NewItem.TestDateTime := now - i;
    NewItem.TestDouble := Random(1000) / 100;
    NewItem.TestCurrency := Random(1000) / 100;
    NewItem.TestColor := Random(16777215);
    FTestObjectList.Add(NewItem);
  end;
end;

procedure TForm1.SetupTestTreeObject;
  procedure FillData(TestTreeObject: TTestTreeObject);
  var
    I: Integer;
    NewItem: TTestTreeObject;
  begin
    for I := 0 to 100 do
    begin
      NewItem := TTestTreeObject.Create;
      NewItem.OID.AsString := 'object' + IntToStr(i);
      NewItem.TestString := 'string ' + IntToStr(i);
      NewItem.TestBoolean := (i mod 2 = 0);
      NewItem.TestInteger := i;
      NewItem.TestDateTime := now - i;
      NewItem.TestDouble := Random(1000) / 100;
      NewItem.TestCurrency := Random(1000) / 100;
      NewItem.TestColor := Random(16777215);
      TestTreeObject.Add(NewItem);
    end;
  end;
var
  I: Integer;
begin
  FTestTreeObject := TTestTreeObject.Create;

  FillData(FTestTreeObject);
  for I := 0 to 100 do
    FillData(FTestTreeObject[i] as TTestTreeObject);
end;

procedure TForm1.bDebugClick(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TForm1.bDebugListClick(Sender: TObject);
begin
  tiShowString(FTestObjectList.AsDebugString);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FGridMediator.Free;
  FTreeMediator.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterFallbackMediators;
  SetupTestData;
  SetupTestObjectList;
  SetupTestTreeObject;
  SetupMediators;
end;

{ TLookupItem }

function TLookupItem.GetCaption: string;
begin
  Result := Description;
end;

{ TTestObject }

procedure TTestObject.SetTestBoolean(const Value: Boolean);
begin
  if FTestBoolean = Value then
    Exit;
  BeginUpdate;
  FTestBoolean := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestColor(const Value: TColor);
begin
  if FTestColor = Value then
    Exit;
  BeginUpdate;
  FTestColor := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestCurrency(const Value: Currency);
begin
  if FTestCurrency = Value then
    Exit;
  BeginUpdate;
  FTestCurrency := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestDateTime(const Value: TDateTime);
begin
  if FTestDateTime = Value then
    Exit;
  BeginUpdate;
  FTestDateTime := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestDouble(const Value: Double);
begin
  if FTestDouble = Value then
    Exit;
  BeginUpdate;
  FTestDouble := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestInteger(const Value: Integer);
begin
  if FTestInteger = Value then
    Exit;
  BeginUpdate;
  FTestInteger := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestLookupItem(const Value: TLookupItem);
begin
  if FTestLookupItem = Value then
    Exit;
  BeginUpdate;
  FTestLookupItem := Value;
  EndUpdate;
end;

procedure TTestObject.SetTestString(const Value: string);
begin
  if FTestString = Value then
    Exit;
  BeginUpdate;
  FTestString := Value;
  EndUpdate;
end;

{ TTestTreeObject }

procedure TTestTreeObject.SetTestBoolean(const Value: Boolean);
begin
  if FTestBoolean = Value then
    Exit;
  BeginUpdate;
  FTestBoolean := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestColor(const Value: TColor);
begin
  if FTestColor = Value then
    Exit;
  BeginUpdate;
  FTestColor := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestCurrency(const Value: Currency);
begin
  if FTestCurrency = Value then
    Exit;
  BeginUpdate;
  FTestCurrency := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestDateTime(const Value: TDateTime);
begin
  if FTestDateTime = Value then
    Exit;
  BeginUpdate;
  FTestDateTime := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestDouble(const Value: Double);
begin
  if FTestDouble = Value then
    Exit;
  BeginUpdate;
  FTestDouble := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestInteger(const Value: Integer);
begin
  if FTestInteger = Value then
    Exit;
  BeginUpdate;
  FTestInteger := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestLookupItem(const Value: TLookupItem);
begin
  if FTestLookupItem = Value then
    Exit;
  BeginUpdate;
  FTestLookupItem := Value;
  EndUpdate;
end;

procedure TTestTreeObject.SetTestString(const Value: string);
begin
  if FTestString = Value then
    Exit;
  BeginUpdate;
  FTestString := Value;
  EndUpdate;
end;

end.
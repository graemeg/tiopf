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
  cxEditRepositoryItems;

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
    procedure FormCreate(Sender: TObject);
    procedure bDebugClick(Sender: TObject);
    procedure bDebugListClick(Sender: TObject);
  private
    FMediator: TtiModelMediator;
    FMediator2: TtiModelMediator;
    FGridMediator: tiDeMediators.TticxCustomGridTableViewMediatorView;
    FData: TTestObject;
    FTestObjectList: TTestObjectList;
    FLookupItemList: TLookupItemList;
    procedure SetupMediators;
    procedure SetupTestData;
    procedure SetupTestObjectList;
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
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    // String controls
    FMediator.AddProperty('TestString', cxTextEdit1);
    FMediator.AddProperty('TestString', cxMaskEdit1);
    FMediator.AddProperty('TestString', cxButtonEdit1);
    FMediator.AddProperty('TestString', cxComboBox1);
    FMediator.AddProperty('TestString', cxLabel1);
    FMediator.AddProperty('TestString', cxMemo1);
    FMediator.AddProperty('TestString', cxHyperLinkEdit1);
    FMediator.AddProperty('TestString', cxMRUEdit1);
//    FMediator.AddProperty('TestString', cxPopupEdit1);
//    FMediator.AddProperty('TestString', cxRichEdit1);

    // Color controls
    FMediator.AddProperty('TestColor', cxColorComboBox1);

    // Boolean controls
    FMediator.AddProperty('TestBoolean', cxCheckBox1);

    // Integer controls
    FMediator.AddProperty('TestInteger', cxTrackBar1);
    FMediator.AddProperty('TestInteger', cxItemComboBox1);
    FMediator.AddProperty('TestInteger', cxSpinEdit1);
    FMediator.AddProperty('TestInteger', cxRadioGroup1);
    FMediator.AddProperty('TestInteger', cxProgressBar1);

    // Single controls
    FMediator.AddProperty('TestDouble', cxCalcEdit1);

    // Lookup item controls
    FMediator.AddProperty('TestLookupItem', cxDynamicComboBox1).ValueList := FLookupItemList;

    // DateTime controls
    FMediator.AddProperty('TestDateTime', cxDateEdit1);
    FMediator.AddProperty('TestDateTime', cxTimeEdit1);

    // Currency controls
    FMediator.AddProperty('TestCurrency', cxCurrencyEdit1);
  end;

  if not Assigned(FMediator2) then
  begin
    FMediator2 := TtiModelMediator.Create(Self);
    FMediator2.AddComposite('TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', cxgbtvTest);
  end;

  FGridMediator := TticxGridTableViewMediatorView.CreateCustom(cxgtvTest, FTestObjectList, 'TestString;TestBoolean;TestInteger;TestDateTime;TestDouble;TestCurrency;TestColor', TTestObject);

  FMediator.Subject := FData;
  FMediator.Active := True;

  FMediator2.Subject := FTestObjectList;
  FMediator2.Active := True;
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

procedure TForm1.bDebugClick(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TForm1.bDebugListClick(Sender: TObject);
begin
  tiShowString(FTestObjectList.AsDebugString);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterFallbackMediators;
  SetupTestData;
  SetupTestObjectList;
  SetupMediators;
end;

{ TLookupItem }

function TLookupItem.GetCaption: string;
begin
  Result := Description;
end;

{ TTestObject }

procedure TTestObject.SetTestBoolean(const Value: Boolean);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestBoolean := Value;
end;

procedure TTestObject.SetTestColor(const Value: TColor);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestColor := Value;
end;

procedure TTestObject.SetTestCurrency(const Value: Currency);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestCurrency := Value;
end;

procedure TTestObject.SetTestDateTime(const Value: TDateTime);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestDateTime := Value;
end;

procedure TTestObject.SetTestDouble(const Value: Double);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestDouble := Value;
end;

procedure TTestObject.SetTestInteger(const Value: Integer);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestInteger := Value;
end;

procedure TTestObject.SetTestLookupItem(const Value: TLookupItem);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestLookupItem := Value;
end;

procedure TTestObject.SetTestString(const Value: string);
var
  Notify: ItiNotifyObserversHelper;
begin
  Notify := NotifyObserversHelper;
  FTestString := Value;
end;

end.
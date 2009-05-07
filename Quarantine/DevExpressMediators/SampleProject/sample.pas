unit sample;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxCheckBox, StdCtrls, cxRadioGroup, tiFormMediator, tiObject, tiDeMediators,
  Menus, cxLookAndFeelPainters, cxButtons, cxGraphics, cxMaskEdit,
  cxDropDownEdit, cxLabel, cxTrackBar, cxMemo, cxStyles, cxSchedulerStorage,
  cxSchedulerCustomControls, cxSchedulerDateNavigator, cxCalendar, cxSpinEdit;

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
    FTheTextEdit: string;
    FTheCheckBox: Boolean;
    FTheCombobox: string;
    FTheItemCombobox: Integer;
    FTheDynamicCombobox: TLookupItem;
    FTheTrackBar: Integer;
    FTheMemo: string;
    FTheDateEdit: TDateTime;
    FTheSpinEdit: Integer;
  published
    property TheTextEdit: string read FTheTextEdit write FTheTextEdit;
    property TheCheckBox: Boolean read FTheCheckBox write FTheCheckBox;
    property TheCombobox: string read FTheCombobox write FTheCombobox;
    property TheItemCombobox: Integer read FTheItemCombobox write FTheItemCombobox;
    property TheDynamicCombobox: TLookupItem read FTheDynamicCombobox write FTheDynamicCombobox;
    property TheStaticText: string read FTheTextEdit;
    property TheTrackBar: Integer read FTheTrackBar write FTheTrackBar;
    property TheMemo: string read FTheMemo write FTheMemo;
    property TheDateEdit: TDateTime read FTheDateEdit write FTheDateEdit;
    property TheSpinEdit: Integer read FTheSpinEdit write FTheSpinEdit;
  end;

  TForm1 = class(TForm)
    cxTextEdit: TcxTextEdit;
    bDebug: TcxButton;
    cxCheckBox: TcxCheckBox;
    cxComboBox: TcxComboBox;
    cxItemComboBox: TcxComboBox;
    cxDynamicComboBox: TcxComboBox;
    cxLabel: TcxLabel;
    cxTrackBar: TcxTrackBar;
    cxMemo: TcxMemo;
    cxDateEdit: TcxDateEdit;
    cxSpinEdit: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure bDebugClick(Sender: TObject);
  private
    FMediator: TFormMediator;
    FData: TTestObject;
    FLookupItemList: TLookupItemList;
    procedure SetupMediators;
    procedure SetupTestData;
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
    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('TheTextEdit', cxTextEdit);
    FMediator.AddProperty('TheCheckBox', cxCheckBox);
    FMediator.AddProperty('TheCombobox', cxComboBox);
    FMediator.AddProperty('TheItemCombobox', cxItemComboBox);
    FMediator.AddProperty('TheDynamicCombobox', cxDynamicComboBox).ValueList := FLookupItemList;
    FMediator.AddProperty('TheStaticText', cxLabel);    
    FMediator.AddProperty('TheTrackbar', cxTrackBar);
    FMediator.AddProperty('TheMemo', cxMemo);
    FMediator.AddProperty('TheDateEdit', cxDateEdit);
    FMediator.AddProperty('TheSpinEdit', cxSpinEdit);


//    FMediator.AddProperty('AddressType', cbType).ValueList := gContactManager.AddressTypeList;
//    FMediator.AddProperty('Nr', ENumber);
//    FMediator.AddProperty('Street', EStreet);
//    FMediator.AddProperty('City', cbCity).ValueList := gContactManager.CityList;
//    FMediator.AddProperty('Telephone1', ETelePhone1);
//    FMediator.AddProperty('Telephone2', ETelePhone2);
//    FMediator.AddProperty('Fax', EFax);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
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
  FData.TheTextEdit := 'This is a string.';
  FData.TheCheckBox := True;
  FData.TheCombobox := 'Value 2';
  FData.TheItemCombobox := 2;
  FData.TheDynamicCombobox := TLookupItem(FLookupItemList.Items[1]);
  FData.TheTrackBar := 5;
  FData.TheMemo := 'This is a memo.';
  FData.TheDateEdit := Now;

end;

procedure TForm1.bDebugClick(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterFallbackMediators;
  SetupTestData;
  SetupMediators;
end;

{ TLookupItem }

function TLookupItem.GetCaption: string;
begin
  Result := Description;
end;

end.

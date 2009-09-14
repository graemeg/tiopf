unit tiDeMediators;

//TODO: View control was moved to base TtiMediatorView. Change descendants:
// - Delete FEditControl private field.
// - Change property EditControl to the following function, cast result as
//   the appropriate control type:
//   function  View: TSomeControlType; reintroduce;
//     result := TSomeControlType(inherited View);
// - Remove SetGUIControl and GetGUIControl methods.
// - Replace FEditControl := AGridView, with SetView(AGridView). [base class]
// - Replace other instances of FEditControl with View
//   (use <> nil instead of Assigned where applicable).

interface

uses
  tiObject
  ,tiBaseMediator
  ,tiExcept
  ,tiGUIConstants
  ,Classes
  ,Controls
  ,TypInfo
  ,Graphics
  ,cxEdit
  ,cxTextEdit
  ,cxMaskEdit
  ,cxButtonEdit
  ,cxCurrencyEdit
  ,cxCalc
  ,cxHyperLinkEdit
  ,cxCheckBox
  ,cxDropDownEdit
  ,cxMRUEdit
  ,cxLabel
  ,cxSpinEdit
  ,cxTimeEdit
  ,cxTrackBar
  ,cxMemo
  ,cxCalendar
  ,cxRadioGroup
  ,cxProgressBar
  ,cxGridCustomView
  ,cxCustomData
  ,cxGridCustomTableView
  ,cxGridTableView
  ,cxGridBandedTableView
  ,cxColorComboBox
  ;

type
  TticxCustomEditMediatorView = class(TtiMediatorView)
  private
    FViewColor: TColor;
    FViewErrorColor: TColor;
    FViewHint: string;
    procedure SetViewErrorColor(const AValue: TColor);
  protected
    function GetCurrentControlColor: TColor; virtual;
    procedure SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
        override;
    procedure UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function ComponentClass: TClass; override;
    procedure SetView(const AValue: TComponent); override;
    function View: TcxCustomEdit; reintroduce;
    property ViewErrorColor: TColor read FViewErrorColor write
        SetViewErrorColor;
  end;

  TticxCustomTextEditMediatorView = class(TticxCustomEditMediatorView)
  private
    FControlReadOnlyColor: TColor;
    procedure SetControlReadOnlyColor(const AValue: TColor);
  protected
    function GetCurrentControlColor: TColor; override;
    procedure SetupGUIandObject; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxCustomTextEdit; reintroduce;
    property ControlReadOnlyColor: TColor read FControlReadOnlyColor write
        SetControlReadOnlyColor;
  end;

  TticxTextEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxTextEdit; reintroduce;
  end;

  TticxMaskEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxMaskEdit; reintroduce;
  end;

  TticxButtonEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxButtonEdit; reintroduce;
  end;

  TticxCurrencyEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxCurrencyEdit; reintroduce;
  end;

  TticxCalcEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxCalcEdit; reintroduce;
  end;

  TticxHyperLinkEditMediatorView = class(TticxCustomTextEditMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxHyperLinkEdit; reintroduce;
  end;

  TticxCheckBoxMediatorView = class(TticxCustomEditMediatorView)
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxCheckBox; reintroduce;
  end;

  { Base class to handle TcxCustomComboBox controls }
  TticxCustomComboBoxMediatorView = class(TticxCustomEditMediatorView)
  protected
    procedure DoObjectToGUI; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxCustomComboBox; reintroduce;
  end;

  TticxComboBoxMediatorView = class(TticxCustomComboBoxMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxComboBox; reintroduce;
  end;

  TticxMRUEditMediatorView = class(TticxCustomComboBoxMediatorView)
  protected
    procedure DoObjectToGUI; override;
  public
    class function ComponentClass: TClass; override;
    function View: TcxMRUEdit; reintroduce;
  end;

  { Sets ItemIndex based on integer property }
  TticxItemComboBoxMediatorView = class(TticxComboBoxMediatorView)
  protected
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
  public
    constructor Create; override;
  end;

  { TComboBox observing a list and setting a Object property }
  TticxDynamicComboBoxMediatorView = class(TticxComboBoxMediatorView)
  private
    FDisplayFieldName: string;
    FExternalOnChange: TNotifyEvent;
    function GetDisplayFieldName: string;
    procedure InternalListRefresh;
  protected
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
    procedure SetOnChangeActive(AValue: Boolean); virtual;
    procedure SetupGUIandObject; override;
  public
    procedure RefreshList; virtual;
    property DisplayFieldName: string read GetDisplayFieldName write
        FDisplayFieldName;
  end;

  TticxColorComboBoxMediatorView = class(TticxCustomComboBoxMediatorView)
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxColorComboBox; reintroduce;
  end;

  { Base class to handle TLabel controls }
  TticxLabelMediatorView = class(TtiMediatorView)
  protected
    procedure SetupGUIandObject; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxLabel; reintroduce;
  end;

  TticxSpinEditMediatorView = class(TticxCustomEditMediatorView)
  protected
    procedure SetupGUIandObject; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxSpinEdit; reintroduce;
  end;

  TticxTimeEditMediatorView = class(TticxCustomEditMediatorView)
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxTimeEdit; reintroduce;
  end;

  { Base class to handle TTrackBar controls }
  TticxTrackBarMediatorView = class(TtiMediatorView)
  protected
    procedure SetupGUIandObject; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxTrackBar; reintroduce;
  end;

  { Base class to handle TMemo controls }
  TticxMemoMediatorView = class(TtiMediatorView)
  protected
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
    procedure SetupGUIandObject; override;
  public
    class function ComponentClass: TClass; override;
    function View: TcxMemo; reintroduce;
  end;

  { Base class to handle TDateTimePicker controls }
  TticxDateEditMediatorView = class(TticxCustomEditMediatorView)
  protected
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxDateEdit; reintroduce;
  end;

  { Base class to handle TcxRadioGroup controls }
  TticxRadioGroupBoxMediatorView = class(TtiMediatorView)
  protected
    procedure DoObjectToGUI; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxRadioGroup; reintroduce;
  end;

  { base class to handle TcxProgresBar }
  TticxProgressbarMediatorView = class(TtiMediatorView)
  private
    procedure ProgressBarMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
    procedure ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
        Integer);
    procedure SetProgressBarBasedOnMouse(X: Integer);
  protected
    procedure DoObjectToGUI; override;
    procedure SetupGUIandObject; override;
  public
    constructor Create; override;
    class function ComponentClass: TClass; override;
    function View: TcxProgressBar; reintroduce;
  end;

  TUserDataSource = class(TcxCustomDataSource)
  private
    FObjectList: TtiObjectList;
    FShowDeleted: Boolean;
    FValueClass: TtiObjectClass;
  protected
    function AppendRecord: Pointer; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
        override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle:
        TcxDataItemHandle): Variant; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle):
        TcxDataRecordHandle; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle:
        TcxDataItemHandle; const AValue: Variant); override;
  public
    property ObjectList: TtiObjectList read FObjectList write FObjectList;
    property ShowDeleted: Boolean read FShowDeleted write FShowDeleted;
    property ValueClass: TtiObjectClass read FValueClass write FValueClass;
  end;

  TticxCustomGridTableViewMediatorView = class(TtiMediatorView)
  private
    FFieldsInfo: TtiMediatorFieldInfoList;
    FUserDataSource: TUserDataSource;
    function GetShowDeleted: Boolean;
    procedure SetFieldsInfo(Value: TtiMediatorFieldInfoList);
    procedure SetShowDeleted(Value: Boolean);
  protected
    procedure CreateColumns; virtual;
    procedure DoObjectToGUI; override;
    procedure SetBooleanColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetColorColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetCurrencyColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetDateTimeColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetFieldName(const AValue: string); override;
    procedure SetFloatColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetIntegerColumn(var ANewColumn: TcxGridColumn); virtual;
    procedure SetStringColumn(var ANewColumn: TcxGridcolumn); virtual;
    procedure SetupGUIandObject; override;
    function View: TcxGridTableView; reintroduce;
  public
    constructor Create; override;
    constructor CreateCustom(AView: TComponent; ASubject: TtiObject;
        AFieldName: string; AValueClass: TtiObjectClass); reintroduce;
    destructor Destroy; override;
    class function ComponentClass: TClass; override;
    class function CompositeMediator: Boolean; override;
    procedure ParseDisplayNames(const AValue: string);
    property FieldsInfo: TtiMediatorFieldInfoList read FFieldsInfo write
        SetFieldsInfo;
    property ShowDeleted: Boolean read GetShowDeleted write SetShowDeleted;
  end;

  TticxGridTableViewMediatorView = class(TticxCustomGridTableViewMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxGridTableView; reintroduce;
  end;

  TticxGridBandedTableViewMediatorView = class(
      TticxCustomGridTableViewMediatorView)
  public
    class function ComponentClass: TClass; override;
    function View: TcxGridBandedTableView; reintroduce;
  end;

procedure RegisterFallBackMediators;

implementation

uses
  SysUtils, StdCtrls, tiOPFManager, Variants, tiRTTI, tiUtils,
  cxDataStorage;

type
  // Friend class to get access to protected methods
  THackcxCustomEdit = class(TcxCustomEdit)
  end;

const
  cFieldDelimiter = ';';
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';
  cErrorPropertyNotClass         = 'Property is not a class type!';
  cErrorAddingItemToCombobox     = 'Error adding list items to combobox ' +
                                   'Message: %s, Item Property Name: %s';

procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TticxTextEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxMaskEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxButtonEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxCurrencyEditMediatorView, TtiObject, [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxCalcEditMediatorView, TtiObject, [tkInteger, tkFloat]);
  gMediatorManager.RegisterMediator(TticxHyperLinkEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxCheckBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TticxComboBoxMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxMRUEditMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxItemComboBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TticxDynamicComboBoxMediatorView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TticxLabelMediatorView, TtiObject);
  gMediatorManager.RegisterMediator(TticxTrackBarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxMemoMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxSpinEditMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxTimeEditMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TticxDateEditMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TticxRadioGroupBoxMediatorView, TtiObject, [tkInteger]);

// not fully working
  gMediatorManager.RegisterMediator(TticxProgressbarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxColorComboBoxMediatorView, TtiObject, [tkInteger]);

  gMediatorManager.RegisterMediator(TticxGridTableViewMediatorView, TtiObjectList);
  gMediatorManager.RegisterMediator(TticxGridBandedTableViewMediatorView, TtiObjectList);
end;


{ TticxTextEditMediatorView }

constructor TticxCustomTextEditMediatorView.Create;
begin
  inherited Create;
  FControlReadOnlyColor := clWindow;
  GUIFieldName := 'Text';
end;

class function TticxCustomTextEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomTextEdit;
end;

function TticxCustomTextEditMediatorView.GetCurrentControlColor: TColor;
begin
  if View.Properties.ReadOnly then
    Result := ColorToRGB(ControlReadOnlyColor)
  else
    Result := inherited GetCurrentControlColor;
end;

procedure TticxCustomTextEditMediatorView.SetControlReadOnlyColor(const AValue:
    TColor);
begin
  if AValue <> FControlReadOnlyColor then
  begin
    FControlReadOnlyColor := AValue;
    TestIfValid; // Update view
  end;
end;

procedure TticxCustomTextEditMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    View.Properties.MaxLength := Ma;
end;

function TticxCustomTextEditMediatorView.View: TcxCustomTextEdit;
begin
  Result := inherited View as TcxCustomTextEdit;
end;

{ TtiCheckBoxMediatorView }

constructor TticxCheckBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Checked';
end;

class function TticxCheckBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxCheckBox;
end;

function TticxCheckBoxMediatorView.View: TcxCheckBox;
begin
  Result := inherited View as TcxCheckBox;
end;


{ TtiComboBoxMediatorView }

constructor TticxCustomComboBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Text';
end;

class function TticxCustomComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomCombobox;
end;

procedure TticxCustomComboBoxMediatorView.DoObjectToGUI;
begin
  View.ItemIndex := View.Properties.Items.IndexOf(Subject.PropValue[FieldName]);
end;

function TticxCustomComboBoxMediatorView.View: TcxCustomComboBox;
begin
  Result := inherited View as TcxCustomComboBox;
end;


{ TticxItemComboBoxMediatorViewView }

constructor TticxItemComboBoxMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'ItemIndex';
end;

procedure TticxItemComboBoxMediatorView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, View.ItemIndex);
end;

procedure TticxItemComboBoxMediatorView.DoObjectToGUI;
begin
  View.ItemIndex := GetOrdProp(Subject, FieldName);
end;

{ TticxDynamicComboBoxMediatorViewView }

procedure TticxDynamicComboBoxMediatorView.DoGUIToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if View.ItemIndex < 0 then
    Exit; //==>

  lValue := TtiObject(ValueList.Items[View.ItemIndex]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    RaiseMediatorError(cErrorPropertyNotClass);
end;

procedure TticxDynamicComboBoxMediatorView.DoObjectToGUI;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  View.ItemIndex := -1;
  if Subject = nil then
    Exit; //==>

  if not Assigned(ValueList) then
    RaiseMediatorError(cErrorListHasNotBeenAssigned);

  lValue := nil;
  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    RaiseMediatorError(cErrorPropertyNotClass);

  for i := 0 to ValueList.Count - 1 do
    if ValueList.Items[i].Equals(lValue) then
    begin
      View.ItemIndex := i;
      Break; //==>
    end;

  SetOnChangeActive(true);
end;

function TticxDynamicComboBoxMediatorView.GetDisplayFieldName: string;
begin
  Result := FDisplayFieldName;
  if Result = '' then
    Result := 'Caption'; // Do not localize.
end;

procedure TticxDynamicComboBoxMediatorView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := View.Properties.Items;
  lItems.Clear;
  View.Text := '';

  if (ValueList = nil) or
     (ValueList.Count < 1) or
     (SameText(FieldName, EmptyStr)) then
    Exit; //==>

  try
    for i := 0 to ValueList.Count - 1 do
    begin
      lItems.AddObject(GetStrProp(ValueList.Items[i],DisplayFieldName),ValueList.Items[i]);
    end;
  except
    on E: Exception do
      RaiseMediatorError(cErrorAddingItemToCombobox,[E.message, FieldName]);
  end;
end;

procedure TticxDynamicComboBoxMediatorView.RefreshList;
begin
  InternalListRefresh;
end;

procedure TticxDynamicComboBoxMediatorView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      View.Properties.OnEditValueChanged := FExternalOnChange
    else
      View.Properties.OnEditValueChanged := DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := View.Properties.OnEditValueChanged;
    View.Properties.OnEditValueChanged := nil;
  end;
end;

procedure TticxDynamicComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  InternalListRefresh;

  if Assigned(ValueList) then
    View.Enabled := ValueList.Count > 0;

  if UseInternalOnChange then
    View.Properties.OnEditValueChanged := DoOnChange; // default OnChange event handler

  if ValueList <> nil then
    View.Enabled := (ValueList.Count > 0);
end;

{ TtiStaticTextMediatorView }

constructor TticxLabelMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Caption';
end;

class function TticxLabelMediatorView.ComponentClass: TClass;
begin
  Result := TcxLabel;
end;

procedure TticxLabelMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Caption := '';
end;

function TticxLabelMediatorView.View: TcxLabel;
begin
  Result := inherited View as TcxLabel;
end;


{ TtiTrackBarMediatorView }

constructor TticxTrackBarMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Position';
end;

class function TticxTrackBarMediatorView.ComponentClass: TClass;
begin
  Result := TcxTrackBar;
end;

procedure TticxTrackBarMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
  begin
    View.Properties.Min := Mi;
    View.Properties.Max := Ma;
  end;
end;

function TticxTrackBarMediatorView.View: TcxTrackBar;
begin
  Result := inherited View as TcxTrackBar;
end;


{ TtiMemoMediatorView }

class function TticxMemoMediatorView.ComponentClass: TClass;
begin
  Result := TcxMemo;
end;

procedure TticxMemoMediatorView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := View.Lines.Text;
end;

procedure TticxMemoMediatorView.DoObjectToGUI;
begin
  View.Lines.Text := Subject.PropValue[FieldName];
end;

procedure TticxMemoMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIAndObject;
  View.Lines.Clear;
  View.Properties.ScrollBars := ssVertical;
  View.Properties.WordWrap   := True;
end;

function TticxMemoMediatorView.View: TcxMemo;
begin
  Result := inherited View as TcxMemo;
end;

{ TtiCalendarComboMediatorView }

constructor TticxDateEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'EditValue';
end;

class function TticxDateEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxDateEdit;
end;

procedure TticxDateEditMediatorView.DoGUIToObject;
begin
  if VarIsNull(View.EditValue) then
    Subject.PropValue[FieldName] := 0
  else
    inherited DoGUIToObject;
end;

procedure TticxDateEditMediatorView.DoObjectToGUI;
begin
  inherited;
  if Subject.PropValue[FieldName] = 0 then
    View.EditValue := '';
end;

function TticxDateEditMediatorView.View: TcxDateEdit;
begin
  Result := inherited View as TcxDateEdit;
end;

{ TticxCalcEditMediatorView }

constructor TticxCalcEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

class function TticxCalcEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCalcEdit;
end;

function TticxCalcEditMediatorView.View: TcxCalcEdit;
begin
  Result := inherited View as TcxCalcEdit;
end;


{ TticxTimeEditMediatorView }

constructor TticxTimeEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Time';
end;

class function TticxTimeEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxTimeEdit;
end;

function TticxTimeEditMediatorView.View: TcxTimeEdit;
begin
  Result := inherited View as TcxTimeEdit;
end;

{ TticxSpinEditMediatorView }

constructor TticxSpinEditMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Value';
end;

class function TticxSpinEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxSpinEdit;
end;

procedure TticxSpinEditMediatorView.SetupGUIandObject;
begin
  inherited;
  View.Value := 0;
end;

function TticxSpinEditMediatorView.View: TcxSpinEdit;
begin
  Result := inherited View as TcxSpinEdit;
end;

{ TticxMRUEditMediatorView }

class function TticxMRUEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxMRUEdit;
end;

procedure TticxMRUEditMediatorView.DoObjectToGUI;
var
  NewString: string;
begin
  // The MRUEdit must have the edit value in the MRU list, otherwise it is not shown.
  NewString := string(Subject.PropValue[Self.FieldName]);
  View.Properties.Items.Add(NewString);
  inherited;
end;

function TticxMRUEditMediatorView.View: TcxMRUEdit;
begin
  Result := inherited View as TcxMRUEdit;
end;

{ TticxRadioGroupBoxMediatorView }

constructor TticxRadioGroupBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'ItemIndex';
end;

class function TticxRadioGroupBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxRadioGroup;
end;

procedure TticxRadioGroupBoxMediatorView.DoObjectToGUI;
var
  iValue: Integer;
begin
  iValue := GetOrdProp(Subject, FieldName);
  if iValue < View.Properties.Items.Count then
    View.ItemIndex := iValue
  else
    View.ItemIndex := -1;
end;

function TticxRadioGroupBoxMediatorView.View: TcxRadioGroup;
begin
  Result := inherited View as TcxRadioGroup;
end;


{ TticxColorComboBoxMediatorView }

constructor TticxColorComboBoxMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'ColorValue';
end;

class function TticxColorComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxColorComboBox;
end;

function TticxColorComboBoxMediatorView.View: TcxColorComboBox;
begin
  Result := inherited View as TcxColorComboBox;
end;

{ TtiMediatorcxProgessbar }

constructor TticxProgressbarMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Position';
end;

class function TticxProgressbarMediatorView.ComponentClass: TClass;
begin
  Result := TcxProgressBar;
end;

procedure TticxProgressbarMediatorView.DoObjectToGUI;
begin
  inherited;
end;

procedure TticxProgressbarMediatorView.ProgressBarMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetProgressBarBasedOnMouse(X);
end;

procedure TticxProgressbarMediatorView.ProgressBarMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    SetProgressBarBasedOnMouse(X);
end;

procedure TticxProgressbarMediatorView.SetProgressBarBasedOnMouse(X: Integer);
var
  iValue: Integer;
  MyBar: TcxProgressBar;
begin
  inherited;
  MyBar := TcxProgressBar(View);
  iValue := round(MyBar.Properties.Max * (X + 2)/ MyBar.Width);
  MyBar.Position := iValue;
  TypInfo.SetInt64Prop(Subject, FieldName, iValue);
  Subject.Dirty := True;
  DoOnChange(View);
end;

procedure TticxProgressbarMediatorView.SetupGUIandObject;
begin
  inherited;
  View.OnMouseDown := ProgressbarMouseDown;
  View.OnMouseMove := ProgressbarMouseMove;
end;

function TticxProgressbarMediatorView.View: TcxProgressBar;
begin
  Result := inherited View as TcxProgressBar;
end;

{ TUserDataSource }

function TUserDataSource.AppendRecord: Pointer;
begin
  if FValueClass <> nil then
  begin
    Result := FValueClass.CreateNew;
    FObjectList.Add(TtiObject(Result));
  end
  else
    Result := nil;

  DataChanged;
end;

procedure TUserDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
begin
  TtiObject(ARecordHandle).Deleted := True;
  DataChanged;
end;

function TUserDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
var
  GridColumn: TcxCustomGridTableItem;
begin
  GridColumn := TcxCustomGridTableItem(DataController.GetItem(AItemIndex));
  Result := TcxDataItemHandle(GridColumn.DataBinding);
end;

function TUserDataSource.GetRecordCount: Integer;
begin
  if FShowDeleted then
    Result := FObjectList.Count
  else
    Result := FObjectList.CountNotDeleted;
end;

function TUserDataSource.GetRecordHandle(ARecordIndex: Integer):
    TcxDataRecordHandle;
var
  I, J: Integer;
begin
  I := 0;
  J := -1;
  while (I < FObjectList.Count) do
  begin
    if not FObjectList[I].Deleted then
      Inc(J);

    if J <> ARecordIndex then
      Inc(I)
    else
      Break;
  end;

  Result := TcxDataRecordHandle(FObjectList[I]);
end;

function TUserDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
    AItemHandle: TcxDataItemHandle): Variant;
var
  DataBinding: TcxGridItemDataBinding;
begin
  DataBinding := TcxGridItemDataBinding(AItemHandle);
  if Assigned(DataBinding.Data) and (DataBinding.Data is TtiMediatorFieldInfo) then
    Result := TtiObject(ARecordHandle).PropValue[TtiMediatorFieldInfo(DataBinding.Data).PropName]
  else
    Result := Null;
end;

function TUserDataSource.InsertRecord(ARecordHandle: TcxDataRecordHandle):
    TcxDataRecordHandle;
var
  NewObject: TtiObject;
begin
  if FValueClass <> nil then
  begin
    NewObject := FValueClass.CreateNew;
    FObjectList.Insert(FObjectList.IndexOf(TtiObject(ARecordHandle)), NewObject);
    Result := NewObject;
  end
  else
    Result := nil;

  DataChanged;
end;

procedure TUserDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
    AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  DataBinding: TcxGridItemDataBinding;
  PropName: string;
begin
  DataBinding := TcxGridItemDataBinding(AItemHandle);
  if Assigned(DataBinding.Data) and (DataBinding.Data is TtiMediatorFieldInfo) then
  begin
    PropName := TtiMediatorFieldInfo(DataBinding.Data).PropName;
    TtiObject(ARecordHandle).PropValue[PropName] := AValue;
    TtiObject(ARecordHandle).Dirty
  end;
end;

{ TticxCustomGridTableViewMediatorView }

constructor TticxCustomGridTableViewMediatorView.Create;
begin
  inherited Create;
  FFieldsInfo := TtiMediatorFieldInfoList.Create(TtiMediatorFieldInfo);
  FUserDataSource := TUserDataSource.Create;
end;

constructor TticxCustomGridTableViewMediatorView.CreateCustom(AView: TComponent;
    ASubject: TtiObject; AFieldName: string; AValueClass: TtiObjectClass);
begin
  Create;
  FieldName    := AFieldName;
  SetView(AView);
  ValueClass   := AValueClass;
  Subject      := ASubject;
end;

destructor TticxCustomGridTableViewMediatorView.Destroy;
begin
  FUserDataSource.Free;
  FFieldsInfo.Free;
  inherited Destroy;
end;

class function TticxCustomGridTableViewMediatorView.ComponentClass: TClass;
begin
  Result := TcxGridTableView;
end;

class function TticxCustomGridTableViewMediatorView.CompositeMediator: Boolean;
begin
  Result := True;
end;

procedure TticxCustomGridTableViewMediatorView.CreateColumns;
var
  APropTypeName: string;
  i: Integer;
  NewColumn: TcxGridcolumn;
  Propinfo: PPropInfo;
begin
  for i := 0 to FFieldsInfo.Count - 1 do
  begin
    if i < View.ColumnCount then
      NewColumn := View.Columns[i]
    else
      NewColumn := View.CreateColumn;

    if NewColumn.Caption = '' then
      NewColumn.Caption := FFieldsInfo[i].Caption;

    if ValueClass <> nil then
    begin
      PropInfo := GetPropInfo(ValueClass, FFieldsInfo[i].PropName, []);
      APropTypeName := PropInfo.PropType^.Name;

      if SameText(ApropTypeName, 'string') then
        SetStringColumn(NewColumn)
      else if SameText(ApropTypeName, 'integer') then
        SetIntegerColumn(NewColumn)
      else if SameText(ApropTypeName, 'TDateTime') then
        SetDateTimeColumn(NewColumn)
      else if SameText(ApropTypeName, 'Boolean') then
        SetBooleanColumn(NewColumn)
      else if SameText(ApropTypeName, 'Currency') then
        SetCurrencyColumn(NewColumn)
      else if SameText(APropTypeName, 'Double') then
        SetFloatColumn(NewColumn)
      else if SameText(APropTypeName, 'TColor') then
        SetColorColumn(NewColumn);
    end;
  end;
end;

procedure TticxCustomGridTableViewMediatorView.DoObjectToGUI;
begin
  if not View.DataController.DataChangedNotifyLocked then
    View.DataController.CustomDataSource.DataChanged;
end;

function TticxCustomGridTableViewMediatorView.GetShowDeleted: Boolean;
begin
  Result := FUserDataSource.ShowDeleted;
end;

procedure TticxCustomGridTableViewMediatorView.ParseDisplayNames(const AValue:
    string);
var
  I: Integer;
  lField: string;
  lInfo: TtiMediatorFieldInfo;
begin
  FFieldsInfo.Clear;
  for I := 1 to tiNumToken(AValue, cFieldDelimiter) do
  begin
    lField         := tiToken(AValue, cFieldDelimiter, I);
    lInfo          := FFieldsInfo.AddFieldInfo;
    lInfo.AsString := lfield;
  end; { Loop }
end;

procedure TticxCustomGridTableViewMediatorView.SetBooleanColumn(var ANewColumn:
    TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxBooleanValueType;
  if ANewColumn.PropertiesClass = nil then
    ANewColumn.PropertiesClass := TcxCheckBoxProperties;
end;

procedure TticxCustomGridTableViewMediatorView.SetColorColumn(var ANewColumn:
    TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  if ANewColumn.PropertiesClass = nil then
  begin
    ANewColumn.PropertiesClass := TcxColorComboBoxProperties;
    TcxColorComboBoxProperties(ANewColumn.Properties).ShowDescriptions := False;
    TcxColorComboBoxProperties(ANewColumn.Properties).AllowSelectColor := True;
  end;
end;

procedure TticxCustomGridTableViewMediatorView.SetCurrencyColumn(var
    ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxCurrencyValueType;
  if ANewColumn.PropertiesClass = nil then
    ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
end;

procedure TticxCustomGridTableViewMediatorView.SetDateTimeColumn(var
    ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxDateTimeValueType;
  if ANewColumn.PropertiesClass = nil then
    ANewColumn.PropertiesClass := TcxDateEditProperties;
end;

procedure TticxCustomGridTableViewMediatorView.SetFieldName(const AValue:
    string);
begin
  inherited SetFieldName(AValue);
  ParseDisplayNames(AValue);
end;

procedure TticxCustomGridTableViewMediatorView.SetFieldsInfo(Value:
    TtiMediatorFieldInfoList);
begin
  FFieldsInfo.Assign(Value);
end;

procedure TticxCustomGridTableViewMediatorView.SetFloatColumn(var ANewColumn:
    TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxFloatValueType;
  if ANewColumn.PropertiesClass = nil then
  begin
    ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
    TcxCurrencyEditProperties(ANewColumn.Properties).DisplayFormat := '';
  end;
end;

procedure TticxCustomGridTableViewMediatorView.SetIntegerColumn(var ANewColumn:
    TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  if ANewColumn.PropertiesClass = nil then
    ANewColumn.PropertiesClass := TcxSpinEditProperties;
end;

procedure TticxCustomGridTableViewMediatorView.SetShowDeleted(Value: Boolean);
begin
  FUserDataSource.ShowDeleted := Value;
end;

procedure TticxCustomGridTableViewMediatorView.SetStringColumn(var ANewColumn:
    TcxGridcolumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TticxCustomGridTableViewMediatorView.SetupGUIandObject;
var
  i: Integer;
begin
  FUserDataSource.ObjectList := Subject as TtiObjectList;
  FUserDataSource.ValueClass := ValueClass;
  View.DataController.CustomDataSource := FUserDataSource;

  CreateColumns;

  for i := 0 to View.ColumnCount - 1 do
    View.Columns[i].DataBinding.Data := FFieldsInfo[i];
end;

function TticxCustomGridTableViewMediatorView.View: TcxGridTableView;
begin
  Result := inherited View as TcxGridTableView;
end;

{ TticxTextEditMediatorView }

class function TticxTextEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxTextEdit;
end;

function TticxTextEditMediatorView.View: TcxTextEdit;
begin
  Result := inherited View as TcxTextEdit;
end;

{ TticxControlMediatorView }

constructor TticxCustomEditMediatorView.Create;
begin
  inherited;
  FViewErrorColor := clError;
end;

destructor TticxCustomEditMediatorView.Destroy;
begin
  if (View <> nil) and Assigned(THackcxCustomEdit(View).Properties.OnEditValueChanged) then
    THackcxCustomEdit(View).Properties.OnEditValueChanged := nil;
  inherited;
end;

class function TticxCustomEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomEdit;
end;

function TticxCustomEditMediatorView.GetCurrentControlColor: TColor;
begin
  Result := ColorToRGB(FViewColor);
end;

procedure TticxCustomEditMediatorView.SetObjectUpdateMoment(const AValue:
    TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
  begin
    if ObjectUpdateMoment in [ouOnchange, ouCustom] then
      THackcxCustomEdit(View).Properties.OnEditValueChanged := DoOnChange
    else
      THackcxCustomEdit(View).OnExit := DoOnChange;
  end;
end;

procedure TticxCustomEditMediatorView.SetView(const AValue: TComponent);
var
  LValue: TcxCustomEdit;
begin
  Assert((AValue = nil) or (AValue is TcxCustomEdit), 'Expected TcxCustomEdit');
  LValue := AValue as TcxCustomEdit;

  if LValue <> View then
  begin
    // Restore state of previous view
    if View <> nil then
    begin
      View.Hint := FViewHint;
      THackcxCustomEdit(View).Color := FViewColor;
    end;

    // Preserve state of new view
    if Assigned(LValue) then
    begin
      FViewHint := LValue.Hint;
      FViewColor := THackcxCustomEdit(LValue).Color;
    end;
  end;

  inherited SetView(AValue);
end;

procedure TticxCustomEditMediatorView.SetViewErrorColor(const AValue: TColor);
begin
  if AValue <> FViewErrorColor then
  begin
    FViewErrorColor := AValue;
    TestIfValid; // Update view
  end;
end;

procedure TticxCustomEditMediatorView.UpdateGUIValidStatus(pErrors:
    TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(RootFieldName);
  if oError <> nil then
  begin
    THackcxCustomEdit(View).Color := ViewErrorColor;
    View.Hint := oError.ErrorMessage;
  end
  else
  begin
    THackcxCustomEdit(View).Color := GetCurrentControlColor;
    View.Hint := FViewHint;
  end;
end;

function TticxCustomEditMediatorView.View: TcxCustomEdit;
begin
  Result := inherited View as TcxCustomEdit;
end;

{ TticxMaskEditMediatorView }

class function TticxMaskEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxMaskEdit;
end;

function TticxMaskEditMediatorView.View: TcxMaskEdit;
begin
  Result := inherited View as TcxMaskEdit;
end;

{ TticxButtonEditMediatorView }

class function TticxButtonEditMediatorView.ComponentClass: TClass;
begin
  Result := TCxButtonEdit;
end;

function TticxButtonEditMediatorView.View: TcxButtonEdit;
begin
  Result := inherited View as TcxButtonEdit;
end;

{ TticxCurrencyEditMediatorView }

constructor TticxCurrencyEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

class function TticxCurrencyEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCurrencyEdit;
end;

function TticxCurrencyEditMediatorView.View: TcxCurrencyEdit;
begin
  Result := inherited View as TcxCurrencyEdit;
end;

{ TticxHyperLinkEditMediatorView }

class function TticxHyperLinkEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxHyperLinkEdit;
end;

function TticxHyperLinkEditMediatorView.View: TcxHyperLinkEdit;
begin
  Result := inherited View as TcxHyperLinkEdit;
end;

{ TticxComboBoxMediatorView }

class function TticxComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxComboBox;
end;

function TticxComboBoxMediatorView.View: TcxComboBox;
begin
  Result := inherited View as TcxComboBox;
end;

{ TticxGridBandedTableViewMediatorView }

class function TticxGridBandedTableViewMediatorView.ComponentClass: TClass;
begin
  Result := TcxGridBandedTableView;
end;

function TticxGridBandedTableViewMediatorView.View: TcxGridBandedTableView;
begin
  Result := inherited View as TcxGridBandedTableView;
end;

{ TticxGridTableMediatorView }

class function TticxGridTableViewMediatorView.ComponentClass: TClass;
begin
  Result := TcxGridTableView;
end;

function TticxGridTableViewMediatorView.View: TcxGridTableView;
begin
  Result := inherited View as TcxGridTableView;
end;

end.

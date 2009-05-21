unit tiDeMediators;

interface

uses
  tiObject,
  tiBaseMediator,
  tiExcept,
  Classes,
  Controls,
  TypInfo,
  Graphics,
  cxTextEdit,
  cxCheckBox,
  cxDropDownEdit,
  cxLabel,
  cxTrackBar,
  cxMemo,
  cxCalendar,
  cxSpinEdit,
  cxCalc,
  cxRadioGroup,
  cxProgressBar,
  cxGridCustomView,
  cxCustomData,
  cxGridTableView,
  cxGridCustomTableView,
  cxEdit,
  cxColorComboBox;

const
  clError = clYellow;

type

  TMediatorcxTextEdit = class(TMediatorView)
  private
    FEditControl: TcxCustomTextEdit;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    EditControl: TcxCustomTextEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TMediatorcxMaskEdit = class(TMediatorcxTextEdit);

  TMediatorcxButtonEdit = class(TMediatorcxTextEdit);

  TMediatorcxCurrencyEdit = class(TMediatorcxTextEdit);

  TMediatorcxCalcEdit = class(TMediatorcxTextEdit)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
  end;

  TMediatorcxHyperLinkEdit = class(TMediatorcxTextEdit);

  TMediatorcxCheckBox = class(TMediatorView)
  private
    FEditControl: TcxCheckBox;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TcxCustomComboBox controls }
  TMediatorcxCustomComboBox = class(TMediatorView)
  private
    FEditControl: TcxCustomComboBox;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
    property    EditControl: TcxCustomComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TMediatorcxMRUEdit = class(TMediatorcxCustomComboBox)
  protected
    procedure DoObjectToGUI; override;
  end;

  { Sets ItemIndex based on integer property }
  TMediatorcxItemComboBox = class(TMediatorcxCustomComboBox)
  protected
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;

  { TComboBox observing a list and setting a Object property }
  TMediatorcxDynamicComboBox = class(TMediatorcxCustomComboBox)
  private
    FExternalOnChange: TNotifyEvent;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
    procedure   RefreshList; virtual;
  end;

  TMediatorcxColorComboBox = class(TMediatorcxCustomComboBox)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;  
  end;

  { Base class to handle TLabel controls }
  TMediatorcxLabel = class(TMediatorView)
  private
    FEditControl: TcxLabel;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxLabel read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TSpinEdit controls }
  TMediatorcxCustomSpinEdit = class(TMediatorView)
  private
    FEditControl: TcxCustomSpinEdit;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TcxCustomSpinEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TMediatorcxSpinEdit = class(TMediatorView)
  private
    FEditControl: TcxSpinEdit;
  protected
    procedure   SetupGUIandObject; override;
  public
    property    EditControl: TcxSpinEdit read FEditControl write FEditControl;
  end;

  { I would expect that the cxTimeEdit is close to the cxDateEdit, but it is closer to cxCustomSpinEdit }
  TMediatorcxTimeEdit = class(TMediatorcxCustomSpinEdit)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
  end;

  { Base class to handle TTrackBar controls }
  TMediatorcxTrackBar = class(TMediatorView)
  private
    FEditControl: TcxTrackBar;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxTrackBar read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TMemo controls }
  TMediatorcxMemo = class(TMediatorView)
  private
    FEditControl: TcxMemo;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
  public
    property    EditControl: TcxMemo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TDateTimePicker controls }
  TMediatorcxDateEdit = class(TMediatorView)
  private
    FEditControl: TcxDateEdit;
  protected
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   DoGUIToObject; override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxDateEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TcxRadioGroup controls }
  TMediatorcxRadioGroupBox = class(TMediatorView)
  private
    FEditControl: TcxRadioGroup;
  protected
    procedure   DoObjectToGUI; override;
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxRadioGroup read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { base class to handle TcxProgresBar }
  TMediatorcxProgressbar = class(TMediatorView)
  private
    FEditControl: TcxProgressBar;
    procedure ProgressBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetProgressBarBasedOnMouse(X: Integer);
  protected
    procedure   DoObjectToGUI; override;
    function    GetGuiControl: TComponent; override;
    procedure   SetGuiControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxProgressBar read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  TString = class(TObject)
  private
    FValue : String;
  public
    constructor create(AValue : String);
    property Value : string read FValue write FValue;
  end;

  TUserDataSource = class(TcxCustomDataSource)
  private
    FObjectList: TtiObjectList;
    FStoreObjectType: TtiObjectClass;
    function GetObjectList: TtiObjectList;
    procedure SetObjectList(const Value: TtiObjectList);
  protected
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;AItemHandle: TcxDataItemHandle): Variant; override;
    function GetDisplayText(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): string; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);  override;
    function AppendRecord: Pointer; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
  public
    constructor Create(AObjectList: TtiObjectList; AStoreObjectType: TtiObjectClass);
    property ObjectList : TtiObjectList read GetObjectList write SetObjectList;
  end;


  { base class to handle TcxGrid }
  TMediatorcxCustomGridView = class(TMediatorView)
  private
    FEditControl: TcxCustomGridView;
    FUserDataSource: TUserDataSource;
    FShowDeleted: Boolean;
    FStoreObjectType: TtiObjectClass;
    procedure   DoOnFilterRecord(ADataController: TcxCustomDataController; ARecordIndex: Integer; var Accept: Boolean);
    procedure SetStringColumn(ANewColumn: TcxGridcolumn);virtual;
    procedure SetIntegerColumn(ANewColumn: TcxGridColumn);virtual;
    procedure SetDateTimeColumn(ANewColumn: TcxGridColumn);virtual;
    procedure SetBooleanColumn(ANewColumn: TcxGridColumn);virtual;
    procedure SetCurrencyColumn(ANewColumn: TcxGridColumn);virtual;
    procedure SetFloatColumn(ANewColumn: TcxGridColumn);virtual;
    procedure SetColorColumn(ANewColumn: TcxGridColumn);virtual;
    procedure DoOnInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
    function GetSubject: TtiObject;
  protected
    procedure   SetupGUIandObject; override;
    function    GetGuiControl: TComponent; override;
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    class function ComponentClass: TClass; override;

    constructor CreateCustom(AGridView: TcxCustomGridView; ASubject: TtiObjectList; AStoreObjectType: TtiObjectClass); reintroduce; virtual;

    property    EditControl: TcxCustomGridView read FEditControl write FEditControl;
    property ShowDeleted: Boolean read FShowDeleted write FShowDeleted;
  end;

const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';

procedure RegisterFallBackMediators;

implementation

uses
  SysUtils, StdCtrls, tiOPFManager, Variants, tiRTTI, tiUtils,
  cxDataStorage, cxGridBandedTableView, cxCurrencyEdit;

procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TMediatorcxTextEdit, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxMaskEdit, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxCurrencyEdit, TtiObject, [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxButtonEdit, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxHyperLinkEdit, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);    
  gMediatorManager.RegisterMediator(TMediatorcxCheckBox, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TMediatorcxCustomComboBox, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorcxMRUEdit, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorcxItemComboBox, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TMediatorcxDynamicComboBox, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TMediatorcxLabel, TtiObject);
  gMediatorManager.RegisterMediator(TMediatorcxTrackBar, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorcxMemo, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorcxDateEdit, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxTimeEdit, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxSpinEdit, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorcxCalcEdit, TtiObject, [tkInteger, tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxRadioGroupBox, TtiObject, [tkInteger]);



// not fully working
  gMediatorManager.RegisterMediator(TMediatorcxProgressbar, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorcxColorComboBox, TtiObject, [tkInteger]);
end;



{ TMediatorcxTextEdit }

class function TMediatorcxTextEdit.ComponentClass: TClass;
begin
  Result := TcxCustomTextEdit;
end;

constructor TMediatorcxTextEdit.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

destructor TMediatorcxTextEdit.Destroy;
begin
  if Assigned(EditControl) and Assigned(EditControl.Properties.OnChange) then
    EditControl.Properties.OnChange := nil;
  inherited Destroy;
end;

function TMediatorcxTextEdit.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxTextEdit.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomTextEdit;
  inherited SetGuiControl(AValue);
end;

procedure TMediatorcxTextEdit.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
  begin
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FeditControl.OnExit := DoOnChange;
  end;
end;

procedure TMediatorcxTextEdit.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    FEditControl.Properties.MaxLength := Ma;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxTextEdit.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Style.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Style.Color  := clWindow;
    EditControl.Hint   := '';
  end;
end;

{ TMediatorCheckBoxView }

class function TMediatorcxCheckBox.ComponentClass: TClass;
begin
  Result := TcxCheckBox;
end;

constructor TMediatorcxCheckBox.Create;
begin
  inherited Create;
  GuiFieldName := 'Checked';
end;

function TMediatorcxCheckBox.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxCheckBox.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCheckBox;
  inherited SetGuiControl(AValue);
end;

procedure TMediatorcxCheckBox.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
  begin
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FeditControl.OnExit := DoOnChange;
  end;
end;

procedure TMediatorcxCheckBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxCheckBox.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Style.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Style.Color  := clBtnFace;
    EditControl.Hint   := '';
  end;
end;

{ TMediatorComboBoxView }

class function TMediatorcxCustomComboBox.ComponentClass: TClass;
begin
  Result := TcxCustomCombobox;
end;

constructor TMediatorcxCustomComboBox.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

procedure TMediatorcxCustomComboBox.DoObjectToGUI;
begin
  EditControl.ItemIndex := EditControl.Properties.Items.IndexOf(Subject.PropValue[FieldName]);
end;

function TMediatorcxCustomComboBox.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxCustomComboBox.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomComboBox;
  inherited SetGuiControl(AValue);
end;

procedure TMediatorcxCustomComboBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxCustomComboBox.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Style.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Style.Color  := clWindow;
    EditControl.Hint   := '';
  end;
end;

{ TMediatorcxItemComboBoxView }

constructor TMediatorcxItemComboBox.Create;
begin
  inherited;

end;

procedure TMediatorcxItemComboBox.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, EditControl.ItemIndex);
end;

procedure TMediatorcxItemComboBox.DoObjectToGUI;
begin
  EditCOntrol.ItemIndex := GetOrdProp(Subject, FieldName);
end;

{ TMediatorcxDynamicComboBoxView }

constructor TMediatorcxDynamicComboBox.Create;
begin
  inherited;
end;

procedure TMediatorcxDynamicComboBox.DoGUIToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if EditControl.ItemIndex < 0 then
    Exit; //==>

  lValue := TtiObject(ValueList.Items[EditControl.ItemIndex]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    raise EtiOPFProgrammerException.Create('Error property type not a Class');
end;

procedure TMediatorcxDynamicComboBox.DoObjectToGUI;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  EditControl.ItemIndex := -1;
  if Subject = nil then
    Exit; //==>

  if not Assigned(ValueList) then
    raise EtiOPFProgrammerException.Create(cErrorListHasNotBeenAssigned);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    raise Exception.Create('Property is not a class type!');

  for i := 0 to ValueList.Count - 1 do
    if ValueList.Items[i] = lValue then
    begin
      EditControl.ItemIndex := i;
      Break; //==>
    end;

  SetOnChangeActive(true);
end;

procedure TMediatorcxDynamicComboBox.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := EditControl.Properties.Items;
  lItems.Clear;
  EditControl.Text := '';

  if (ValueList = nil) or
     (ValueList.Count < 1) or
     (SameText(FieldName, EmptyStr)) then
    Exit; //==>

  try
    for i := 0 to ValueList.Count - 1 do
    begin
      lItems.Add(ValueList.Items[i].Caption);
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error adding list items to combobox ' +
                                 'Message: %s, Item Property Name: %s',
                                 [E.message, FieldName]);
  end;

  ObjectToGui;

end;

procedure TMediatorcxDynamicComboBox.RefreshList;
begin
  InternalListRefresh;
end;

procedure TMediatorcxDynamicComboBox.SetListObject(const AValue: TtiObjectList);
begin
  Inherited;
  InternalListRefresh;
end;

procedure TMediatorcxDynamicComboBox.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      EditControl.Properties.OnChange := FExternalOnChange
    else
      EditControl.Properties.OnChange := DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := EditControl.Properties.OnChange;
    EditControl.Properties.OnChange := nil;
  end;
end;

procedure TMediatorcxDynamicComboBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    EditControl.Properties.OnChange := DoOnChange; // default OnChange event handler

  EditControl.Enabled   := (ValueList.Count > 0);
end;

{ TMediatorStaticTextView }

class function TMediatorcxLabel.ComponentClass: TClass;
begin
  Result := TcxLabel;
end;

constructor TMediatorcxLabel.Create;
begin
  inherited Create;
  GuiFieldName := 'Caption';
end;

function TMediatorcxLabel.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxLabel.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxLabel;
end;

procedure TMediatorcxLabel.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Caption := '';
end;

{ TMediatorTrackBarView }

class function TMediatorcxTrackBar.ComponentClass: TClass;
begin
  Result := TcxTrackBar;
end;

constructor TMediatorcxTrackBar.Create;
begin
  inherited;
  GuiFieldName := 'Position';
end;

function TMediatorcxTrackBar.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxTrackBar.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxTrackBar;
end;

procedure TMediatorcxTrackBar.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
  begin
    FEditControl.Properties.Min := Mi;
    FEditControl.Properties.Max := Ma;
  end;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FeditControl.OnExit := DoOnChange;
end;

{ TMediatorMemoView }

class function TMediatorcxMemo.ComponentClass: TClass;
begin
  Result := TcxMemo;
end;

procedure TMediatorcxMemo.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TMediatorcxMemo.DoObjectToGUI;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

function TMediatorcxMemo.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxMemo.SetGuiControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TcxMemo;
end;

procedure TMediatorcxMemo.SetupGUIandObject;
begin
  inherited SetupGUIAndObject;
  EditControl.Lines.Clear;
  EditControl.Properties.ScrollBars := ssVertical;
  EditControl.Properties.WordWrap   := True;
  if Assigned(FEditControl) then
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TMediatorCalendarComboView }

class function TMediatorcxDateEdit.ComponentClass: TClass;
begin
  Result := TcxDateEdit;
end;

constructor TMediatorcxDateEdit.Create;
begin
  inherited Create;
  GUIFieldName := 'EditValue';
end;

procedure TMediatorcxDateEdit.DoGUIToObject;
begin
  If (EditControl.Properties.Kind = ckDate) Then
    Subject.PropValue[FieldName] := Trunc(EditControl.Date)
  Else
    Subject.PropValue[FieldName] := EditControl.Date;
end;

function TMediatorcxDateEdit.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxDateEdit.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxDateEdit;
  inherited;
end;

procedure TMediatorcxDateEdit.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxDateEdit.SetupGUIAndObject;
begin
  inherited SetupGUIandObject;
  if Assigned(FEditControl) then
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TMediatorcxSpinEditView }

class function TMediatorcxCustomSpinEdit.ComponentClass: TClass;
begin
  Result := TcxCustomSpinEdit;
end;

constructor TMediatorcxCustomSpinEdit.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

function TMediatorcxCustomSpinEdit.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxCustomSpinEdit.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomSpinEdit;
end;

procedure TMediatorcxCustomSpinEdit.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxCustomSpinEdit.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Style.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Style.Color  := clWindow;
    EditControl.Hint   := '';
  end;
end;

{ TMediatorcxCalcEdit }

constructor TMediatorcxCalcEdit.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

procedure TMediatorcxCalcEdit.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TMediatorcxTimeEdit }

constructor TMediatorcxTimeEdit.Create;
begin
  inherited Create;
  GuiFieldName := 'Time';
end;

procedure TMediatorcxTimeEdit.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TMediatorcxSpinEdit }

procedure TMediatorcxSpinEdit.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Value := 0;
end;

{ TMediatorcxMRUEdit }

procedure TMediatorcxMRUEdit.DoObjectToGUI;
var
  NewString: string;
begin
  // The MRUEdit must have the edit value in the MRU list, otherwise it is not shown.
  NewString := string(Subject.PropValue[Self.FieldName]);
  FEditControl.Properties.Items.Add(NewString);
  inherited;
end;

{ TMediatorcxRadioGroupBox }

class function TMediatorcxRadioGroupBox.ComponentClass: TClass;
begin
  Result := TcxRadioGroup;
end;

constructor TMediatorcxRadioGroupBox.Create;
begin
  inherited Create;
  GuiFieldName := 'ItemIndex';
end;

procedure TMediatorcxRadioGroupBox.DoObjectToGUI;
var
  iValue: integer;
begin
  iValue := GetOrdProp(Subject, FieldName);
  if iValue < EditControl.Properties.Items.Count then
    EditCOntrol.ItemIndex := iValue
  else
    EditCOntrol.ItemIndex := -1; 
end;

function TMediatorcxRadioGroupBox.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxRadioGroupBox.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxRadioGroup;
  inherited SetGuiControl(AValue);
end;

procedure TMediatorcxRadioGroupBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TMediatorcxColorComboBox }

constructor TMediatorcxColorComboBox.Create;
begin
  inherited;
  GuiFieldName := 'ColorValue';
end;

procedure TMediatorcxColorComboBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TMediatorcxProgessbar }

class function TMediatorcxProgressbar.ComponentClass: TClass;
begin
  Result := TcxProgressBar;
end;

constructor TMediatorcxProgressbar.Create;
begin
  inherited;
  GuiFieldName := 'Position';
end;

procedure TMediatorcxProgressbar.DoObjectToGUI;
begin
  inherited;
end;

function TMediatorcxProgressbar.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxProgressbar.ProgressBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetProgressBarBasedOnMouse(X);
end;

procedure TMediatorcxProgressbar.ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    SetProgressBarBasedOnMouse(X);
end;

procedure TMediatorcxProgressbar.SetGuiControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxProgressBar;
end;

procedure TMediatorcxProgressbar.SetProgressBarBasedOnMouse(X: Integer);
var
  iValue : integer;
  MyBar: TcxProgressBar;
begin
  inherited;
  MyBar := TcxProgressBar(EditControl);
  iValue := round(MyBar.Properties.Max * (X + 2)/ MyBar.Width);
  MyBar.Position := iValue;
  TypInfo.SetInt64Prop(Subject, FieldName, iValue);
  Subject.Dirty := True;
  DoOnChange(EditControl);
end;

procedure TMediatorcxProgressbar.SetupGUIandObject;
begin
  inherited;
  EditControl.OnMouseDown := ProgressbarMouseDown;
  EditControl.OnMouseMove := ProgressbarMouseMove;
end;

{ TMediatorcxCustomGridView }

class function TMediatorcxCustomGridView.ComponentClass: TClass;
begin
  Result := TcxCustomGridView; 
end;

constructor TMediatorcxCustomGridView.CreateCustom(AGridView: TcxCustomGridView; ASubject: TtiObjectList; AStoreObjectType: TtiObjectClass);
begin
  Create;
  FEditControl := AGridView;
  FStoreObjectType := AStoreObjectType;
  Subject := ASubject;
end;

procedure TMediatorcxCustomGridView.DoGUIToObject;
begin
  // Do nothing. List is essentially read-only.
end;

procedure TMediatorcxCustomGridView.DoObjectToGUI;
begin
// Refresh the list?
end;

procedure TMediatorcxCustomGridView.DoOnFilterRecord(ADataController: TcxCustomDataController; ARecordIndex: Integer; var Accept: Boolean);
begin
  if not ShowDeleted then
    Accept := not TtiObjectList(Subject)[ARecordIndex].Deleted;
end;

procedure TMediatorcxCustomGridView.DoOnInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
var
  AIndex: Integer;
  AObject: TtiObject;
  AFieldName: string;
  AGUIFieldName: string;
begin
  AIndex := Sender.IndexOfItem(AItem);
  AObject :=  TUserDataSource(Sender.DataController.CustomDataSource).ObjectList.Items[AIndex];
  AFieldName:= TString(AItem.DataBinding.Data).Value;

  if      (AEdit is TcxTextEdit) then
    TMediatorcxTextEdit.CreateCustom(AEdit, AObject, AFieldName, 'Text')
  else if (AEdit is TcxSpinEdit) then
    TMediatorcxCustomSpinEdit.CreateCustom(AEdit, AObject, AFieldName, 'Value');

end;

function TMediatorcxCustomGridView.GetGuiControl: TComponent;
begin
  Result := FEditControl;
end;

function TMediatorcxCustomGridView.GetSubject: TtiObject;
begin

end;

procedure TMediatorcxCustomGridView.SetBooleanColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxBooleanValueType;
  ANewColumn.PropertiesClass := TcxCheckBoxProperties;
end;

procedure TMediatorcxCustomGridView.SetFloatColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxFloatValueType;
  ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
  TcxCurrencyEditProperties(ANewColumn.Properties).DisplayFormat := '';
end;

procedure TMediatorcxCustomGridView.SetColorColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  ANewColumn.PropertiesClass := TcxColorComboBoxProperties;
  TcxColorComboBoxProperties(ANewColumn.Properties).ShowDescriptions := False;
  TcxColorComboBoxProperties(ANewColumn.Properties).AllowSelectColor := True;
end;

procedure TMediatorcxCustomGridView.SetCurrencyColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxCurrencyValueType;
  ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
end;

procedure TMediatorcxCustomGridView.SetDateTimeColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxDateTimeValueType;
  ANewColumn.PropertiesClass := TcxDateEditProperties;
end;

procedure TMediatorcxCustomGridView.SetIntegerColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  ANewColumn.PropertiesClass := TcxSpinEditProperties;
end;

procedure TMediatorcxCustomGridView.SetStringColumn(ANewColumn: TcxGridcolumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TMediatorcxCustomGridView.SetupGUIandObject;
var
  ATempObject: TtiObject;
  AProperties: TStringList;
  NewColumn: TcxGridcolumn;
  I: Integer;
  Propinfo: PPropInfo;
  APropTypeName: string;
begin
  FUserDataSource := TUserDataSource.Create(TtiObjectList(Subject), FStoreObjectType);
  FEditControl.BeginUpdate;
  FEDitControl.DataController.CustomDataSource := FUserDataSource;
  FEditControl.DataController.OnFilterRecord := DoOnFilterRecord;

  AProperties := TStringList.Create;
  ATempObject := FStoreObjectType.Create;
  try
    tiGetPropertyNames(FStoreObjectType, AProperties, ctkAll);
    for I := 0 to AProperties.Count - 1 do
    begin
      if (EditControl is TcxGridBandedTableView) then
        NewColumn := TcxGridBandedTableView(EditControl).CreateColumn
      else
        NewColumn := TcxGridTableView(EditControl).CreateColumn;

      NewColumn.Caption := AProperties[i];
      PropInfo := GetPropInfo(FStoreObjectType, AProperties[i],  []);
      APropTypeName := PropInfo.PropType^.Name;
      if SameText(ApropTypeName, 'string') then SetStringColumn(NewColumn);
      if SameText(ApropTypeName, 'integer') then SetIntegerColumn(NewColumn);
      if SameText(ApropTypeName, 'TDateTime') then SetDateTimeColumn(NewColumn);
      if SameText(ApropTypeName, 'Boolean') then SetBooleanColumn(NewColumn);
      if SameText(ApropTypeName, 'Currency') then SetCurrencyColumn(NewColumn);
      if SameText(APropTypeName, 'Double') then SetFloatColumn(NewColumn);
      if SameText(APropTypeName, 'TColor') then SetColorColumn(NewColumn);
      NewColumn.DataBinding.Data := TString.Create(AProperties[i]);
    end;
  finally
    ATempObject.Free;
    AProperties.Free;
  end;

  if (EditControl is TcxGridBandedTableView) then
    TcxGridBandedTableView(EditControl).OnInitEdit := DoOnInitEdit
  else
    TcxGridTableView(EditControl).OnInitEdit := DoOnInitEdit;


  FEditControl.EndUpdate;
end;

{ TUserDataSource }

function TUserDataSource.AppendRecord: Pointer;
begin
{ TODO : implement function }
  DataChanged;
end;

constructor TUserDataSource.Create(AObjectList: TtiObjectList; AStoreObjectType: TtiObjectClass);
begin
  inherited Create;
  FObjectList := AObjectList;
  FStoreObjectType := AStoreObjectType;
end;

procedure TUserDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
var
  AObject: TtiObject;
begin
  inherited;
  AObject := FObjectList.Items[PtrInt(ARecordHandle)];
  AObject.ObjectState := posDelete;
  DataChanged;
end;

function TUserDataSource.GetDisplayText(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): string;
var
  Temp: Variant;
begin
  Temp := GetValue(ARecordHandle, AItemHandle);
  if Temp = null then
    Result := ''
  else
    Result := Temp;
end;

function TUserDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result := TcxDataItemHandle(TcxCustomGridTableItem(DataController.GetItem(AItemIndex)).DataBinding);
end;

function TUserDataSource.GetObjectList: TtiObjectList;
begin
  Result := FObjectList;
end;

function TUserDataSource.GetRecordCount: Integer;
begin
//  if not (FObjectList.ObjectState in [PosPk,PosClean]) then
//    GTIOPFManager.ReadPK(FObjectList);
  Result := FObjectList.Count;
end;

function TUserDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  ADataBinding: TcxGridItemDataBinding;
  AObject: TtiObject;
  ATempObj : TtiObject;
begin
  ADataBinding := TcxGridItemDataBinding(AItemHandle);
  AObject := FObjectList.Items[PtrInt(ARecordHandle)];
//  if AObject.ObjectState in [posPK, posEmpty] then GTIOPFManager.ReadThis(AObject);
  if not assigned(ADataBinding.data) then
    Result := Null
  else if PropIsType(AObject, TString(ADataBinding.Data).Value, tkClass) then
    begin
    ATempObj := TtiObject(GetObjectProp(AObject, TString(ADataBinding.Data).Value, TtiObject));
    if assigned(ATempObj) and assigned(ATempObj.OID) then
    begin
      if ATempObj.ObjectState in [posPK, posEmpty] then GTIOPFManager.ReadThis(ATempObj);
      Result := ATempObj.Caption;
    end
    else
      Result := Null
    end
  else
  begin
    Result := AObject.PropValue[TString(ADataBinding.Data).Value];
  end;
end;

procedure TUserDataSource.SetObjectList(const Value: TtiObjectList);
begin
  FObjectList := Value;
end;

procedure TUserDataSource.SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  ADataBinding: TcxGridItemDataBinding;
  AObject: TtiObject;
begin
  ADataBinding := TcxGridItemDataBinding(AItemHandle);
  AObject := FObjectList.Items[PtrInt(ARecordHandle)];

// The following lines of code were used to set a value in the grid to an other object. Right now, there is no generic solution for this.
//  if PropIsType(AObject,TString(ADataBinding.Data).Value,tkClass) then
//  begin
//    if (tiGetPropertyClass(AObject.ClassType, TString(ADataBinding.Data).Value).InheritsFrom(TLookupItem)) then
//    begin
//      SetObjectProp(AObject, TString(ADataBinding.Data).Value, TLookupItemClass(tiGetPropertyClass(AObject.ClassType, TString(ADataBinding.Data).Value)).getLookupLijst.FindByProps(['Omschrijving'], [AValue]));
//      AObject.Dirty := true;
//    end;
//  end
//  else
//  begin
//    if (VarCompareValue(AValue, null) = vrEqual) and (TRDBObject(AObject).GetPropertyMetadata(GetPropInfo(AObject,TString(ADataBinding.Data).Value)^).FieldType in [RDBDateTime,RDBDate]) then
//      AObject.PropValue[TString(ADataBinding.Data).Value] := 0
//    else
      AObject.PropValue[TString(ADataBinding.Data).Value] := AValue;
//  end;
end;

{ TString }

constructor TString.create(AValue: String);
begin
  FValue := AValue;
end;

end.

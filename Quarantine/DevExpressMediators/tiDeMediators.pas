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

  TticxTextEditMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxCustomTextEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    EditControl: TcxCustomTextEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TticxMaskEditMediatorView = class(TticxTextEditMediatorView);

  TticxButtonEditMediatorView = class(TticxTextEditMediatorView);

  TticxCurrencyEditMediatorView = class(TticxTextEditMediatorView);

  TticxCalcEditMediatorView = class(TticxTextEditMediatorView)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
  end;

  TticxHyperLinkEditMediatorView = class(TticxTextEditMediatorView);

  TticxCheckBoxMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxCheckBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TcxCustomComboBox controls }
  TticxCustomComboBoxMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxCustomComboBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
    property    EditControl: TcxCustomComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TticxMRUEditMediatorView = class(TticxCustomComboBoxMediatorView)
  protected
    procedure DoObjectToGUI; override;
  end;

  { Sets ItemIndex based on integer property }
  TticxItemComboBoxMediatorView = class(TticxCustomComboBoxMediatorView)
  protected
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;

  { TComboBox observing a list and setting a Object property }
  TticxDynamicComboBoxMediatorView = class(TticxCustomComboBoxMediatorView)
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

  TticxColorComboBoxMediatorView = class(TticxCustomComboBoxMediatorView)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;  
  end;

  { Base class to handle TLabel controls }
  TticxLabelMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxLabel;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxLabel read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TSpinEdit controls }
  TticxCustomSpinEditMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxCustomSpinEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TcxCustomSpinEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TticxSpinEditMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxSpinEdit;
  protected
    procedure   SetupGUIandObject; override;
  public
    property    EditControl: TcxSpinEdit read FEditControl write FEditControl;
  end;

  { I would expect that the cxTimeEdit is close to the cxDateEdit, but it is closer to cxCustomSpinEdit }
  TticxTimeEditMediatorView = class(TticxCustomSpinEditMediatorView)
  protected
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
  end;

  { Base class to handle TTrackBar controls }
  TticxTrackBarMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxTrackBar;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxTrackBar read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TMemo controls }
  TticxMemoMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxMemo;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
  public
    property    EditControl: TcxMemo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TDateTimePicker controls }
  TticxDateEditMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxDateEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoGUIToObject; override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxDateEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TcxRadioGroup controls }
  TticxRadioGroupBoxMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxRadioGroup;
  protected
    procedure   DoObjectToGUI; override;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TcxRadioGroup read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { base class to handle TcxProgresBar }
  TticxProgressbarMediatorView = class(TtiMediatorView)
  private
    FEditControl: TcxProgressBar;
    procedure ProgressBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetProgressBarBasedOnMouse(X: Integer);
  protected
    procedure   DoObjectToGUI; override;
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
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
  TticxCustomGridViewMediatorView = class(TtiMediatorView)
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
    function    GetGUIControl: TComponent; override;
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
  gMediatorManager.RegisterMediator(TticxTextEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxMaskEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxCurrencyEditMediatorView, TtiObject, [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxButtonEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TticxHyperLinkEditMediatorView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);    
  gMediatorManager.RegisterMediator(TticxCheckBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TticxCustomComboBoxMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxMRUEditMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxItemComboBoxMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TticxDynamicComboBoxMediatorView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TticxLabelMediatorView, TtiObject);
  gMediatorManager.RegisterMediator(TticxTrackBarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxMemoMediatorView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TticxDateEditMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TticxTimeEditMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TticxSpinEditMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxCalcEditMediatorView, TtiObject, [tkInteger, tkFloat]);
  gMediatorManager.RegisterMediator(TticxRadioGroupBoxMediatorView, TtiObject, [tkInteger]);



// not fully working
  gMediatorManager.RegisterMediator(TticxProgressbarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TticxColorComboBoxMediatorView, TtiObject, [tkInteger]);
end;



{ TticxTextEditMediatorView }

class function TticxTextEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomTextEdit;
end;

constructor TticxTextEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Text';
end;

destructor TticxTextEditMediatorView.Destroy;
begin
  if Assigned(EditControl) and Assigned(EditControl.Properties.OnChange) then
    EditControl.Properties.OnChange := nil;
  inherited Destroy;
end;

function TticxTextEditMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxTextEditMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomTextEdit;
  inherited SetGUIControl(AValue);
end;

procedure TticxTextEditMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
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

procedure TticxTextEditMediatorView.SetupGUIandObject;
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

procedure TticxTextEditMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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

{ TtiCheckBoxMediatorView }

class function TticxCheckBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxCheckBox;
end;

constructor TticxCheckBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Checked';
end;

function TticxCheckBoxMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxCheckBoxMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCheckBox;
  inherited SetGUIControl(AValue);
end;

procedure TticxCheckBoxMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
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

procedure TticxCheckBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TticxCheckBoxMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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

{ TtiComboBoxMediatorView }

class function TticxCustomComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomCombobox;
end;

constructor TticxCustomComboBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Text';
end;

procedure TticxCustomComboBoxMediatorView.DoObjectToGUI;
begin
  EditControl.ItemIndex := EditControl.Properties.Items.IndexOf(Subject.PropValue[FieldName]);
end;

function TticxCustomComboBoxMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxCustomComboBoxMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomComboBox;
  inherited SetGUIControl(AValue);
end;

procedure TticxCustomComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TticxCustomComboBoxMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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

{ TticxItemComboBoxMediatorViewView }

constructor TticxItemComboBoxMediatorView.Create;
begin
  inherited;

end;

procedure TticxItemComboBoxMediatorView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, EditControl.ItemIndex);
end;

procedure TticxItemComboBoxMediatorView.DoObjectToGUI;
begin
  EditCOntrol.ItemIndex := GetOrdProp(Subject, FieldName);
end;

{ TticxDynamicComboBoxMediatorViewView }

constructor TticxDynamicComboBoxMediatorView.Create;
begin
  inherited;
end;

procedure TticxDynamicComboBoxMediatorView.DoGUIToObject;
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

procedure TticxDynamicComboBoxMediatorView.DoObjectToGUI;
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

procedure TticxDynamicComboBoxMediatorView.InternalListRefresh;
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

  ObjectToGUI;

end;

procedure TticxDynamicComboBoxMediatorView.RefreshList;
begin
  InternalListRefresh;
end;

procedure TticxDynamicComboBoxMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  Inherited;
  InternalListRefresh;
end;

procedure TticxDynamicComboBoxMediatorView.SetOnChangeActive(AValue: Boolean);
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

procedure TticxDynamicComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    EditControl.Properties.OnChange := DoOnChange; // default OnChange event handler

  EditControl.Enabled   := (ValueList.Count > 0);
end;

{ TtiStaticTextMediatorView }

class function TticxLabelMediatorView.ComponentClass: TClass;
begin
  Result := TcxLabel;
end;

constructor TticxLabelMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Caption';
end;

function TticxLabelMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxLabelMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxLabel;
end;

procedure TticxLabelMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Caption := '';
end;

{ TtiTrackBarMediatorView }

class function TticxTrackBarMediatorView.ComponentClass: TClass;
begin
  Result := TcxTrackBar;
end;

constructor TticxTrackBarMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Position';
end;

function TticxTrackBarMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxTrackBarMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxTrackBar;
end;

procedure TticxTrackBarMediatorView.SetupGUIandObject;
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

{ TtiMemoMediatorView }

class function TticxMemoMediatorView.ComponentClass: TClass;
begin
  Result := TcxMemo;
end;

procedure TticxMemoMediatorView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TticxMemoMediatorView.DoObjectToGUI;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

function TticxMemoMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxMemoMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TcxMemo;
end;

procedure TticxMemoMediatorView.SetupGUIandObject;
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

{ TtiCalendarComboMediatorView }

class function TticxDateEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxDateEdit;
end;

constructor TticxDateEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'EditValue';
end;

procedure TticxDateEditMediatorView.DoGUIToObject;
begin
  If (EditControl.Properties.Kind = ckDate) Then
    Subject.PropValue[FieldName] := Trunc(EditControl.Date)
  Else
    Subject.PropValue[FieldName] := EditControl.Date;
end;

function TticxDateEditMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxDateEditMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxDateEdit;
  inherited;
end;

procedure TticxDateEditMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

procedure TticxDateEditMediatorView.SetupGUIAndObject;
begin
  inherited SetupGUIandObject;
  if Assigned(FEditControl) then
    If ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.Properties.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TticxSpinEditMediatorViewView }

class function TticxCustomSpinEditMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomSpinEdit;
end;

constructor TticxCustomSpinEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

function TticxCustomSpinEditMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxCustomSpinEditMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCustomSpinEdit;
end;

procedure TticxCustomSpinEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TticxCustomSpinEditMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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

{ TticxCalcEditMediatorView }

constructor TticxCalcEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

procedure TticxCalcEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TticxTimeEditMediatorView }

constructor TticxTimeEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Time';
end;

procedure TticxTimeEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnEditValueChanged := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TticxSpinEditMediatorView }

procedure TticxSpinEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Value := 0;
end;

{ TticxMRUEditMediatorView }

procedure TticxMRUEditMediatorView.DoObjectToGUI;
var
  NewString: string;
begin
  // The MRUEdit must have the edit value in the MRU list, otherwise it is not shown.
  NewString := string(Subject.PropValue[Self.FieldName]);
  FEditControl.Properties.Items.Add(NewString);
  inherited;
end;

{ TticxRadioGroupBoxMediatorView }

class function TticxRadioGroupBoxMediatorView.ComponentClass: TClass;
begin
  Result := TcxRadioGroup;
end;

constructor TticxRadioGroupBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'ItemIndex';
end;

procedure TticxRadioGroupBoxMediatorView.DoObjectToGUI;
var
  iValue: integer;
begin
  iValue := GetOrdProp(Subject, FieldName);
  if iValue < EditControl.Properties.Items.Count then
    EditCOntrol.ItemIndex := iValue
  else
    EditCOntrol.ItemIndex := -1; 
end;

function TticxRadioGroupBoxMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxRadioGroupBoxMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxRadioGroup;
  inherited SetGUIControl(AValue);
end;

procedure TticxRadioGroupBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TticxColorComboBoxMediatorView }

constructor TticxColorComboBoxMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'ColorValue';
end;

procedure TticxColorComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

{ TtiMediatorcxProgessbar }

class function TticxProgressbarMediatorView.ComponentClass: TClass;
begin
  Result := TcxProgressBar;
end;

constructor TticxProgressbarMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Position';
end;

procedure TticxProgressbarMediatorView.DoObjectToGUI;
begin
  inherited;
end;

function TticxProgressbarMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TticxProgressbarMediatorView.ProgressBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetProgressBarBasedOnMouse(X);
end;

procedure TticxProgressbarMediatorView.ProgressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    SetProgressBarBasedOnMouse(X);
end;

procedure TticxProgressbarMediatorView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxProgressBar;
end;

procedure TticxProgressbarMediatorView.SetProgressBarBasedOnMouse(X: Integer);
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

procedure TticxProgressbarMediatorView.SetupGUIandObject;
begin
  inherited;
  EditControl.OnMouseDown := ProgressbarMouseDown;
  EditControl.OnMouseMove := ProgressbarMouseMove;
end;

{ TticxCustomGridViewMediatorView }

class function TticxCustomGridViewMediatorView.ComponentClass: TClass;
begin
  Result := TcxCustomGridView; 
end;

constructor TticxCustomGridViewMediatorView.CreateCustom(AGridView: TcxCustomGridView; ASubject: TtiObjectList; AStoreObjectType: TtiObjectClass);
begin
  Create;
  FEditControl := AGridView;
  FStoreObjectType := AStoreObjectType;
  Subject := ASubject;
end;

procedure TticxCustomGridViewMediatorView.DoGUIToObject;
begin
  // Do nothing. List is essentially read-only.
end;

procedure TticxCustomGridViewMediatorView.DoObjectToGUI;
begin
// Refresh the list?
end;

procedure TticxCustomGridViewMediatorView.DoOnFilterRecord(ADataController: TcxCustomDataController; ARecordIndex: Integer; var Accept: Boolean);
begin
  if not ShowDeleted then
    Accept := not TtiObjectList(Subject)[ARecordIndex].Deleted;
end;

procedure TticxCustomGridViewMediatorView.DoOnInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);
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
    TticxTextEditMediatorView.CreateCustom(AEdit, AObject, AFieldName, 'Text')
  else if (AEdit is TcxSpinEdit) then
    TticxCustomSpinEditMediatorView.CreateCustom(AEdit, AObject, AFieldName, 'Value');

end;

function TticxCustomGridViewMediatorView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

function TticxCustomGridViewMediatorView.GetSubject: TtiObject;
begin

end;

procedure TticxCustomGridViewMediatorView.SetBooleanColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxBooleanValueType;
  ANewColumn.PropertiesClass := TcxCheckBoxProperties;
end;

procedure TticxCustomGridViewMediatorView.SetFloatColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxFloatValueType;
  ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
  TcxCurrencyEditProperties(ANewColumn.Properties).DisplayFormat := '';
end;

procedure TticxCustomGridViewMediatorView.SetColorColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  ANewColumn.PropertiesClass := TcxColorComboBoxProperties;
  TcxColorComboBoxProperties(ANewColumn.Properties).ShowDescriptions := False;
  TcxColorComboBoxProperties(ANewColumn.Properties).AllowSelectColor := True;
end;

procedure TticxCustomGridViewMediatorView.SetCurrencyColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxCurrencyValueType;
  ANewColumn.PropertiesClass := TcxCurrencyEditProperties;
end;

procedure TticxCustomGridViewMediatorView.SetDateTimeColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxDateTimeValueType;
  ANewColumn.PropertiesClass := TcxDateEditProperties;
end;

procedure TticxCustomGridViewMediatorView.SetIntegerColumn(ANewColumn: TcxGridColumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxIntegerValueType;
  ANewColumn.PropertiesClass := TcxSpinEditProperties;
end;

procedure TticxCustomGridViewMediatorView.SetStringColumn(ANewColumn: TcxGridcolumn);
begin
  ANewColumn.DataBinding.ValueTypeClass := TcxStringValueType;
end;

procedure TticxCustomGridViewMediatorView.SetupGUIandObject;
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

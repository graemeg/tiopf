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
  cxSpinEdit;

const
  clError = clYellow;


type
  TMediatorcxTextEdit = class(TMediatorView)
  private
    FEditControl: TcxTextEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    EditControl: TcxTextEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  TMediatorcxCheckBox = class(TMediatorView)
  private
    FEditControl: TcxCheckBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TComboBox controls }
  TMediatorcxComboBox = class(TMediatorView)
  private
    FEditControl: TcxComboBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGui; override;
  public
    constructor Create; override;
    property    EditControl: TcxComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Sets ItemIndex based on integer property }
  TMediatorcxItemComboBox = class(TMediatorcxComboBox)
  protected
    Procedure   DoGUIToObject; override;
    Procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;

  { TComboBox observing a list and setting a Object property }
  TMediatorcxDynamicComboBox = class(TMediatorcxComboBox)
  private
    FExternalOnChange: TNotifyEvent;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGuiToObject; override;
    procedure   DoObjectToGui; override;
  public
    constructor Create; override;
    procedure   RefreshList; virtual;
  end;

  { Base class to handle TLabel controls }
  TMediatorcxLabel = class(TMediatorView)
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
  TMediatorcxSpinEdit = class(TMediatorView)
  private
    FEditControl: TcxSpinEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    EditControl: TcxSpinEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TTrackBar controls }
  TMediatorcxTrackBar = class(TMediatorView)
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
  TMediatorcxMemo = class(TMediatorView)
  private
    FEditControl: TcxMemo;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
  public
    property    EditControl: TcxMemo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TDateTimePicker controls }
  TMediatorcxDateEdit = class(TMediatorView)
  private
    FEditControl: TcxDateEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoGuiToObject; override;
    procedure   SetupGUIAndObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TcxDateEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;



const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';

procedure RegisterFallBackMediators;

implementation

uses
  SysUtils, StdCtrls;

procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TMediatorCxTextEdit, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxCheckBox, TtiObject, [tkInteger, tkEnumeration]); // ???  Delphi doesn't have a tkBool like FPC ???
  gMediatorManager.RegisterMediator(TMediatorcxComboBox, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorcxItemComboBox, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TMediatorcxDynamicComboBox, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TMediatorcxLabel, TtiObject);
  gMediatorManager.RegisterMediator(TMediatorcxTrackBar, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorcxMemo, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorcxDateEdit, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorcxSpinEdit, TtiObject, [tkInteger]);
end;



{ TMediatorcxTextEdit }

class function TMediatorcxTextEdit.ComponentClass: TClass;
begin
  Result := TcxTextEdit;
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

function TMediatorcxTextEdit.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxTextEdit.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxTextEdit;
  inherited SetGUIControl(AValue);
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

function TMediatorcxCheckBox.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxCheckBox.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxCheckBox;
  inherited SetGUIControl(AValue);
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

class function TMediatorcxComboBox.ComponentClass: TClass;
begin
  Result := TcxCombobox;
end;

constructor TMediatorcxComboBox.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

procedure TMediatorcxComboBox.DoObjectToGui;
begin
  EditControl.ItemIndex := EditControl.Properties.Items.IndexOf(Subject.PropValue[FieldName]);
end;

function TMediatorcxComboBox.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxComboBox.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxComboBox;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorcxComboBox.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorcxComboBox.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
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

procedure TMediatorcxDynamicComboBox.DoGuiToObject;
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

procedure TMediatorcxDynamicComboBox.DoObjectToGui;
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

function TMediatorcxLabel.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxLabel.SetGUIControl(const AValue: TComponent);
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

function TMediatorcxTrackBar.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxTrackBar.SetGUIControl(const AValue: TComponent);
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

procedure TMediatorcxMemo.DoGuiToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TMediatorcxMemo.DoObjectToGui;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

function TMediatorcxMemo.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxMemo.SetGUIControl(const AValue: TComponent);
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
  GUIFieldName := 'Date';
end;

procedure TMediatorcxDateEdit.DoGuiToObject;
begin
  If (EditControl.Properties.Kind = ckDate) Then
    Subject.PropValue[FieldName] := Trunc(EditControl.Date)
  Else
    Subject.PropValue[FieldName] := EditControl.Date;
end;

function TMediatorcxDateEdit.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxDateEdit.SetGUIControl(const AValue: TComponent);
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

class function TMediatorcxSpinEdit.ComponentClass: TClass;
begin
  Result := TcxSpinEdit;
end;

constructor TMediatorcxSpinEdit.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;

function TMediatorcxSpinEdit.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorcxSpinEdit.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TcxSpinEdit;
end;

procedure TMediatorcxSpinEdit.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  if ObjectUpdateMoment in [ouOnChange,ouCustom] then
    FEditControl.Properties.OnChange := DoOnChange
  else
    FEditControl.OnExit := DoOnChange;
  EditControl.Value := 0;
end;

procedure TMediatorcxSpinEdit.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
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

end.

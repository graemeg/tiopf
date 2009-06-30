{
  Purpose:
    Abstract mediating view and Mediator Factory. This allows you to use
    standard edit components and make them object-aware.  See the demo
    application for usage.
}

unit tiMediators;

{$I tiDefines.inc}

interface
uses
  tiObject
  ,Classes
  ,tiBaseMediator
  ,Controls
  ,StdCtrls   { TEdit, TComboBox, TStaticText }
  {$IFDEF FPC}
  ,Spin       { TSpinEdit - standard component included in Lazarus LCL }
  {$ELSE}
  ,tiSpin     { TSpinEdit - tiSpin.pas cloned from Borland's Spin.pas so remove package import warning}
  {$ENDIF}
  ,ComCtrls   { TTrackBar, TDateTimePicker }
  ;

type

  { Base class to handle TEdit controls }
  TMediatorEditView = class(TMediatorView)
  private
    FEditControl: TEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
  protected
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    EditControl: TEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TCheckBox controls }
  TMediatorCheckBoxView = class(TMediatorView)
  private
    FEditControl: TCheckBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    EditControl: TCheckBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TLabel controls }
  TMediatorStaticTextView = class(TMediatorView)
  private
    FEditControl: TLabel;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    property    EditControl: TLabel read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TSpinEdit controls }
  TMediatorSpinEditView = class(TMediatorView)
  private
    FEditControl: TSpinEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TSpinEdit read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TTrackBar controls }
  TMediatorTrackBarView = class(TMediatorView)
  private
    FEditControl: TTrackBar;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TTrackBar read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TComboBox controls }
  TMediatorComboBoxView = class(TMediatorView)
  private
    FEditControl: TComboBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent); override;
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGui; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TComboBox read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Sets ItemIndex based on integer property }
  TMediatorItemComboBoxView = class(TMediatorComboBoxView)
  protected
    Procedure   DoGUIToObject; override;
    Procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;


  { TComboBox observing a list and setting a Object property }
  TMediatorDynamicComboBoxView = class(TMediatorComboBoxView)
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
    procedure   RefreshList; virtual;
  end;


  { Base class to handle TMemo controls }
  TMediatorMemoView = class(TMediatorView)
  private
    FEditControl: TMemo;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    property    EditControl: TMemo read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TDateTimePicker controls }
  TMediatorCalendarComboView = class(TMediatorView)
  private
    FEditControl: TDateTimePicker;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoObjectToGui; override;
    procedure   DoGuiToObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TDateTimePicker read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


// Registering generic mediators which can handle most cases by default.
procedure RegisterFallBackMediators;


implementation
uses
  SysUtils
  ,TypInfo
  ,tiExcept
  ,tiGUIConstants   // for error color
  ,tiLog
  ,Graphics
  ;

const
  cErrorListHasNotBeenAssigned   = 'List has not been assigned';


procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TMediatorEditView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TMediatorCheckBoxView, TtiObject, [tkInteger, tkEnumeration]); // ???  Delphi doesn't have a tkBool like FPC ???
  gMediatorManager.RegisterMediator(TMediatorComboboxView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorItemComboBoxView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TMediatorDynamicComboBoxView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TMediatorStaticTextView, TtiObject);
  gMediatorManager.RegisterMediator(TMediatorTrackBarView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TMediatorMemoView, TtiObject, [tkString,tkLString]);
  gMediatorManager.RegisterMediator(TMediatorCalendarComboView, TtiObject, [tkFloat]);
end;

{ TMediatorEditView }

function TMediatorEditView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TEdit;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorEditView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Color  := ColorToRGB(clWindow);
    EditControl.Hint   := '';
  end;
end;

procedure TMediatorEditView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited SetupGUIandObject;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    FEditControl.MaxLength := Ma;
end;

procedure TMediatorEditView.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

constructor TMediatorEditView.Create;
begin
  inherited Create;
  GuiFieldName:='Text';
end;

destructor TMediatorEditView.Destroy;
begin
  if Assigned(EditControl) and Assigned(EditControl.OnChange) then
    EditControl.OnChange := nil;
  inherited Destroy;
end;

class function TMediatorEditView.ComponentClass: TClass;
begin
  Result := TEdit;
end;


{ TMediatorSpinEditView}
class function TMediatorSpinEditView.ComponentClass: TClass;
begin
  Result := TSpinEdit;
end;

function TMediatorSpinEditView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorSpinEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TSpinEdit;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorSpinEditView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Value := 0;
end;

procedure TMediatorSpinEditView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Color  := ColorToRGB(clWindow);
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorSpinEditView.Create;
begin
  inherited Create;
  GuiFieldName := 'Value';
end;


procedure TMediatorSpinEditView.SetObjectUpdateMoment(
  const AValue: TObjectUpdateMoment);
begin
  inherited;
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnChange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TMediatorTrackBarView}

function TMediatorTrackBarView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorTrackBarView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TTrackBar;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorTrackBarView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
  begin
    FEditControl.Min := Mi;
    FEditControl.Max := Ma;
  end;
end;

constructor TMediatorTrackBarView.Create;
begin
  inherited;
  GuiFieldName := 'Position';
end;

class function TMediatorTrackBarView.ComponentClass: TClass;
begin
  Result := TTrackBar;
end;

procedure TMediatorTrackBarView.SetObjectUpdateMoment(
  const AValue: TObjectUpdateMoment);
begin
  inherited;
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnChange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TMediatorComboBoxView }

class function TMediatorComboBoxView.ComponentClass: TClass;
begin
  Result := TComboBox;
end;

function TMediatorComboBoxView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorComboBoxView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TComboBox;
  inherited;
end;

procedure TMediatorComboBoxView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Color  := ColorToRGB(clWindow);
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorComboBoxView.Create;
begin
  inherited Create;
  GuiFieldName := 'Text';
end;

procedure TMediatorComboBoxView.DoObjectToGui;
begin
  EditControl.ItemIndex :=
      EditControl.Items.IndexOf(Subject.PropValue[FieldName]);
end;


procedure TMediatorComboBoxView.SetObjectUpdateMoment(
  const AValue: TObjectUpdateMoment);
begin
  inherited;
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnChange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

{ TMediatorMemoView }

class function TMediatorMemoView.ComponentClass: TClass;
begin
  Result := TMemo;
end;

procedure TMediatorMemoView.DoGuiToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TMediatorMemoView.DoObjectToGui;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

function TMediatorMemoView.GetGUIControl: TComponent;
begin
  Result:=FEditControl;
end;

procedure TMediatorMemoView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TMemo;
  inherited;
end;

procedure TMediatorMemoView.SetObjectUpdateMoment(
  const AValue: TObjectUpdateMoment);
begin
  inherited;
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorMemoView.SetupGUIandObject;
begin
  inherited SetupGUIAndObject;
  EditControl.Lines.Clear;
  EditControl.ScrollBars := ssVertical;
  EditControl.WordWrap   := True;
end;


{ TMediatorDynamicComboBoxView }

procedure TMediatorDynamicComboBoxView.SetListObject(const AValue: TtiObjectList);
begin
  inherited;
  InternalListRefresh;
end;

procedure TMediatorDynamicComboBoxView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := EditControl.Items;
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

procedure TMediatorDynamicComboBoxView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      EditControl.OnChange := FExternalOnChange
    else
      EditControl.OnChange := DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := EditControl.OnChange;
    EditControl.OnChange := nil;
  end;
end;

procedure TMediatorDynamicComboBoxView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    EditControl.OnChange := DoOnChange; // default OnChange event handler

  EditControl.Enabled   := (ValueList.Count > 0);
end;

procedure TMediatorDynamicComboBoxView.DoGuiToObject;
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

procedure TMediatorDynamicComboBoxView.DoObjectToGui;
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

procedure TMediatorDynamicComboBoxView.RefreshList;
begin
  InternalListRefresh;
end;


{ TMediatorCheckBoxView }

function TMediatorCheckBoxView.GetGUIControl: TComponent;
begin
  Result:=FEditControl;
end;

procedure TMediatorCheckBoxView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl:=AValue as TCheckBox;
  inherited SetGUIControl(AValue);
end;

procedure TMediatorCheckBoxView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGuiValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    EditControl.Color  := clError;
    EditControl.Hint   := oError.ErrorMessage;
  end
  else
  begin
    EditControl.Color  := ColorToRGB(clWindow);
    EditControl.Hint   := '';
  end;
end;

constructor TMediatorCheckBoxView.Create;
begin
  inherited Create;
  GuiFieldName:='Checked';
end;

class function TMediatorCheckBoxView.ComponentClass: TClass;
begin
  Result := TCheckBox;
end;


procedure TMediatorCheckBoxView.DoGuiToObject;
begin
  inherited;
  if FEditControl.Checked then
    Subject.PropValue[FieldName] := 'True'
  else
    Subject.PropValue[FieldName] := 'False';
end;

procedure TMediatorCheckBoxView.DoObjectToGui;
begin
  inherited;
  FEditControl.Checked := (Subject.PropValue[FieldName] = 'True');
end;

procedure TMediatorCheckBoxView.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    FEditControl.OnClick := DoOnChange;
end;

destructor TMediatorCheckBoxView.Destroy;
begin
  if Assigned(EditControl) and Assigned(EditControl.OnClick) then
    EditControl.OnClick := nil;
  inherited;
end;

procedure TMediatorCheckBoxView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  ObjectUpdateMoment := ouCustom;
end;

{ TMediatorStaticTextView }

procedure TMediatorStaticTextView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  EditControl.Caption := '';
end;

function TMediatorStaticTextView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorStaticTextView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TLabel;
  inherited SetGUIControl(AValue);
end;

constructor TMediatorStaticTextView.Create;
begin
  inherited Create;
  GuiFieldName := 'Caption';
end;

class function TMediatorStaticTextView.ComponentClass: TClass;
begin
  Result := TLabel;
end;


{ TMediatorCalendarComboView }

function TMediatorCalendarComboView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorCalendarComboView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TDateTimePicker;
  inherited;
end;

// Stupid bug in TDateTimePicker. It always returns the current time in
// dtkDate mode which can cause weird problems if you are not aware of it.
procedure TMediatorCalendarComboView.DoGuiToObject;
begin
  If (EditControl.Kind = dtkDate) Then
    Subject.PropValue[FieldName] := Trunc(EditControl.DateTime)
  Else
    Subject.PropValue[FieldName] := Frac(EditControl.DateTime);
end;

constructor TMediatorCalendarComboView.Create;
begin
  inherited Create;
  GUIFieldName := 'Date';
end;

class function TMediatorCalendarComboView.ComponentClass: TClass;
begin
  Result := TDateTimePicker;
end;

procedure TMediatorCalendarComboView.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      FEditControl.OnChange := DoOnChange
    else
      FEditControl.OnExit := DoOnChange;
end;

procedure TMediatorCalendarComboView.DoObjectToGui;
begin
  if (EditControl.Kind = dtkDate) Then
    EditControl.DateTime := Trunc(Subject.PropValue[FieldName])
  else
    EditControl.DateTime := Frac(Subject.PropValue[FieldName]);
end;

{ TMediatorItemComboBoxView }

procedure TMediatorItemComboBoxView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, EditControl.ItemIndex);
end;

procedure TMediatorItemComboBoxView.DoObjectToGUI;
begin
  EditCOntrol.ItemIndex := GetOrdProp(Subject, FieldName);
end;

constructor TMediatorItemComboBoxView.Create;
begin
  inherited Create;
  GuiFieldName := 'ItemIndex';
end;

end.


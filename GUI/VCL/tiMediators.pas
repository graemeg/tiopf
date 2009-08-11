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
  ,Graphics
  ,StdCtrls   { TCustomEdit, TEdit, TComboBox, TStaticText }
  {$IFDEF FPC}
  ,Spin       { TSpinEdit - standard component included in Lazarus LCL }
  {$ELSE}
  ,tiSpin     { TSpinEdit - tiSpin.pas cloned from Borland's Spin.pas so remove package import warning}
  {$ENDIF}
  ,ComCtrls   { TTrackBar, TDateTimePicker }
  ,ExtCtrls   { TLabeledEdit }
  ;

type

  { Base class to handle TCustomEdit controls (TEdit, TMemo, TLabeledEdit) }
  TMediatorCustomEditView = class(TMediatorView)
  private
    FEditControl: TCustomEdit;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
    function    GetEditControl: TCustomEdit; virtual;
    procedure   SetEditControl(const AValue: TCustomEdit); virtual;

    procedure   SetControlMaxLength(const AMaxLength: integer); virtual; abstract;
    procedure   SetControlColor(const AColor: TColor); virtual; abstract;
    procedure   SetControlHint(const AHint: string); virtual; abstract;
    procedure   GetControlOnChange(out AOnChange: TNotifyEvent); virtual; abstract;
    procedure   SetControlOnChange(const AOnChange: TNotifyEvent); virtual; abstract;
    procedure   SetControlOnExit(const AOnExit: TNotifyEvent); virtual; abstract;
  public
    destructor  Destroy; override;
    property    EditControl: TCustomEdit read GetEditControl write SetEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TEdit controls }
  TMediatorEditView = class(TMediatorCustomEditView)
  protected
    function    GetEditControl: TEdit; reintroduce; virtual;
    procedure   SetEditControl(const AValue: TEdit); reintroduce; virtual;
    procedure   SetControlMaxLength(const AMaxLength: integer); override;
    procedure   SetControlColor(const AColor: TColor); override;
    procedure   SetControlHint(const AHint: string); override;
    procedure   GetControlOnChange(out AOnChange: TNotifyEvent); override;
    procedure   SetControlOnChange(const AOnChange: TNotifyEvent); override;
    procedure   SetControlOnExit(const AOnExit: TNotifyEvent); override;
  public
    constructor Create; override;
    property    EditControl: TEdit read GetEditControl write SetEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TCheckBox controls }
  TMediatorCheckBoxView = class(TMediatorView)
  private
    FEditControl: TCheckBox;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
    procedure   SetupGUIandObject; override;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
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
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
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
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
    procedure   DoObjectToGUI; override;
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
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    procedure   RefreshList; virtual;
  end;


  { Base class to handle TMemo controls }
  TMediatorMemoView = class(TMediatorCustomEditView)
  protected
    function    GetEditControl: TMemo; reintroduce; virtual;
    procedure   SetEditControl(const AValue: TMemo); reintroduce; virtual;
    procedure   SetControlMaxLength(const AMaxLength: integer); override;
    procedure   SetControlColor(const AColor: TColor); override;
    procedure   SetControlHint(const AHint: string); override;
    procedure   GetControlOnChange(out AOnChange: TNotifyEvent); override;
    procedure   SetControlOnChange(const AOnChange: TNotifyEvent); override;
    procedure   SetControlOnExit(const AOnExit: TNotifyEvent); override;
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
  public
    property    EditControl: TMemo read GetEditControl write SetEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TDateTimePicker controls }
  TMediatorCalendarComboView = class(TMediatorView)
  private
    FEditControl: TDateTimePicker;
  protected
    function    GetGUIControl: TComponent; override;
    procedure   SetGUIControl(const AValue: TComponent);override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    EditControl: TDateTimePicker read FEditControl write FEditControl;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TLabeledEdit controls }
{$IFDEF DELPHI2006ORABOVE}
  TMediatorLabeledEditView = class(TMediatorCustomEditView)
  protected
    function    GetEditControl: TLabeledEdit; reintroduce; virtual;
    procedure   SetEditControl(const AValue: TLabeledEdit); reintroduce; virtual;
    procedure   SetControlMaxLength(const AMaxLength: integer); override;
    procedure   SetControlColor(const AColor: TColor); override;
    procedure   SetControlHint(const AHint: string); override;
    procedure   GetControlOnChange(out AOnChange: TNotifyEvent); override;
    procedure   SetControlOnChange(const AOnChange: TNotifyEvent); override;
    procedure   SetControlOnExit(const AOnExit: TNotifyEvent); override;
  public
    constructor Create; override;
    property    EditControl: TLabeledEdit read GetEditControl write SetEditControl;
    class function ComponentClass: TClass; override;
  end;
{$ENDIF}

// Registering generic mediators which can handle most cases by default.
procedure RegisterFallBackMediators;


implementation
uses
  SysUtils
  ,TypInfo
  ,tiExcept
  ,tiGUIConstants   // for error color
  ,tiLog
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
  gMediatorManager.RegisterMediator(TMediatorLabeledEditView, TtiObject, [tkString,tkLString,tkInteger,tkFloat]);
end;

{ TMediatorCustomEditView }

destructor TMediatorCustomEditView.Destroy;
var
  LOnChange: TNotifyEvent;
begin
  if Assigned(FEditControl) then
  begin
    GetControlOnChange(LOnChange);
    if Assigned(LOnChange) then
      SetControlOnChange(nil);
  end;
  inherited;
end;

class function TMediatorCustomEditView.ComponentClass: TClass;
begin
  Result := TCustomEdit;
end;

function TMediatorCustomEditView.GetEditControl: TCustomEdit;
begin
  result := FEditControl;
end;

procedure TMediatorCustomEditView.SetEditControl(const AValue: TCustomEdit);
begin
  FEditControl := AValue;
end;

function TMediatorCustomEditView.GetGUIControl: TComponent;
begin
  Result := FEditControl;
end;

procedure TMediatorCustomEditView.SetGUIControl(const AValue: TComponent);
begin
  FEditControl := AValue as TCustomEdit;
  inherited;
end;

procedure TMediatorCustomEditView.SetObjectUpdateMoment(
  const AValue: TObjectUpdateMoment);
begin
  inherited;
  if Assigned(FEditControl) then
    if ObjectUpdateMoment in [ouOnchange,ouCustom] then
      SetControlOnChange(DoOnChange)
    else
      SetControlOnExit(DoOnChange);
end;

procedure TMediatorCustomEditView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    SetControlMaxLength(Ma);
end;

procedure TMediatorCustomEditView.UpdateGUIValidStatus(
  pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

  oError := pErrors.FindByErrorProperty(FieldName);
  if oError <> nil then
  begin
    SetControlColor(clError);
    SetControlHint(oError.ErrorMessage);
  end
  else
  begin
    SetControlColor(ColorToRGB(clWindow));
    SetControlHint('');
  end;
end;

{ TMediatorEditView }

constructor TMediatorEditView.Create;
begin
  inherited;
  GUIFieldName := 'Text';
end;

class function TMediatorEditView.ComponentClass: TClass;
begin
  Result := TEdit;
end;

function TMediatorEditView.GetEditControl: TEdit;
begin
  result := (inherited GetEditControl) as TEdit;
end;

procedure TMediatorEditView.SetEditControl(const AValue: TEdit);
begin
  inherited SetEditControl(AValue);
end;

procedure TMediatorEditView.SetControlColor(const AColor: TColor);
begin
  EditControl.Color := AColor;
end;

procedure TMediatorEditView.SetControlHint(const AHint: string);
begin
  EditControl.Hint := AHint;
end;

procedure TMediatorEditView.SetControlMaxLength(const AMaxLength: integer);
begin
  EditControl.MaxLength := AMaxLength;
end;

procedure TMediatorEditView.GetControlOnChange(out AOnChange: TNotifyEvent);
begin
  AOnChange := EditControl.OnChange;
end;

procedure TMediatorEditView.SetControlOnChange(const AOnChange: TNotifyEvent);
begin
  EditControl.OnChange := AOnChange;
end;

procedure TMediatorEditView.SetControlOnExit(const AOnExit: TNotifyEvent);
begin
  EditControl.OnExit := AOnExit;
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

procedure TMediatorSpinEditView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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
  GUIFieldName := 'Value';
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
  GUIFieldName := 'Position';
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

procedure TMediatorComboBoxView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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
  GUIFieldName := 'Text';
end;

procedure TMediatorComboBoxView.DoObjectToGUI;
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

function TMediatorMemoView.GetEditControl: TMemo;
begin
  result := (inherited GetEditControl) as TMemo;
end;

procedure TMediatorMemoView.SetEditControl(const AValue: TMemo);
begin
  inherited SetEditControl(AValue);
end;

procedure TMediatorMemoView.SetupGUIandObject;
begin
  inherited SetupGUIAndObject;
  EditControl.Lines.Clear;
  EditControl.ScrollBars := ssVertical;
  EditControl.WordWrap   := True;
end;

procedure TMediatorMemoView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := EditControl.Lines.Text;
end;

procedure TMediatorMemoView.DoObjectToGUI;
begin
  EditControl.Lines.Text := Subject.PropValue[FieldName];
end;

procedure TMediatorMemoView.SetControlColor(const AColor: TColor);
begin
  EditControl.Color := AColor;
end;

procedure TMediatorMemoView.SetControlHint(const AHint: string);
begin
  EditControl.Hint := AHint;
end;

procedure TMediatorMemoView.SetControlMaxLength(const AMaxLength: integer);
begin
  EditControl.MaxLength := AMaxLength;
end;

procedure TMediatorMemoView.GetControlOnChange(out AOnChange: TNotifyEvent);
begin
  AOnChange := EditControl.OnChange;
end;

procedure TMediatorMemoView.SetControlOnChange(const AOnChange: TNotifyEvent);
begin
  EditControl.OnChange := AOnChange;
end;

procedure TMediatorMemoView.SetControlOnExit(const AOnExit: TNotifyEvent);
begin
  EditControl.OnExit := AOnExit;
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

  ObjectToGUI;
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

procedure TMediatorDynamicComboBoxView.DoGUIToObject;
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

procedure TMediatorDynamicComboBoxView.DoObjectToGUI;
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

procedure TMediatorCheckBoxView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

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
  GUIFieldName:='Checked';
end;

class function TMediatorCheckBoxView.ComponentClass: TClass;
begin
  Result := TCheckBox;
end;

procedure TMediatorCheckBoxView.DoGUIToObject;
begin
  inherited;
  if FEditControl.Checked then
    Subject.PropValue[FieldName] := 'True'
  else
    Subject.PropValue[FieldName] := 'False';
end;

procedure TMediatorCheckBoxView.DoObjectToGUI;
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
  GUIFieldName := 'Caption';
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
procedure TMediatorCalendarComboView.DoGUIToObject;
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

procedure TMediatorCalendarComboView.DoObjectToGUI;
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
  GUIFieldName := 'ItemIndex';
end;

{ TMediatorLabeledEditView }

{$IFDEF DELPHI2006ORABOVE}
constructor TMediatorLabeledEditView.Create;
begin
  inherited;
  GUIFieldName := 'Text';
end;

class function TMediatorLabeledEditView.ComponentClass: TClass;
begin
  Result := TLabeledEdit;
end;

function TMediatorLabeledEditView.GetEditControl: TLabeledEdit;
begin
  result := (inherited GetEditControl) as TLabeledEdit;
end;

procedure TMediatorLabeledEditView.SetEditControl(const AValue: TLabeledEdit);
begin
  inherited SetEditControl(AValue);
end;

procedure TMediatorLabeledEditView.SetControlColor(const AColor: TColor);
begin
  EditControl.Color := AColor;
end;

procedure TMediatorLabeledEditView.SetControlHint(const AHint: string);
begin
  EditControl.Hint := AHint;
end;

procedure TMediatorLabeledEditView.SetControlMaxLength(
  const AMaxLength: integer);
begin
  EditControl.MaxLength := AMaxLength;
end;

procedure TMediatorLabeledEditView.GetControlOnChange(
  out AOnChange: TNotifyEvent);
begin
  AOnChange := EditControl.OnChange;
end;

procedure TMediatorLabeledEditView.SetControlOnChange(
  const AOnChange: TNotifyEvent);
begin
  EditControl.OnChange := AOnChange;
end;

procedure TMediatorLabeledEditView.SetControlOnExit(
  const AOnExit: TNotifyEvent);
begin
  EditControl.OnExit := AOnExit;
end;
{$ENDIF}

end.


{
  Purpose:
    Abstract mediating view and Mediator Factory. This allows you to use
    standard edit components and make them object-aware.  See the demo
    application for usage.
}

unit tiMediators;

{$mode objfpc}{$H+}

interface
uses
  tiObject
  ,Classes
  ,tiBaseMediator
  ,Controls
  ,StdCtrls   { TCustomEdit, TEdit, TComboBox, TStaticText }
  ,ExtCtrls   { TLabeledEdit }
  ,ComCtrls   { TTrackBar }
  ,Spin       { TSpinEdit - standard component included in Lazarus LCL }
  ,EditBtn
  ,Graphics
  ;

type

  { Base class to handle TControl controls }
  TtiControlMediatorView = class(TtiMediatorView)
  private
    FViewErrorVisible: boolean;
    FViewColor: TColor;
    FViewHint: string;
    FViewErrorColor: TColor;
    procedure   SetViewErrorVisible(const AValue: boolean);
    procedure   SetViewErrorColor(const AValue: TColor);
    procedure   SetViewState(const AColor: TColor; const AHint: string);
  protected
    function    GetCurrentControlColor: TColor; virtual;
    procedure   UpdateGUIValidStatus(pErrors: TtiObjectErrors); override;
  public
    constructor Create; override;
    property    ViewErrorVisible: boolean read FViewErrorVisible write SetViewErrorVisible;
    property    ViewErrorColor: TColor read FViewErrorColor write SetViewErrorColor;
    procedure   SetView(const AValue: TComponent); override;
    function    View: TControl; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TCustomEdit controls (TEdit, TMemo, TLabeledEdit) }
  TtiCustomEditMediatorView = class(TtiControlMediatorView)
  private
    FControlReadOnlyColor: TColor;
    procedure   SetControlReadOnlyColor(const AValue: TColor);
  protected
    function    GetCurrentControlColor: TColor; override;
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    property    ControlReadOnlyColor: TColor read FControlReadOnlyColor write SetControlReadOnlyColor;
    function    View: TCustomEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TEdit controls }
  TtiEditMediatorView = class(TtiCustomEditMediatorView)
  public
    function    View: TEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TCheckBox controls }
  TtiCheckBoxMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    View: TCheckBox; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TLabel controls }
  TtiStaticTextMediatorView = class(TtiControlMediatorView)
  private
    FFormatString: string;
    procedure   SetFormatString(const AValue: string);
  protected
    procedure   SetupGUIandObject; override;
    procedure   GetObjectPropValue(var AValue: Variant); override;
  public
    constructor Create; override;
    function    View: TLabel; reintroduce;
    class function ComponentClass: TClass; override;
    property FormatString: string read FFormatString write SetFormatString;
  end;


  { Base class to handle TSpinEdit controls }
  TtiSpinEditMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TSpinEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TTrackBar controls }
  TtiTrackBarMediatorView = class(TtiControlMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TTrackBar; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TComboBox controls }
  TtiComboBoxMediatorView = class(TtiControlMediatorView)
  protected
    procedure   DoObjectToGUI; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TComboBox; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Sets ItemIndex based on integer property }
  TtiComboBoxItemMediatorView = class(TtiComboBoxMediatorView)
  protected
    Procedure   DoGUIToObject; override;
    Procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;


  { TComboBox observing a list and setting a Object property }
  TtiDynamicComboBoxMediatorView = class(TtiComboBoxMediatorView)
  private
    FDisplayFieldName: String;
    FExternalOnChange: TNotifyEvent;
    function    GetDisplayFieldName: String;
    procedure   InternalListRefresh;
  protected
    procedure   SetListObject(const AValue: TtiObjectList); override;
    procedure   SetOnChangeActive(AValue: Boolean); virtual;
    procedure   SetupGUIandObject; override;
    procedure   DoGUIToObject; override;
    procedure   DoObjectToGUI; override;
  public
    procedure   RefreshList; virtual;
    Property    DisplayFieldName : String Read GetDisplayFieldName Write FDisplayFieldName;
  end;


  { Base class to handle TMemo controls }
  TtiMemoMediatorView = class(TtiCustomEditMediatorView)
  protected
    procedure   SetupGUIandObject; override;
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
  public
    function    View: TMemo; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TDateEdit controls }
  TtiDateEditMediatorView = class(TtiControlMediatorView)
  protected
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    constructor Create; override;
    function    View: TDateEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TLabeledEdit controls }
  TtiLabeledEditMediatorView = class(TtiCustomEditMediatorView)
  public
    function    View: TLabeledEdit; reintroduce;
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
  ,tiRTTI
  ,tiUtils
  ;

type
  // Friend class to get access to protected methods
  THackControl = class(TControl);
  THackCustomEdit = class(TCustomEdit);

const
  cErrorPropertyNotClass         = 'Property is not a class type!';
  cErrorAddingItemToCombobox     = 'Error adding list items to combobox ' +
                                   'Message: %s, Item Property Name: %s';

procedure RegisterFallBackMediators;
begin
  gMediatorManager.RegisterMediator(TtiEditMediatorView, TtiObject, ctkMultiCharString + [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TtiCheckBoxMediatorView, TtiObject, [tkBool]);
  gMediatorManager.RegisterMediator(TtiComboBoxMediatorView, TtiObject, ctkMultiCharString);
  gMediatorManager.RegisterMediator(TtiComboBoxItemMediatorView, TtiObject, [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TtiDynamicComboBoxMediatorView, TtiObject, [tkClass]);
  gMediatorManager.RegisterMediator(TtiStaticTextMediatorView, TtiObject);
  gMediatorManager.RegisterMediator(TtiTrackBarMediatorView, TtiObject, [tkInteger]);
  gMediatorManager.RegisterMediator(TtiMemoMediatorView, TtiObject, ctkMultiCharString);
  gMediatorManager.RegisterMediator(TtiSpinEditMediatorView, TtiObject, [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TtiDateEditMediatorView, TtiObject, [tkFloat]);
  gMediatorManager.RegisterMediator(TtiLabeledEditMediatorView, TtiObject, ctkMultiCharString + [tkInteger,tkFloat]);
end;

{ TtiControlMediatorView }

constructor TtiControlMediatorView.Create;
begin
  inherited;
  FViewErrorVisible := true;
  FViewErrorColor := clError;
end;

class function TtiControlMediatorView.ComponentClass: TClass;
begin
  Result := TControl;
end;

function TtiControlMediatorView.View: TControl;
begin
  result := TControl(inherited View);
end;

procedure TtiControlMediatorView.SetView(const AValue: TComponent);
var
  LValue: TControl;
begin
  Assert((AValue = nil) or (AValue is TControl), 'Expected TControl');
  LValue := AValue as TControl;

  if LValue <> View then
  begin
    // Restore state of previous view
    if View <> nil then
      SetViewState(FViewColor, FViewHint);

    // Preserve state of new view
    if Assigned(LValue) then
    begin
      FViewHint := LValue.Hint;
      FViewColor := THackControl(LValue).Color;
    end;
  end;

  inherited SetView(AValue);
end;

procedure TtiControlMediatorView.SetViewErrorColor(const AValue: TColor);
begin
  if AValue <> FViewErrorColor then
  begin
    FViewErrorColor := AValue;
    if ViewErrorVisible then
      TestIfValid; // Update view
  end;
end;

procedure TtiControlMediatorView.SetViewErrorVisible(const AValue: boolean);
begin
  if AValue <> FViewErrorVisible then
  begin
    FViewErrorVisible := AValue;
    if FViewErrorVisible then
      TestIfValid // Update view
    else
      SetViewState(GetCurrentControlColor, FViewHint);
  end;
end;

procedure TtiControlMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
var
  oError: TtiObjectError;
begin
  inherited UpdateGUIValidStatus(pErrors);

  if ViewErrorVisible then
  begin
    oError := pErrors.FindByErrorProperty(RootFieldName);
    if oError <> nil then
      SetViewState(ViewErrorColor, oError.ErrorMessage)
    else
      SetViewState(GetCurrentControlColor, FViewHint);
  end;
end;

function TtiControlMediatorView.GetCurrentControlColor: TColor;
begin
  result := ColorToRGB(FViewColor);
end;

procedure TtiControlMediatorView.SetViewState(const AColor: TColor;
  const AHint: string);
begin
  if View <> nil then
  begin
    THackControl(View).Color := AColor;
    View.Hint := AHint;
  end;
end;

{ TtiCustomEditMediatorView }

constructor TtiCustomEditMediatorView.Create;
begin
  inherited Create;
  FControlReadOnlyColor := clWindow;
  GUIFieldName := 'Text';
end;

class function TtiCustomEditMediatorView.ComponentClass: TClass;
begin
  Result := TCustomEdit;
end;

function TtiCustomEditMediatorView.GetCurrentControlColor: TColor;
begin
  if THackCustomEdit(View).ReadOnly then
    result := ColorToRGB(ControlReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

function TtiCustomEditMediatorView.View: TCustomEdit;
begin
  result := TCustomEdit(inherited View);
end;

procedure TtiCustomEditMediatorView.SetControlReadOnlyColor(
  const AValue: TColor);
begin
  if AValue <> FControlReadOnlyColor then
  begin
    FControlReadOnlyColor := AValue;
    TestIfValid; // Update view
  end;
end;

procedure TtiCustomEditMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: THackCustomEdit(View).OnChange := @DoOnChange;
      ouOnExit: THackCustomEdit(View).OnExit := @DoOnChange;
      ouNone:
      begin
        THackCustomEdit(View).OnChange := nil;
        THackCustomEdit(View).OnExit := nil;
      end;
    end;
end;

procedure TtiCustomEditMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
    THackCustomEdit(View).MaxLength := Ma;
end;

{ TtiEditMediatorView }

class function TtiEditMediatorView.ComponentClass: TClass;
begin
  Result := TEdit;
end;

function TtiEditMediatorView.View: TEdit;
begin
  result := TEdit(inherited View);
end;

{ TtiSpinEditMediatorView}

class function TtiSpinEditMediatorView.ComponentClass: TClass;
begin
  Result := TSpinEdit;
end;

function TtiSpinEditMediatorView.View: TSpinEdit;
begin
  Result := TSpinEdit(inherited View);
end;

procedure TtiSpinEditMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Value := 0;
end;

constructor TtiSpinEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Value';
end;

procedure TtiSpinEditMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := @DoOnChange;
      ouOnExit: View.OnExit := @DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

{ TtiTrackBarMediatorView}

function TtiTrackBarMediatorView.View: TTrackBar;
begin
  Result := TTrackBar(inherited View);
end;

procedure TtiTrackBarMediatorView.SetupGUIandObject;
var
  Mi, Ma: Integer;
begin
  inherited;
  if Subject.GetFieldBounds(FieldName,Mi,Ma) and (Ma>0) then
  begin
    View.Min := Mi;
    View.Max := Ma;
  end;
end;

constructor TtiTrackBarMediatorView.Create;
begin
  inherited;
  GUIFieldName := 'Position';
end;

class function TtiTrackBarMediatorView.ComponentClass: TClass;
begin
  Result := TTrackBar;
end;

procedure TtiTrackBarMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := @DoOnChange;
      ouOnExit: View.OnExit := @DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

{ TtiComboBoxMediatorView }

class function TtiComboBoxMediatorView.ComponentClass: TClass;
begin
  Result := TComboBox;
end;

function TtiComboBoxMediatorView.View: TComboBox;
begin
  Result := TComboBox(inherited View);
end;

constructor TtiComboBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Text';
end;

procedure TtiComboBoxMediatorView.DoObjectToGUI;
begin
  View.ItemIndex :=
      View.Items.IndexOf(tiVariantAsStringDef(Subject.PropValue[FieldName]));
end;

procedure TtiComboBoxMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := @DoOnChange;
      ouOnExit: View.OnExit := @DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

{ TtiMemoMediatorView }

class function TtiMemoMediatorView.ComponentClass: TClass;
begin
  Result := TMemo;
end;

function TtiMemoMediatorView.View: TMemo;
begin
  result := TMemo(inherited View);
end;

procedure TtiMemoMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIAndObject;
  View.Lines.Clear;
  View.ScrollBars := ssVertical;
  View.WordWrap   := True;
end;

procedure TtiMemoMediatorView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := View.Lines.Text;
end;

procedure TtiMemoMediatorView.DoObjectToGUI;
begin
  View.Lines.Text := tiVariantAsStringDef(Subject.PropValue[FieldName]);
end;


{ TtiDynamicComboBoxMediatorView }

procedure TtiDynamicComboBoxMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  inherited;
  InternalListRefresh;
  if Assigned(ValueList) then
    View.Enabled := ValueList.Count > 0;
end;

procedure TtiDynamicComboBoxMediatorView.InternalListRefresh;
var
  lItems: TStrings;
  i: Integer;
begin
  lItems := View.Items;
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

  ObjectToGUI;
end;

function TtiDynamicComboBoxMediatorView.GetDisplayFieldName: String;
begin
  Result:=FDisplayFieldName;
  If (Result='') then
    Result:='Caption'; // Do not localize.
end;

procedure TtiDynamicComboBoxMediatorView.SetOnChangeActive(AValue: Boolean);
begin
  if AValue then
  begin
    if not UseInternalOnChange then
      View.OnChange := FExternalOnChange
    else
      View.OnChange := @DoOnChange;
  end
  else
  begin
    if not UseInternalOnChange then
      FExternalOnChange := View.OnChange;
    View.OnChange := nil;
  end;
end;

procedure TtiDynamicComboBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;

  if UseInternalOnChange then
    View.OnChange := @DoOnChange; // default OnChange event handler

  {$Note As far as I can see, ValueList is always going to be nil here! - Graeme }
  if ValueList <> nil then
    View.Enabled   := (ValueList.Count > 0);
end;

procedure TtiDynamicComboBoxMediatorView.DoGUIToObject;
var
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if View.ItemIndex < 0 then
    Exit; //==>

  // Can't use ValueList.Items[] when View has sorted = true, then the indices
  // for ValueList and View.List are different for the same object
  lValue := TtiObject(View.Items.Objects[View.ItemIndex]);

  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    typinfo.SetObjectProp(Subject, FieldName, lValue)
  else
    RaiseMediatorError(cErrorPropertyNotClass);
end;

procedure TtiDynamicComboBoxMediatorView.DoObjectToGUI;
var
  i: Integer;
  lValue: TtiObject;
  lPropType: TTypeKind;
begin
  SetOnChangeActive(false);

  //  Set the index only (We're assuming the item is present in the list)
  View.ItemIndex := -1;
  if (Subject = nil) or (not Assigned(ValueList)) then
    Exit; //==>

  lValue := nil;
  lPropType := typinfo.PropType(Subject, FieldName);
  if lPropType = tkClass then
    lValue := TtiObject(typinfo.GetObjectProp(Subject, FieldName))
  else
    RaiseMediatorError(cErrorPropertyNotClass);

  // Can't use ValueList.Items[] when View has sorted = true, then the indices
  // for ValueList and View.List are different for the same object
  i := View.Items.count;
  repeat
     dec (i)
  until (i < 0) or (View.Items.Objects[i] = lValue);
  View.ItemIndex := i;

  SetOnChangeActive(true);
end;

procedure TtiDynamicComboBoxMediatorView.RefreshList;
begin
  InternalListRefresh;
end;


{ TtiCheckBoxMediatorView }

function TtiCheckBoxMediatorView.View: TCheckBox;
begin
  result := TCheckBox(inherited View);
end;

constructor TtiCheckBoxMediatorView.Create;
begin
  inherited Create;
  GUIFieldName:='Checked';
end;

class function TtiCheckBoxMediatorView.ComponentClass: TClass;
begin
  Result := TCheckBox;
end;

procedure TtiCheckBoxMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnClick := @DoOnChange;
      ouOnExit: View.OnExit := @DoOnChange;
      ouNone:
      begin
        View.OnClick := nil;
        View.OnExit := nil;
      end;
    end;
end;

destructor TtiCheckBoxMediatorView.Destroy;
begin
  if (View <> nil) and Assigned(View.OnClick) then
    View.OnClick := nil;
  inherited;
end;

procedure TtiCheckBoxMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  ObjectUpdateMoment := ouCustom;
end;

{ TtiStaticTextMediatorView }

procedure TtiStaticTextMediatorView.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Caption := '';
end;

function TtiStaticTextMediatorView.View: TLabel;
begin
  result := TLabel(inherited View);
end;

constructor TtiStaticTextMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Caption';
end;

class function TtiStaticTextMediatorView.ComponentClass: TClass;
begin
  Result := TLabel;
end;

procedure TtiStaticTextMediatorView.SetFormatString(const AValue: string);
begin
  if FFormatString <> AValue then
  begin
    FFormatString := AValue;
    ObjectToGUI;
  end;
end;

procedure TtiStaticTextMediatorView.GetObjectPropValue(var AValue: Variant);
begin
  if FFormatString <> '' then
    AValue := Format(FFormatString, [AValue]);
end;

{ TtiDateEditMediatorView }

function TtiDateEditMediatorView.View: TDateEdit;
begin
  Result := TDateEdit(inherited View);
end;

procedure TtiDateEditMediatorView.DoObjectToGUI;
begin
  View.Date := Subject.PropValue[FieldName];
end;

procedure TtiDateEditMediatorView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := View.Date;
end;

procedure TtiDateEditMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  inherited SetObjectUpdateMoment(AValue);
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := @DoOnChange;
      ouOnExit: View.OnExit := @DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

constructor TtiDateEditMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'Date';
end;

class function TtiDateEditMediatorView.ComponentClass: TClass;
begin
  Result := TDateEdit;
end;

{ TtiComboBoxItemMediatorView }

procedure TtiComboBoxItemMediatorView.DoGUIToObject;
begin
  SetOrdProp(Subject, FieldName, View.ItemIndex);
end;

procedure TtiComboBoxItemMediatorView.DoObjectToGUI;
begin
  View.ItemIndex := GetOrdProp(Subject, FieldName);
end;

constructor TtiComboBoxItemMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'ItemIndex';
end;

{ TtiLabeledEditMediatorView }

class function TtiLabeledEditMediatorView.ComponentClass: TClass;
begin
  Result := TLabeledEdit;
end;

function TtiLabeledEditMediatorView.View: TLabeledEdit;
begin
  result := TLabeledEdit(inherited View);
end;

end.

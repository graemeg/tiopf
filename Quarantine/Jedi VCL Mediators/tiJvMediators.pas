unit tiJvMediators;

interface

uses
  Graphics,
  tiObject,
  tiBaseMediator,
  tiMediators,
  JvToolEdit,
  JvComboBox,
  JvCheckBox,
  JvEdit,
  JvSpin,
  JvMemo,
  JvDatePickerEdit,
  JvComCtrls;




type
  { Base class to handle TJvEdit controls }
  TtiJvEditMediatorView = class(TtiEditMediatorView)
  public
    function    View: TJvEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TJvComboEdit controls }
  TtiJvComboEditMediatorView = class(TtiEditMediatorView)
  public
    function    View: TJvComboEdit; reintroduce;
    class function ComponentClass: TClass; override;
  end;

  { Base class to handle TJvTimeEdit controls }
  TtiJvTimeEditMediatorView = class(TtiControlMediatorView)
  protected
    procedure   DoObjectToGUI; override;
    procedure   DoGUIToObject; override;
    function    GetCurrentControlColor: TColor; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    class function ComponentClass: TClass; override;

    constructor Create; override;

    function View: TJvTimeEdit; reintroduce;
  end;

  { Base class to handle TJvDateEdit controls }
  TtiJvDateEditMediatorView = class(TtiControlMediatorView)
  protected
    procedure   ClearGUI; override;
    procedure   DoObjectToGUI; override;
    function    GetCurrentControlColor: TColor; override;
    procedure   SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); override;
  public
    class function ComponentClass: TClass; override;

    constructor Create; override;

    function View: TJvDateEdit; reintroduce;
  end;

  { Base class to handle TJVDatePickerEdit controls }
  TtiJvDatePickerEditMediatorView = class(TtiJvDateEditMediatorView)
  protected
    procedure   DoObjectToGUI; override;
  public
    class function ComponentClass: TClass; override;

    function View: TJvDatePickerEdit; reintroduce;
  end;

  { Base class to handle TJvCheckBox controls }
  TtiJvCheckBoxMediatorView = class(TtiCheckBoxMediatorView)
  public
    class function ComponentClass: TClass; override;

    function    View: TJvCheckBox; reintroduce;
  end;

  { Base class to handle TJvSpinEdit controls }
  TtiJvSpinEditMediatorView = class(TtiSpinEditMediatorView)
  protected
    function    GetCurrentControlColor: TColor; override;
  public
    class function ComponentClass: TClass; override;

    function    View: TJvSpinEdit; reintroduce;
  end;

  { Base class to handle TJVComboBox controls }
  TtiJvComboBoxMediatorView = class(TtiComboBoxMediatorView)
  protected
    function GetCurrentControlColor: TColor; override;
  public
    class function ComponentClass: TClass; override;

    function View: TJvComboBox; reintroduce;
  end;

  { Sets ItemIndex based on integer property }
  TtiJvComboBoxItemMediatorView = class(TtiJvComboBoxMediatorView)
  protected
    Procedure   DoGUIToObject; override;
    Procedure   DoObjectToGUI; override;
  public
    constructor Create; override;
  end;

  { TJvComboBox observing a list and setting a Object property }
  TtiJvDynamicComboBoxMediatorView = class(TtiDynamicComboBoxMediatorView)
  protected
    function GetCurrentControlColor: TColor; override;
  public
    class function ComponentClass: TClass; override;

    function View: TJvComboBox; reintroduce;
  end;

  { Base class to handle TJvMemo controls }
  TtiJvMemoMediatorView = class(TtiMemoMediatorView)
  public
    class function ComponentClass: TClass; override;

    function    View: TJvMemo; reintroduce;
  end;

  { Base class to handle TJvTrackBar controls }
  TtiJvTrackBarMediatorView = class(TtiTrackBarMediatorView)
  public
    class function ComponentClass: TClass; override;

    function    View: TJvTrackBar; reintroduce;
  end;



  procedure RegisterJvMediators;





implementation

uses
  Variants,
  TypInfo,
  tiRTTI,
  tiUtils;




procedure RegisterJvMediators;
begin
  gMediatorManager.RegisterMediator(TtiJvEditMediatorView, TtiObject,
    ctkMultiCharString + [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TtiJvComboEditMediatorView, TtiObject,
    ctkMultiCharString + [tkInteger,tkFloat]);
  gMediatorManager.RegisterMediator(TtiJvDateEditMediatorView, TtiObject,
    [tkFloat]);
  gMediatorManager.RegisterMediator(TtiJvDatePickerEditMediatorView, TtiObject,
    [tkFloat]);
  gMediatorManager.RegisterMediator(TtiJvTimeEditMediatorView, TtiObject,
    [tkFloat]);
  gMediatorManager.RegisterMediator(TtiJvCheckBoxMediatorView, TtiObject,
    [tkInteger, tkEnumeration]); // Delphi doesn't have a tkBool like FPC.
  gMediatorManager.RegisterMediator(TtiJvComboBoxMediatorView, TtiObject,
    ctkMultiCharString);
  gMediatorManager.RegisterMediator(TtiJvComboBoxItemMediatorView, TtiObject,
    [tkInteger, tkEnumeration]);
  gMediatorManager.RegisterMediator(TtiJvComboBoxMediatorView, TtiObject,
    ctkMultiCharString);
  gMediatorManager.RegisterMediator(TtiJvDynamicComboBoxMediatorView, TtiObject,
    [tkClass]);
  gMediatorManager.RegisterMediator(TtiJvMemoMediatorView, TtiObject,
    ctkMultiCharString);
  gMediatorManager.RegisterMediator(TtiJvSpinEditMediatorView, TtiObject,
    [tkInteger,tkFloat]);

//  gMediatorManager.RegisterMediator(TtiStaticTextMediatorView, TtiObject);
  gMediatorManager.RegisterMediator(TtiJvTrackBarMediatorView, TtiObject,
    [tkInteger]);
end;




{ TtiJvDateEditMediatorView }

procedure TtiJvDateEditMediatorView.ClearGUI;
begin
  inherited;

  if View <> nil then
    View.Clear;
end;

class function TtiJvDateEditMediatorView.ComponentClass: TClass;
begin
  result := TJvDateEdit;
end;

constructor TtiJvDateEditMediatorView.Create;
begin
  inherited;

  GUIFieldName := 'Date';
end;

procedure TtiJvDateEditMediatorView.DoObjectToGUI;
var
  LValue: Variant;
begin
  CheckFieldNames;
  LValue := Subject.PropValue[FieldName];

  if LValue <> Null then
    View.Date := Trunc(LValue)
  else
    View.Clear;
end;

function TtiJvDateEditMediatorView.GetCurrentControlColor: TColor;
begin
  if View.ReadOnly then
    result := ColorToRGB(ViewReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

procedure TtiJvDateEditMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := DoOnChange;
      ouOnExit: View.OnExit := DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

function TtiJvDateEditMediatorView.View: TJvDateEdit;
begin
  result := TJvDateEdit(inherited View);
end;

{ TtiJvComboBoxMediatorView }

class function TtiJvComboBoxMediatorView.ComponentClass: TClass;
begin
  result := TJvComboBox;
end;

function TtiJvComboBoxMediatorView.GetCurrentControlColor: TColor;
begin
  if View.ReadOnly then
    result := ColorToRGB(ViewReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

function TtiJvComboBoxMediatorView.View: TJvComboBox;
begin
  result := TJvComboBox(inherited View);
end;

{ TtiJvDynamicComboBoxMediatorView }

class function TtiJvDynamicComboBoxMediatorView.ComponentClass: TClass;
begin
  result := TJvComboBox;
end;

function TtiJvDynamicComboBoxMediatorView.GetCurrentControlColor: TColor;
begin
  if View.ReadOnly then
    result := ColorToRGB(ViewReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

function TtiJvDynamicComboBoxMediatorView.View: TJvComboBox;
begin
  result := TJvComboBox(inherited View);
end;

{ TtiJvCheckBoxMediatorView }

class function TtiJvCheckBoxMediatorView.ComponentClass: TClass;
begin
  result := TJvCheckBox;
end;

function TtiJvCheckBoxMediatorView.View: TJvCheckBox;
begin
  result := TJvCheckBox(inherited View);
end;

{ TtiJvMemoMediatorView }

class function TtiJvMemoMediatorView.ComponentClass: TClass;
begin
  result := TJvMemo;
end;

function TtiJvMemoMediatorView.View: TJvMemo;
begin
  result := TJvMemo(inherited View);
end;

{ TtiJvEditMediatorView }

class function TtiJvEditMediatorView.ComponentClass: TClass;
begin
  result := TJvEdit;
end;

function TtiJvEditMediatorView.View: TJvEdit;
begin
  result := TJvEdit(inherited View);
end;

{ TtiJvSpinEditMediatorView }

class function TtiJvSpinEditMediatorView.ComponentClass: TClass;
begin
  result := TJvSpinEdit;
end;

function TtiJvSpinEditMediatorView.GetCurrentControlColor: TColor;
begin
  if View.ReadOnly then
    result := ColorToRGB(ViewReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

function TtiJvSpinEditMediatorView.View: TJvSpinEdit;
begin
  result := TJvSpinEdit(inherited View);
end;

{ TtiJvTrackBarMediatorView }

class function TtiJvTrackBarMediatorView.ComponentClass: TClass;
begin
  result := TJvTrackBar;
end;

function TtiJvTrackBarMediatorView.View: TJvTrackBar;
begin
  result := TJvTrackBar(inherited View);
end;

{ TtiJvDatePickerMediatorView }

class function TtiJvDatePickerEditMediatorView.ComponentClass: TClass;
begin
  result := TJvDatePickerEdit;
end;

procedure TtiJvDatePickerEditMediatorView.DoObjectToGUI;
var
  LValue: Variant;
begin
  CheckFieldNames;
  LValue := Subject.PropValue[FieldName];

  if LValue <> Null then
    View.Date := Trunc(LValue)
  else
    View.Clear;
end;

function TtiJvDatePickerEditMediatorView.View: TJvDatePickerEdit;
begin
  result := TJvDatePickerEdit(inherited View);
end;

{ TtiJvTimeEditMediatorView }

class function TtiJvTimeEditMediatorView.ComponentClass: TClass;
begin
  result := TJvTimeEdit;
end;

constructor TtiJvTimeEditMediatorView.Create;
begin
  inherited;

  GUIFieldName := 'Time';
end;

procedure TtiJvTimeEditMediatorView.DoGUIToObject;
begin
  Subject.PropValue[FieldName] := Frac(View.Time);
end;

procedure TtiJvTimeEditMediatorView.DoObjectToGUI;
begin
  View.Time := Frac(Subject.PropValue[FieldName]);
end;

function TtiJvTimeEditMediatorView.GetCurrentControlColor: TColor;
begin
  if View.ReadOnly then
    result := ColorToRGB(ViewReadOnlyColor)
  else
    result := inherited GetCurrentControlColor;
end;

procedure TtiJvTimeEditMediatorView.SetObjectUpdateMoment(
  const AValue: TtiObjectUpdateMoment);
begin
  inherited;
  if View <> nil then
    case ObjectUpdateMoment of
      ouOnChange, ouCustom, ouDefault: View.OnChange := DoOnChange;
      ouOnExit: View.OnExit := DoOnChange;
      ouNone:
      begin
        View.OnChange := nil;
        View.OnExit := nil;
      end;
    end;
end;

function TtiJvTimeEditMediatorView.View: TJvTimeEdit;
begin
  result := TJvTimeEdit(inherited View);
end;

{ TtiJvComboEditMediatorView }

class function TtiJvComboEditMediatorView.ComponentClass: TClass;
begin
  result := TJvComboEdit;
end;

function TtiJvComboEditMediatorView.View: TJvComboEdit;
begin
  result := TJvComboEdit(inherited View);
end;

{ TtiJvComboBoxItemMediatorView }

constructor TtiJvComboBoxItemMediatorView.Create;
begin
  inherited Create;
  GUIFieldName := 'ItemIndex';
end;

procedure TtiJvComboBoxItemMediatorView.DoGUIToObject;
begin
//  SetOrdProp(Subject, FieldName, View.ItemIndex);
//  tiSetProperty(Subject, FieldName, View.ItemIndex);
  if tiGetSimplePropType(Subject, FieldName) = tiTKInteger then
    tiSetProperty(Subject, FieldName, View.ItemIndex);
end;

procedure TtiJvComboBoxItemMediatorView.DoObjectToGUI;
begin
//  View.ItemIndex := GetOrdProp(Subject, FieldName);
  if tiGetSimplePropType(Subject, FieldName) = tiTKInteger then
  try
    View.ItemIndex := tiGetProperty(Subject, FieldName, false);
  except
    View.ItemIndex := -1;
  end;
end;

end.

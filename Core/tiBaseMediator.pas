{
  This is a GUI independent unit.
  It forms the basis for the GUI Mediators (MGM) implementation.
}

unit tiBaseMediator;

{$I tiDefines.inc}

interface

uses
  TypInfo
  ,Classes
  ,SysUtils
  ,Contnrs
  ,tiObject
  ;

type
  // forward declaration
  TtiMediatorView = class;
  TtiCustomListMediatorView = class;

  TtiObjectToGUIEvent = procedure(Sender: TtiMediatorView; Src: TtiObject; Dest: TComponent; var Handled: Boolean) of object;
  TtiBeforeGUIToObjectEvent = procedure(Sender: TtiMediatorView; Src: TComponent; Dest: TtiObject; var Handled: Boolean) of object;
  TtiMediatorEvent = procedure(AMediatorView: TtiMediatorView) of object;
  TtiComponentNotificationEvent = procedure(AComponent: TComponent; Operation: TOperation) of object;

  TtiObjectUpdateMoment = (ouNone, ouDefault, ouOnChange, ouOnExit, ouCustom);

  TtiMediatorViewComponentHelper = class;

  { Base class to inherit from to make more customised Mediator Views. }
  TtiMediatorView = class(TtiObject)
  private
    FActive: Boolean;
    FObjectUpdateMoment: TtiObjectUpdateMoment;
    FListObject: TtiObjectList;
    FOnBeforeGUIToObject: TtiBeforeGUIToObjectEvent;
    FOnObjectToGUI: TtiObjectToGUIEvent;
    FSettingUp: Boolean;
    FFieldName: string; // Published property of model used to get/set value
    FRootFieldName: string; // The underlying property used by FieldName (usually the same)
    FSubject: TtiObject;
    FView: TComponent;
    FGUIFieldName: string;
    FViewHelper: TtiMediatorViewComponentHelper;
    FCopyingCount: integer;
    procedure ViewNotification(AComponent: TComponent; Operation: TOperation);
  protected
    UseInternalOnChange: Boolean;
    procedure CheckFieldNames;
    // Check model valid and allow view to change behavior
    procedure TestIfValid;
    // If GUI and Object and fieldnames are assigned, calls SetupGUIandObject.
    procedure CheckSetupGUIandObject;
    // Set up GUI and Object. Does nothing by default
    procedure SetupGUIandObject; virtual;
    // For use by descendents. Will call GUIChanged
    procedure DoOnChange(Sender: TObject); virtual;
    // Returns FSubject by default.
    function GetSubject: TtiObject; virtual;
    // Do something with errors.
    procedure UpdateGUIValidStatus(pErrors: TtiObjectErrors); virtual;
    // Check whether data and GUI property are OK. Not used in this class
    function DataAndPropertyValid: Boolean;
    // By default, copies published FieldName to published GUIFieldName.
    procedure DoGUIToObject; virtual;
    // Copy object property to GUI. By default it copies published GUIFieldName to published FieldName
    procedure DoObjectToGUI; virtual;
    // Allow descendants to change or format the displayed value
    procedure GetObjectPropValue(var AValue: Variant); virtual;
    // Set value list object. Override to provide additional handling.
    procedure SetListObject(const AValue: TtiObjectList); virtual;
    // Set up subject, attach as observer. Override to provide additional handling.
    procedure SetSubject(const AValue: TtiObject); virtual;
    // Set active - attaches/Detaches observer. Override to provide additional handling.
    procedure SetActive(const AValue: Boolean); virtual;
    // Set fieldname. Override to provide additional handling;
    procedure SetFieldName(const AValue: string); virtual;
    // Set ObjectUpdateMoment. Override to provide additional handling;
    procedure SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment); virtual;
    // Raise an error which shows more information about the control, subject and fieldname.
    procedure RaiseMediatorError(const Msg: string); overload;
    // Format version
    procedure RaiseMediatorError(const Fmt: string; Args: array of const); overload;
    // Returns nil by default
    function GetSelectedObject: TtiObject; virtual;
    // Does nothing by default
    procedure SetSelectedObject(const AValue: TtiObject); virtual;
  public
    constructor Create; override;
    constructor CreateCustom(AView: TComponent; ASubject: TtiObject; AFieldName: string; AGUIFieldName: string); overload; virtual;
    destructor Destroy; override;
    // By default, copying GUI <-> Object is one way. If this method returns true, then
    // it will copy till no more change events are generated. By default, false is returned
    class function AllowRecursiveCopy: Boolean; virtual;
    // Must return a minimum GUI class for which this mediator is valid.
    class function ComponentClass: TClass; virtual;
    // Must return TRUE if the class is a composite mediator.
    // In that case, the fieldnames property will not be checked.
    // By default, it returns False.
    class function CompositeMediator: Boolean; virtual;
    // Copy GUI to Object. Calls OnGUIToObject if set, and then calls DoGUIToObject if needed
    procedure GUIToObject;
    // Copy GUI to Object. Calls OnObjectToGUI if set, and then calls DoGUIToObject if needed
    procedure ObjectToGUI(const AForceUpdate: boolean = false);
    // Called by NotifyObservers of subject. Calls ObjectToGUI by default.
    procedure Update(ASubject: TtiObject; AOperation: TNotifyOperation); override;
    // Call when GUI changed. Will call GUIToObject.
    procedure GUIChanged;
    // Set view. Override if additional handling required.
    procedure SetView(const AValue: TComponent); virtual;
    // Returns FView by default. Reintroduce to cast into descendant type.
    function View: TComponent;
    // The subject of observation...
    property Subject: TtiObject read GetSubject write SetSubject;
    // Descendents that need a list of values can use this.
    property ValueList: TtiObjectList read FListObject write SetListObject;
    {: Get/set the selected object. List mediator views use the selected item
       in the list if appropriate. Get/Set subject by default. }
    property SelectedObject: TtiObject read GetSelectedObject write SetSelectedObject;
  published
    // Property of subject.
    property FieldName: string read FFieldName write SetFieldName;
    // Root property of subject. Normally the same as FieldName but could be
    // different. e.g. MyProperty: TSomeType and MyPropertyAsGUIString: string.
    // MyPropertyAsGUIString is the FieldName used for mapping to the view
    // but MyProperty is the root property that is read/modified through
    // MyPropertyAsGUIString. Defaults to FieldName.
    property RootFieldName: string read FRootFieldName write FRootFieldName;
    // Property of GUI to handle.
    property GUIFieldName: string read FGUIFieldName write FGUIFieldName;
    // Property ObjectUpdateMoment : Do action e.g. in OnExit instead of OnChange.
    // Up to the descendent class to decide this.
    property ObjectUpdateMoment: TtiObjectUpdateMoment read FObjectUpdateMoment write SetObjectUpdateMoment default ouDefault;
    property OnBeforeGUIToObject: TtiBeforeGUIToObjectEvent read FOnBeforeGUIToObject write FOnBeforeGUIToObject;
    property OnObjectToGUI: TtiObjectToGUIEvent read FOnObjectToGUI write FOnObjectToGUI;
    // Observing or not ?
    property Active: Boolean read FActive write SetActive;
  end;


  TtiMediatorViewClass = class of TtiMediatorView;


  TtiMediatorViewComponentHelper = class(TComponent)
  private
    FOnNotification: TtiComponentNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const AOnNotification: TtiComponentNotificationEvent); reintroduce; virtual;
    destructor Destroy; override;
  end;


  TtiSubjectClass = class of TtiObject;


  TtiMediatorFieldInfo = class(TCollectionItem)
  private
    FWidth: integer;
    FCaption: string;
    FPropName: string;
    FAlign: TAlignment;
    FOrigStyle: Boolean;
    function GetCaption: string;
  protected
    function GetAsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    // Setting this will parse everything.
    property AsString: string read GetAsString write SetAsString;
  published
    property Caption: string read GetCaption write FCaption;
    property PropName: string read FPropName write FPropName;
    property FieldWidth: integer read FWidth write FWidth;
    property Alignment: TAlignment read FAlign write FAlign default taLeftJustify;
  end;


  TtiMediatorFieldInfoList = class(TCollection)
  private
    FMediator : TtiCustomListMediatorView;
    function GetAsString: string;
    function GetI(Index: integer): TtiMediatorFieldInfo;
    procedure SetI(Index: integer; const AValue: TtiMediatorFieldInfo);
  protected
    procedure Notify(Item: TCollectionItem;Action: TCollectionNotification); override;
    Property Mediator : TtiCustomListMediatorView read FMediator;
  public
    function FieldInfoByName(Const pName : String) : TtiMediatorFieldInfo;
    function AddFieldInfo: TtiMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName : String; AFieldWidth : Integer) : TtiMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName,ACaption : String; AFieldWidth : Integer) : TtiMediatorFieldInfo; overload;
    function AddFieldInfo (Const APropName,ACaption : String; AFieldWidth : Integer; AAlignment : TAlignment) : TtiMediatorFieldInfo; overload;
    property FieldInfo[Index: integer]: TtiMediatorFieldInfo read GetI write SetI; default;
    property AsString: string read GetAsString;
  end;

  { Event object used for OnBeforeSetupField event. Is used to allow formatting
    of fields before written to listview Caption or Items. }
  TtiOnBeforeSetupField = procedure(AObject: TtiObject; const AFieldName: string; var AValue: string) of object;


  TtiListItemMediator = class(TtiObject)
  private
    FModel: TtiObject;
    FListMediator: TtiCustomListMediatorView;
    FActive: Boolean;
    FOnBeforeSetupField: TtiOnBeforeSetupField;
    function GetDisplayNames: string;
    procedure SetActive(const AValue: Boolean);
  protected
    FFieldsInfo: TtiMediatorFieldInfoList;
    procedure SetModel(const AValue: TtiObject); virtual;
    procedure StopObserving(ASubject: TtiObject); override;
  public
    destructor Destroy; override;
    procedure Update(ASubject: TtiObject); override;
    property ListMediator: TtiCustomListMediatorView read FListMediator write FListMediator;
    property OnBeforeSetupField: TtiOnBeforeSetupField read FOnBeforeSetupField write FOnBeforeSetupField;
    property DisplayNames: string read GetDisplayNames;
    property FieldsInfo: TtiMediatorFieldInfoList read FFieldsInfo;
  published
    property Model: TtiObject read FModel write SetModel;
    property Active: Boolean read FActive write SetActive;
  end;


  { Custom mediator that handles lists of objects. }
  TtiCustomListMediatorView = class(TtiMediatorView)
  private
    FOnBeforeSetupField: TtiOnBeforeSetupField;
    FShowDeleted: Boolean;
    FMediatorList: TObjectList;
    FListChanged : Boolean;
    function GetDisplayNames: string;
    function GetIsObserving: Boolean;
    function GetModel: TtiObjectList;
    procedure SetDisplayNames(const AValue: string);
    procedure SetFieldsInfo(const AValue: TtiMediatorFieldInfoList);
    procedure SetIsObserving(const AValue: Boolean);
    procedure SetShowDeleted(const AValue: Boolean);
    procedure SetOnBeforeSetupField(const Value: TtiOnBeforeSetupField);
  protected
    FFieldsInfo: TtiMediatorFieldInfoList;
    procedure FieldInfoChanged(Item: TtiMediatorFieldInfo;Action: TCollectionNotification); virtual;
    procedure CreateColumns; virtual; abstract;
    procedure ClearList; virtual; abstract;
    function DoCreateItemMediator(AData: TtiObject; ARowIdx: integer): TtiListItemMediator; virtual; abstract;
    procedure DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator); virtual;
    procedure ParseDisplayNames(const AValue: string);
    procedure CreateSubMediators; virtual;
    procedure RebuildList; virtual; abstract;
    function DataAndPropertyValid(const AData: TtiObject): Boolean;
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
    procedure SetSubject(const AValue: TtiObject); override;
    procedure SetFieldName(const AValue: string); override;
    procedure SetActive(const AValue: Boolean); override;
    Function FindObjectMediator(AObject : TTiObject; out AtIndex : Integer) : TtiListItemMediator;
    property MediatorList: TObjectList read FMediatorList;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function CompositeMediator: Boolean; override;
    procedure Update(ASubject: TtiObject; AOperation : TNotifyOperation); override;
    procedure HandleSelectionChanged; virtual; // Called from the GUI to trigger events
    procedure ItemDeleted(const ASubject: TtiObject); virtual;
  published
    property OnBeforeSetupField: TtiOnBeforeSetupField read FOnBeforeSetupField write SetOnBeforeSetupField;
    property Model: TtiObjectList read GetModel;
    property DisplayNames: string read GetDisplayNames write SetDisplayNames;
    property ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
    // For backwards compatibility.
    property IsObserving: Boolean read GetIsObserving write SetIsObserving;
    property FieldsInfo: TtiMediatorFieldInfoList read FFieldsInfo write SetFieldsInfo;
  end;


  TtiMediatorDef = class(TCollectionItem)
  private
    FMC: TtiMediatorViewClass;
    FMSC: TtiSubjectClass;
    FPN: string;
    FPT: TTypeKinds;
  public
    // Return True if this definition handles the Subject,GUI,APropinfo trio
    function Handles(ASubjectClass: TClass; AGUI: TComponent; APropInfo: PPropInfo): Boolean; overload;
    function Handles(ASubject: TtiObject; AGUI: TComponent; APropInfo: PPropInfo): Boolean; overload;
    // Return True if this definition matches 'closer' than M.
    // Note that both current and M must have Handles() returned true for this to be useful.
    function BetterMatch(M: TtiMediatorDef): Boolean;
    // Definition
    property MediatorClass: TtiMediatorViewClass read FMC write FMC;
    property MinSubjectClass: TtiSubjectClass read FMSC write FMSC;
    property PropertyTypes: TTypeKinds read FPT write FPT;
    property PropertyName: string read FPN write FPN;
  end;


  TtiMediatorDefs = class(TCollection)
  private
    function GetDef(Index: integer): TtiMediatorDef;
    procedure SetDef(Index: integer; const AValue: TtiMediatorDef);
  public
    function AddDef: TtiMediatorDef;
    property Defs[Index: integer]: TtiMediatorDef read GetDef write SetDef; default;
  end;


  TtiMediatorManager = class(TObject)
  private
    FDefs: TtiMediatorDefs;
  public
    constructor Create;
    destructor Destroy; override;
    // If APropName is empty or APropInfo is Nil, a composite mediator will be searched.
    function FindDefFor(ASubject: TtiObject; AGUI: TComponent): TtiMediatorDef; overload;
    function FindDefFor(ASubject: TtiObject; AGUI: TComponent; APropName: string): TtiMediatorDef; overload;
    function FindDefFor(ASubjectClass: TClass; AGUI: TComponent; APropInfo: PPropInfo): TtiMediatorDef; overload;
    function FindDefFor(ASubject: TtiObject; AGUI: TComponent; APropInfo: PPropInfo): TtiMediatorDef; overload;
    function RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass): TtiMediatorDef; overload;
    function RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass; PropertyName: string): TtiMediatorDef; overload;
    function RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass; PropertyTypes: TTypeKinds): TtiMediatorDef; overload;
    property Defs: TtiMediatorDefs read FDefs;
  end;

  EtiMediator = class(Exception);

function gMediatorManager: TtiMediatorManager;
function tiFieldName(const AField: string): string;
function tiFieldWidth(const AField: string): integer;
function tiFieldCaption(const AField: string): string;
function tiFieldAlignment(const AField: string): TAlignment;
function tiValueFieldName(const AFieldName: string): string;
function tiRootFieldName(const AFieldName: string): string;
procedure MediatorError(Sender: TObject; const Msg: string); overload;
procedure MediatorError(Sender: TObject; Fmt: string; Args: array of const); overload;

implementation

uses
  tiUtils
  ,tiLog
  ,tiRTTI
  ;

var
  uMediatorManager: TtiMediatorManager;

resourcestring
  sErrInvalidFieldName      = 'No fieldname specified for column %d';
  sErrInvalidAlignmentChar  = 'Invalid alignment character "%s" specified for column %d';
  sErrInvalidWidthSpecifier = 'Invalid with "%s" specified for column %d';
  sErrNotListObject         = '%s is not a TtiObjectList';
  sErrCompositeNeedsList    = '%s needs a TtiObjectList class but is registered with %s';
  SErrActive                = 'Operation not allowed while the mediator is active';
  SErrNoGUIFieldName        = 'no gui fieldname set';
  SErrNoSubjectFieldName    = 'no subject fieldname set';
  SErrInvalidPropertyName   = '<%s> is not a property of <%s>';

const
  DefFieldWidth = 75;   // default width
  cFieldDelimiter = ';';
  cBrackets = '()';
  cRootFieldNameDelimiter = ':';
  
procedure MediatorError(Sender: TObject; const Msg: string); overload;
var
  M : TtiMediatorView;
  V : TComponent;
  S : TTiObject;
  CN,SN,Err : String;
begin
  if (Sender=Nil) then
    Err:=Msg
  else If Sender is TtiMediatorView then
    begin
    M:=Sender as TtiMediatorView;
    V:=M.View;
    S:=M.Subject;
    If Assigned(V) then
      begin
      CN:=V.Name;
      If (CN='') then
        CN:=V.ClassName+' instance';
      end
    else
      CN:='Nil';
    If Assigned(S) then
      SN:=S.ClassName
    else
      SN:='Nil';
    Err:=Format('Mediator %s (%s,%s,%s) : %s',[M.ClassName,SN,CN,M.FieldName,Msg]);
    end
  else if (Sender is TComponent) and (TComponent(Sender).Name<>'') then
    Err:=Format('%s : %s',[TComponent(Sender).Name,Msg])
  else
    Err:=Format('%s : %s',[Sender.ClassName,Msg]);
  Raise EtiMediator.Create(Err);
end;

procedure MediatorError(Sender: TObject; Fmt: string; Args: array of const); overload;
begin
  MediatorError(Sender, Format(Fmt, Args));
end;


function gMediatorManager: TtiMediatorManager;
begin
  if (uMediatorManager = nil) then
    uMediatorManager := TtiMediatorManager.Create;
  Result := uMediatorManager;
end;


{ TtiMediatorView }

constructor TtiMediatorView.Create;
begin
  inherited;
  FViewHelper := TtiMediatorViewComponentHelper.Create(ViewNotification);
  UseInternalOnChange := True;
  FActive := True;
  FObjectUpdateMoment := ouDefault;
  FSettingUp := False;
end;

constructor TtiMediatorView.CreateCustom(AView: TComponent; ASubject: TtiObject; AFieldName: string; AGUIFieldName: string);
begin
  Create;
  FSettingUp   := True;
  FieldName    := tiValueFieldName(AFieldName);
  RootFieldName := tiRootFieldName(AFieldName);
  GUIFieldName := AGUIFieldName;
  Subject      := ASubject;
  SetView(AView); // At this point, SetupGUIAndObject is called
  FSettingUp   := False;
end;

destructor TtiMediatorView.Destroy;
begin
  if Assigned(FView) then
    FView.RemoveFreeNotification(FViewHelper);
  if Assigned(FListObject) then
    FListObject.DetachObserver(self);
//  Active := false;
  Subject := nil; // Will call DetachObserver
  FreeAndNil(FViewHelper);
  inherited Destroy;
end;

class function TtiMediatorView.AllowRecursiveCopy: Boolean;
begin
  Result := False;
end;

class function TtiMediatorView.ComponentClass: TClass;
begin
  Result := TComponent;
end;

class function TtiMediatorView.CompositeMediator: Boolean;
begin
  Result := False;
end;

procedure TtiMediatorView.GUIChanged;
begin
  if not FSettingUp then
  begin
    GUIToObject;
    TestIfValid;
  end;
end;

procedure TtiMediatorView.UpdateGUIValidStatus(pErrors: TtiObjectErrors);
begin
  // do nothing
end;

function TtiMediatorView.DataAndPropertyValid: Boolean;
begin
  Result := (FSubject <> nil) and ((not CompositeMediator) or (FFieldName <> ''));
  if not Result then
    Exit; //==>

  if not CompositeMediator then
  begin
    Result := (IsPublishedProp(FSubject, FFieldName));
    if not Result then
      RaiseMediatorError('<%s> is not a property of <%s>',
          [FFieldName, FSubject.ClassName]);
  end;

  //  View.ReadOnly := ReadOnly or IsPropReadOnly;
end;

procedure TtiMediatorView.DoOnChange(Sender: TObject);
begin
  GUIChanged;
end;

procedure TtiMediatorView.TestIfValid;
var
  Errors: TtiObjectErrors;
begin
  Errors := TtiObjectErrors.Create;
  try
    Subject.IsValid(Errors);
    UpdateGUIValidStatus(Errors); // always execute this as it also resets View
  finally
    Errors.Free;
  end;
end;

procedure TtiMediatorView.CheckSetupGUIandObject;
begin
  if Assigned(Subject) and Assigned(FView) then
    SetupGUIandObject;
end;

function TtiMediatorView.View: TComponent;
begin
  result := FView;
end;

procedure TtiMediatorView.SetView(const AValue: TComponent);
begin
  if Assigned(FView) then
  begin
    FView.RemoveFreeNotification(FViewHelper);
    SetObjectUpdateMoment(ouNone);
  end;
  FView := AValue;
  if Assigned(FView) then
    FView.FreeNotification(FViewHelper);

  CheckSetupGUIAndObject;

  if Assigned(FView) then
    SetObjectUpdateMoment(FObjectUpdateMoment);
end;

procedure TtiMediatorView.SetupGUIandObject;
begin
  // Does nothing by default
end;

procedure TtiMediatorView.ViewNotification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FView) and (AComponent = FView) and
     (Operation = opRemove) then
    FView := nil;
end;

procedure TtiMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  if FListObject = AValue then
    Exit;
  if Assigned(FListObject) then
    FListObject.DetachObserver(self);
  FListObject := AValue;
  if Assigned(FListObject) then
    FListObject.AttachObserver(self);
end;

procedure TtiMediatorView.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  { 2009-06-30 graemeg - Commented this check because it prevents descendant
    implementations from firing which is required if EditComponent changes,
    but FObjectUpdateMoment stayed the same. }
  //if FObjectUpdateMoment = AValue then
  //  Exit;
  FObjectUpdateMoment := AValue;
end;

procedure TtiMediatorView.RaiseMediatorError(const Msg: string);
begin
  MediatorError(Self,Msg);
end;

procedure TtiMediatorView.RaiseMediatorError(const Fmt: string; Args: array of const);
begin
  RaiseMediatorError(Format(FMT,Args));
end;

procedure TtiMediatorView.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TtiMediatorView.SetSelectedObject(const AValue: TtiObject);
begin
  // Do nothing by default. Can be overridden in descendant.
end;

procedure TtiMediatorView.SetSubject(const AValue: TtiObject);
begin
  if (FSubject = AValue) then
    Exit;
  if Assigned(FSubject) then
    FSubject.DetachObserver(Self);
  FSubject := AValue;
  if Assigned(FSubject) and FActive then
    FSubject.AttachObserver(self);
  CheckSetupGUIAndObject;
  if (not FSettingUp) and Assigned(FSubject) and FActive then
    Update(FSubject,noChanged);
end;

procedure TtiMediatorView.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
begin
  inherited Update(ASubject, AOperation);
  { We can be observing FSubject and ValueList, so make sure we handle the
    correct one. }
  if FSubject = ASubject then
  begin
    if (AOperation=noChanged) and Active then
    begin
      ObjectToGUI;
      TestIfValid;
    end
    else if (AOperation=noFree) and (ASubject=FSubject) then
      FSubject:=Nil;
  end;
end;

function TtiMediatorView.GetSelectedObject: TtiObject;
begin
  // nil by default. Can be overridden in descendant.
  result := nil;
end;

function TtiMediatorView.GetSubject: TtiObject;
begin
  Result := FSubject;
end;

procedure TtiMediatorView.CheckFieldNames;
begin
  if (GUIFieldName = '') then
    RaiseMediatorError(SErrNoGUIFieldName);
  if (FieldName = '') then
    RaiseMediatorError(SErrNoSubjectFieldName)
end;

procedure TtiMediatorView.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;

  if Assigned(FListObject) then
  begin
    if Active then
    begin
      FListObject.AttachObserver(self);
      FListObject.NotifyObservers;
    end
    else
      FListObject.DetachObserver(self);
  end;

  if Assigned(FSubject) then
  begin
    if Active then
    begin
      FSubject.AttachObserver(Self);
      FSubject.NotifyObservers;
    end
    else
      FSubject.DetachObserver(Self);
  end;

  if Assigned(FView) then
  begin
    if Active then
      SetObjectUpdateMoment(FObjectUpdateMoment)
    else
      SetObjectUpdateMoment(ouNone);
  end;
end;

procedure TtiMediatorView.DoGUIToObject;
begin
  CheckFieldNames;
  Subject.PropValue[FieldName] := TypInfo.GetPropValue(View, GUIFieldName);
end;

procedure TtiMediatorView.GUIToObject;
var
  B: Boolean;
begin
  if (FCopyingCount > 0) and (not AllowRecursiveCopy) then
    Exit;
  Inc(FCopyingCount);
  try
    B := False;
    if Assigned(FOnBeforeGUIToObject) then
      FOnBeforeGUIToObject(Self, View, Subject, B);
    if not B then
      DoGUIToObject;
  finally
    Dec(FCopyingCount);
  end;
end;

procedure TtiMediatorView.ObjectToGUI(const AForceUpdate: boolean);
var
  B: Boolean;
begin
  if (FCopyingCount > 0) and (not AllowRecursiveCopy) and (not AForceUpdate) then
    Exit;
  Inc(FCopyingCount);
  try
    B := False;
    if Assigned(FOnObjectToGUI) then
      FOnObjectToGUI(Self, Subject, View, B);
    if not B then
      DoObjectToGUI;
  finally
    Dec(FCopyingCount);
  end;
end;

procedure TtiMediatorView.DoObjectToGUI;
var
  LPropInfo: PPropInfo;
  LValue: Variant;
begin
  CheckFieldNames;
  LPropInfo := tiGetPropInfo(View.ClassType, GUIFieldName, nil {PInstance});
  if tiGetTypeInfo(LPropInfo)^.Kind in ctkString then
    LValue := tiVariantAsStringDef(Subject.PropValue[FieldName])
  else
    LValue := Subject.PropValue[FieldName];
  GetObjectPropValue(LValue);
  TypInfo.SetPropValue(View, GUIFieldName, LValue);
end;

procedure TtiMediatorView.GetObjectPropValue(var AValue: Variant);
begin
  // Do nothing. Can be overridden in descendants.
end;


{ TtiMediatorViewComponentHelper }

constructor TtiMediatorViewComponentHelper.Create(
  const AOnNotification: TtiComponentNotificationEvent);
begin
  Assert(Assigned(AOnNotification), 'AOnNotification must be assigned');
  inherited Create(nil); // No owner required
  FOnNotification := AOnNotification;
end;

destructor TtiMediatorViewComponentHelper.Destroy;
begin
  FOnNotification := nil;
  inherited Destroy;
end;

procedure TtiMediatorViewComponentHelper.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FOnNotification) then
    FOnNotification(AComponent, Operation);
  inherited;
end;


{ TtiMediatorManager }

constructor TtiMediatorManager.Create;
begin
  FDefs := TtiMediatorDefs.Create(TtiMediatorDef);
end;

destructor TtiMediatorManager.Destroy;
begin
  FreeAndNil(FDefs);
  inherited Destroy;
end;

function TtiMediatorManager.FindDefFor(ASubject: TtiObject; AGUI: TComponent): TtiMediatorDef;
begin
  Result := FindDefFor(ASubject.ClassType, AGUI, PPropInfo(nil));
end;

function TtiMediatorManager.FindDefFor(ASubject: TtiObject; AGUI: TComponent; APropName: string): TtiMediatorDef;
var
  LPropInfo: PPropInfo;
  LSubject: TtiObject;
begin
  LSubject := ASubject;
  LPropInfo := tiGetPropInfo(ASubject.ClassType, APropName, @LSubject);
  if Assigned(LSubject) then
    Result := FindDefFor(LSubject.ClassType, AGUI, LPropInfo)
  else
    Result := FindDefFor(tiGetTargetClass(ASubject.ClassType, APropName), AGUI, LPropInfo);
end;

function TtiMediatorManager.FindDefFor(ASubjectClass: TClass; AGUI: TComponent; APropInfo: PPropInfo): TtiMediatorDef;
var
  D: TtiMediatorDef;
  I: integer;
begin
  Result := nil;
  for I := 0 to FDefs.Count - 1 do
  begin
    D := FDefs[I];
    if D.Handles(ASubjectClass, AGUI, APropInfo) then
      if (D.BetterMatch(Result)) then
        Result := D;
  end;
end;

function TtiMediatorManager.FindDefFor(ASubject: TtiObject; AGUI: TComponent; APropInfo: PPropInfo): TtiMediatorDef;
begin
  result := FindDefFor(ASubject.ClassType, AGUI, APropInfo);
end;

function TtiMediatorManager.RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass): TtiMediatorDef;
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

  if not (MinSubjectClass.inheritsfrom(TtiObjectList)) and MediatorClass.CompositeMediator then
    MediatorError(Self,sErrCompositeNeedsList, [MediatorClass.ClassName, MinSubjectClass.ClassName]);
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := tkProperties - [tkClass, tkInterface, tkDynArray {$IFDEF FPC}, tkObject, tkInterfaceRaw{$ENDIF}];
end;

function TtiMediatorManager.RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass; PropertyName: string): TtiMediatorDef;
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := PropertyName;
  Result.FPT  := [];
end;

function TtiMediatorManager.RegisterMediator(MediatorClass: TtiMediatorViewClass; MinSubjectClass: TtiSubjectClass; PropertyTypes: TTypeKinds): TtiMediatorDef;
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := PropertyTypes;
end;


{ TtiMediatorDefs }

function TtiMediatorDefs.GetDef(Index: integer): TtiMediatorDef;
begin
  Result := TtiMediatorDef(Items[Index]);
end;

procedure TtiMediatorDefs.SetDef(Index: integer; const AValue: TtiMediatorDef);
begin
  Items[Index] := AValue;
end;

function TtiMediatorDefs.AddDef: TtiMediatorDef;
begin
  Result := Add as TtiMediatorDef;
end;


{ TtiMediatorDef }

function TtiMediatorDef.Handles(ASubjectClass: TClass; AGUI: TComponent; APropInfo: PPropInfo): Boolean;
var
  N: string;
begin
  if (APropInfo = nil) then
    Result := FMC.CompositeMediator
  else
  begin
    N      := string(APropInfo^.Name);
    Result := True;
  end;
  if not Result then
    Exit; // ==>
  // At least the classes must match
  // We check subject class and not the subject itself as this allows us to
  // have a nil subject (e.g. class property where property is unassigned,
  // such as Order.Product.ProductName where Product is nil).
  Result := AGUI.InheritsFrom(FMC.ComponentClass) and ASubjectClass.InheritsFrom(FMSC);
  if Result and not FMC.CompositeMediator then
    if (PropertyName <> '') then
      Result := (CompareText(N, PropertyName) = 0)
    else // Property kind should match. Note that property MUST be set to something.
      Result := (APropInfo^.PropType^.Kind in PropertyTypes); // If PropertyName is set, it must match
end;

function TtiMediatorDef.Handles(ASubject: TtiObject; AGUI: TComponent; APropInfo: PPropInfo): Boolean;
begin
  result := Handles(ASubject.ClassType, AGUI, APropInfo);
end;

function TtiMediatorDef.BetterMatch(M: TtiMediatorDef): Boolean;
begin
  Result := (M = nil);
  if not Result then
  begin
    Result := (FMC.CompositeMediator = M.MediatorClass.CompositeMediator);
    if Result then
    begin
      Result := (PropertyName <> '') and (M.PropertyName = '');
      if not Result then
      begin
        // M's property matches closer
        Result := not ((M.PropertyName <> '') and (PropertyName = ''));
        if Result then
        begin
          // Properties are on equal level. Check GUI class.
          // Closer GUI class ?
          Result := FMC.ComponentClass.InheritsFrom(M.MediatorClass.ComponentClass);
          if not Result then
          begin
            // M's GUI class matches closer ?
            Result := not (M.MediatorClass.ComponentClass.InheritsFrom(FMC.ComponentClass));
            if Result then
            begin
              // GUI classes are on equal level (different branches in tree). Check subject class.
              // Closer Subject class ?
              Result := FMSC.InheritsFrom(M.FMSC);
              if not Result then
                // M's subject class matches closer ?
                Result := not M.FMSC.InheritsFrom(FMSC);
            end;
          end;
        end;
      end;
    end;
  end;
end;


{ Helper functions }

{ Extract the field name part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Quantity
  Width and Field Caption is optional }
function tiFieldName(const AField: string): string;
begin
  Result := tiToken(AField, cBrackets[1], 1);
end;

{ Extract the width part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")  will return: 25
  Width and Field Caption is optional }
function tiFieldWidth(const AField: string): integer;
var
  s: string;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) = '' then
    Result := DefFieldWidth
  else
    Result := StrToInt(tiToken(s, ',', 1));
end;

{ Extracts the alignment from the AField string which is in the format
 fieldname(width,"field caption",a)  where a is the alignment character.
 Legal values for the alignment character are :-
 < - left aligned
 > - right aligned
 | - centre aligned
  eg:  Quantity(25,"Qty",>)   will return: Qty with a width of 25 and
 the column will be right-aligned. Width, Field Caption and alignment
 are optional }
function tiFieldAlignment(const AField: string): TAlignment;
var
  s, a: string;
  lAlignChar: char;
begin
  Result := taLeftJustify;
  s      := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) <> '' then
  begin
    a := tiToken(s, ',', 3);
    if a <> '' then
    begin
      lAlignChar := a[1];
      case lAlignChar of
        '<': Result := taLeftJustify;
        '>': Result := taRightJustify;
        '|': Result := taCenter;
      end; { case }
    end;
  end;
end;

{ Extract the field caption part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Qty
  Width and Field Caption is optional }
function tiFieldCaption(const AField: string): string;
var
  s: string;
  p: PChar;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2]);
  if (trim(s) = '') or (Pos(',', s) = 0) then
    // It's only got a width or is blank, so we default to field name
    Result := tiFieldName(AField)
  else
  begin
    s      := tiToken(s, ',', 2);
    p      := PChar(s);
    Result := AnsiExtractQuotedStr(p, '"');
  end;
end;

{ Extract value field name from field name in the format:
  FieldName:RootFieldName }
function tiValueFieldName(const AFieldName: string): string;
begin
  result := tiToken(AFieldName, cRootFieldNameDelimiter, 1);
end;

{ Extract root field name from field name in the format:
  FieldName:RootFieldName
  Default to FieldName if no RootFieldName specified }
function tiRootFieldName(const AFieldName: string): string;
begin
  result := tiToken(AFieldName, cRootFieldNameDelimiter, 2);
  if result = '' then
    result := AFieldName;
end;


{ ---------------------------------------------------------------------
  General list
  --------------------------------------------------------------------- }

const
  AlignChars: array[TAlignMent] of char     = ('l', 'r', 'c');
  OrigAlignChars: array[TAlignMent] of char = ('<', '>', '|');
  CMediatorFieldSeparator = '|';

{ TtiMediatorFieldInfo }

function TtiMediatorFieldInfo.GetCaption: string;
begin
  Result:=FCaption;
  if Result='' then
    Result:=FPropName;
end;

// Field definitions:
// New style:
//   PropertyName|Alignment|FieldWidth|Caption[;PropertyName|Alignment|FieldWidth|Caption;...]
//   Alignment is l (left), r (right), c (center)
//   e.g. FirstName|r|100|First Name
// or old style:
//   PropertyName(FieldWidth,"Caption",Alignment)[;PropertyName(FieldWidth,"Caption",Alignment);...]
//   Alignment is < (left), > (right), | (center)
//   e.g. FirstName(100,"First Name",>)

function TtiMediatorFieldInfo.GetAsString: string;
begin
  if FOrigStyle then
    Result := Format('%s (%d, "%s", %s)', [PropName, FieldWidth, Caption, origAlignChars[Alignment]])
  else
    Result := Format('%s%c%s%c%d%c%s', [PropName, CMediatorFieldSeparator,
        AlignChars[Alignment], CMediatorFieldSeparator, FieldWidth,
        CMediatorFieldSeparator, Caption]);
end;

procedure TtiMediatorFieldInfo.SetAsString(const AValue: string);
var
  S: string;
  A: TAlignment;
  I: integer;
  P1, P2: integer;
begin
  I := 0;
  P1         := Pos('(', AVAlue);
  P2         := Pos(CMediatorFieldSeparator, AVAlue);
  // Have ( and Not (have | and | before ()
  FOrigStyle := (P1 <> 0) and ((P2 = 0) or (P2 > P1));
  if FOrigStyle then
  begin
    PropName   := tiFieldName(AValue);
    Caption    := tiFieldCaption(AValue);
    FieldWidth := tiFieldWidth(AValue);
    Alignment  := tiFieldAlignment(AValue);
  end
  else
  begin
    // Property name
    PropName := tiToken(AValue, CMediatorFieldSeparator, 1);
    if (PropName = '') then
      MediatorError(Self,SErrInvalidFieldName, [Index + 1]);
    Caption    := PropName;
    Alignment  := taLeftJustify;
    FieldWidth := DefFieldWidth;
    if tiNumToken(AValue, CMediatorFieldSeparator) > 1 then
    begin
      // Alignment
      S := tiToken(AValue, CMediatorFieldSeparator, 2);
      if (S <> '') then
      begin
        if (length(S) <> 1) then
          MediatorError(Self,SErrInvalidAlignmentChar, [S, Index + 1]);
        for A := Low(Talignment) to High(TAlignment) do
          if (Upcase(AlignChars[A]) = Upcase(S[1])) then
            Alignment := A;
      end;

      // Field width
      if tiNumToken(AValue, CMediatorFieldSeparator) > 2 then
      begin
        S := tiToken(AValue, CMediatorFieldSeparator, 3);
        if (S <> '') then
        begin
          if not TryStrToInt(S, i) then
            MediatorError(Self,SErrInvalidWidthSpecifier, [S]);
          FieldWidth := I;
        end;

        // Caption
        S := tiToken(AValue, CMediatorFieldSeparator, 4);
        if (S <> '') then
          Caption := S;
      end;
    end;
  end;
end;

procedure TtiMediatorFieldInfo.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;


{ TtiMediatorFieldInfoList }

function TtiMediatorFieldInfoList.GetAsString: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ';';
    Result := Result + FieldInfo[i].AsString;
  end;
end;

function TtiMediatorFieldInfoList.GetI(Index: integer): TtiMediatorFieldInfo;
begin
  Result := TtiMediatorFieldInfo(Items[Index]);
end;

procedure TtiMediatorFieldInfoList.SetI(Index: integer; const AValue: TtiMediatorFieldInfo);
begin
  Items[Index] := AValue;
end;

procedure TtiMediatorFieldInfoList.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  If Assigned(FMediator) then
    FMediator.FieldInfoChanged(Item as TtiMediatorFieldInfo,Action)
end;

function TtiMediatorFieldInfoList.AddFieldInfo: TtiMediatorFieldInfo;
begin
  Result := Add as TtiMediatorFieldInfo;
end;

function TtiMediatorFieldInfoList.AddFieldInfo(const APropName: String;
  AFieldWidth: Integer): TtiMediatorFieldInfo;
begin
  Result:=AddFieldInfo();
  Result.PropName:=APropName;
  Result.FieldWidth:=AFieldWidth;
end;

function TtiMediatorFieldInfoList.AddFieldInfo(const APropName,
  ACaption: String; AFieldWidth: Integer): TtiMediatorFieldInfo;
begin
  Result:=AddFieldInfo(APropName,AFieldWidth);
  Result.Caption:=ACaption;
end;

function TtiMediatorFieldInfoList.AddFieldInfo(const APropName,
  ACaption: String; AFieldWidth: Integer; AAlignment: TAlignment
  ): TtiMediatorFieldInfo;
begin
  Result:=AddFieldInfo(APropName,ACaption,AFieldWidth);
  Result.Alignment:=AAlignment;
end;


function TtiMediatorFieldInfoList.FieldInfoByName(const pName: String): TtiMediatorFieldInfo;
Var
  I : Integer;
begin
  Result := nil;
  for I := 0 To pred(Count) do
  begin
    if (FieldInfo[I].Caption = pName) then
    begin
      Result := FieldInfo[I];
      Break;
    end;
  end; { Loop }
end;

{ TtiListItemMediator }

function TtiListItemMediator.GetDisplayNames: string;
begin
  Result := FFieldsInfo.AsString;
end;

procedure TtiListItemMediator.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if Assigned(FModel) then
    if Active then
      FModel.AttachObserver(Self)
    else
      FModel.DetachObserver(Self);
end;

procedure TtiListItemMediator.SetModel(const AValue: TtiObject);
var
  B : Boolean;
begin
  if Avalue=FModel then
    Exit;
  B := Assigned(FModel);
  if B then
    FModel.DetachObserver(Self);
  FModel := Avalue;
  if Assigned(FModel) then
  begin
    FModel.AttachObserver(Self);
    if B and Active then
      Update(FModel,noChanged);
  end;
end;

procedure TtiListItemMediator.StopObserving(ASubject: TtiObject);
begin
  FModel:=Nil;
end;

procedure TtiListItemMediator.Update(ASubject: TtiObject);
begin
  Assert(Model = ASubject);
  inherited;
  if ASubject.Deleted and Assigned(ListMediator) then
    // WARNING: this may result in us being deleted.
    ListMediator.ItemDeleted(ASubject);
end;

destructor TtiListItemMediator.Destroy;
begin
  Active := False;
  Model := nil;
  inherited Destroy;
end;


{ TtiCustomListMediatorView }

procedure TtiCustomListMediatorView.SetOnBeforeSetupField(const Value: TtiOnBeforeSetupField);
var
  I: integer;
begin
  FOnBeforeSetupField := Value;
  for I := 0 to FMediatorList.Count - 1 do
    TtiListItemMediator(FMediatorList[i]).OnBeforeSetupField := Value;
end;

procedure TtiCustomListMediatorView.FieldInfoChanged(Item: TtiMediatorFieldInfo;
  Action: TCollectionNotification);
begin
  If Active  then
    RaiseMediatorError(SErrActive);
end;

procedure TtiCustomListMediatorView.SetSubject(const AValue: TtiObject);
begin
  if (AValue=GetSubject) then Exit;
  if (AValue <> nil) then
  begin
    if not (AValue is TtiObjectList) then
      RaiseMediatorError(SErrNotListObject, [AValue.ClassName]);
  end
  else
    ClearList;
  FListChanged:=True;
  inherited SetSubject(AValue);
end;

procedure TtiCustomListMediatorView.SetFieldName(const AValue: string);
begin
  inherited SetFieldName(AValue);
  ParseDisplayNames(AValue);
end;

procedure TtiCustomListMediatorView.SetActive(const AValue: Boolean);
var
  I: integer;
begin
  inherited SetActive(AValue);
  for I := 0 to FMediatorList.Count - 1 do
    TtiListItemMediator(FMediatorList[i]).Active := AValue;
end;

function TtiCustomListMediatorView.FindObjectMediator(AObject: TTiObject; out AtIndex: Integer): TtiListItemMediator;
begin
  AtIndex:=FMediatorList.Count-1;
  While (AtIndex>=0) and (TtiListItemMediator(FMediatorList[AtIndex]).Model<>AObject) do
    Dec(AtIndex);
  If (AtIndex=-1) then
    Result:=Nil
  else
    Result:=TtiListItemMediator(FMediatorList[AtIndex]);
end;

function TtiCustomListMediatorView.GetModel: TtiObjectList;
begin
  Result := Subject as TtiObjectList;
end;

function TtiCustomListMediatorView.GetDisplayNames: string;
begin
  Result := FFieldsInfo.AsString;
end;

function TtiCustomListMediatorView.GetIsObserving: Boolean;
begin
  Result := Active;
end;

procedure TtiCustomListMediatorView.SetDisplayNames(const AValue: string);
begin
  FieldName := AValue;
end;

procedure TtiCustomListMediatorView.SetFieldsInfo(const AValue: TtiMediatorFieldInfoList);
begin
  FFieldsInfo.Assign(AValue);
end;

procedure TtiCustomListMediatorView.SetIsObserving(const AValue: Boolean);
begin
  Active := AValue;
end;

procedure TtiCustomListMediatorView.DoDeleteItemMediator(AIndex : Integer; AMediator : TtiListItemMediator);
begin
  MediatorList.Delete(AIndex);
end;

procedure TtiCustomListMediatorView.SetShowDeleted(const AValue: Boolean);
begin
  if FShowDeleted = AValue then
    Exit; //==>
  BeginUpdate;
  try
    FShowDeleted := AValue;
    RebuildList;
  finally
    EndUpdate;
  end;
end;

procedure TtiCustomListMediatorView.CreateSubMediators;
var
  i: integer;
  LItemMediator: TtiListItemMediator;
begin
  CreateColumns;
  for i := 0 to Model.Count - 1 do
    begin
    if (not Model.Items[i].Deleted) or FShowDeleted then
      if i < MediatorList.Count then
        TtiListItemMediator(MediatorList[i]).Model := Model.Items[i]
      else
      begin
        LItemMediator := DoCreateItemMediator(Model.Items[i], i);
        LItemMediator.ListMediator := Self;
      end;
    end;
  for i := MediatorList.Count-1 downto Model.Count do
    DoDeleteItemMediator(I,TtiListItemMediator(MediatorList[i]));
  FListChanged:=False;
end;

function TtiCustomListMediatorView.DataAndPropertyValid(const AData: TtiObject): Boolean;
var
  c: integer;
  lField: string;
begin
  Result := (Subject <> nil) and (FFieldsInfo.Count > 0);
  if not Result then
    Exit; //==>

  for c := 0 to FFieldsInfo.Count - 1 do
  begin
    lField := FFieldsInfo[c].PropName;
    { WRONG!!  We should test the items of the Model }
    result := tiIsPublishedProp(AData, lField);
    if not Result then
      RaiseMediatorError(SErrInvalidPropertyName,[lField, AData.ClassName]);
  end;
end;

procedure TtiCustomListMediatorView.DoGUIToObject;
begin
  // Do nothing. List is essentially read-only.
  // inherited DoGUIToObject;
end;

procedure TtiCustomListMediatorView.DoObjectToGUI;
begin
  RebuildList;
end;

constructor TtiCustomListMediatorView.Create;
begin
  inherited Create;
  FFieldsInfo   := TtiMediatorFieldInfoList.Create(TtiMediatorFieldInfo);
  FFieldsInfo.FMediator:=Self;
  FMediatorList := TObjectList.Create;
  FShowDeleted  := False;
  Active        := False;
end;

destructor TtiCustomListMediatorView.Destroy;
begin
  Active:=False;
  FreeAndNil(FMediatorList);
  FreeAndNil(FFieldsInfo);
  inherited Destroy;
end;

class function TtiCustomListMediatorView.CompositeMediator: Boolean;
begin
  Result := True;
end;

procedure TtiCustomListMediatorView.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
var
  M: TtiListItemMediator;
begin
  // Do not call inherited, it will rebuild the list !!
  Case AOperation of
    noAddItem    : begin
                     M := DoCreateItemMediator(ASubject,Model.Count-1); // always at the end...
                     M.ListMediator := Self;
                   end;
    noDeleteItem : ItemDeleted(ASubject);
    noFree       : if (ASubject = FSubject) then
                     Subject := nil;
    noChanged    :
                  begin
                    if ShowDeleted then
                    begin
                      if FListChanged or (Model.Count<>MediatorList.Count) or (Model.Count=0) then // Safety measure
                       RebuildList;
                    end
                    else
                    begin
                      if FListChanged or (Model.CountNotDeleted<>MediatorList.Count) or (Model.CountNotDeleted=0) then // Safety measure
                       RebuildList;
                    end;
                  end;
    noReSort     : RebuildList;
  end;
end;

procedure TtiCustomListMediatorView.HandleSelectionChanged;
begin
  // do nothing. Descendants can complete this if needed.
end;

procedure TtiCustomListMediatorView.ItemDeleted(const ASubject: TtiObject);
var
  LItemMediator: TtiListItemMediator;
  I: integer;
begin
  LItemMediator := FindObjectMediator(ASubject,I);
  if Assigned(LItemMediator) then
    DoDeleteItemMediator(I, LItemMediator);
end;

procedure TtiCustomListMediatorView.ParseDisplayNames(const AValue: string);
var
  I: integer;
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


initialization
  // nothing

finalization
  FreeAndNil(uMediatorManager);

end.


unit tiModelMediator;

{$I tiDefines.inc}

interface

uses
  Classes,
  SysUtils,
  tiObject,
  tiBaseMediator;

type
  TtiModelMediator = class;


  { TtiPropertyLinkDef }

  TtiPropertyLinkDef = class(TCollectionItem)
  private
    FComponent: TComponent;
    FComposite: Boolean;
    FObjectUpdateMoment: TtiObjectUpdateMoment;
    FFieldName: string;
    FGUIFieldName: string;
    FMediator: TtiMediatorView;
    FMediatorDef: TtiMediatorDef;
    FOnBeforeGUIToObject: TtiBeforeGUIToObjectEvent;
    FOnAfterGUIToObject: TtiAfterGUIToObjectEvent;
    FOnObjectToGUI: TtiObjectToGUIEvent;
    FOnSetupMediator: TtiMediatorEvent;
    FValueList: TtiObjectList;
    procedure SetComponent(const AValue: TComponent);
    procedure SetComposite(const AValue: Boolean);
    procedure SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
    procedure SetFieldName(const AValue: string);
    procedure SetGUIFieldName(const AValue: string);
    procedure SetOnBeforeGUIToObject(const AValue: TtiBeforeGUIToObjectEvent);
    procedure SetOnAfterGUIToObject(const AValue: TtiAfterGUIToObjectEvent);
    procedure SetOnObjectToGUI(const AValue: TtiObjectToGUIEvent);
    procedure SetValueList(const AValue: TtiObjectList);
    function GetValidGUIValue: Boolean;
  protected
    function GetDisplayName: string; override;
    procedure CreateMediator; virtual;
    procedure FreeMediator(FreeDef: Boolean = True); virtual;
    property MediatorDef: TtiMediatorDef read FMediatorDef;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function ModelMediator: TtiModelMediator;
    property Mediator: TtiMediatorView read FMediator;
    property ValueList: TtiObjectList read FValueList write SetValueList;
    property ValidGUIValue: Boolean read GetValidGUIValue;
  published
    property Composite: Boolean read FComposite write SetComposite;
    // Format: FieldName[:RootFieldName] (see TtiMediatorView.FieldName/RootFieldName)
    property FieldName: string read FFieldName write SetFieldName;
    property GUIFieldName: string read FGUIFieldName write SetGUIFieldName;
    property Component: TComponent read FComponent write SetComponent;
    property ObjectUpdateMoment: TtiObjectUpdateMoment read FObjectUpdateMoment write SetObjectUpdateMoment;
    property OnBeforeGUIToObject: TtiBeforeGUIToObjectEvent read FOnBeforeGUIToObject write SetOnBeforeGUIToObject;
    property OnAfterGUIToObject: TtiAfterGUIToObjectEvent read FOnAfterGUIToObject write SetOnAfterGUIToObject;
    property OnObjectToGUI: TtiObjectToGUIEvent read FOnObjectToGUI write SetOnObjectToGUI;
    Property OnSetupMediator: TtiMediatorEvent Read FOnSetupMediator Write FOnSetupMediator;
  end;


  { TtiPropertyLinkDefs }

  TtiPropertyLinkDefs = class(TCollection)
  private
    FModelMediator: TtiModelMediator;
    function GetD(Index: integer): TtiPropertyLinkDef;
    procedure SetD(Index: integer; const AValue: TtiPropertyLinkDef);
  public
    function GetOwner: TPersistent; override;
    function AddPropertyLinkDef: TtiPropertyLinkDef;
    function IndexOfComponent(AComponent: TComponent): integer;
    function IndexOfMediator(AMediator: TtiMediatorView): integer;
    function IndexOfTag(ATag: LongInt): integer;
    function FindByComponent(AComponent: TComponent): TtiPropertyLinkDef;
    function FindByMediator(AMediator: TtiMediatorView): TtiPropertyLinkDef;
    function FindByTag(ATag: LongInt): TtiPropertyLinkDef;
    property ModelMediator: TtiModelMediator read FModelMediator;
    property Defs[Index: integer]: TtiPropertyLinkDef read GetD write SetD; default;
  end;


  { TtiModelMediator }

  TtiModelMediator = class(TComponent)
  private
    FActive: Boolean;
    FDefs: TtiPropertyLinkDefs;
    FSubject: TtiObject;
    FSubjectObserver: TtiObserverProxy;
    procedure CreateMediators;
    procedure SetActive(const AValue: Boolean);
    procedure SetPropertyLinkDefs(const AValue: TtiPropertyLinkDefs);
    procedure SetSubject(const AValue: TtiObject);
    procedure SubjectUpdate(ASubject: TtiObject; AOperation: TNotifyOperation);
    function GetMediatorView(AComponent: TComponent): TtiMediatorView;
    function GetSelectedObject(AComponent: TComponent): TtiObject;
    procedure SetSelectedObject(AComponent: TComponent; AObject: TtiObject);
    function GetValidGUIValues: Boolean;
  protected
    function CreatePropertyDefs: TtiPropertyLinkDefs; virtual;
    function CreateProperty(const AFieldName: string; const AGUIComponent: TComponent; AGUIFieldName: string = ''): TtiPropertyLinkDef; virtual;
    procedure AfterPropertyCreated(ADef: TtiPropertyLinkDef);
    procedure CheckSubject;
    procedure CheckInactive;
    procedure CheckMediator(ADef: TtiPropertyLinkDef);
    procedure CheckMediators;
    procedure Bind;
    procedure UnBind;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AddProperty(const AFieldName: string; const AGUIComponent: TComponent; AGUIFieldName: string = ''): TtiPropertyLinkDef;
    function AddComposite(const ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef;
    function FindByComponent(AComponent: TComponent): TtiPropertyLinkDef;
    function FindByMediator(AMediator: TtiMediatorView): TtiPropertyLinkDef;
    function FindByTag(ATag: LongInt): TtiPropertyLinkDef;
    {: Find the mediator view for the given component. Returns nil if not found. }
    function FindMediatorView(AComponent: TComponent): TtiMediatorView;
    {: Find the component for the given mediator view. Returns nil if not found. }
    function FindComponent(AMediator: TtiMediatorView): TComponent;
    procedure SubjectChanged;

    property Subject: TtiObject read FSubject write SetSubject;
    property Active: Boolean read FActive write SetActive;

    property ValidGUIValues: Boolean read GetValidGUIValues;

    {: Find the mediator view for the given component. If the component is not found an exception is raised. }
    property MediatorView[AComponent: TComponent]: TtiMediatorView read GetMediatorView;
    {: Find the selected object in the given component. For list mediator views this returns
       the selected item in the list. For other mediators this returns the subject.
       If the component is not found an exception is raised. }
    property SelectedObject[AComponent: TComponent]: TtiObject read GetSelectedObject write SetSelectedObject;
  published
    property PropertyLinks: TtiPropertyLinkDefs read FDefs write SetPropertyLinkDefs;
  end;


  TtiModelMediatorCollection = class;
  TtiModelMediatorList = class;

  { TtiModelMediatorItem }

  {: Design-time model mediator using TCollection property editor }
  TtiModelMediatorItem = class(TCollectionItem)
  private
    FModelMediator: TtiModelMediator;
    FModelMediatorName: string;
    FSubject: TtiObject;
    procedure SetModelMediatorName(const AValue: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ModelMediatorCollection: TtiModelMediatorCollection;
    function ModelMediatorList: TtiModelMediatorList;
    {: The subject for this item. There can be more than one item for the same subject } 
    property Subject: TtiObject read FSubject write FSubject;
    {: Find the mediator view for the given component. Returns nil if not found. }
    function FindMediatorView(AComponent: TComponent): TtiMediatorView;
  published
    {: Model mediator for this item }
    property ModelMediator: TtiModelMediator read FModelMediator;
    {: Unique name for the model mediator }
    property ModelMediatorName: string read FModelMediatorName write SetModelMediatorName;
  end;


  { TtiModelMediatorCollection }

  {: Design-time list of model mediators using TCollection property editor }
  TtiModelMediatorCollection = class(TCollection)
  private
    FModelMediatorList: TtiModelMediatorList;
    FNextNameID: integer;
    function GetItem(Index: integer): TtiModelMediatorItem;
    procedure SetItem(Index: integer; const AValue: TtiModelMediatorItem);
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function GetOwner: TPersistent; override;
    function FindByName(const AModelMediatorName: string): TtiModelMediatorItem;
    function FindBySubject(const ASubject: TtiObject): TtiModelMediatorItem;
    function NextName(const ANamePrefix: string): string;
    property ModelMediatorList: TtiModelMediatorList read FModelMediatorList;
    property Items[Index: integer]: TtiModelMediatorItem read GetItem write SetItem; default;
  end;


  { TtiModelMediatorList }

  {: A list of model mediators. One model mediator is required per subject.
     This component, which can be used at design-time or run-time, groups the
     field-component links by model name or model instance. The various methods
     provide access by name or instance or by model class (which is really by
     name using the class name). }
  TtiModelMediatorList = class(TComponent)
  private
    FModelMediators: TtiModelMediatorCollection;
    function GetItemByName(const AModelMediatorName: string): TtiModelMediatorItem;
    // Returns the *first* model mediator item for the given subject.
    function GetItemBySubject(const ASubject: TtiObject): TtiModelMediatorItem;
    procedure SetModelMediators(const AValue: TtiModelMediatorCollection);
    function FindCreate(const AModelMediatorName: string): TtiModelMediator; overload;
    function FindCreate(const ASubject: TtiObject): TtiModelMediator; overload;

    function GetActiveByClass(ASubjectClass: TtiObjectClass): boolean;
    procedure SetActiveByClass(ASubjectClass: TtiObjectClass; const AValue: boolean);
    function GetActiveByName(AModelMediatorName: string): boolean;
    procedure SetActiveByName(AModelMediatorName: string; const AValue: boolean);
    function GetAllActive: boolean;
    procedure SetAllActive(const AValue: boolean);
    function GetSubjectByClass(ASubjectClass: TtiObjectClass): TtiObject;
    procedure SetSubjectByClass(ASubjectClass: TtiObjectClass; const AValue: TtiObject);
    function GetSubjectByName(AModelMediatorName: string): TtiObject;
    procedure SetSubjectByName(AModelMediatorName: string; const AValue: TtiObject);
    function GetActiveBySubject(ASubject: TtiObject): boolean;
    procedure SetActiveBySubject(ASubject: TtiObject; const AValue: boolean);
    function GetMediatorView(AComponent: TComponent): TtiMediatorView;
    function GetSelectedObject(AComponent: TComponent): TtiObject;
    procedure SetSelectedObject(AComponent: TComponent; AObject: TtiObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {: Add a model mediator for the given name }
    function Add(const AModelMediatorName: string): TtiModelMediator;

    {: Add a field-component mapping for a model by name }
    function AddProperty(const AModelMediatorName: string;
        const AFieldName: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;
    {: Add a list field-component mapping for a model by name }
    function AddComposite(const AModelMediatorName: string;
        const ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;
    {: Add a field-component mapping for a model by class (one model per class) }
    function AddProperty(const ASubjectClass: TtiObjectClass;
        const AFieldName: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;
    {: Add a list field-component mapping for a model by class (one model per class) }
    function AddComposite(const ASubjectClass: TtiObjectClass;
        const ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;
    {: Add a field-component mapping for a model by instance. If there are
       multiple model mediators for the same model instance then the property
       is added to the first found in the list }
    function AddProperty(const ASubject: TtiObject;
        const AFieldName: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;
    {: Add a list field-component mapping for a model by instance. If there are
       multiple model mediators for the same model instance then the property
       is added to the first found in the list }
    function AddComposite(const ASubject: TtiObject;
        const ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef; overload;

    {: Model mediator for the given model class }
    function ModelMediator(const ASubjectClass: TtiObjectClass): TtiModelMediator; overload;
    {: Model mediator for the given name }
    function ModelMediator(const AModelMediatorName: string): TtiModelMediator; overload;
    {: Model mediator for the given model instance. Note that there can be more
       than one model mediator for the same model instance. In this case the
       first model mediator will be returned }
    function ModelMediator(const ASubject: TtiObject): TtiModelMediator; overload;

    {: Find the mediator view for the given component. Returns nil if not found. }
    function FindMediatorView(AComponent: TComponent): TtiMediatorView;

    {: Notify model mediator for the given model class that the subjects data changed }
    procedure SubjectChanged(const ASubjectClass: TtiObjectClass); overload;
    {: Notify model mediator for the given model name that the subjects data changed }
    procedure SubjectChanged(const AModelMediatorName: string); overload;
    {: Notify model mediator for the given model instance that the subjects data changed }
    procedure SubjectChanged(const ASubject: TtiObject); overload;
    {: Notify all model mediators that one or more subjects data changed }
    procedure SubjectChanged; overload;

    {: The model instance for the given model class }
    property SubjectByClass[ASubjectClass: TtiObjectClass]: TtiObject read GetSubjectByClass write SetSubjectByClass;
    {: The model instance for the given model name }
    property SubjectByName[AModelMediatorName: string]: TtiObject read GetSubjectByName write SetSubjectByName;

    {: Active for the given model class }
    property ActiveByClass[ASubjectClass: TtiObjectClass]: boolean read GetActiveByClass write SetActiveByClass;
    {: Active for the given model name }
    property ActiveByName[AModelMediatorName: string]: boolean read GetActiveByName write SetActiveByName;
    {: Active for the given model instance }
    property ActiveBySubject[ASubject: TtiObject]: boolean read GetActiveBySubject write SetActiveBySubject;
    {: Active for all model mediators }
    property AllActive: boolean read GetAllActive write SetAllActive;

    {: Find the mediator view for the given component. If the component is not found an exception is raised. }
    property MediatorView[AComponent: TComponent]: TtiMediatorView read GetMediatorView;
    {: Find the selected object in the given component. For list mediator views this returns
       the selected item in the list. For other mediators this returns the subject.
       If the component is not found an exception is raised. }
    property SelectedObject[AComponent: TComponent]: TtiObject read GetSelectedObject write SetSelectedObject;
  published
    {: Design-time and run-time support for adding model mediators. }
    property ModelMediators: TtiModelMediatorCollection read FModelMediators write SetModelMediators;
  end;


implementation

uses
  tiConstants
  ;

const
  CDefaultModelMediatorName = 'ModelMediator'; // Do not localize

resourcestring
  SErrNoSubject  = 'Cannot perform this operation if subject is not set.';
  SErrActive     = 'Cannot perform this operation while active.';
  SErrNoMediator = 'Cannot find a mediator for control %s (%s), property %s.';
  SErrNoMediatorViewForComponent = 'Cannot find a mediator view for control %s (%s).';
  SErrNoModelMediatorName = 'ModelMediatorName is required';
  SErrDuplicateModelMediatorName = 'ModelMediatorName %s must be unique';
  SErrNoModelMediator = 'Cannot find model mediator %s.';


{ TtiPropertyLinkDef }

constructor TtiPropertyLinkDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FObjectUpdateMoment := ouDefault;
end;

procedure TtiPropertyLinkDef.SetComponent(const AValue: TComponent);
begin
  if FComponent = AValue then
    Exit;
  FComponent := AValue;
end;

procedure TtiPropertyLinkDef.SetComposite(const AValue: Boolean);
var
  F: TtiModelMediator;
begin
  if FComposite = AValue then
    Exit;
  F := ModelMediator;
  if Assigned(F) then
    F.CheckInactive;
  FComposite := AValue;
end;

procedure TtiPropertyLinkDef.SetObjectUpdateMoment(const AValue: TtiObjectUpdateMoment);
begin
  if FObjectUpdateMoment = AValue then
    Exit;
  FObjectUpdateMoment := AValue;
  if Assigned(FMediator) then
    FMediator.ObjectUpdateMoment := FObjectUpdateMoment;
end;

procedure TtiPropertyLinkDef.SetFieldName(const AValue: string);
begin
  if FFieldName = AValue then
    Exit;
  FFieldName   := AValue;
  FMediatorDef := nil;
end;

procedure TtiPropertyLinkDef.SetGUIFieldName(const AValue: string);
begin
  if FGUIFieldName = AValue then
    Exit;
  FGUIFieldName   := AValue;
end;

procedure TtiPropertyLinkDef.SetOnBeforeGUIToObject(const AValue: TtiBeforeGUIToObjectEvent);
begin
  FOnBeforeGUIToObject := AValue;
  if Assigned(Mediator) then
    Mediator.OnBeforeGUIToObject := AValue;
end;

procedure TtiPropertyLinkDef.SetOnAfterGUIToObject(const AValue: TtiAfterGUIToObjectEvent);
begin
  FOnAfterGUIToObject := AValue;
  if Assigned(Mediator) then
    Mediator.OnAfterGUIToObject := AValue;
end;

procedure TtiPropertyLinkDef.SetOnObjectToGUI(const AValue: TtiObjectToGUIEvent);
begin
  FOnObjectToGUI := AValue;
  if Assigned(Mediator) then
    Mediator.OnObjectToGUI := AValue;
end;

procedure TtiPropertyLinkDef.SetValueList(const AValue: TtiObjectList);
begin
  if FValueList = AValue then
    Exit;
  FValueList := AValue;
  if Assigned(FMediator) then
    FMediator.ValueList := FValueList;
end;

procedure TtiPropertyLinkDef.CreateMediator;
begin
  if Assigned(FMediator) then
    Exit; //==>
  if (FMediatorDef = nil) then
    MediatorError(Self,SErrNoMediator, [Component.Name, Component.ClassName, FieldName]);
  FMediator := FMediatorDef.MediatorClass.Create;
  if FMediator.CompositeMediator then
  begin
    FMediator.FieldName := FieldName;
    FMediator.RootFieldName := '';
  end else begin
    FMediator.FieldName := tiValueFieldName(FieldName);
    if GUIFieldName <> '' then
      FMediator.GUIFieldName := GUIFieldName;
    FMediator.RootFieldName := tiRootFieldName(FieldName);
  end;
  FMediator.OnBeforeGUIToObject := Self.OnBeforeGUIToObject;
  FMediator.OnAfterGUIToObject := Self.OnAfterGUIToObject;
  FMediator.OnObjectToGUI := Self.OnObjectToGUI;
  FMediator.SetView(Self.Component);
  FMediator.ValueList := Self.ValueList;
  FMediator.Subject := ModelMediator.Subject;
  FMediator.ObjectUpdateMoment := Self.ObjectUpdateMoment;
  FMediator.Active := True;
  if Assigned(FOnSetupMediator) then
    FOnSetupMediator(FMediator);
end;

procedure TtiPropertyLinkDef.FreeMediator(FreeDef: Boolean = True);
begin
  if Assigned(FMediator) then
  begin
    FMediator.Active := False;
    FMediator.Subject := nil;
    FMediator.SetView(nil);
    FreeAndNil(FMediator);
  end;
  if FreeDef then
    FMediatorDef := nil;
end;

procedure TtiPropertyLinkDef.Assign(Source: TPersistent);
var
  D: TtiPropertyLinkDef;
begin
  if (Source is TtiPropertyLinkDef) then
  begin
    D          := Source as TtiPropertyLinkDef;
    FComponent := D.Component;
    FObjectUpdateMoment := D.ObjectUpdateMoment;
    FFieldName := D.FieldName;
    FOnBeforeGUIToObject := D.OnBeforeGUIToObject;
    FOnAfterGUIToObject := D.OnAfterGUIToObject;
    FOnObjectToGUI := D.OnObjectToGUI;
    FValueList := D.ValueList;
    FCOmposite := D.Composite;
    { Not sure about those. Assign is normally only used in design,
      when these are not set.
      FMediator: TtiMediatorView;
      FMediatorDef : TtiMediatorDef;
    }
  end
  else
    inherited Assign(Source);
end;

function TtiPropertyLinkDef.GetDisplayName: string;
begin
  if Assigned(FComponent) then
    result := FComponent.Name + ' <=> ' + FFieldName
  else if FFieldName <> '' then
    result := FFieldName
  else
    result := inherited GetDisplayName;
end;

function TtiPropertyLinkDef.GetValidGUIValue: Boolean;
begin
  Result := Assigned(FMediator) and FMediator.ValidGUIValue;
end;

function TtiPropertyLinkDef.ModelMediator: TtiModelMediator;
begin
  Result := nil;
  if Assigned(Collection) then
    Result := (Collection as TtiPropertyLinkDefs).ModelMediator;
end;


{ TtiModelMediator }

procedure TtiModelMediator.SetActive(const AValue: Boolean);
begin
  if (FActive = AValue) then
    Exit;
  if AValue and Assigned(FSubject) then
    Bind
  else
    UnBind;
  FActive := AValue;
  if Assigned(FSubject) then
    FSubject.NotifyObservers;
end;

procedure TtiModelMediator.SetPropertyLinkDefs(const AValue: TtiPropertyLinkDefs);
begin
  if FDefs = AValue then
    Exit;
  FDefs.Clear;
  FDefs.Assign(AValue);
end;

procedure TtiModelMediator.SetSelectedObject(AComponent: TComponent;
  AObject: TtiObject);
var
  LView: TtiMediatorView;
begin
  LView := FindMediatorView(AComponent);
  if Assigned(LView) then
    LView.SelectedObject := AObject
  else
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

procedure TtiModelMediator.SetSubject(const AValue: TtiObject);
var
  LSubjectWasAssigned: boolean;
  I: integer;
begin
  if FSubject = AValue then
    Exit;
  LSubjectWasAssigned := Assigned(FSubject);

  FSubject := AValue;
  FSubjectObserver.Subject := AValue;
  if (FSubject = nil) then
    Active := False
  else if Active then
  begin
    if not LSubjectWasAssigned then
      Bind;

    for I := 0 to FDefs.Count - 1 do
      if Assigned(FDefs[I].FMediator) then
        FDefs[I].FMediator.Subject := AValue;
    FSubject.NotifyObservers;
  end;
end;

procedure TtiModelMediator.SubjectChanged;
var
  i: integer;
  LMediator: TtiMediatorView;
begin
  for i := 0 to FDefs.Count - 1 do
  begin
    LMediator := FDefs[i].Mediator;
    if Assigned(LMediator) then
      LMediator.UpdateView;
  end;
end;

procedure TtiModelMediator.SubjectUpdate(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
  if (ASubject = Subject) and (AOperation = noFree) then
    Subject := nil;
end;

procedure TtiModelMediator.CreateMediators;
var
  I: integer;
begin
  for I := 0 to FDefs.Count - 1 do
    FDefs[I].CreateMediator;
end;

procedure TtiModelMediator.UnBind;
var
  I: integer;
begin
  for I := 0 to FDefs.Count - 1 do
    FDefs[I].FreeMediator(True);
end;

procedure TtiModelMediator.Bind;
begin
  CheckMediators;
  CreateMediators;
end;

function TtiModelMediator.CreatePropertyDefs: TtiPropertyLinkDefs;
begin
  Result := TtiPropertyLinkDefs.Create(TtiPropertyLinkDef);
end;

procedure TtiModelMediator.CheckSubject;
begin
  if not Assigned(FSubject) then
    MediatorError(Self,SErrNoSubject);
end;

procedure TtiModelMediator.CheckInactive;
begin
  if Active then
    MediatorError(Self,SErrActive);
end;

procedure TtiModelMediator.CheckMediator(ADef: TtiPropertyLinkDef);
begin
  if (ADef.FMediator = nil) then
  begin
    if ADef.Composite then
      ADef.FMediatorDef := gMediatorManager.FindDefFor(Subject, ADef.Component)
    else
      ADef.FMediatorDef := gMediatorManager.FindDefFor(Subject, ADef.Component, tiValueFieldName(ADef.FieldName));
    if (ADef.FMediatorDef = nil) then
      MediatorError(Self, SErrNoMediator, [ADef.Component.Name, ADef.Component.ClassName, ADef.FieldName]);
  end;
end;

procedure TtiModelMediator.CheckMediators;
var
  I: integer;
begin
  for I := 0 to FDefs.Count - 1 do
    CheckMediator(FDefs[i]);
end;

constructor TtiModelMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefs := CreatePropertyDefs;
  FDefs.FModelMediator := Self;
  FSubjectObserver := TtiObserverProxy.Create(nil, SubjectUpdate);
end;

destructor TtiModelMediator.Destroy;
begin
  Active := False;
  FreeAndNil(FSubjectObserver);
  FreeAndNil(FDefs);
  inherited Destroy;
end;

function TtiModelMediator.CreateProperty(const AFieldName: string; const AGUIComponent: TComponent; AGUIFieldName: string = ''): TtiPropertyLinkDef;
begin
  Result           := FDefs.AddPropertyLinkDef;
  Result.FieldName := AFieldName;
  Result.GUIFieldName:= AGUIFieldName;
  Result.Component := AGUICOmponent;
end;

procedure TtiModelMediator.AfterPropertyCreated(ADef: TtiPropertyLinkDef);
begin
  if Assigned(FSubject) then
  begin
    CheckMediator(ADef);
    ADef.CreateMediator;
  end;
end;

procedure TtiModelMediator.Assign(Source: TPersistent);
var
  LSource: TtiModelMediator;
begin
  inherited Assign(Source);
  if Source is TtiModelMediator then
  begin
    LSource := Source as TtiModelMediator;
    Subject := LSource.Subject;
    FDefs.Assign(LSource.FDefs);
    Active := LSource.Active;
  end;
end;

function TtiModelMediator.AddProperty(const AFieldName: string; const AGUIComponent: TComponent; AGUIFieldName: string = ''): TtiPropertyLinkDef;
begin
  Result := CreateProperty(AFieldName, AGUIComponent, AGUIFieldName);
  AfterPropertyCreated(Result);
end;

function TtiModelMediator.AddComposite(const ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  Result           := CreateProperty(ADisplayNames, AGUIComponent);
  Result.Composite := True;
  AfterPropertyCreated(Result);
end;

function TtiModelMediator.FindByComponent(AComponent: TComponent): TtiPropertyLinkDef;
begin
  Result := FDefs.FindByComponent(AComponent);
end;

function TtiModelMediator.FindByMediator(AMediator: TtiMediatorView): TtiPropertyLinkDef;
begin
  Result := FDefs.FindByMediator(AMediator);
end;

function TtiModelMediator.FindByTag(ATag: LongInt): TtiPropertyLinkDef;
begin
  Result := FDefs.FindByTag(ATag);
end;

function TtiModelMediator.GetMediatorView(
  AComponent: TComponent): TtiMediatorView;
begin
  result := FindMediatorView(AComponent);
  if not Assigned(result) then
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

function TtiModelMediator.GetSelectedObject(AComponent: TComponent): TtiObject;
var
  LView: TtiMediatorView;
begin
  result := nil;
  LView := FindMediatorView(AComponent);
  if Assigned(LView) then
    result := LView.SelectedObject
  else
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

function TtiModelMediator.GetValidGUIValues: Boolean;
var
  i: Integer;
begin
  for i := 0 to FDefs.Count - 1 do
    if not FDefs[i].ValidGUIValue then
      Exit(false); //==>
  result := true;
end;

function TtiModelMediator.FindMediatorView(AComponent: TComponent): TtiMediatorView;
var
  L: TtiPropertyLinkDef;
begin
  L := FindByComponent(AComponent);
  if (L = nil) then
    Result := nil
  else
    Result := L.Mediator;
end;

function TtiModelMediator.FindComponent(AMediator: TtiMediatorView): TComponent;
var
  L: TtiPropertyLinkDef;
begin
  L := FindByMediator(AMediator);
  if (L = nil) then
    Result := nil
  else
    Result := L.Component;
end;


{ TtiPropertyLinkDefs }

function TtiPropertyLinkDefs.GetD(Index: integer): TtiPropertyLinkDef;
begin
  Result := TtiPropertyLinkDef(Items[Index]);
end;

procedure TtiPropertyLinkDefs.SetD(Index: integer; const AValue: TtiPropertyLinkDef);
begin
  Items[Index] := Avalue;
end;

function TtiPropertyLinkDefs.GetOwner: TPersistent;
begin
  Result := FModelMediator;
end;

function TtiPropertyLinkDefs.AddPropertyLinkDef: TtiPropertyLinkDef;
begin
  Result := Add as TtiPropertyLinkDef;
end;

function TtiPropertyLinkDefs.IndexOfComponent(AComponent: TComponent): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Component <> AComponent) do
    Dec(Result);
end;

function TtiPropertyLinkDefs.IndexOfMediator(AMediator: TtiMediatorView): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Mediator <> AMediator) do
    Dec(Result);
end;

function TtiPropertyLinkDefs.IndexOfTag(ATag: LongInt): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Component.Tag <> ATag) do
    Dec(Result);
end;

function TtiPropertyLinkDefs.FindByComponent(AComponent: TComponent): TtiPropertyLinkDef;
var
  I: integer;
begin
  I := IndexOfComponent(AComponent);
  if (I = -1) then
    Result := nil
  else
    Result := GetD(I);
end;

function TtiPropertyLinkDefs.FindByMediator(AMediator: TtiMediatorView): TtiPropertyLinkDef;
var
  I: integer;
begin
  I := IndexOfMediator(AMediator);
  if (I = -1) then
    Result := nil
  else
    Result := GetD(I);
end;

function TtiPropertyLinkDefs.FindByTag(ATag: LongInt): TtiPropertyLinkDef;
var
  i: integer;
begin
  i := IndexOfTag(ATag);
  if (i = -1) then
    Result := nil
  else
    Result := GetD(i);
end;

{ TtiModelMediatorItem }

constructor TtiModelMediatorItem.Create(Collection: TCollection);
begin
  Assert(Collection is TtiModelMediatorCollection, 'Expected TtiModelMediatorCollection');
  inherited;
  FModelMediator := TtiModelMediator.Create(ModelMediatorList);
  // Streaming of sub-component properties.
  FModelMediator.SetSubComponent(True);
  FModelMediatorName := ModelMediatorCollection.NextName(CDefaultModelMediatorName);
end;

destructor TtiModelMediatorItem.Destroy;
begin
  FModelMediator.Free;
  inherited;
end;

function TtiModelMediatorItem.FindMediatorView(
  AComponent: TComponent): TtiMediatorView;
begin
  result := FModelMediator.FindMediatorView(AComponent);
end;

function TtiModelMediatorItem.GetDisplayName: string;
begin
  if FModelMediatorName <> '' then
    result := FModelMediatorName
  else
    result := inherited GetDisplayName;
end;

function TtiModelMediatorItem.ModelMediatorCollection: TtiModelMediatorCollection;
begin
  if Assigned(Collection) then
    result := (Collection as TtiModelMediatorCollection)
  else
    result := nil;
end;

function TtiModelMediatorItem.ModelMediatorList: TtiModelMediatorList;
var
  LModelMediatorCollection: TtiModelMediatorCollection;
begin
  LModelMediatorCollection := ModelMediatorCollection;
  if Assigned(LModelMediatorCollection) then
    result := LModelMediatorCollection.ModelMediatorList
  else
    result := nil;
end;

procedure TtiModelMediatorItem.SetModelMediatorName(const AValue: string);
begin
  if AValue <> FModelMediatorName then
  begin
    // Must have a name
    if AValue = '' then
      MediatorError(nil, SErrNoModelMediatorName);
    // Must be unique
    if ModelMediatorCollection.FindByName(AValue) <> nil then
      MediatorError(nil, SErrDuplicateModelMediatorName, [AValue]);
    FModelMediatorName := AValue;
  end;
end;

procedure TtiModelMediatorItem.Assign(Source: TPersistent);
var
  LSource: TtiModelMediatorItem;
begin
  inherited Assign(Source);
  if Source is TtiModelMediatorItem then
  begin
    LSource := Source as TtiModelMediatorItem;
    FModelMediator.Assign(LSource.ModelMediator);
    Subject := LSource.Subject;
    ModelMediatorName := LSource.ModelMediatorName;
  end;
end;

{ TtiModelMediatorCollection }

constructor TtiModelMediatorCollection.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FNextNameID := 0;
end;

function TtiModelMediatorCollection.FindBySubject(
  const ASubject: TtiObject): TtiModelMediatorItem;
var
  i: integer;
  LItem: TtiModelMediatorItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    LItem := Items[i];
    // Pre or post subject binding
    if (LItem.Subject = ASubject) or
       (LItem.ModelMediator.Subject = ASubject) then
    begin
      result := LItem;
      break; //==>
    end;
  end;
end;

function TtiModelMediatorCollection.FindByName(
  const AModelMediatorName: string): TtiModelMediatorItem;
var
  i: integer;
  LItem: TtiModelMediatorItem;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    LItem := Items[i];
    if LItem.ModelMediatorName = AModelMediatorName then
    begin
      result := LItem;
      break; //==>
    end;
  end;
end;

function TtiModelMediatorCollection.GetItem(
  Index: integer): TtiModelMediatorItem;
begin
  Result := TtiModelMediatorItem(inherited Items[Index]);
end;

function TtiModelMediatorCollection.GetOwner: TPersistent;
begin
  result := FModelMediatorList;
end;

function TtiModelMediatorCollection.NextName(const ANamePrefix: string): string;
begin
  repeat
    result := ANamePrefix + IntToStr(FNextNameID);
    Inc(FNextNameID);
  until FindByName(result) = nil;
end;

procedure TtiModelMediatorCollection.SetItem(Index: integer;
  const AValue: TtiModelMediatorItem);
begin
  TtiModelMediatorItem(inherited Items[Index]).Assign(AValue);
end;

{ TtiModelMediatorList }

constructor TtiModelMediatorList.Create(AOwner: TComponent);
begin
  inherited;
  FModelMediators := TtiModelMediatorCollection.Create(TtiModelMediatorItem);
  FModelMediators.FModelMediatorList := Self;
end;

destructor TtiModelMediatorList.Destroy;
begin
  FModelMediators.Free;
  inherited;
end;

function TtiModelMediatorList.FindCreate(
  const AModelMediatorName: string): TtiModelMediator;
var
  LItem: TtiModelMediatorItem;
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  LItem := ModelMediators.FindByName(AModelMediatorName);
  if not Assigned(LItem) then
  begin
    LItem := TtiModelMediatorItem.Create(ModelMediators);
    LItem.ModelMediatorName := AModelMediatorName;
  end;
  result := LItem.ModelMediator;
end;

function TtiModelMediatorList.FindCreate(
  const ASubject: TtiObject): TtiModelMediator;
var
  LItem: TtiModelMediatorItem;
begin
  Assert(ASubject.TestValid(TtiObject), CTIErrorInvalidObject);
  LItem := ModelMediators.FindBySubject(ASubject);
  if not Assigned(LItem) then
  begin
    LItem := TtiModelMediatorItem.Create(ModelMediators);
    LItem.Subject := ASubject;
    LItem.ModelMediator.Subject := ASubject;
  end;
  result := LItem.ModelMediator;
end;

function TtiModelMediatorList.FindMediatorView(
  AComponent: TComponent): TtiMediatorView;
var
  i: integer;
begin
  result := nil;
  for i := 0 to ModelMediators.Count - 1 do
  begin
    result := ModelMediators.Items[i].FindMediatorView(AComponent);
    if Assigned(result) then
      break;
  end;
end;

function TtiModelMediatorList.Add(
  const AModelMediatorName: string): TtiModelMediator;
begin
  result := FindCreate(AModelMediatorName);
end;

function TtiModelMediatorList.AddProperty(const ASubjectClass: TtiObjectClass;
  const AFieldName: string;
  const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := AddProperty(ASubjectClass.ClassName, AFieldName, AGUIComponent);
end;

function TtiModelMediatorList.AddProperty(const AModelMediatorName,
  AFieldName: string; const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := FindCreate(AModelMediatorName).AddProperty(AFieldName, AGUIComponent);
end;

function TtiModelMediatorList.AddProperty(const ASubject: TtiObject;
  const AFieldName: string;
  const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := FindCreate(ASubject).AddProperty(AFieldName, AGUIComponent);
end;

function TtiModelMediatorList.AddComposite(const ASubjectClass: TtiObjectClass;
  const ADisplayNames: string;
  const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := AddComposite(ASubjectClass.ClassName, ADisplayNames, AGUIComponent);
end;

function TtiModelMediatorList.AddComposite(const AModelMediatorName,
  ADisplayNames: string; const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := FindCreate(AModelMediatorName).AddComposite(ADisplayNames, AGUIComponent);
end;

function TtiModelMediatorList.AddComposite(const ASubject: TtiObject;
  const ADisplayNames: string;
  const AGUIComponent: TComponent): TtiPropertyLinkDef;
begin
  result := FindCreate(ASubject).AddComposite(ADisplayNames, AGUIComponent);
end;

function TtiModelMediatorList.GetActiveByClass(
  ASubjectClass: TtiObjectClass): boolean;
begin
  result := GetActiveByName(ASubjectClass.ClassName);
end;

function TtiModelMediatorList.GetActiveByName(
  AModelMediatorName: string): boolean;
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  result := GetItemByName(AModelMediatorName).ModelMediator.Active;
end;

function TtiModelMediatorList.GetActiveBySubject(ASubject: TtiObject): boolean;
begin
  result := GetItemBySubject(ASubject).ModelMediator.Active;
end;

function TtiModelMediatorList.GetAllActive: boolean;
var
  i: integer;
  LItem: TtiModelMediatorItem;
begin
  result := true;
  for i := 0 to ModelMediators.Count - 1 do
  begin
    LItem := ModelMediators.Items[i];
    if not LItem.ModelMediator.Active then
    begin
      result := false;
      break; //==>
    end;
  end;
end;

function TtiModelMediatorList.GetItemByName(
  const AModelMediatorName: string): TtiModelMediatorItem;
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  result := ModelMediators.FindByName(AModelMediatorName);
  if not Assigned(result) then
    MediatorError(Self, SErrNoModelMediator, [AModelMediatorName]);
end;

function TtiModelMediatorList.GetItemBySubject(
  const ASubject: TtiObject): TtiModelMediatorItem;
begin
  result := ModelMediators.FindBySubject(ASubject);
  if not Assigned(result) then
    MediatorError(Self, SErrNoModelMediator, [ASubject.ClassName + ' instance']);
end;

function TtiModelMediatorList.GetMediatorView(
  AComponent: TComponent): TtiMediatorView;
begin
  result := FindMediatorView(AComponent);
  if not Assigned(result) then
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

function TtiModelMediatorList.GetSelectedObject(
  AComponent: TComponent): TtiObject;
var
  LView: TtiMediatorView;
begin
  result := nil;
  LView := FindMediatorView(AComponent);
  if Assigned(LView) then
    result := LView.SelectedObject
  else
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

function TtiModelMediatorList.GetSubjectByClass(
  ASubjectClass: TtiObjectClass): TtiObject;
begin
  result := GetSubjectByName(ASubjectClass.ClassName);
end;

function TtiModelMediatorList.GetSubjectByName(
  AModelMediatorName: string): TtiObject;
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  result := GetItemByName(AModelMediatorName).ModelMediator.Subject;
end;

function TtiModelMediatorList.ModelMediator(
  const ASubjectClass: TtiObjectClass): TtiModelMediator;
begin
  result := ModelMediator(ASubjectClass.ClassName);
end;

function TtiModelMediatorList.ModelMediator(
  const AModelMediatorName: string): TtiModelMediator;
var
  LItem: TtiModelMediatorItem;
begin
  LItem := ModelMediators.FindByName(AModelMediatorName);
  if Assigned(LItem) then
    result := LItem.ModelMediator
  else
    result := nil;
end;

function TtiModelMediatorList.ModelMediator(
  const ASubject: TtiObject): TtiModelMediator;
var
  LItem: TtiModelMediatorItem;
begin
  LItem := ModelMediators.FindBySubject(ASubject);
  if Assigned(LItem) then
    result := LItem.ModelMediator
  else
    result := nil;
end;

procedure TtiModelMediatorList.SetActiveByClass(ASubjectClass: TtiObjectClass;
  const AValue: boolean);
begin
  SetActiveByName(ASubjectClass.ClassName, AValue);
end;

procedure TtiModelMediatorList.SetActiveByName(AModelMediatorName: string;
  const AValue: boolean);
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  GetItemByName(AModelMediatorName).ModelMediator.Active := AValue;
end;

procedure TtiModelMediatorList.SetActiveBySubject(ASubject: TtiObject;
  const AValue: boolean);
begin
  GetItemBySubject(ASubject).ModelMediator.Active := AValue;
end;

procedure TtiModelMediatorList.SetAllActive(const AValue: boolean);
var
  i: integer;
begin
  for i := 0 to ModelMediators.Count - 1 do
    ModelMediators.Items[i].ModelMediator.Active := AValue;
end;

procedure TtiModelMediatorList.SetModelMediators(
  const AValue: TtiModelMediatorCollection);
begin
  if FModelMediators = AValue then
    Exit;
  FModelMediators.Assign(AValue);
end;

procedure TtiModelMediatorList.SetSelectedObject(AComponent: TComponent;
  AObject: TtiObject);
var
  LView: TtiMediatorView;
begin
  LView := FindMediatorView(AComponent);
  if Assigned(LView) then
    LView.SelectedObject := AObject
  else
    MediatorError(nil, SErrNoMediatorViewForComponent, [AComponent.Name, AComponent.ClassName]);
end;

procedure TtiModelMediatorList.SetSubjectByClass(ASubjectClass: TtiObjectClass;
  const AValue: TtiObject);
begin
  SetSubjectByName(ASubjectClass.ClassName, AValue);
end;

procedure TtiModelMediatorList.SetSubjectByName(AModelMediatorName: string;
  const AValue: TtiObject);
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  GetItemByName(AModelMediatorName).ModelMediator.Subject := AValue;
end;

procedure TtiModelMediatorList.SubjectChanged;
var
  i: integer;
begin
  for i := 0 to ModelMediators.Count - 1 do
    ModelMediators.Items[i].ModelMediator.SubjectChanged;
end;

procedure TtiModelMediatorList.SubjectChanged(
  const ASubjectClass: TtiObjectClass);
begin
  SubjectChanged(ASubjectClass.ClassName);
end;

procedure TtiModelMediatorList.SubjectChanged(const ASubject: TtiObject);
begin
  GetItemBySubject(ASubject).ModelMediator.SubjectChanged;
end;

procedure TtiModelMediatorList.SubjectChanged(const AModelMediatorName: string);
begin
  Assert(AModelMediatorName <> '', 'AModelMediatorName cannot be blank');
  GetItemByName(AModelMediatorName).ModelMediator.SubjectChanged;
end;

end.


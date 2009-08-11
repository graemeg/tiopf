unit tiFormMediator;

{$I tiDefines.inc}

interface

uses
  Classes,
  SysUtils,
  tiObject,
  tiBaseMediator;

type
  TFormMediator = class;


  { TPropertyLinkDef }

  TPropertyLinkDef = class(TCollectionItem)
  private
    FComponent: TComponent;
    FComposite: Boolean;
    FObjectUpdateMoment: TObjectUpdateMoment;
    FFieldName: string;
    FMediator: TMediatorView;
    FMediatorDef: TMediatorDef;
    FOnGUIToObject: TGUIToObjectEvent;
    FOnObjectToGUI: TObjectToGUIEvent;
    FOnSetupMediator: TNotifyEvent;
    FValueList: TtiObjectList;
    procedure SetComponent(const AValue: TComponent);
    procedure SetComposite(const AValue: Boolean);
    procedure SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
    procedure SetFieldName(const AValue: string);
    procedure SetOnGUIToObject(const AValue: TGUIToObjectEvent);
    procedure SetOnObjectToGUI(const AValue: TObjectToGUIEvent);
    procedure SetValueList(const AValue: TtiObjectList);
  protected
    procedure CreateMediator; virtual;
    procedure FreeMediator(FreeDef: Boolean = True); virtual;
    property MediatorDef: TMediatorDef read FMediatorDef;
  public
    procedure Assign(Source: TPersistent); override;
    function FormMediator: TFormMediator;
    property Mediator: TMediatorView read FMediator;
    property ValueList: TtiObjectList read FValueList write SetValueList;
  published
    property Composite: Boolean read FComposite write SetComposite;
    property FieldName: string read FFieldName write SetFieldName;
    property Component: TComponent read FComponent write SetComponent;
    property ObjectUpdateMoment: TObjectUpdateMoment read FObjectUpdateMoment write SetObjectUpdateMoment;
    property OnGUIToObject: TGUIToObjectEvent read FOnGUIToObject write SetOnGUIToObject;
    property OnObjectToGUI: TObjectToGUIEvent read FOnObjectToGUI write SetOnObjectToGUI;
    Property OnSetupMediator : TNotifyEvent Read FOnSetupMediator Write FOnSetupMediator;
  end;


  { TPropertyLinkDefs }

  TPropertyLinkDefs = class(TCollection)
  private
    FFormMediator: TFormMediator;
    function GetD(Index: integer): TPropertyLinkDef;
    procedure SetD(Index: integer; const AValue: TPropertyLinkDef);
  public
    function GetOwner: TPersistent; override;
    function AddPropertyLinkDef: TPropertyLinkDef;
    function IndexOfComponent(AComponent: TComponent): integer;
    function IndexOfMediator(AMediator: TMediatorView): integer;
    function IndexOfTag(ATag: LongInt): integer;
    function FindByComponent(AComponent: TComponent): TPropertyLinkDef;
    function FindByMediator(AMediator: TMediatorView): TPropertyLinkDef;
    function FindByTag(ATag: LongInt): TPropertyLinkDef;
    property FormMediator: TFormMediator read FFormMediator;
    property Defs[Index: integer]: TPropertyLinkDef read GetD write SetD; default;
  end;


  { TFormMediator }

  TFormMediator = class(TComponent)
  private
    FActive: Boolean;
    FDefs: TPropertyLinkDefs;
    FSubject: TtiObject;
    procedure CreateMediators;
    procedure SetActive(const AValue: Boolean);
    procedure SetPropertyLinkDefs(const AValue: TPropertyLinkDefs);
    procedure SetSubject(const AValue: TtiObject);
  protected
    function CreatePropertyDefs: TPropertyLinkDefs; virtual;
    procedure CheckSubject;
    procedure CheckInactive;
    procedure CheckMediators;
    procedure Bind;
    procedure UnBind;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddProperty(const AFieldName: string; const AGUIComponent: TComponent): TPropertyLinkDef;
    function AddComposite(const ADisplayNames: string; const AGUIComponent: TComponent): TPropertyLinkDef;
    function FindByComponent(AComponent: TComponent): TPropertyLinkDef;
    function FindByMediator(AMediator: TMediatorView): TPropertyLinkDef;
    function FindByTag(ATag: LongInt): TPropertyLinkDef;
    function ComponentMediator(AComponent: TComponent): TMediatorView;
    function MediatorComponent(AMediator: TMediatorView): TComponent;
    property Subject: TtiObject read FSubject write SetSubject;
    property Active: Boolean read FActive write SetActive;
  published
    property PropertyLinks: TPropertyLinkDefs read FDefs write SeTPropertyLinkDefs;
  end;


implementation

resourcestring
  SErrNoSubject  = 'Cannot perform this operation if subject is not set.';
  SErrActive     = 'Cannot perform this operation while active.';
  SErrNoMediator = 'Cannot find a mediator for control %s (%s), property %s.';


{ TPropertyLinkDef }

procedure TPropertyLinkDef.SetComponent(const AValue: TComponent);
begin
  if FComponent = AValue then
    Exit;
  FComponent := AValue;
end;

procedure TPropertyLinkDef.SetComposite(const AValue: Boolean);
var
  F: TFormMediator;
begin
  if FComposite = AValue then
    Exit;
  F := FormMediator;
  if Assigned(F) then
    F.CheckInactive;
  FComposite := AValue;
end;

procedure TPropertyLinkDef.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  if FObjectUpdateMoment = AValue then
    Exit;
  FObjectUpdateMoment := AValue;
  if Assigned(FMediator) then
    FMediator.ObjectUpdateMoment := FObjectUpdateMoment;
end;

procedure TPropertyLinkDef.SetFieldName(const AValue: string);
var
  F: TFormMediator;
begin
  if FFieldName = AValue then
    Exit;
  F := FormMediator;
  if Assigned(F) then
    F.CheckInactive;
  FFieldName   := AValue;
  FMediatorDef := nil;
end;

procedure TPropertyLinkDef.SetOnGUIToObject(const AValue: TGUIToObjectEvent);
begin
  FOnGUIToObject := AValue;
  if Assigned(Mediator) then
    Mediator.OnGUIToObject := AValue;
end;

procedure TPropertyLinkDef.SetOnObjectToGUI(const AValue: TObjectToGUIEvent);
begin
  FOnObjectToGUI := AValue;
  if Assigned(Mediator) then
    Mediator.OnObjectToGUI := AValue;
end;

procedure TPropertyLinkDef.SetValueList(const AValue: TtiObjectList);
begin
  if FValueList = AValue then
    Exit;
  FValueList := AValue;
  if Assigned(FMediator) then
    FMediator.ValueList := FValueList;
end;

procedure TPropertyLinkDef.CreateMediator;
begin
  if (FMediatorDef = nil) then
    MediatorError(Self,SErrNoMediator, [Component.Name, Component.ClassName, FieldName]);
  FMediator           := FMediatorDef.MediatorClass.Create;
  FMediator.FieldName := FieldName;
  FMediator.OnGUIToObject := Self.OnGUIToObject;
  FMediator.OnObjectToGUI := Self.OnObjectToGUI;
  FMediator.GUIControl := Self.Component;
  FMediator.ValueList := Self.ValueList;
  FMediator.Subject   := FormMediator.Subject;
  FMediator.Active    := True;
  if Assigned(FOnSetupMediator) then
    FOnSetupMediator(FMediator);
end;

procedure TPropertyLinkDef.FreeMediator(FreeDef: Boolean = True);
begin
  FMediator.Active     := False;
  FMediator.Subject    := nil;
  FMediator.GUIControl := nil;
  FreeAndNil(FMediator);
  if FreeDef then
    FMediatorDef := nil;
end;

procedure TPropertyLinkDef.Assign(Source: TPersistent);
var
  D: TPropertyLinkDef;
begin
  if (Source is TPropertyLinkDef) then
  begin
    D          := Source as TPropertyLinkDef;
    FComponent := D.Component;
    FObjectUpdateMoment := D.ObjectUpdateMoment;
    FFieldName := D.FieldName;
    FOnGUIToObject := D.OnGUIToObject;
    FOnObjectToGUI := D.OnObjectToGUI;
    FValueList := D.ValueList;
    FCOmposite := D.Composite;
    { Not sure about those. Assign is normally only used in design,
      when these are not set.
      FMediator: TMediatorView;
      FMediatorDef : TMediatorDef;
    }
  end
  else
    inherited Assign(Source);
end;

function TPropertyLinkDef.FormMediator: TFormMediator;
begin
  Result := nil;
  if Assigned(Collection) then
    Result := (Collection as TPropertyLinkDefs).FormMediator;
end;


{ TFormMediator }

procedure TFormMediator.SetActive(const AValue: Boolean);
begin
  if (FActive = AValue) then
    Exit;
  CheckSubject;
  if AValue then
    Bind
  else
    UnBind;
  FActive := AValue;
  FSubject.NotifyObservers;
end;

procedure TFormMediator.SetPropertyLinkDefs(const AValue: TPropertyLinkDefs);
begin
  if FDefs = AValue then
    Exit;
  FDefs.Clear;
  FDefs.Assign(AValue);
end;

procedure TFormMediator.SetSubject(const AValue: TtiObject);
var
  I: integer;
begin
  if FSubject = AValue then
    Exit;
  FSubject := AValue;
  if (FSubject = nil) then
    Active := False
  else if Active then
  begin
    for I := 0 to FDefs.Count - 1 do
      if Assigned(FDefs[I].FMediator) then
        FDefs[I].FMediator.Subject := AValue;
    FSubject.NotifyObservers;
  end;
end;

procedure TFormMediator.CreateMediators;
var
  I: integer;
begin
  for I := 0 to FDefs.Count - 1 do
    FDefs[I].CreateMediator;
end;

procedure TFormMediator.UnBind;
var
  I: integer;
begin
  for I := 0 to FDefs.Count - 1 do
    FDefs[I].FreeMediator(True);
end;

procedure TFormMediator.Bind;
begin
  CheckMediators;
  CreateMediators;
end;

function TFormMediator.CreatePropertyDefs: TPropertyLinkDefs;
begin
  Result := TPropertyLinkDefs.Create(TPropertyLinkDef);
end;

procedure TFormMediator.CheckSubject;
begin
  if not Assigned(FSubject) then
    MediatorError(Self,SErrNoSubject);
end;

procedure TFormMediator.CheckInactive;
begin
  if Active then
    MediatorError(Self,SErrActive);
end;

procedure TFormMediator.CheckMediators;
var
  I: integer;
  D: TPropertyLinkDef;
begin
  for I := 0 to FDefs.Count - 1 do
  begin
    D := FDefs[i];
    if (D.FMediator = nil) then
    begin
      if D.Composite then
        D.FMediatorDef := gMediatorManager.FindDefFor(Subject, D.Component)
      else
        D.FMediatorDef := gMediatorManager.FindDefFor(Subject, D.Component, D.FieldName);
      if (D.FMediatorDef = nil) then
        MediatorError(Self,SErrNoMediator, [D.Component.Name, D.Component.ClassName, D.FieldName]);
    end;
  end;
end;

constructor TFormMediator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefs := CreatePropertyDefs;
  FDefs.FFormMediator := Self;
end;

destructor TFormMediator.Destroy;
begin
  Active := False;
  FreeAndNil(FDefs);
  inherited Destroy;
end;

function TFormMediator.AddProperty(const AFieldName: string; const AGUIComponent: TComponent): TPropertyLinkDef;
begin
  Result           := FDefs.AddPropertyLinkDef;
  Result.FieldName := AFieldName;
  Result.Component := AGUICOmponent;
end;

function TFormMediator.AddComposite(const ADisplayNames: string; const AGUIComponent: TComponent): TPropertyLinkDef;
begin
  Result           := AddProperty(ADisplayNames, AGUIComponent);
  Result.Composite := True;
end;

function TFormMediator.FindByComponent(AComponent: TComponent): TPropertyLinkDef;
begin
  Result := FDefs.FindByComponent(AComponent);
end;

function TFormMediator.FindByMediator(AMediator: TMediatorView): TPropertyLinkDef;
begin
  Result := FDefs.FindByMediator(AMediator);
end;

function TFormMediator.FindByTag(ATag: LongInt): TPropertyLinkDef;
begin
  Result := FDefs.FindByTag(ATag);
end;

function TFormMediator.ComponentMediator(AComponent: TComponent): TMediatorView;
var
  L: TPropertyLinkDef;
begin
  L := FindByComponent(AComponent);
  if (L = nil) then
    Result := nil
  else
    Result := L.Mediator;
end;

function TFormMediator.MediatorComponent(AMediator: TMediatorView): TComponent;
var
  L: TPropertyLinkDef;
begin
  L := FindByMediator(AMediator);
  if (L = nil) then
    Result := nil
  else
    Result := L.Component;
end;


{ TPropertyLinkDefs }

function TPropertyLinkDefs.GetD(Index: integer): TPropertyLinkDef;
begin
  Result := TPropertyLinkDef(Items[Index]);
end;

procedure TPropertyLinkDefs.SetD(Index: integer; const AValue: TPropertyLinkDef);
begin
  Items[Index] := Avalue;
end;

function TPropertyLinkDefs.GetOwner: TPersistent;
begin
  Result := FFormMediator;
end;

function TPropertyLinkDefs.AddPropertyLinkDef: TPropertyLinkDef;
begin
  Result := Add as TPropertyLinkDef;
end;

function TPropertyLinkDefs.IndexOfComponent(AComponent: TComponent): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Component <> AComponent) do
    Dec(Result);
end;

function TPropertyLinkDefs.IndexOfMediator(AMediator: TMediatorView): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Mediator <> AMediator) do
    Dec(Result);
end;

function TPropertyLinkDefs.IndexOfTag(ATag: LongInt): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (GetD(Result).Component.Tag <> ATag) do
    Dec(Result);
end;

function TPropertyLinkDefs.FindByComponent(AComponent: TComponent): TPropertyLinkDef;
var
  I: integer;
begin
  I := IndexOfComponent(AComponent);
  if (I = -1) then
    Result := nil
  else
    Result := GetD(I);
end;

function TPropertyLinkDefs.FindByMediator(AMediator: TMediatorView): TPropertyLinkDef;
var
  I: integer;
begin
  I := IndexOfMediator(AMediator);
  if (I = -1) then
    Result := nil
  else
    Result := GetD(I);
end;

function TPropertyLinkDefs.FindByTag(ATag: LongInt): TPropertyLinkDef;
var
  i: integer;
begin
  i := IndexOfTag(ATag);
  if (i = -1) then
    Result := nil
  else
    Result := GetD(i);
end;

end.


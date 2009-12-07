unit RelationshipManagerUnit;

interface

uses
  Contnrs,
  tiObject;

type
  TRelationshipManager = class;

  TBusinessObject = class(TtiObject)
    strict private
      FRelationshipManager: TRelationshipManager;
      procedure SetRelationshipManager(const Value: TRelationshipManager);
    public
      constructor Create; override;
      property RelationshipManager: TRelationshipManager read FRelationshipManager write SetRelationshipManager;
    end;

  TAssociationObject = class(TtiObject)
    strict private
      FSourceObject: TBusinessObject;
      FTargetObject: TBusinessObject;
      FRelType: Integer;
      procedure SetSourceObject(const Value: TBusinessObject);
      procedure SetTargetObject(const Value: TBusinessObject);
      procedure SetRelType(const Value: Integer);
    public
      constructor Create(AFromObject,AToObject: TBusinessObject); overload;
      constructor Create(AFromObject,AToObject: TBusinessObject; ARelType: Integer); reintroduce; overload;
      property SourceObject: TBusinessObject read FSourceObject write SetSourceObject;
      property TargetObject: TBusinessObject read FTargetObject write SetTargetObject;
      property RelType: Integer read FRelType write SetRelType;
    end;

  TAssociationObjectList = class(TtiObjectList)
    strict private
      function GetItems(AIndex: integer): TAssociationObject; reintroduce;
    public
      procedure Add(AAssociationObject: TAssociationObject); reintroduce;
      procedure Remove(AAssociationObject: TAssociationObject); reintroduce;
      property Items[AIndex: integer]: TAssociationObject read GetItems; default;
    end;

  TListStrategy = class(TObject)
    public
      constructor Create; virtual;
      function GetSourceList(ARelType: integer): TtiObjectList; virtual; abstract;
      function GetTargetList(ARelType: integer): TtiObjectList; virtual; abstract;
    end;

  TRelationshipManager = class(TObject)
    strict private
      FRelationships: TAssociationObjectList;
      FListStrategy: TListStrategy;
      procedure SetListStrategy(const Value: TListStrategy);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Read;
      procedure Save;
      procedure AddRelationship(ASourceObject,ATargetObject: TBusinessObject; ARelType: integer);
      procedure RemoveRelationship(ASourceObject,ATargetObject: TBusinessObject; ARelType: integer);
      procedure DeleteObjectRelationships(AObject: TBusinessObject);
      function RelationshipExists(ASourceObject,ATargetObject: TBusinessObject; ARelType: integer): boolean; overload;
      function FindSourceObject(ATargetObject: TBusinessObject; ARelType: integer): TBusinessObject;
      procedure FindSourceObjects(ATargetObject: TBusinessObject; ARelType: integer; AList: TtiObjectList);
      function FindTargetObject(ASourceObject: TBusinessObject; ARelType: integer): TBusinessObject;
      procedure FindTargetObjects(ASourceObject: TBusinessObject; ARelType: integer; AList: TtiObjectList);
      function GetSourceList(ARelType: integer): TtiObjectList;
      function GetTargetList(ARelType: integer): TtiObjectList;
      property ListStrategy: TListStrategy read FListStrategy write SetListStrategy;
      property Relationships: TAssociationObjectList read FRelationships;
    end;

  function GetRelationshipManager: TRelationshipManager;

implementation

uses
  SysUtils;

var
  RM: TRelationshipManager;

function GetRelationshipManager: TRelationshipManager;
begin
  if not Assigned(RM) then
    RM := TRelationshipManager.Create;
  result := RM;
end;

{ TBusinessObject }

constructor TBusinessObject.Create;
begin
  inherited;
  FRelationshipManager := GetRelationshipManager;
end;

procedure TBusinessObject.SetRelationshipManager(
  const Value: TRelationshipManager);
begin
  FRelationshipManager := Value;
end;

{ TAssociationObject }

constructor TAssociationObject.Create(AFromObject, AToObject: TBusinessObject;
  ARelType: Integer);
begin
  inherited Create;
  FSourceObject := AFromObject;
  FTargetObject := AToObject;
  FRelType := ARelType;
end;

procedure TAssociationObject.SetSourceObject(const Value: TBusinessObject);
begin
  FSourceObject := Value;
end;

procedure TAssociationObject.SetRelType(const Value: Integer);
begin
  FRelType := Value;
end;

procedure TAssociationObject.SetTargetObject(const Value: TBusinessObject);
begin
  FTargetObject := Value;
end;

constructor TAssociationObject.Create(AFromObject, AToObject: TBusinessObject);
begin
  Create(AFromObject,AToObject,1);
end;

{ TAssociationObjectList }

procedure TAssociationObjectList.Add(AAssociationObject: TAssociationObject);
begin
  inherited Add(AAssociationObject);
end;

function TAssociationObjectList.GetItems(AIndex: integer): TAssociationObject;
begin
  result := TAssociationObject(inherited Items[AIndex]);
end;

procedure TAssociationObjectList.Remove(AAssociationObject: TAssociationObject);
begin
  inherited Remove(AAssociationObject);
end;

{ TListStrategy }

constructor TListStrategy.Create;
begin
  inherited;
end;

{ TRelationshipManager }

constructor TRelationshipManager.Create;
begin
  FRelationships := TAssociationObjectList.Create;
end;

destructor TRelationshipManager.Destroy;
begin
  FreeAndNil(FRelationships);
  inherited;
end;

procedure TRelationshipManager.DeleteObjectRelationships(
  AObject: TBusinessObject);
var
  i: integer;
begin
  for i := pred(FRelationships.Count) downto 0 do
    if (FRelationships[i].SourceObject = AObject) or
       (FRelationships[i].TargetObject = AObject) then
       FRelationships[i].Deleted := true;
end;

function TRelationshipManager.FindSourceObject(ATargetObject: TBusinessObject;
  ARelType: integer): TBusinessObject;
var
  i: integer;
begin
  result := nil;
  for i := 0 to pred(FRelationships.Count) do
    if (FRelationships[i].TargetObject = ATargetObject) and
       (FRelationships[i].RelType = ARelType) and
       (FRelationships[i].Deleted = false) then
      begin
        result := FRelationships[i].SourceObject;
        break;
      end;
end;

procedure TRelationshipManager.FindSourceObjects(ATargetObject: TBusinessObject;
  ARelType: integer; AList: TtiObjectList);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to pred(FRelationships.Count) do
    if (FRelationships[i].TargetObject = ATargetObject) and
       (FRelationships[i].RelType = ARelType) and
       (FRelationships[i].Deleted = false) then
      AList.Add(FRelationships[i].SourceObject);
end;

function TRelationshipManager.FindTargetObject(ASourceObject: TBusinessObject;
  ARelType: integer): TBusinessObject;
var
  i: integer;
begin
  result := nil;
  for i := 0 to pred(FRelationships.Count) do
    if (FRelationships[i].SourceObject = ASourceObject) and
       (FRelationships[i].RelType = ARelType) and
       (FRelationships[i].Deleted = false) then
      begin
        result := FRelationships[i].TargetObject;
        break;
      end;
end;

procedure TRelationshipManager.FindTargetObjects(ASourceObject: TBusinessObject;
  ARelType: integer; AList: TtiObjectList);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to pred(FRelationships.Count) do
    if (FRelationships[i].SourceObject = ASourceObject) and
       (FRelationships[i].RelType = ARelType) and
       (FRelationships[i].Deleted = false) then
      AList.Add(FRelationships[i].TargetObject);
end;

function TRelationshipManager.GetSourceList(ARelType: integer): TtiObjectList;
begin
  result := FListStrategy.GetSourceList(ARelType);
end;

function TRelationshipManager.GetTargetList(ARelType: integer): TtiObjectList;
begin
  result := FListStrategy.GetTargetList(ARelType);
end;

procedure TRelationshipManager.AddRelationship(ASourceObject,
  ATargetObject: TBusinessObject; ARelType: integer);
var
  Association: TAssociationObject;
begin
  if Assigned(ASourceObject) and
     Assigned(ATargetObject) and
     (not RelationshipExists(ASourceObject,ATargetObject,ARelType)) then
      begin
        Association := TAssociationObject.CreateNew;
        Association.SourceObject := ASourceObject;
        Association.TargetObject := ATargetObject;
        Association.RelType := ARelType;

        FRelationships.Add(Association);
      end;
end;

function TRelationshipManager.RelationshipExists(ASourceObject,
  ATargetObject: TBusinessObject; ARelType: integer): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to pred(FRelationships.Count) do
    if (FRelationships[i].SourceObject = ASourceObject) and
       (FRelationships[i].TargetObject = ATargetObject) and
       (FRelationships[i].RelType = ARelType) and
       (FRelationships[i].Deleted = false) then
        begin
          result := true;
          break;
        end;
end;

procedure TRelationshipManager.RemoveRelationship(ASourceObject,
  ATargetObject: TBusinessObject; ARelType: integer);
var
  i: Integer;
begin
  for i := pred(FRelationships.Count) downto 0 do
    if (FRelationships[i].SourceObject = ASourceObject) and
       (FRelationships[i].TargetObject = ATargetObject) and
       (FRelationships[i].RelType = ARelType) then
       begin
        FRelationships[i].Deleted := true;
        break;
       end;
end;

procedure TRelationshipManager.Read;
begin
  FRelationships.Clear;
  FRelationships.Read;
end;

procedure TRelationshipManager.Save;
begin
  FRelationships.Save;
end;

procedure TRelationshipManager.SetListStrategy(const Value: TListStrategy);
begin
  FListStrategy := Value;
end;

initialization

finalization
  FreeAndNil(RM);

end.

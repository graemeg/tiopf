// Note, this pas file will not compile on it's own. It is designed to be
// included into tiOIDAbs when the conditional define OID_AS_INT64
// is used. See tiOIDAbs for details.

interface

uses
  Classes
  ,tiBaseObject
  ,Contnrs
  ,tiVisitor
 ;

const
  cDefaulTtiOIDFieldName       = 'OID';

type

  TtiOID = Int64;
  TtiOIDGenerator = class(TtiBaseObject)
  public
    function NextOID: TtiOID; virtual; abstract;
  end;

  TtiOIDAsInt64Generator = class(TtiOIDGenerator)
  private
    FList : TObjectList;
  protected
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    NextOID: TtiOID; override;
    procedure   UnloadNexTtiOIDGenerator(const ADatabaseName : string);
    function    FindByDatabaseName(const ADatabaseName : string): TtiBaseObject;
  end;

  TtiOIDGeneratorClass = class of TtiOIDAsInt64Generator;

  TtiOIDList = class(TtiBaseObject)
  private
    FOIDs: array of TtiOID;
    FParent: TtiBaseObject;
    FSize, FIncrement: integer;
    function    GetItems(const i: Integer): TtiOID;
    procedure   SetItems(const i: Integer; const AValue: TtiOID);
    function    GetCount: Integer;
    procedure SetParent(const AValue: TtiBaseObject);
    procedure Delete(const i: integer);
  public
    constructor Create(const AIncrement: integer = 4); virtual;
    procedure   Add(const AValue: TtiOID);
    procedure   Clear;
    function    IndexOf(const AValue: TtiOID): Integer;
    function    Remove(const AValue: TtiOID): Integer;
    property    Items[const i: Integer]: TtiOID Read GetItems Write SetItems; default;
    property    Count: Integer Read GetCount;
    property    Parent: TtiBaseObject read FParent write SetParent;
  end;

  function OIDToString(AOID : TtiOID): string;
  function OIDEquals(AOID1, AOID2 : TtiOID): boolean;
  function OIDCompare(AOID1, AOID2 : TtiOID): Integer;

implementation

uses
  tiQuery
  ,SysUtils
  ,tiUtils
  ,tiOPFManager
  ,tiDialogs
  ,tiObject
  ,tiVisitorDB
  ,tiConstants
  ,tiPersistenceLayers
  ,tiExcept
 ;

type

  TNexTtiOIDGenerator = class(TtiObject)
  private
    FHigh : TtiOID;
    FLow : TtiOID;
    FLowRange: TtiOID;
    FDirty: boolean;
    FDatabaseName: string;
    procedure SetHigh(const AValue: TtiOID);
  public
    constructor Create; override;
    function NexTtiOID : TtiOID;
  published
    property High    : TtiOID read FHigh  write SetHigh;
    property Low     : TtiOID read FLow;
    property LowRange : TtiOID read FLowRange write FLowRange;
    property DatabaseName : string read FDatabaseName write FDatabaseName;
  end;

  TVisDBNexTtiOIDAmblerRead = class(TtiObjectVisitor)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisDBNexTtiOIDAmblerUpdate = class(TtiObjectVisitor)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

const
  cuLowRange = 100;

function OIDToString(AOID : TtiOID): string;
begin
  result := IntToStr(AOID);
end;

function OIDEquals(AOID1, AOID2 : TtiOID): boolean;
begin
  result := AOID1 = AOID2;
end;

function OIDCompare(AOID1, AOID2 : TtiOID): Integer;
begin
  if AOID1 < AOID2 then
    result := -1
  else if AOID1 > AOID2 then
    result := 1
  else
    result := 0;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNexTtiOIDGenerator
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNexTtiOIDGenerator.Create;
begin
  inherited;
  FLow := 0;
  FHigh := 0;
  FLowRange := cuLowRange;
  FDirty := true;
end;

function TNexTtiOIDGenerator.NexTtiOID: TtiOID;
begin
  if FDirty then
    gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, Self);

  Inc(FLow);
  if FLow >= FLowRange then
    FDirty := true;

  result := (FHigh * FLowRange) + FLow;

end;

procedure TNexTtiOIDGenerator.SetHigh(const AValue : TtiOID);
begin
  FHigh := AValue;
  FLow := 0;
  FDirty := false;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiOIDAsInt64Generator
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiOIDAsInt64Generator.Create;
begin
  inherited create;
  FList := TObjectList.Create;
end;

destructor TtiOIDAsInt64Generator.destroy;
begin
  FList.Free;
  inherited;
end;

function TtiOIDAsInt64Generator.FindByDatabaseName(const ADatabaseName: string): TtiBaseObject;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(ADatabaseName,
                 TNexTtiOIDGenerator(FList.Items[i]).DatabaseName) then
    begin
      result := TNexTtiOIDGenerator(FList.Items[i]);
      Exit; //==>
    end;

  if result = nil then
  begin
    result := TNexTtiOIDGenerator.Create;
    TNexTtiOIDGenerator(result).DatabaseName := ADatabaseName;
    FList.Add(result);
  end;

end;

function TtiOIDAsInt64Generator.NextOID: TtiOID;
var
  lNexTtiOIDGenerator : TNexTtiOIDGenerator;
  lDatabaseName : string;
begin
  lDatabaseName := gTIOPFManager.DefaultDBConnectionName;
  lNexTtiOIDGenerator := (FindByDatabaseName(lDatabaseName) as TNexTtiOIDGenerator);
  Assert(lNexTtiOIDGenerator <> nil,
         'No NexTtiOIDGenerator found for ' + lDatabaseName);
  Result := lNexTtiOIDGenerator.NexTtiOID;
end;

procedure TtiOIDAsInt64Generator.UnloadNexTtiOIDGenerator(const ADatabaseName: string);
var
  lNexTtiOIDGenerator : TNexTtiOIDGenerator;
begin
  lNexTtiOIDGenerator := (FindByDatabaseName(ADatabaseName) as TNexTtiOIDGenerator);
  Assert(lNexTtiOIDGenerator <> nil,
         'No NexTtiOIDGenerator found for ' + ADatabaseName);
  FList.Remove(lNexTtiOIDGenerator);
end;

function TVisDBNexTtiOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := (TtiVisited(Visited)is TNexTtiOIDGenerator);
end;

procedure TVisDBNexTtiOIDAmblerRead.Execute(const AData: TtiVisited);
begin

  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow('Next_OID', nil);
  try
    TNexTtiOIDGenerator(Visited).High := Query.FieldAsInteger[ 'OID' ];
  finally
    Query.Close;
  end;
end;

{ TVisDBNexTtiOIDAmblerUpdate }

function TVisDBNexTtiOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := (TtiVisited(Visited) is TNexTtiOIDGenerator);
end;

procedure TVisDBNexTtiOIDAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams : TtiQueryParams;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', TNexTtiOIDGenerator(Visited).High + 1);
    Query.UpdateRow('Next_OID', lParams, nil);
  finally
    lParams.Free;
  end;
end;


{ TtiOIDList }

procedure TtiOIDList.Add(const AValue: TtiOID);
begin

  if FSize > High(FOIDs) then
    SetLength(FOIDs, Length(FOIDs) + FIncrement);

  FOIDs[FSize] := AValue;
  Inc(FSize);
end;


procedure TtiOIDList.Clear;
begin
  SetLength(FOIDs, 0);
  FSize := 0;
end;


constructor TtiOIDList.Create(const AIncrement: integer);
begin
  inherited Create;
  Assert(AIncrement > 0);
  FIncrement := AIncrement;
end;


procedure TtiOIDList.Delete(const i: integer);
begin

  if i < FSize then
    Move(FOIDs[i + 1], FOIDs[i], (FSize - i - 1) * SizeOf(TtiOID));

  Dec(FSize);

  if FSize < Length(FOIDs) - FIncrement then
    SetLength(FOIDs, Length(FOIDs) - FIncrement);

end;

function TtiOIDList.GetCount: Integer;
begin
  Result := FSize;
end;


function TtiOIDList.GetItems(const i: Integer): TtiOID;
begin
  Assert( (i > 0) and (i < FSize) );
  Result := FOIDs[i];
end;


function TtiOIDList.IndexOf(const AValue: TtiOID): integer;
var
  i: Integer;
begin
  for i:= FSize - 1 downto 0 do
    if FOIDs[i] = AValue then
    begin
      Result:= i;
      Exit; //==>
    end;
  Result:= -1;
end;


function TtiOIDList.Remove(const AValue: TtiOID): integer;
var
  LIndex: Integer;
begin
  LIndex:= IndexOf(AValue);
  if LIndex = -1 then
    raise EtiOPFDataException.CreateFmt(
      'Attempt to remove an integer from %s that does not exist: "%d"',
      [ClassName, AValue]);
  Delete(LIndex);
  Result:= LIndex;
end;


procedure TtiOIDList.SetItems(const i: Integer; const AValue: TtiOID);
begin
  Assert( (i > 0) and (i < FSize) );
  FOIDs[i] := AValue;
end;


procedure TtiOIDList.SetParent(const AValue: TtiBaseObject);
begin
  FParent := AValue;
end;

initialization
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNexTtiOIDAmblerRead);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNexTtiOIDAmblerUpdate);



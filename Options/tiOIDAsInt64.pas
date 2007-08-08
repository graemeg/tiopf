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
  cDefaultOIDFieldName       = 'OID';

type

  TOID = Int64;
  TNextOIDMgr = class(TtiBaseObject)
  private
    FList : TObjectList;
  protected
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    NextOID(const ADatabaseName : string = ''): TOID; virtual;
    procedure   UnloadNextOIDGenerator(const ADatabaseName : string);
    function    FindByDatabaseName(const ADatabaseName : string): TtiBaseObject;
  end;

  function OIDToString(AOID : TOID): string;
  function OIDEquals(AOID1, AOID2 : TOID): boolean;
  function OIDCompare(AOID1, AOID2 : TOID): Integer;

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
 ;

type

  TNextOIDGenerator = class(TtiObject)
  private
    FHigh : TOID;
    FLow : TOID;
    FLowRange: TOID;
    FDirty: boolean;
    FDatabaseName: string;
    procedure SetHigh(const AValue: TOID);
  public
    constructor Create; override;
    function NextOID : TOID;
  published
    property High    : TOID read FHigh  write SetHigh;
    property Low     : TOID read FLow;
    property LowRange : TOID read FLowRange write FLowRange;
    property DatabaseName : string read FDatabaseName write FDatabaseName;
  end;

  TVisDBNextOIDAmblerRead = class(TtiObjectVisitor)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisDBNextOIDAmblerUpdate = class(TtiObjectVisitor)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

const
  cuLowRange = 100;

function OIDToString(AOID : TOID): string;
begin
  result := IntToStr(AOID);
end;

function OIDEquals(AOID1, AOID2 : TOID): boolean;
begin
  result := AOID1 = AOID2;
end;

function OIDCompare(AOID1, AOID2 : TOID): Integer;
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
// * TNextOIDGenerator
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNextOIDGenerator.Create;
begin
  inherited;
  FLow := 0;
  FHigh := 0;
  FLowRange := cuLowRange;
  FDirty := true;
end;

function TNextOIDGenerator.NextOID: TOID;
begin
  if FDirty then
    gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, Self);

  Inc(FLow);
  if FLow >= FLowRange then
    FDirty := true;

  result := (FHigh * FLowRange) + FLow;

end;

procedure TNextOIDGenerator.SetHigh(const AValue : TOID);
begin
  FHigh := AValue;
  FLow := 0;
  FDirty := false;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDMgr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TNextOIDMgr.Create;
begin
  inherited create;
  FList := TObjectList.Create;
end;

destructor TNextOIDMgr.destroy;
begin
  FList.Free;
  inherited;
end;

function TNextOIDMgr.FindByDatabaseName(const ADatabaseName: string): TtiBaseObject;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(ADatabaseName,
                 TNextOIDGenerator(FList.Items[i]).DatabaseName) then
    begin
      result := TNextOIDGenerator(FList.Items[i]);
      Exit; //==>
    end;

  if result = nil then
  begin
    result := TNextOIDGenerator.Create;
    TNextOIDGenerator(result).DatabaseName := ADatabaseName;
    FList.Add(result);
  end;

end;

{
function TNextOIDMgr.NewPerObjAbs(AClass : TtiClass): TtiObject;
begin
  result    := AClass.Create;
  result.OID := NextOID;
  result.ObjectState := posCreate;
end;
}
function TNextOIDMgr.NextOID(const ADatabaseName : string = ''): TOID;
var
  lNextOIDGenerator : TNextOIDGenerator;
  lDatabaseName : string;
begin
  if ADatabaseName = '' then
    lDatabaseName := gTIOPFManager.DefaultDBConnectionName
  else
    lDatabaseName := ADatabaseName;
  lNextOIDGenerator := (FindByDatabaseName(lDatabaseName) as TNextOIDGenerator);
  Assert(lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + lDatabaseName);
  result := lNextOIDGenerator.NextOID;
end;

procedure TNextOIDMgr.UnloadNextOIDGenerator(const ADatabaseName: string);
var
  lNextOIDGenerator : TNextOIDGenerator;
begin
  lNextOIDGenerator := (FindByDatabaseName(ADatabaseName) as TNextOIDGenerator);
  Assert(lNextOIDGenerator <> nil,
         'No NextOIDGenerator found for ' + ADatabaseName);
  FList.Remove(lNextOIDGenerator);
end;

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := (TtiVisited(Visited)is TNextOIDGenerator);
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
begin

  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow('Next_OID', nil);
  try
    TNextOIDGenerator(Visited).High := Query.FieldAsInteger[ 'OID' ];
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := (TtiVisited(Visited) is TNextOIDGenerator);
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const AData: TtiVisited);
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
    lParams.SetValueAsInteger('OID', TNextOIDGenerator(Visited).High + 1);
    Query.UpdateRow('Next_OID', lParams, nil);
  finally
    lParams.Free;
  end;
end;


initialization
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);



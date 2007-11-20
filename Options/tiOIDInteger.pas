unit tiOIDInteger;

{$I tiDefines.inc}  

{$IFNDEF OID_AS_INT64}

interface
uses
  tiOID
  ,tiBaseObject
  ,tiObject
  ,tiVisitorDB
  ,tiVisitor
  ,SyncObjs
 ;

type

  TOIDInteger = class(TOID)
  private
    FAsInteger : integer;
  protected
    function  GetAsString: ShortString; override;
    procedure SetAsString(const AValue: ShortString); override;
    function  GetAsVariant: Variant;override;
    procedure SetAsVariant(const AValue: Variant);override;
  public
    function  IsNull : boolean; override;
    procedure AssignToTIQueryParam(const AFieldName : string; const AParams : TtiBaseObject); override;
    procedure AssignToTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    procedure AssignFromTIQuery(const AFieldName : string; const AQuery : TtiBaseObject); override;
    function  EqualsQueryField(const AFieldName : string; const AQuery : TtiBaseObject): boolean; override;
    procedure Assign(const ASource : TOID); override;
    function  Compare(const ACompareWith : TOID): integer; override;
    procedure SetToNull; override;
    function  NullOIDAsString: String; override;
    property  AsInteger : Integer read FAsInteger write FAsInteger;
  end;

  TNextOIDData = class(TtiObject)
  private
    FNextOID: integer;
  public
    property NextOID : integer read FNextOID write FNextOID;
  end;

  TNextOIDGeneratorInteger = class(TNextOIDGenerator)
  private
    FLow : Integer;
    FLowRange: Integer;
    FDirty: boolean;
    FNextOIDData : TNextOIDData;
    FCritSect: TCriticalSection;
    function NextOID(const ADatabaseName : string; APersistenceLayerName : string): Integer;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   AssignNextOID(const AAssignTo : TOID; const ADatabaseName : string; APersistenceLayerName : string); override;
  end;

  TVisDBNextOIDAmbler = class(TtiObjectVisitor)
  protected
    function CanSupportMultiUser: Boolean;
  end;

  TVisDBNextOIDAmblerRead = class(TVisDBNextOIDAmbler)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisDBNextOIDAmblerUpdate = class(TVisDBNextOIDAmbler)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

  TVisDBNextOIDSql = class(TVisDBNextOIDAmbler)
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AData : TtiVisited); override;
  end;

const
  cOIDClassNameInteger = 'OIDClassNameInteger';

implementation

uses
  tiQuery
  ,tiOPFManager
  ,tiConstants
  ,tiPersistenceLayers
  ,SysUtils
 ;

{ TOIDInteger }

function TOIDInteger.getAsString: ShortString;
begin
  result := IntToStr(FAsInteger);
end;

procedure TOIDInteger.SetAsString(const AValue: ShortString);
begin
  if AValue <> '' then
    FAsInteger := StrToInt(AValue)
  else
    FAsInteger := 0;
end;

function TOIDInteger.IsNull: boolean;
begin
  result := FAsInteger = cNullOIDInteger;
end;

procedure TOIDInteger.AssignFromTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  FAsInteger := lQuery.FieldAsInteger[ AFieldName ];
end;

procedure TOIDInteger.AssignToTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  lQuery.ParamAsInteger[ AFieldName ]:= FAsInteger;
end;

function TOIDInteger.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  result := (FAsInteger = lQuery.FieldAsInteger[ AFieldName ]);
end;

procedure TOIDInteger.Assign(const ASource: TOID);
begin
  AsString := ASource.AsString;
end;

function TOIDInteger.Compare(const ACompareWith: TOID): integer;
begin
  Assert(ACompareWith is TOIDInteger, 'ACompareWith not a ACompareWith');
  if AsInteger < TOIDInteger(ACompareWith).AsInteger then
    result := -1
  else if AsInteger > TOIDInteger(ACompareWith).AsInteger then
    result := 1
  else
    result := 0;
end;

const
  cuLowRange = 100;
  cMaxRetries = 10;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TNextOIDGeneratorInteger
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TNextOIDGeneratorInteger.AssignNextOID(const AAssignTo: TOID; const ADatabaseName : string; APersistenceLayerName : string);
begin
  Assert(AAssignTo.TestValid(TOID), CTIErrorInvalidObject);
  AAssignTo.AsString := IntToStr(NextOID(ADatabaseName, APersistenceLayerName));
end;

constructor TNextOIDGeneratorInteger.Create;
begin
  inherited;
  FLow := 0;
  FLowRange := cuLowRange;
  FDirty := true;
  FNextOIDData := TNextOIDData.Create;
  FCritSect:= TCriticalSection.Create;
end;

destructor TNextOIDGeneratorInteger.Destroy;
begin
  FCritSect.Free;
  FNextOIDData.Free;
  inherited;
end;

function TNextOIDGeneratorInteger.NextOID(const ADatabaseName : string; APersistenceLayerName : string): Integer;
begin
  FCritSect.Enter;
  try
    if FDirty then
    begin
      gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData, ADatabaseName, APersistenceLayerName);
      FDirty := false;
    end;

    result := (FNextOIDData.NextOID * FLowRange) + FLow;

    Inc(FLow);
    if FLow = FLowRange then
    begin
      FDirty := true;
      FLow := 0;
    end;
  finally
    FCritSect.Leave;
  end;
end;

procedure TOIDInteger.SetToNull;
begin
  FAsInteger := cNullOIDInteger;
end;

function TOIDInteger.GetAsVariant: Variant;
begin
  result := FAsInteger;
end;

procedure TOIDInteger.SetAsVariant(const AValue: Variant);
begin
  FAsInteger := AValue;
end;

function TOIDInteger.NullOIDAsString: String;
begin
  result := IntToStr(cNullOIDInteger);
end;

procedure TOIDInteger.AssignToTIQueryParam(const AFieldName: string; const AParams : TtiBaseObject);
var
  lParams : TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsInteger(AFieldName, FAsInteger);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDData) and not CanSupportMultiUser;
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
    TNextOIDData(Visited).NextOID := Query.FieldAsInteger[ 'OID' ];
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDData) and not CanSupportMultiUser;
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
    lParams.SetValueAsInteger('OID', Integer(TNextOIDData(Visited).NextOID + 1));
    Query.UpdateRow('Next_OID', lParams, nil);
  finally
    lParams.Free;
  end;
end;

{ TVisDBNextOIDSql }

function TVisDBNextOIDSql.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDData) and CanSupportMultiUser;
end;

procedure TVisDBNextOIDSql.Execute(const AData: TtiVisited);
var i: integer;
begin
  if gTIOPFManager.Terminated then
    Exit;

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit;

  Query.SQL.Text:= 'update Next_OID set OID = OID + 1';
  for i := 0 to cMaxRetries do
  begin
    try
      Query.ExecSQL;
      break;
    except
      // database may be locked, wait and try again later
      Sleep(50);
    end;
  end;

  Query.SelectRow('Next_OID', nil);
  try
    TNextOIDData(Visited).NextOID:= Query.FieldAsInteger[ 'OID' ] - 1;
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDAmbler }

function TVisDBNextOIDAmbler.CanSupportMultiUser: Boolean;
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  Assert(Persistencelayer<>nil, 'FPersistenceLayer not assigned');
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    PersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    Result:= LDefaults.CanSupportMultiUser;
  finally
    LDefaults.Free;
  end;
end;

{ TNextOIDData }

initialization

  gTIOPFManager.OIDFactory.RegisterMapping(cOIDClassNameInteger, TOIDInteger, TNextOIDGeneratorInteger) ;
  if gTIOPFManager.DefaultOIDClassName = '' then
    gTIOPFManager.DefaultOIDClassName := cOIDClassNameInteger;

  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDSql);
{$ELSE}
interface
implementation
{$ENDIF}
end.

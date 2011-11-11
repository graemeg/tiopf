unit tiOIDInteger;

{
  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorInteger.Create;
}

{$I tiDefines.inc}

interface

uses
  tiOID,
  tiBaseObject,
  tiObject,
  tiVisitorDB,
  tiVisitor,
  SyncObjs;

type

  TOIDInteger = class(TtiOID)
  private
    FAsInteger: integer;
  protected
    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
    function GetAsVariant: variant; override;
    procedure SetAsVariant(const AValue: variant); override;
  public
    function IsNull: boolean; override;
    procedure AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject); override;
    procedure AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject); override;
    procedure AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject); override;
    function EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean; override;
    procedure Assign(const ASource: TtiOID); override;
    function Compare(const ACompareWith: TtiOID): integer; override;
    procedure SetToNull; override;
    function NullOIDAsString: string; override;
    property AsInteger: integer read FAsInteger write FAsInteger;
  end;

  TNextOIDData = class(TtiObject)
  private
    FNextOID: integer;
  public
    property NextOID: integer read FNextOID write FNextOID;
  end;

  TtiOIDGeneratorInteger = class(TtiOIDGenerator)
  private
    FLow:         integer;
    FLowRange:    integer;
    FDirty:       boolean;
    FNextOIDData: TNextOIDData;
    FCritSect:    TCriticalSection;
    function NextOID(const ADatabaseAliasName: string; const APersistenceLayerName: string): integer;
  public
    constructor Create; override;
    constructor CreateEx(const ALowRange: integer); reintroduce;
    destructor Destroy; override;
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

  TVisDBNextOIDAmbler = class(TtiObjectVisitor)
  protected
    function CanSupportMultiUser: boolean;
  end;

  TVisDBNextOIDAmblerRead = class(TVisDBNextOIDAmbler)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisDBNextOIDAmblerUpdate = class(TVisDBNextOIDAmbler)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisDBNextOIDSql = class(TVisDBNextOIDAmbler)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

implementation

uses
  tiQuery,
  tiOPFManager,
  tiConstants,
  tiPersistenceLayers,
  SysUtils;

{ TOIDInteger }

function TOIDInteger.getAsString: String;
begin
  Result := IntToStr(FAsInteger);
end;

procedure TOIDInteger.SetAsString(const AValue: String);
begin
  if AValue <> '' then
    FAsInteger := StrToInt(AValue)
  else
    FAsInteger := 0;
end;

function TOIDInteger.IsNull: boolean;
begin
  Result := FAsInteger = cNullOIDInteger;
end;

procedure TOIDInteger.AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery     := TtiQuery(AQuery);
  FAsInteger := lQuery.FieldAsInteger[AFieldName];
end;

procedure TOIDInteger.AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  if IsNull then
    lQuery.ParamIsNull[AFieldName] := True
  else
    lQuery.ParamAsInteger[AFieldName] := FAsInteger;
end;

function TOIDInteger.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  Result := (FAsInteger = lQuery.FieldAsInteger[AFieldName]);
end;

procedure TOIDInteger.Assign(const ASource: TtiOID);
begin
  AsString := ASource.AsString;
end;

function TOIDInteger.Compare(const ACompareWith: TtiOID): integer;
begin
  Assert(ACompareWith is TOIDInteger, 'ACompareWith not a ACompareWith');
  if AsInteger < TOIDInteger(ACompareWith).AsInteger then
    Result := -1
  else if AsInteger > TOIDInteger(ACompareWith).AsInteger then
    Result := 1
  else
    Result := 0;
end;

const
  cuLowRange  = 100;
  cMaxRetries = 10;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiOIDGeneratorInteger
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiOIDGeneratorInteger.AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  AAssignTo.AsString := IntToStr(NextOID(ADBConnectionName, APersistenceLayerName));
end;

constructor TtiOIDGeneratorInteger.Create;
begin
  inherited;
  FLow         := 0;
  FLowRange    := cuLowRange;
  FDirty       := True;
  FNextOIDData := TNextOIDData.Create;
  FCritSect    := TCriticalSection.Create;
end;

constructor TtiOIDGeneratorInteger.CreateEx(const ALowRange: integer);
begin
  Create;
  FLowRange := ALowRange;
end;

destructor TtiOIDGeneratorInteger.Destroy;
begin
  FCritSect.Free;
  FNextOIDData.Free;
  inherited;
end;

function TtiOIDGeneratorInteger.NextOID(const ADatabaseAliasName: string;
  const APersistenceLayerName: string): integer;
begin
  FCritSect.Enter;
  try
    if FDirty then
    begin
      GTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData,
        ADatabaseAliasName, APersistenceLayerName);
      FDirty := False;
    end;

    Result := (FNextOIDData.NextOID * FLowRange) + FLow;

    Inc(FLow);
    if FLow = FLowRange then
    begin
      FDirty := True;
      FLow   := 0;
    end;
  finally
    FCritSect.Leave;
  end;
end;

class function TtiOIDGeneratorInteger.OIDClass: TtiOIDClass;
begin
  Result := TOIDInteger;
end;

procedure TOIDInteger.SetToNull;
begin
  FAsInteger := cNullOIDInteger;
end;

function TOIDInteger.GetAsVariant: variant;
begin
  Result := FAsInteger;
end;

procedure TOIDInteger.SetAsVariant(const AValue: variant);
begin
  FAsInteger := AValue;
end;

function TOIDInteger.NullOIDAsString: string;
begin
  Result := IntToStr(cNullOIDInteger);
end;

procedure TOIDInteger.AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject);
var
  lParams: TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsInteger(AFieldName, FAsInteger);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDData) and not CanSupportMultiUser;
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
begin

  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow('Next_OID', nil);
  try
    TNextOIDData(Visited).NextOID := Query.FieldAsInteger['OID'];
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDData) and not CanSupportMultiUser;
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams: TtiQueryParams;
begin
  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', integer(TNextOIDData(Visited).NextOID + 1));
    Query.UpdateRow('Next_OID', lParams, nil);
  finally
    lParams.Free;
  end;
end;

{ TVisDBNextOIDSql }

function TVisDBNextOIDSql.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDData) and CanSupportMultiUser;
end;

procedure TVisDBNextOIDSql.Execute(const AData: TtiVisited);
var
  i: integer;
begin
  if GTIOPFManager.Terminated then
    Exit;

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit;

  Query.SQL.Text := 'update Next_OID set OID = OID + 1';
  for i := 0 to cMaxRetries do
    try
      Query.ExecSQL;
      break;
    except
      // database may be locked, wait and try again later
      Sleep(50);
    end;

  Query.SelectRow('Next_OID', nil);
  try
    TNextOIDData(Visited).NextOID := Query.FieldAsInteger['OID'] - 1;
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDAmbler }

function TVisDBNextOIDAmbler.CanSupportMultiUser: boolean;
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  Assert(Persistencelayer <> nil, 'FPersistenceLayer not assigned');
  LDefaults := TtiPersistenceLayerDefaults.Create;
  try
    PersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    Result := LDefaults.CanSupportMultiUser;
  finally
    LDefaults.Free;
  end;
end;

initialization
  GTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  GTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);
  GTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDSql);

end.

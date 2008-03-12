unit tiOIDInt64;

{
  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorInt64.Create;
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

  TOIDInt64 = class(TtiOID)
  private
    FAsInt64: int64;
  protected
    function GetAsString: ShortString; override;
    procedure SetAsString(const AValue: ShortString); override;
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
    property AsInt64: int64 read FAsInt64 write FAsInt64;
    function NullOIDAsString: string; override;
  end;

  TNextOIDData = class(TtiObject)
  private
    FNextOID: int64;
  public
    property NextOID: int64 read FNextOID write FNextOID;
  end;

  TtiOIDGeneratorInt64 = class(TtiOIDGenerator)
  private
    FLow:         int64;
    FLowRange:    int64;
    FDirty:       boolean;
    FNextOIDData: TNextOIDData;
    FCritSection: TCriticalSection;
    function NextOID(const ADBConnectionName: string; const APersistenceLayerName: string): int64;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

  TVisDBNextOIDAmblerRead = class(TtiObjectVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisDBNextOIDAmblerUpdate = class(TtiObjectVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

implementation

uses
  tiQuery,
  tiUtils,
  tiOPFManager,
  tiConstants,
  tiExcept,
  SysUtils;

{ TOIDInt64 }

function TOIDInt64.getAsString: ShortString;
begin
  Result := IntToStr(FAsInt64);
end;

procedure TOIDInt64.SetAsString(const AValue: ShortString);
begin
  FAsInt64 := StrToInt(AValue);
end;

function TOIDInt64.IsNull: boolean;
begin
  Result := FAsInt64 = cNullOIDInteger;
end;

procedure TOIDInt64.AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery   := TtiQuery(AQuery);
  FAsInt64 := lQuery.FieldAsInteger[AFieldName];
end;

procedure TOIDInt64.AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  lQuery.ParamAsInteger[AFieldName] := FAsInt64;
end;

function TOIDInt64.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  Result := (FAsInt64 = lQuery.FieldAsInteger[AFieldName]);
end;

procedure TOIDInt64.Assign(const ASource: TtiOID);
begin
  AsString := ASource.AsString;
end;

function TOIDInt64.Compare(const ACompareWith: TtiOID): integer;
begin
  Assert(ACompareWith is TOIDInt64, 'ACompareWith not a ACompareWith');
  if AsInt64 < TOIDInt64(ACompareWith).AsInt64 then
    Result := -1
  else if AsInt64 > TOIDInt64(ACompareWith).AsInt64 then
    Result := 1
  else
    Result := 0;
end;

const
  cuLowRange = 100;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiOIDGeneratorInt64
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiOIDGeneratorInt64.AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  AAssignTo.AsString := IntToStr(NextOID(ADBConnectionName, APersistenceLayerName));
end;

constructor TtiOIDGeneratorInt64.Create;
begin
  inherited;
  FLow         := 0;
  FLowRange    := cuLowRange;
  FDirty       := True;
  FNextOIDData := TNextOIDData.Create;
  FCritSection := TCriticalSection.Create;
end;

destructor TtiOIDGeneratorInt64.Destroy;
begin
  FCritSection.Free;
  FNextOIDData.Free;
  inherited;
end;

function TtiOIDGeneratorInt64.NextOID(const ADBConnectionName: string; const APersistenceLayerName: string): int64;
begin
  FCritSection.Enter;
  try
    if FDirty then
    begin
      gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData,
        ADBConnectionName, APersistenceLayerName);
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
    FCritSection.Leave;
  end;

end;

class function TtiOIDGeneratorInt64.OIDClass: TtiOIDClass;
begin
  Result := TOIDInt64;
end;

procedure TOIDInt64.SetToNull;
begin
  FAsInt64 := cNullOIDInteger;
end;

function TOIDInt64.GetAsVariant: variant;
begin
  Result := FAsInt64;
end;

procedure TOIDInt64.SetAsVariant(const AValue: variant);
begin
  FAsInt64 := AValue;
end;

function TOIDInt64.NullOIDAsString: string;
begin
  Result := IntToStr(cNullOIDInteger);
end;

procedure TOIDInt64.AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject);
var
  lParams: TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsInteger(AFieldName, FAsInt64);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDData);
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
begin

  if gTIOPFManager.Terminated then
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
  Result := (Visited is TNextOIDData);
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams: TtiQueryParams;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', int64(TNextOIDData(Visited).NextOID + 1));
    //    lParams.ParamAsVariant[ 'OID' ]:= Int64(TNextOIDData(Visited).NextOID + 1);
    Query.UpdateRow('Next_OID', lParams, nil);
  finally
    lParams.Free;
  end;
end;

initialization
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);

end.

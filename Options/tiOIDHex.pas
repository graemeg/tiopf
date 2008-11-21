{-----------------------------------------------------------------------------
 Unit Name: tiOIDHex
 Author:    Lukasz Zeligowski
 Purpose:   OID generator. Each OID is 32 character Hexadecimal number. This
            is equal to 128 bit unsigned INTEGER.
            By default 'cache' is 256 OIDs
            There are three usefull 'static' functions:
            TtiOID.CheckValue - checks if given parameter is proper HEX value
            TtiOID.IncHex - inc. HEX given by parameter by one
            TtiOID.ZeroHex - gives ZERO AValue Hex with the proper no. of chars

 History:   0.9Beta first version
-----------------------------------------------------------------------------}

unit tiOIDHex;

{
  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorHex.Create;
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
  TOIDHex = class(TtiOID)
  private
    FAsString: string;
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
    function NullOIDAsString: string; override;
    class function CheckValue(AValue: ShortString): boolean;
    class function IncHex(pHex: ShortString; pInc: integer = 1): ShortString;
    class function ZeroHex: ShortString;
  end;

  TNextOIDHexData = class(TtiObject)
  private
    FNextHexOID: ShortString;
  public
    property NextHexOID: ShortString read FNextHexOID write FNextHexOID;
  end;

  TtiOIDGeneratorHex = class(TtiOIDGenerator)
  private
    FLow, FLowRange: integer;
    FLowRangeMask:   string;
    FLastOIDValue:   string;
    FDirty:          boolean;
    FNextOIDHexData: TNextOIDHexData;
    FCritSect:       TCriticalSection;
    function NextOID(const ADatabaseAliasName: string; const APersistenceLayerName: string): string;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

  TVisDBNextOIDHexAmblerRead = class(TtiObjectVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisDBNextOIDHexAmblerUpdate = class(TtiObjectVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;


const
  CNextOIDHexReadHigh = 'NextOIDHexReadHigh';
  COIDHexSize         = 32;
  COIDHexChacheSize   = 2;

implementation

uses
  tiQuery,
  SysUtils,
  tiUtils,
  tiOPFManager,
  tiConstants;

const
  cOIDHexNumber: array [0..15] of char =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

{ TOIDHex }

procedure TOIDHex.Assign(const ASource: TtiOID);
begin
  AsString := ASource.AsString;
end;

procedure TOIDHex.AssignFromTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery    := TtiQuery(AQuery);
  FAsString := lQuery.FieldAsString[AFieldName];
end;

procedure TOIDHex.AssignToTIQuery(const AFieldName: string; const AQuery: TtiBaseObject);
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  if IsNull then
    lQuery.ParamIsNull[AFieldName] := True
  else
    lQuery.ParamAsString[AFieldName] := FAsString;
end;

procedure TOIDHex.AssignToTIQueryParam(const AFieldName: string; const AParams: TtiBaseObject);
var
  lParams: TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsString(AFieldName, FAsString);
end;

class function TOIDHex.CheckValue(AValue: ShortString): boolean;
var
  //  lI64 : int64;
  lHex1, lHex2: string;
begin
  // Length 32 chars
  Result := False;
  if length(AValue) <> 32 then
    exit;
  // Divide: 2 parts 16 chars is 8 bytes is 64 bits...
  lHex1 := '$' + Copy(AValue, 1, 16);
  lHex2 := '$' + Copy(AValue, 17, 16);
  try
    StrToInt64(lHex1);
    StrToInt64(lHex2);
    //lI64:=StrToInt64(lHex1);
    //lI64:=StrToInt64(lHex2);
    Result := True;
  except
  end;
end;

function TOIDHex.Compare(const ACompareWith: TtiOID): integer;
begin
  if AsString < ACompareWith.AsString then
    Result := -1
  else if AsString > ACompareWith.AsString then
    Result := 1
  else
    Result := 0;
end;

function TOIDHex.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery: TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  Result := (FAsString = lQuery.FieldAsString[AFieldName]);
end;

function TOIDHex.GetAsString: ShortString;
begin
  Result := FAsString;
end;

function TOIDHex.GetAsVariant: variant;
begin
  Result := FAsString;
end;

class function TOIDHex.IncHex(pHex: ShortString; pInc: integer): ShortString;

  procedure _IncHex(APos: integer);
  var
    lChar:  char;
    lValue: integer;
  begin
    if APos > length(Result) then
      raise Exception.Create('Inc Hex (1) exception');
    if APos < 1 then
      raise Exception.Create('Inc Hex (2) exception');
    lChar  := Result[APos];
    lValue := StrToInt('$' + lChar);
    Inc(lValue);
    if lValue > 15 then
      _IncHex(APos - 1);
    lValue       := lValue mod 16;
    lChar        := cOIDHexNumber[lValue];
    Result[APos] := lChar;
  end;

begin
  if pInc <> 1 then
    raise Exception.Create('IncHex only with 1');
  Result := pHex;
  _IncHex(length(Result));
end;

function TOIDHex.IsNull: boolean;
begin
  Result := (FAsString = NullOIDAsString);
end;

function TOIDHex.NullOIDAsString: string;
begin
  Result := '';
end;

procedure TOIDHex.SetAsString(const AValue: ShortString);
begin
  if CheckValue(AValue) then
    FAsString := AValue;
end;

procedure TOIDHex.SetAsVariant(const AValue: variant);
begin
  FAsString := AValue;
end;

procedure TOIDHex.SetToNull;
begin
  FAsString := NullOIDAsString;
end;

class function TOIDHex.ZeroHex: ShortString;
begin
  Result := StringOfChar('0', COIDHexSize);
end;

{ TtiOIDGeneratorHex }

procedure TtiOIDGeneratorHex.AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  AAssignTo.AsString := NextOID(ADBConnectionName, APersistenceLayerName);
end;

constructor TtiOIDGeneratorHex.Create;
begin
  inherited;
  FLow          := 0;
  FLowRangeMask := StringOfChar('0', COIDHexChacheSize);
  FLowRange     := StrToInt('$1' + FLowRangeMask);
  FDirty        := True;
  FNextOIDHexData := TNextOIDHexData.Create;
  FCritSect     := TCriticalSection.Create;
end;

destructor TtiOIDGeneratorHex.Destroy;
begin
  FNextOIDHexData.Free;
  FCritSect.Free;
  inherited;
end;

function TtiOIDGeneratorHex.NextOID(const ADatabaseAliasName: string; const APersistenceLayerName: string): string;
begin
  FCritSect.Enter;
  try
    if FDirty then
    begin
      GTIOPFManager.VisitorManager.Execute(CNextOIDHexReadHigh,
        FNextOIDHexData, ADatabaseAliasName, APersistenceLayerName);
      FDirty        := False;
      FLastOIDValue := FNextOIDHexData.NextHexOID + FLowRangeMask;
    end;

    Result := TOIDHex.IncHex(FLastOIDValue);


    Inc(FLow);
    if FLow = FLowRange then
    begin
      FDirty := True;
      FLow   := 0;
    end;

    FLastOIDValue := Result;
  finally
    FCritSect.Leave;
  end;
end;

class function TtiOIDGeneratorHex.OIDClass: TtiOIDClass;
begin
  Result := TOIDHex;
end;

{ TVisDBNextOIDHexAmblerRead }

function TVisDBNextOIDHexAmblerRead.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDHexData);
end;

procedure TVisDBNextOIDHexAmblerRead.Execute(const AData: TtiVisited);
begin
  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow('Next_OIDHEX', nil);
  try
    TNextOIDHexData(Visited).NextHexOID := Query.FieldAsString['OID'];
    if TNextOIDHexData(Visited).NextHexOID = '' then
      TNextOIDHexData(Visited).NextHexOID := StringOfChar('0', COIDHexSize - COIDHexChacheSize);
  finally
    Query.Close;
  end;
end;

{ TVisDBNextOIDHexAmblerUpdate }

function TVisDBNextOIDHexAmblerUpdate.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDHexData);
end;

procedure TVisDBNextOIDHexAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams: TtiQueryParams;
  lHex:    ShortString;
begin
  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lParams := TtiQueryParams.Create;
  try
    lHex := TNextOIDHexData(Visited).NextHexOID;
    lHex := TOIDHex.IncHex(lHex);
    lParams.SetValueAsString('OID', string(lHex));
    Query.UpdateRow('Next_OIDHEX', lParams, nil);
  finally
    lParams.Free;
  end;
end;

initialization

  GTIOPFManager.VisitorManager.RegisterVisitor(CNextOIDHexReadHigh, TVisDBNextOIDHexAmblerRead);
  GTIOPFManager.VisitorManager.RegisterVisitor(CNextOIDHexReadHigh, TVisDBNextOIDHexAmblerUpdate);

end.

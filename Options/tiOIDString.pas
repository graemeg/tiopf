unit tiOIDString;

{$I tiDefines.inc}

interface
uses
  tiOID,
  tiBaseObject,
  tiObject,
  tiVisitorDB,
  tiVisitor,
  SyncObjs;

const
  cErrorInvalidStartID = 'Invalid start ID <%s>';

type

  TtiOIDGeneratorString = class;

  TOIDString = class(TtiOID)
  private
    FAsString : String;
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
    procedure Assign(const ASource : TtiOID); override;
    function  Compare(const ACompareWith : TtiOID): Integer; override;
    procedure SetToNull; override;
    function  NullOIDAsString: String; override;
  end;

  TNextOIDData = class(TtiObject)
  private
    FNextOID: String;
    FThisOIDGenerator: TtiOIDGeneratorString;
  public
    property NextOID : String read FNextOID write FNextOID;
    // There is a function called OIDGenerator on TtiObject
    property ThisOIDGenerator : TtiOIDGeneratorString read FThisOIDGenerator write FThisOIDGenerator;
  end;

  TtiOIDGeneratorString = class(TtiOIDGenerator)
  private
    FOIDChars : array of char;
    FLow : String;
    FDirty: boolean;
    FNextOIDData : TNextOIDData;
    FOIDLength: Byte;
    FOIDPrefix: string;
    FCritSect: TCriticalSection;
    function  NextOID(
  const ADatabaseAliasName: string;
  const APersistenceLayerName: string): String;
    function  GetOIDChars: string;
    procedure SetOIDChars(const AValue: string);
    function  PadToLength(const AValue : string): string;
    function  GetHighOIDLength: Byte;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function OIDClass: TtiOIDClass; override;
    procedure   AssignNextOID(
      const AAssignTo : TtiOID;
      const ADBConnectionName: string = '';
      const APersistenceLayerName: string = ''); override;
    function    IncOID(const pCurrentOID: String): String;
    property    HighOIDLength : Byte read GetHighOIDLength;

    // You might want to set these at startup
    property    OIDChars : string read GetOIDChars write SetOIDChars;
    property    OIDLength : Byte read FOIDLength write FOIDLength;
    property    OIDPrefix : string read FOIDPrefix write FOIDPrefix;
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
  CNextOIDTableName   = 'Next_OID';
  CNextOIDFieldName   = 'OID';
  // '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  // 88 Characters, so if the OID string is 4 characters long, there are
  // Power(88, 4) available OIDs = 59969536
  // Power(88, 5) available OIDs = 5277319168
  // Integer gives                   2147483647
  // Int64 gives                     4610000000000000000

  COIDChars           = '!#$%&()*+,-./0123456789:;<=>?@'+
                        'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                        '[\]^_' +
                        'abcdefghijklmnopqrstuvwxyz';


implementation
uses
  tiQuery,
  tiConstants,
  tiOPFManager,
  tiExcept,
  SysUtils;

{ TOIDString }

function TOIDString.getAsString: ShortString;
begin
  result := FAsString;
end;

procedure TOIDString.SetAsString(const AValue: ShortString);
begin
  FAsString := AValue;
end;

function TOIDString.IsNull: boolean;
begin
  result := FAsString = NullOIDAsString;
end;

procedure TOIDString.AssignFromTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  FAsString := lQuery.FieldAsString[ AFieldName ];
end;

procedure TOIDString.AssignToTIQuery(const AFieldName : string; const AQuery: TtiBaseObject);
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  lQuery.ParamAsString[ AFieldName ]:= FAsString;
end;

function TOIDString.EqualsQueryField(const AFieldName: string; const AQuery: TtiBaseObject): boolean;
var
  lQuery : TtiQuery;
begin
  Assert(AQuery is TtiQuery, 'AQuery not a TtiQuery');
  lQuery := TtiQuery(AQuery);
  result := (FAsString = lQuery.FieldAsString[ AFieldName ]);
end;

procedure TOIDString.Assign(const ASource: TtiOID);
begin
  AsString := ASource.AsString;
end;

function TOIDString.Compare(const ACompareWith: TtiOID): Integer;
begin
  Assert(ACompareWith is TOIDString, 'ACompareWith not a ACompareWith');
  if AsString < TOIDString(ACompareWith).AsString then
    result := -1
  else if AsString > TOIDString(ACompareWith).AsString then
    result := 1
  else
    result := 0;
end;

const
  cuLowRange = 100;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiOIDGeneratorString
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiOIDGeneratorString.AssignNextOID(
      const AAssignTo : TtiOID;
      const ADBConnectionName: string = '';
      const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  AAssignTo.AsString := NextOID(ADBConnectionName, APersistenceLayerName);
end;

constructor TtiOIDGeneratorString.Create;
begin
  inherited;
  SetOIDChars(cOIDChars);
  OIDLength := 10;
  FDirty := true;
  FNextOIDData := TNextOIDData.Create;
  FCritSect:= TCriticalSection.Create;
end;

destructor TtiOIDGeneratorString.Destroy;
begin
  FNextOIDData.Free;
  FCritSect.Free;
  inherited;
end;

function TtiOIDGeneratorString.NextOID(
  const ADatabaseAliasName: string;
  const APersistenceLayerName: string): String;
begin
  FCritSect.Enter;
  try
    if FDirty then
    begin
      FLow := FOIDChars[0];
      FNextOIDData.ThisOIDGenerator := Self;
      gTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData,
        ADatabaseAliasName, APersistenceLayerName);
      FDirty := false;
    end else
    begin
      FLow := IncOID(FLow);
      if FLow = FOIDChars[High(FOIDChars)] then
        FDirty := true;
    end;
    result := FOIDPrefix + FNextOIDData.NextOID + FLow;
  finally
    FCritSect.Leave;
  end;
end;

class function TtiOIDGeneratorString.OIDClass: TtiOIDClass;
begin
  result:= TOIDString;
end;

procedure TOIDString.SetToNull;
begin
  FAsString := NullOIDAsString;
end;

function TOIDString.GetAsVariant: Variant;
begin
  result := FAsString;
end;

procedure TOIDString.SetAsVariant(const AValue: Variant);
begin
  FAsString := AValue;
end;

function TOIDString.NullOIDAsString: String;
begin
  result := '';
end;

procedure TOIDString.AssignToTIQueryParam(const AFieldName: string;const AParams: TtiBaseObject);
var
  lParams : TtiQueryParams;
begin
  Assert(AParams is TtiQueryParams, 'AQuery not a TtiQuery');
  lParams := TtiQueryParams(AParams);
  lParams.SetValueAsString(AFieldName, FAsString);
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDData);
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
var
  lNextOIDData : TNextOIDData;
  lNextOID : String;
begin

  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow(cNextOIDTableName, nil);
  try
    lNextOID := Query.FieldAsString[ cNextOIDFieldName ];
  finally
    Query.Close;
  end;
  lNextOIDData := (Visited as TNextOIDData);
  TNextOIDData(Visited).NextOID :=
    lNextOIDData.ThisOIDGenerator.PadToLength(lNextOID);

end;

{ TVisDBNextOIDAmblerUpdate }

function TVisDBNextOIDAmblerUpdate.AcceptVisitor: boolean;
begin
  result := (Visited is TNextOIDData);
end;

function TtiOIDGeneratorString.IncOID(const pCurrentOID: String): String;
  function _GeTNextIDGenerator(const pCurrentOID: String): String;
  var
    i : integer;
    lLastPos : integer;
    lLastChar : Char;
  begin
    result := pCurrentOID;
    lLastPos := Length(pCurrentOID);
    lLastChar := pCurrentOID[lLastPos];
    if lLastChar = FOIDChars[High(FOIDChars)] then
    begin
      result :=
       _GeTNextIDGenerator(
          Copy(pCurrentOID,1, lLastPos-1));
      result := result + FOIDChars[Low(FOIDChars)];
      if lLastPos = 1 then
        result := FOIDChars[Low(FOIDChars)+1] + result;
      Exit; //==>
    end;

    for i := Low(FOIDChars) to High(FOIDChars)-1 do
      if FOIDChars[i] = lLastChar then
      begin
        result := Copy(pCurrentOID, 1, Length(pCurrentOID)-1) + FOIDChars[i+1];
        Exit; //==>
      end;
    end;
var
  i, j : integer;
  lChar : Char;
  lCurrentOID : string;
begin
  lCurrentOID := pCurrentOID;     
  for i := 1 to Length(lCurrentOID) do
    for j := Low(FOIDChars) to High(FOIDChars) do
    begin
      lChar := lCurrentOID[i];
      if lChar = FOIDChars[j] then
        Break //==>
      else if j < High(FOIDChars) then
        Continue; //==>
      // Should only get here if there is an invalid character
      raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidStartID, [lCurrentOID]);
    end;
  result := _GetNextIDGenerator(lCurrentOID);
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams : TtiQueryParams;
  lNextOID : string;
  lNextOIDData : TNextOIDData;
begin
  if gTIOPFManager.Terminated then
    Exit; //==>

  Inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lNextOIDData := (Visited as TNextOIDData);
  lParams := TtiQueryParams.Create;
  try
    lNextOID := lNextOIDData.ThisOIDGenerator.IncOID(lNextOIDData.NextOID);
    lParams.SetValueAsString(cNextOIDFieldName, lNextOID) ;
    Query.UpdateRow(cNextOIDTableName, lParams, nil);
  finally
    lParams.Free;
  end;
end;

function TtiOIDGeneratorString.GetOIDChars: string;
var
  i : integer;
begin
  result := '';
  for i := Low(FOIDChars) to High(FOIDChars) do
    result := result + FOIDChars[i];
end;

procedure TtiOIDGeneratorString.SetOIDChars(const AValue: string);
var
  i : integer;
begin
  SetLength(FOIDChars, Length(AValue));
  for i := 1 to Length(AValue) do
    FOIDChars[i-1]:= AValue[i];
end;

function TtiOIDGeneratorString.PadToLength(const AValue: string): string;
begin
  Result := AValue;
  if length(AValue) < HighOIDLength then begin
    while length(Result) < HighOIDLength do begin
      result := OIDChars[1] + result;
    end;
  end
  else if length(AValue) > HighOIDLength then
    result := copy(AValue, length(AValue)-HighOIDLength+1, HighOIDLength);
end;

function TtiOIDGeneratorString.GetHighOIDLength: Byte;
begin
  result := OIDLength - 1 - Length(FOIDPrefix);
end;

initialization

  gTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorString.Create;
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  gTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);

end.




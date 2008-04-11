unit tiOIDString;

{
  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorString.Create;
}


{$I tiDefines.inc}

interface

uses
  tiOID,
  tiObject,
  tiVisitorDB,
  tiVisitor,
  SyncObjs;

const
  cErrorInvalidStartID = 'Invalid start ID <%s>';

type

  TtiOIDGeneratorString = class;

  TOIDString = class(TOIDStringAbs)
  end;

  TNextOIDData = class(TtiObject)
  private
    FNextOID:          string;
    FThisOIDGenerator: TtiOIDGeneratorString;
  public
    property NextOID: string read FNextOID write FNextOID;
    // There is a function called OIDGenerator on TtiObject
    property ThisOIDGenerator: TtiOIDGeneratorString read FThisOIDGenerator write FThisOIDGenerator;
  end;

  TtiOIDGeneratorString = class(TtiOIDGenerator)
  private
    FOIDChars:    array of char;
    FLow:         string;
    FDirty:       boolean;
    FNextOIDData: TNextOIDData;
    FOIDLength:   byte;
    FOIDPrefix:   string;
    FCritSect:    TCriticalSection;
    function NextOID(const ADatabaseAliasName: string; const APersistenceLayerName: string): string;
    function GetOIDChars: string;
    procedure SetOIDChars(const AValue: string);
    function PadToLength(const AValue: string): string;
    function GetHighOIDLength: byte;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
      const APersistenceLayerName: string = ''); override;
    function IncOID(const pCurrentOID: string): string;
    property HighOIDLength: byte read GetHighOIDLength;

    // You might want to set these at startup
    property OIDChars: string read GetOIDChars write SetOIDChars;
    property OIDLength: byte read FOIDLength write FOIDLength;
    property OIDPrefix: string read FOIDPrefix write FOIDPrefix;
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

const
  CNextOIDTableName = 'Next_OID';
  CNextOIDFieldName = 'OID';
  // '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  // 88 Characters, so if the OID string is 4 characters long, there are
  // Power(88, 4) available OIDs = 59969536
  // Power(88, 5) available OIDs = 5277319168
  // Integer gives                   2147483647
  // Int64 gives                     4610000000000000000

  COIDChars         = '!#$%&()*+,-./0123456789:;<=>?@' +
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    '[\]^_' +
    'abcdefghijklmnopqrstuvwxyz';


implementation

uses
  tiBaseObject,
  tiQuery,
  tiConstants,
  tiOPFManager,
  tiExcept,
  SysUtils;

const
  cuLowRange = 100;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiOIDGeneratorString
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiOIDGeneratorString.AssignNextOID(const AAssignTo: TtiOID; const ADBConnectionName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  AAssignTo.AsString := NextOID(ADBConnectionName, APersistenceLayerName);
end;

constructor TtiOIDGeneratorString.Create;
begin
  inherited;
  SetOIDChars(cOIDChars);
  OIDLength    := 10;
  FDirty       := True;
  FNextOIDData := TNextOIDData.Create;
  FCritSect    := TCriticalSection.Create;
end;

destructor TtiOIDGeneratorString.Destroy;
begin
  FNextOIDData.Free;
  FCritSect.Free;
  inherited;
end;

function TtiOIDGeneratorString.NextOID(const ADatabaseAliasName: string; const APersistenceLayerName: string): string;
begin
  FCritSect.Enter;
  try
    if FDirty then
    begin
      FLow   := FOIDChars[0];
      FNextOIDData.ThisOIDGenerator := Self;
      GTIOPFManager.VisitorManager.Execute(cNextOIDReadHigh, FNextOIDData,
        ADatabaseAliasName, APersistenceLayerName);
      FDirty := False;
    end
    else
    begin
      FLow := IncOID(FLow);
      if FLow = FOIDChars[High(FOIDChars)] then
        FDirty := True;
    end;
    Result := FOIDPrefix + FNextOIDData.NextOID + FLow;
  finally
    FCritSect.Leave;
  end;
end;

class function TtiOIDGeneratorString.OIDClass: TtiOIDClass;
begin
  Result := TOIDString;
end;

{ TVisDBNextOIDAmblerRead }

function TVisDBNextOIDAmblerRead.AcceptVisitor: boolean;
begin
  Result := (Visited is TNextOIDData);
end;

procedure TVisDBNextOIDAmblerRead.Execute(const AData: TtiVisited);
var
  lNextOIDData: TNextOIDData;
  lNextOID:     string;
begin

  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  Query.SelectRow(cNextOIDTableName, nil);
  try
    lNextOID := Query.FieldAsString[cNextOIDFieldName];
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
  Result := (Visited is TNextOIDData);
end;

function TtiOIDGeneratorString.IncOID(const pCurrentOID: string): string;

  function _GeTNextIDGenerator(const pCurrentOID: string): string;
  var
    i:         integer;
    lLastPos:  integer;
    lLastChar: char;
  begin
    Result    := pCurrentOID;
    lLastPos  := Length(pCurrentOID);
    lLastChar := pCurrentOID[lLastPos];
    if lLastChar = FOIDChars[High(FOIDChars)] then
    begin
      Result :=
        _GeTNextIDGenerator(
        Copy(pCurrentOID, 1, lLastPos - 1));
      Result := Result + FOIDChars[Low(FOIDChars)];
      if lLastPos = 1 then
        Result := FOIDChars[Low(FOIDChars) + 1] + Result;
      Exit; //==>
    end;

    for i := Low(FOIDChars) to High(FOIDChars) - 1 do
      if FOIDChars[i] = lLastChar then
      begin
        Result := Copy(pCurrentOID, 1, Length(pCurrentOID) - 1) + FOIDChars[i + 1];
        Exit; //==>
      end;
  end;

var
  i, j:        integer;
  lChar:       char;
  lCurrentOID: string;
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
  Result := _GetNextIDGenerator(lCurrentOID);
end;

procedure TVisDBNextOIDAmblerUpdate.Execute(const AData: TtiVisited);
var
  lParams:      TtiQueryParams;
  lNextOID:     string;
  lNextOIDData: TNextOIDData;
begin
  if GTIOPFManager.Terminated then
    Exit; //==>

  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  lNextOIDData := (Visited as TNextOIDData);
  lParams      := TtiQueryParams.Create;
  try
    lNextOID := lNextOIDData.ThisOIDGenerator.IncOID(lNextOIDData.NextOID);
    lParams.SetValueAsString(cNextOIDFieldName, lNextOID);
    Query.UpdateRow(cNextOIDTableName, lParams, nil);
  finally
    lParams.Free;
  end;
end;

function TtiOIDGeneratorString.GetOIDChars: string;
var
  i: integer;
begin
  Result := '';
  for i := Low(FOIDChars) to High(FOIDChars) do
    Result := Result + FOIDChars[i];
end;

procedure TtiOIDGeneratorString.SetOIDChars(const AValue: string);
var
  i: integer;
begin
  SetLength(FOIDChars, Length(AValue));
  for i := 1 to Length(AValue) do
    FOIDChars[i - 1] := AValue[i];
end;

function TtiOIDGeneratorString.PadToLength(const AValue: string): string;
begin
  Result := AValue;
  if length(AValue) < HighOIDLength then
    while length(Result) < HighOIDLength do
      Result := OIDChars[1] + Result
  else if length(AValue) > HighOIDLength then
    Result := copy(AValue, length(AValue) - HighOIDLength + 1, HighOIDLength);
end;

function TtiOIDGeneratorString.GetHighOIDLength: byte;
begin
  Result := OIDLength - 1 - Length(FOIDPrefix);
end;

initialization
  GTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerRead);
  GTIOPFManager.VisitorManager.RegisterVisitor(cNextOIDReadHigh, TVisDBNextOIDAmblerUpdate);

end.

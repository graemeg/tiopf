unit tiCommandLineParams;

{$I tiDefines.inc}
{$DEFINE NEW_VERSION_CLP}

interface
uses
  tiBaseObject
  ,Classes
  ,tiParams
  ,SysUtils
 ;

type

//  Model of command line params:
//  *** As determined by reverse engineering of code 21/05/2013 ***
//  1) <CommandLineArgs> is a sequence of 0..N <NameValue>
//  2) <NameValue> is a <Name> followed by 0..1 <Value>
//  3) <Name> begins with 1..N <Delimiter> unless it is the first argument,
//     when 1..N <delimiter> are not required. <Name> cannot contain whitespace.
//  4) <Value> does not begin with a <Delimiter>, and is terminated by, and
//     exclusive of, a <Delimiter>. <Value> may contain whitespace, but may not
//     contain <Delimiter>.
//  5) <Delimiter> = dash/minus character '-'

{$IFDEF NEW_VERSION_CLP}

  ECLParamName = class(Exception);
  ECLParamValue = class(Exception);

  TtiCommandLineParams = class(TtiBaseObject)
  private
    FParams : TStringList;
    // FCML stored for use in GetAsString()
    FCLParams: ICLParams;
    function GetAsString: string;
    function IsName(const ACLArg: string): boolean;
    procedure ReadParams(const ACLParams: ICLParams);
    function MakeName(const AName: string): string;
    procedure CheckIsValue(const ACLArg: string);
//    function IsQuoted(const ACLArg: string): boolean;
  public
    constructor Create(const ACLParams: ICLParams = nil);
    destructor  Destroy; override;
    function    IsParam(const AParam : string) : boolean; overload;
    function    IsParam(const AParams : array of string) : boolean; overload;
    function    GetParam(const AParam : string): string ;
    property    AsString : string read GetAsString;
    property    Params : TStringList read FParams;
  end;

{$ELSE}

  TtiCommandLineParams = class(TtiBaseObject)
  private
    FsParams : string;
    FslParams : TStringList;
    procedure ReadParams(const ACLParams: ICLParams);
    function  WordExtract(const AInput : string;
                           const APos : integer;
                           const ADelims : string): string;
    function  WordCount(const AStrToProcess : string; ADelims : string): integer;
    function  WordPosition(const AN : integer; const AStr : string;
                            ADelims : string): integer;
    function  ExtractChar(const AValue : string; const APos : integer): char;
    function  CharInStr(const AChr : char; const AStr : string): boolean;
    function  StripLeadingDelims(const AStrToProcess: string; ADelims: string): string;
    function  StripTrailingDelims(const AStrToProcess: string; ADelims: string): string;
    function  NumToken(const AValue, AToken: string): integer;
    function  Token(const AValue, AToken: string; const APos: integer): string;
    function  StrTran(AValue, ADel, AIns: string): string;

  public
    constructor Create(const ACLParams: ICLParams = nil);
    destructor  Destroy; override;
    function    IsParam(const AParam : string) : boolean; overload;
    function    IsParam(const AParams : array of string) : boolean; overload;
    function    GetParam(const AParam : string): string ;
    property    Params : TStringList read FslParams;
    property    AsString : string read FsParams;
  end;

{$ENDIF}

// Singleton
function GCommandLineParams(const ACLParams: ICLParams = nil) : TtiCommandLineParams;

implementation

const
  ctiCommandLineParamPrefix = '-';

var
  UCommandLineParams : TtiCommandLineParams;

// Singleton
function GCommandLineParams(const ACLParams: ICLParams) : TtiCommandLineParams;
begin
  if UCommandLineParams = nil then
    UCommandLineParams := TtiCommandLineParams.Create(ACLParams);
  result := UCommandLineParams;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCommandLineParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{$IFDEF NEW_VERSION_CLP}

{ TtiCommandLineParams }

constructor TtiCommandLineParams.Create(const ACLParams: ICLParams);
begin
  inherited Create;
  FParams := TStringList.Create;

  if ACLParams = nil then
    FCLParams := tiParams.CreateSystemCLParams
  else
    FCLParams := ACLParams;

  ReadParams(FCLParams);
end;

destructor TtiCommandLineParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TtiCommandLineParams.CheckIsValue(const ACLArg: string);
begin
//  if not ((Pos(ctiCommandLineParamPrefix, ACLArg) = 0) or IsQuoted(ACLArg)) then
  if Pos(ctiCommandLineParamPrefix, ACLArg) = 1 then
     raise ECLParamValue.CreateFmt('Error: "%s" is an invalid parameter value',
      [ACLArg]);
end;

// This function is unused in the OPDMS code base, and, frankly, isn't very
// useful, except perhaps for testing. Evaluate on demand as it's rarely/never used.
function TtiCommandLineParams.GetAsString: string;
var
  LArgIndex, LMAxArgIndex: integer;

begin
  LMAxArgIndex := FCLParams.ParamCount;

  for LArgIndex := 1 to LMAxArgIndex do
    FmtStr(Result, '%s%s ', [Result, FCLParams.ParamStr(LArgIndex)]);
  Result := Trim(Result);
end;

function TtiCommandLineParams.GetParam(const AParam: string): string;
begin
  Result := FParams.Values[AParam];
end;

function TtiCommandLineParams.IsName(const ACLArg: string): boolean;
begin
  Result := (Pos(ctiCommandLineParamPrefix, ACLArg) = 1);
end;

function TtiCommandLineParams.IsParam(const AParam: string): boolean;
begin
  Result := (FParams.IndexOfName(AParam) >= 0);
end;

// This function is badly-named. It returns true if any of AParams is a
// parameter - a better function name would be IsAnyParam()
// Carried over as-is from old implementation - this function is unused in the
// OPDMS code-base
function  TtiCommandLineParams.IsParam(const AParams : array of string) : boolean;
var
  i : integer;

begin
  result := false;
  for i := Low(AParams) to High(AParams) do
    if IsParam(AParams[i]) then
    begin
      result := true;
      Exit; //==>
    end;
end;

//function TtiCommandLineParams.IsQuoted(const ACLArg: string): boolean;
//const
//  cQuotes = ['"', ''''];
//
//begin
//  Result := (Length(ACLArg) > 2) and (ACLArg[1] = ACLArg[Length(ACLArg)]) and
//    (ACLArg[1] in cQuotes) and (ACLArg[Length(ACLArg)] in cQuotes);
//end;

procedure TtiCommandLineParams.ReadParams(const ACLParams: ICLParams);
var
  LArgIndex, LMAxArgIndex: integer;
  LArg, LName: string;

begin
  LMAxArgIndex := ACLParams.ParamCount;

  for LArgIndex := 1 to LMAxArgIndex do
  begin
    LArg := ACLParams.ParamStr(LArgIndex);

    if IsName(LArg) or (LArgIndex = 1) then
    begin
      LName := MakeName(LArg);
      FParams.Add(LName + '=');
    end
    else
    begin
      CheckIsValue(LArg);

      if FParams.Values[LName] <> '' then
        raise ECLParamValue.CreateFmt(
          'Error: Parameter value "%s" without preceding name', [LArg]);

      FParams.Values[LName] := LArg;
    end;

  end ;

end;

function TtiCommandLineParams.MakeName(const AName: string): string;
begin
  result := UpperCase(AName);

  while (Length(result) > 0) and (result[1] = ctiCommandLineParamPrefix) do
    Delete(result, 1, 1);

  if Pos(ctiCommandLineParamPrefix, result) <> 0 then
    raise ECLParamName.CreateFmt('Error: "%s" is an invalid parameter name',
      [result]);

end;

{$ELSE}

constructor TtiCommandLineParams.Create(const ACLParams: ICLParams);
begin
  inherited Create;
  FslParams := TStringList.Create;
  if ACLParams = nil then
    ReadParams(tiParams.CreateCMLParams)
  else
    ReadParams(ACLParams);
end;

destructor TtiCommandLineParams.destroy;
begin
  FslParams.Free;
  inherited;
end;

function TtiCommandLineParams.GetParam(const AParam: string): string;
begin
  result := FslParams.Values[ upperCase(AParam)];
end;

function TtiCommandLineParams.IsParam(const AParam: string): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to FslParams.Count - 1 do begin
    if FslParams.Names[i] = upperCase(AParam) then begin
      result := true;
      break; //==>
    end;
  end;
end;

function  TtiCommandLineParams.IsParam(const AParams : array of string) : boolean;
var
  i : integer;
begin
  result := false;
  for i := Low(AParams) to High(AParams) do
    if IsParam(AParams[i]) then
    begin
      result := true;
      Exit; //==>
    end;
end;

procedure TtiCommandLineParams.ReadParams(const ACLParams: ICLParams);
var
  i : integer;
  j : integer;
  lsNameValue : string;
  lsValue : string;
  lsName : string;
const
  cDelim  = ' ';
begin
  FsParams := '';
  j := ACLParams.ParamCount;
  for i := 1 to j do begin
    if FsParams <> '' then FsParams := FsParams + cDelim;
    FsParams := FsParams + ACLParams.ParamStr(i);
  end ;

  j := WordCount(FsParams, ctiCommandLineParamPrefix);
  for i := 1 to j do begin
    lsNameValue := WordExtract(FsParams, i, ctiCommandLineParamPrefix);
    lsName := Token(lsNameValue, cDelim, 1);
    lsValue := copy(lsNameValue, length(lsName) + 1,
                     length(FsParams) - length(lsValue));

    lsValue := Trim(lsValue);
    lsName := StrTran(lsName, ctiCommandLineParamPrefix, '');
    lsName := upperCase(lsName);

    FslParams.Add(lsName + '=' + lsValue);

  end;

end;

function TtiCommandLineParams.StrTran(AValue, ADel, AIns : string): string;
var i : integer;
    sToChange : string;
begin
  result := '';
  sToChange := AValue;
  i := pos(ADel, sToChange);
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + AIns;
    delete(sToChange, 1, i+length(ADel)-1);
    i := pos(ADel, sToChange);
  end;
  result := result + sToChange;
end;

function TtiCommandLineParams.NumToken(const AValue, AToken : string): integer;
var
  i, iCount : integer;
  lsValue : string;
begin

  result := 0;
  if AValue = '' then
    Exit; //==>

  iCount := 0;
  lsValue := AValue;
  i := pos(AToken, lsValue);
  while i <> 0 do begin
    delete(lsValue, i, length(AToken));
    inc(iCount);
    i := pos(AToken, lsValue);
  end;
  result := iCount + 1;

end;

function TtiCommandLineParams.Token(const AValue, AToken : string; const APos : integer): string;
var
  i, iCount, iNumToken : integer;
  lsValue : string;
begin

  result := '';

  iNumToken := NumToken(AValue, AToken);
  if APos = 1 then begin
    if pos(AToken, AValue) = 0 then result := AValue
    else result := copy(AValue, 1, pos(AToken, AValue)-1);
    end
  else if (iNumToken < APos-1) or (APos<1) then begin
    result := '';
    end
  else begin

    { Remove leading blocks }
    iCount := 1;
    lsValue := AValue;
    i := pos(AToken, lsValue);
    while (i<>0) and (iCount<APos) do begin
      delete(lsValue, 1, i + length(AToken) - 1);
      inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i=0) and (iCount=APos) then result := lsValue
    else if (i=0) and (iCount<>APos) then result := ''
    else result := copy(lsValue, 1, i-1);

  end;
end;

function TtiCommandLineParams.WordExtract(const AInput : string;
                        const APos   : integer;
                        const ADelims : string): string;
var iStart : integer;
    i     : integer;
	iLen  : integer;
begin
  result := '';

  // Find the starting pos of the Nth word
  iStart := WordPosition(APos, AInput, ADelims);

  if iStart <> 0 then begin
    i := iStart;
    iLen := length(AInput);
    // Build up result until we come to our next wordDelim
    // while (i <= iLen) and not(S[i] in ADelims) do begin
    while (i <= iLen) and not(CharInStr(ExtractChar(AInput, i), ADelims)) do begin
      result := result + ExtractChar(AInput, i);
      inc(i);
    end;
  end;
end;

function TtiCommandLineParams.WordPosition(const AN : integer; const AStr : string;
                           ADelims : string): integer;
var
  lCount : integer;
  lI    : Word;
  lSLen : integer;
begin
  lCount := 0;
  lI    := 1;
  Result := 0;
  lSLen := length(AStr);

  while (lI <= lSLen) and (lCount <> AN) do begin
    while (lI <= lSLen) and (CharInStr(ExtractChar(AStr, lI), ADelims)) do begin
      Inc(lI);
    end;

    // if we're not beyond end of S, we're at the start of a word
    if lI <= lSLen then begin
      Inc(lCount);
    end;

    // if not finished, find the end of the current word
    if lCount <> AN then begin
      while (lI <= lSLen) and not(CharInStr(ExtractChar(AStr, lI), ADelims)) do begin
        Inc(lI);
      end;
    end else begin
      Result := lI;
    end;
  end;
end;

function TtiCommandLineParams.ExtractChar(const AValue : string; const APos : integer): char;
var lResult : string;
begin
    if APos > length(AValue) then begin
        result := ' ';
        exit;
    end;
  lResult := copy(AValue, APos, 1);
  result := lResult[1];
end;

function TtiCommandLineParams.StripLeadingDelims(const AStrToProcess : string;
                                     ADelims : string): string;
begin
  result := AStrToProcess;
  while (Length(result) > 0) and (Pos(result[1], ADelims) <> 0) do
    Delete(result, 1, 1)
end;

// Strip any trailing ADelims
function TtiCommandLineParams.StripTrailingDelims(const AStrToProcess : string;
                                      ADelims : string): string;
begin
  result := AStrToProcess;
  while (Length(result) > 0) and (Pos(result[Length(result)], ADelims) <> 0) do
    Delete(result, Length(result), 1)
end;

// Given a set of word delimiters, return number of words in S
function TtiCommandLineParams.WordCount(const AStrToProcess : string; ADelims : string): integer;
var i : integer;
    lCharLast : char;
    lCharCurrent : char;
    lStrToProcess : string;
begin

  // Strip any leading ADelims
  lStrToProcess := StripLeadingDelims(AStrToProcess, ADelims);
  lStrToProcess := StripTrailingDelims(lStrToProcess, ADelims);

  // If lStrToProcess is empty, then there are no words
  if lStrToProcess = '' then begin
    result := 0;
    exit; //==>
  end;

  // lStrToProcess is not empty, therefore there must be at least one word
  // Every wordDelim we find equals another word:
  // 0 word delim := 1 word
  // 1 word delim := 2 words...
  result := 1;

  // lCharLast is used to check for more than 1 wordDelim together
  lCharLast := #0;

  for i := 1 to length(lStrToProcess) do begin
    lCharCurrent := ExtractChar(lStrToProcess, i);
    if CharInStr(lCharCurrent, ADelims) and
       not(CharInStr(lCharLast, ADelims)) then begin
      inc(result);
    end;
    lCharLast := lCharCurrent;
  end;
end;

// Is AChr in the string AStr ?
function TtiCommandLineParams.CharInStr(const AChr : char; const AStr : string): boolean;
begin
  result := pos(AChr, AStr) <> 0;
end;

{$ENDIF}

initialization

finalization
  UCommandLineParams.Free;

end.















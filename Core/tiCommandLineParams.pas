unit tiCommandLineParams;

{$I tiDefines.inc}

interface
uses
  tiBaseObject
  ,Classes
  ,tiParams
 ;

const
  ctiCommandLineParamPrefix = '-';

type


  TtiCommandLineParams = class(TtiBaseObject)
  private
    FsParams : string;
    FslParams : TStringList;
    procedure ReadParams(const ACML: ICMLParams);
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
    constructor Create(const ACML: ICMLParams = nil);
    destructor  Destroy; override;
    function    IsParam(const AParam : string) : boolean; overload;
    function    IsParam(const AParams : array of string) : boolean; overload;
    function    GetParam(const AParam : string): string ;
    property    Params : TStringList read FslParams;
    property    AsString : string read FsParams;
  end;

// Singleton
function GCommandLineParams(const ACML: ICMLParams = nil) : TtiCommandLineParams;

implementation
uses
  SysUtils
 ;

var
  UCommandLineParams : TtiCommandLineParams;

// Singleton
function GCommandLineParams(const ACML: ICMLParams) : TtiCommandLineParams;
begin
  if UCommandLineParams = nil then
    UCommandLineParams := TtiCommandLineParams.Create(ACML);
  result := UCommandLineParams;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCommandLineParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCommandLineParams.Create(const ACML: ICMLParams);
begin
  inherited Create;
  FslParams := TStringList.Create;
  if ACML = nil then
    ReadParams(tiParams.CreateCMLParams)
  else
    ReadParams(ACML);
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

procedure TtiCommandLineParams.ReadParams(const ACML: ICMLParams);
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
  j := ACML.ParamCount;
  for i := 1 to j do begin
    if FsParams <> '' then FsParams := FsParams + cDelim;
    FsParams := FsParams + ACML.ParamStr(i);
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
var i : integer;
    lCharCurrent : char;
begin
  result := AStrToProcess;
  // Loop through each char in the string
  for i := 1 to length(AStrToProcess) do begin
    // Extract the current character
    lCharCurrent := ExtractChar(AStrToProcess, i);

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr(lCharCurrent, ADelims) then begin
      result := copy(AStrToProcess, i,
                      length(AStrToProcess) - i + 1);
      exit; //==>
    // The current char is a word delim, but we are at the end of the string -
    // so no words
    end else begin
      if i = length(AStrToProcess) then begin
        result := '';
      end;
    end;
  end;
end;

// Strip any trailing ADelims
function TtiCommandLineParams.StripTrailingDelims(const AStrToProcess : string;
                                      ADelims : string): string;
var i : integer;
    lCharCurrent : char;
begin
  result := AStrToProcess;
  // Loop through each char in the string
  for i := length(AStrToProcess) downto 1 do begin
    // Extract the current character
    lCharCurrent := ExtractChar(AStrToProcess, i);

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr(lCharCurrent, ADelims) then begin
      result := copy(AStrToProcess, 1, i);
      exit; //==>
    // The current char is a word delim, but we are at the beginning of the string -
    // so no words
    end else begin
      if i = length(AStrToProcess) then begin
        result := '';
      end;
    end;
  end;
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

initialization

finalization
  UCommandLineParams.Free;

end.















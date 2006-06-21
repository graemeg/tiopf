unit tiCommandLineParams;

{$I tiDefines.inc}

interface
uses
  tiBaseObject
  ,Classes
  ;

const
  ctiCommandLineParamPrefix = '-';

type

  TtiCommandLineParams = class(TtiBaseObject)
  private
    FsParams : string ;
    FslParams : TStringList ;
    procedure ReadParams ;
    function  WordExtract( const psInput : string ;
                           const piPos : integer ;
                           const psDelims : string ) : string;
    function  WordCount(const pStrToProcess : string; psDelims : string ) : integer;
    function  WordPosition( const pN : integer; const pS : string ;
                            psDelims : string) : integer ;
    function  ExtractChar( const pValue : string ; const pPos : integer ) : char ;
    function  CharInStr( const pChr : char ; const pStr : string ) : boolean ;
    function  StripLeadingDelims(const pStrToProcess: string; psDelims: string): string;
    function  StripTrailingDelims(const pStrToProcess: string; psDelims: string): string;
    function  NumToken(const pStrValue, pStrToken: string): integer;
    function  Token(const pStrValue, pStrToken: string; const pIntNum: integer): string;
    function  StrTran(pStrValue, pStrDel, pStrIns: string): string;

  public
    constructor Create ;
    destructor  Destroy ; override ;
    function    IsParam( const psParam : string )  : boolean ; overload ;
    function    IsParam( const pParams : array of string )  : boolean ; overload ;
    function    GetParam( const psParam : string ) : string  ;
    property    Params : TStringList read FslParams ;
    property    AsString : string read FsParams ;
  end ;

// Singleton
function gCommandLineParams : TtiCommandLineParams ;

implementation
uses
  SysUtils
  ;

var
  uCommandLineParams : TtiCommandLineParams ;

// Singleton
function gCommandLineParams : TtiCommandLineParams ;
begin
  if uCommandLineParams = nil then
    uCommandLineParams := TtiCommandLineParams.Create ;
  result := uCommandLineParams ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCommandLineParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiCommandLineParams.Create;
begin
  inherited ;
  FslParams := TStringList.Create ;
  ReadParams ;
end;

destructor TtiCommandLineParams.destroy;
begin
  FslParams.Free ;
  inherited ;
end;

function TtiCommandLineParams.GetParam(const psParam: string): string;
begin
  result := FslParams.Values[ upperCase( psParam )] ;
end;

function TtiCommandLineParams.IsParam(const psParam: string): boolean;
var
  i : integer ;
begin
  result := false ;
  for i := 0 to FslParams.Count - 1 do begin
    if FslParams.Names[i] = upperCase( psParam ) then begin
      result := true ;
      break ; //==>
    end ;
  end ;
end;

function  TtiCommandLineParams.IsParam( const pParams : array of string )  : boolean ;
var
  i : integer ;
begin
  result := false ;
  for i := Low( pParams ) to High( pParams ) do
    if IsParam( pParams[i] ) then
    begin
      result := true ;
      Exit ; //==>
    end ;
end ;

procedure TtiCommandLineParams.ReadParams;
var
  i : integer ;
  j : integer ;
  lsNameValue : string ;
  lsValue : string ;
  lsName  : string ;
const
  cDelim  = ' ' ;
begin
  FsParams := '' ;
  j := ParamCount ;
  for i := 1 to j do begin
    if FsParams <> '' then FsParams := FsParams + cDelim ;
    FsParams := FsParams + ParamStr( i ) ;
  end  ;

  j := WordCount( FsParams, ctiCommandLineParamPrefix ) ;
  for i := 1 to j do begin
    lsNameValue := WordExtract( FsParams, i, ctiCommandLineParamPrefix ) ;
    lsName  := Token( lsNameValue, cDelim, 1 ) ;
    lsValue := copy( lsNameValue, length( lsName ) + 1,
                     length( FsParams ) - length( lsValue )) ;

    lsValue := Trim( lsValue ) ;
    lsName  := StrTran( lsName, ctiCommandLineParamPrefix, '' ) ;
    lsName  := upperCase( lsName ) ;

    FslParams.Add( lsName + '=' + lsValue ) ;

  end ;

end;

function TtiCommandLineParams.StrTran( pStrValue, pStrDel, pStrIns : string ) : string ;
var i : integer ;
    sToChange : string ;
begin
  result := '' ;
  sToChange := pStrValue ;
  i := pos( pStrDel, sToChange ) ;
  while i <> 0 do begin
    result := result + copy( sToChange, 1, i-1 ) + pStrIns ;
    delete( sToChange, 1, i+length( pStrDel )-1) ;
    i := pos( pStrDel, sToChange ) ;
  end ;
  result := result + sToChange ;
end ;

function TtiCommandLineParams.NumToken( const pStrValue, pStrToken : string ) : integer ;
var
  i, iCount : integer ;
  lsValue : string ;
begin

  result := 0 ;
  if pStrValue = '' then
    Exit ; //==>

  iCount := 0 ;
  lsValue := pStrValue ;
  i := pos( pStrToken, lsValue ) ;
  while i <> 0 do begin
    delete( lsValue, i, length( pStrToken )) ;
    inc( iCount ) ;
    i := pos( pStrToken, lsValue ) ;
  end ;
  result := iCount + 1 ;

end ;

function TtiCommandLineParams.Token( const pStrValue, pStrToken : string; const pIntNum : integer ) : string ;
var
  i, iCount, iNumToken : integer ;
  lsValue : string ;
begin

  result := '' ;

  iNumToken := NumToken( pStrValue, pStrToken ) ;
  if pIntNum = 1 then begin
    if pos( pStrToken, pStrValue ) = 0 then result := pStrValue
    else result := copy( pStrValue, 1, pos( pStrToken, pStrValue )-1) ;
    end
  else if (iNumToken < pIntNum-1) or (pIntNum<1) then begin
    result := '' ;
    end
  else begin

    { Remove leading blocks }
    iCount := 1 ;
    lsValue := pStrValue ;
    i := pos( pStrToken, lsValue ) ;
    while (i<>0) and (iCount<pIntNum) do begin
      delete( lsValue, 1, i + length( pStrToken ) - 1 ) ;
      inc( iCount ) ;
      i := pos( pStrToken, lsValue ) ;
    end ;

    if (i=0) and (iCount=pIntNum) then result := lsValue
    else if (i=0) and (iCount<>pIntNum) then result := ''
    else result := copy( lsValue, 1, i-1) ;

  end ;
end ;

function TtiCommandLineParams.WordExtract( const psInput  : string ;
                        const piPos    : integer ;
                        const psDelims : string ) : string;
var iStart : integer ;
    i      : integer ;
	iLen   : integer ;
begin
  result := '' ;

  // Find the starting pos of the Nth word
  iStart := WordPosition( piPos, psInput, psDelims );

  if iStart <> 0 then begin
    i := iStart ;
    iLen := length( psInput ) ;
    // Build up result until we come to our next wordDelim
    // while (i <= iLen) and not(S[i] in psDelims) do begin
    while (i <= iLen) and not(CharInStr( ExtractChar( psInput, i ), psDelims )) do begin
      result := result + ExtractChar( psInput, i ) ;
      inc( i ) ;
    end ;
  end ;
end;

function TtiCommandLineParams.WordPosition( const pN : integer; const pS : string ;
                           psDelims : string) : integer ;
var
  lCount : integer;
  lI     : Word;
  lSLen  : integer ;
begin
  lCount := 0 ;
  lI     := 1 ;
  Result := 0 ;
  lSLen  := length( pS ) ;

  while ( lI <= lSLen) and ( lCount <> pN ) do begin
    while ( lI <= lSLen ) and ( CharInStr( ExtractChar( pS, lI ), psDelims )) do begin
      Inc( lI ) ;
    end;

    // if we're not beyond end of S, we're at the start of a word
    if lI <= lSLen then begin
      Inc( lCount ) ;
    end;

    // if not finished, find the end of the current word
    if lCount <> pN then begin
      while (lI <= lSLen) and not( CharInStr( ExtractChar( pS, lI ), psDelims )) do begin
        Inc( lI ) ;
      end;
    end else begin
      Result := lI ;
    end ;
  end ;
end ;

function TtiCommandLineParams.ExtractChar( const pValue : string ; const pPos : integer ) : char ;
var lResult : string ;
begin
    if pPos > length(pValue) then begin
        result := ' ';
        exit;
    end;
  lResult := copy( pValue, pPos, 1 ) ;
  result  := lResult[1] ;
end ;

function TtiCommandLineParams.StripLeadingDelims( const pStrToProcess : string;
                                     psDelims : string ) : string ;
var i : integer ;
    lCharCurrent : char ;
begin
  result := pStrToProcess ;
  // Loop through each char in the string
  for i := 1 to length( pStrToProcess ) do begin
    // Extract the current character
    lCharCurrent := ExtractChar( pStrToProcess, i ) ;

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr( lCharCurrent, psDelims) then begin
      result := copy( pStrToProcess, i,
                      length( pStrToProcess ) - i + 1 ) ;
      exit ; //==>
    // The current char is a word delim, but we are at the end of the string -
    // so no words
    end else begin
      if i = length( pStrToProcess ) then begin
        result := '' ;
      end ;
    end ;
  end ;
end ;

// Strip any trailing psDelims
function TtiCommandLineParams.StripTrailingDelims( const pStrToProcess : string;
                                      psDelims : string ) : string ;
var i : integer ;
    lCharCurrent : char ;
begin
  result := pStrToProcess ;
  // Loop through each char in the string
  for i := length( pStrToProcess ) downto 1 do begin
    // Extract the current character
    lCharCurrent := ExtractChar( pStrToProcess, i ) ;

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr( lCharCurrent, psDelims ) then begin
      result := copy( pStrToProcess, 1, i ) ;
      exit ; //==>
    // The current char is a word delim, but we are at the beginning of the string -
    // so no words
    end else begin
      if i = length( pStrToProcess ) then begin
        result := '' ;
      end ;
    end ;
  end ;
end ;

// Given a set of word delimiters, return number of words in S
function TtiCommandLineParams.WordCount(const pStrToProcess : string; psDelims : string ) : integer;
var i : integer ;
    lCharLast : char ;
    lCharCurrent : char ;
    lStrToProcess : string ;
begin

  // Strip any leading psDelims
  lStrToProcess := StripLeadingDelims( pStrToProcess, psDelims ) ;
  lStrToProcess := StripTrailingDelims( lStrToProcess, psDelims ) ;

  // If lStrToProcess is empty, then there are no words
  if lStrToProcess = '' then begin
    result := 0 ;
    exit ; //==>
  end ;

  // lStrToProcess is not empty, therefore there must be at least one word
  // Every wordDelim we find equals another word:
  // 0 word delim := 1 word
  // 1 word delim := 2 words...
  result := 1 ;

  // lCharLast is used to check for more than 1 wordDelim together
  lCharLast := #0 ;

  for i := 1 to length( lStrToProcess ) do begin
    lCharCurrent := ExtractChar( lStrToProcess, i ) ;
    if CharInStr( lCharCurrent, psDelims ) and
       not( CharInStr( lCharLast, psDelims )) then begin
      inc( result ) ;
    end ;
    lCharLast := lCharCurrent ;
  end ;
end ;

// Is pChr in the string pStr ?
function TtiCommandLineParams.CharInStr( const pChr : char ; const pStr : string ) : boolean ;
begin
  result := pos( pChr, pStr ) <> 0 ;
end ;

initialization

finalization
  uCommandLineParams.Free ;

end.


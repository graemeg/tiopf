unit tiDataBuffer_Cli;

{$I tiDefines.inc}

interface
uses
  tiDataBuffer_BOM
  ,Contnrs
  ,Classes
  ,SysUtils
  ,tiQuery
 ;

type

  TTextFileMetaData = (tfmdFieldName{, tfmdFieldKind, tfmdFieldWidth});
  TTextFileMetaDatas = set of TTextFileMetaData;

// ToDo: Merge with tiXMLToTIDataset.pas
//       There are more routines to convert TtiDataSets to an from XML in tiXMLToTIDataSets.pas
function  tiQueryToTIDataSet(const AQuery: TtiQuery; const pDataSet: TtiDataBuffer): Integer;
function  tiDataSetToString(pDataSet : TtiDataBuffer): string;
procedure tiDataSetToTextFile(pDataSet : TtiDataBuffer; AFileName : TFileName);
function  tiDataSetToHTML(    const pDataSet : TtiDataBuffer): string;
function  tiDataSetToHTMLV(   const pDataSet : TtiDataBuffer): string;

{
  From TurboPowers SysTools help file, which is available on
  http://sourceforge.net

  Parses a string into a series of tokens as defined by a given set of
  delimiters and returns the tokens in a string list.

  S is the string to be parsed. Delims is a string of one or more characters
  that define token delimiters. This is usually a space or comma but other
  characters are allowed. Any tokens contained within a pair of QuoteChars is
  considered a single token. If there is only one QuoteChar in S, it is not the
  last character in S, and there are one or more characters after QuoteChar that
  are not in Delims, then those characters are combined to make a single token.

  If AllowNulls is True, successive Delims characters are considered to
  represent an empty token. If AllowNulls is False, successive Delims are
  considered a single token. For example, consider the following string:

  ',,,This is a test'

  With Delims set to ' ,' (space and comma), if AllowNulls is set to True, 7
  tokens are returned (three empty tokens and four words). If AllowNulls is
  False, 4 tokens are returned (four words only).

  If S or Delims is an empty string, the function returns an empty string list.
  The following example fills a list box with 7 items (tokens):

  ExtractTokensS(
    ',,,This is a test', ' ,', '"', True, ListBox1.Items);
}

type
  TExtractTokenEvent = procedure(AIndex : integer; const AValue : string) of object;

function stExtractTokensL(const S : AnsiString;
                           const Delims : AnsiString;
                           QuoteChar : AnsiChar;
                           AllowNulls : Boolean;
                           pExtractTokenEvent : TExtractTokenEvent): Cardinal;

implementation
uses
  Math
  ,tiBaseObject
  ,tiObject
  ,tiUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiConstants
 ;

function  tiQueryToTIDataSet(const AQuery: TtiQuery; const pDataSet: TtiDataBuffer): Integer;
  procedure _AssignMetaData(const AQuery: TtiQuery; const pDataSet: TtiDataBuffer);
    var
    i : integer;
    lField : TtiDBMetaDataField;
  begin
    for i := 0 to AQuery.FieldCount - 1 do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.ObjectState := posClean;
      lField.Name := AQuery.FieldName(i);
      lField.Kind := AQuery.FieldKind(i);
      pDataSet.Fields.Add(lField);
    end;
  end;

  procedure _QueryRowToDataSet(const AQuery: TtiQuery; const pDataSet: TtiDataBuffer);
  var
    i    : integer;
    lRow : TtiDataBufferRow;
    lCell : TtiDataBufferCell;
    lStream : TMemoryStream;
  begin
    lRow := TtiDataBufferRow.Create;
    pDataSet.Add(lRow);
    for i := 0 to AQuery.FieldCount - 1 do
    begin
      lCell := TtiDataBufferCell.Create;
      case AQuery.FieldKind(i) of
      qfkString,                                  
      qfkLongString : lCell.ValueAsString  := AQuery.FieldAsStringByIndex[i];
      qfkInteger   : lCell.ValueAsInteger := AQuery.FieldAsIntegerByIndex[i];
      qfkFloat     : lCell.ValueAsFloat   := AQuery.FieldAsFloatByIndex[i];
      qfkDateTime  : lCell.ValueAsDateTime := AQuery.FieldAsDateTimeByIndex[i];
      qfkLogical   : lCell.ValueAsBool    := AQuery.FieldAsBooleanByIndex[i];
      qfkBinary    : begin
                        lStream := TMemoryStream.Create;
                        try
                          AQuery.AssignFieldAsStreamByIndex(i,lStream);
                          lCell.AssignFromStream(lStream);
                        finally
                          lStream.Free;
                        end;
                      end;
      else
        raise Exception.Create('Invalid tiQuery.FieldKind');
      end;
      lRow.Add(lCell);
    end;
  end;
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  Assert(pDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  _AssignMetaData(AQuery, pDataSet);
  while not AQuery.EOF do
  begin
    _QueryRowToDataSet(AQuery, pDataSet);
    AQuery.Next;
  end;
  Result := 0;
end;

function  tiDataSetToString(pDataSet : TtiDataBuffer): string;
var
  i, j : integer;
  lsLine : string;
begin

  Result := '';

  for i := 0 to pDataSet.Fields.Count-1 do
  begin
    Result := tiAddTrailingValue(Result, ',');
    Result := Result + pDataSet.Fields.Items[i].Name;
  end;
  Result := Result + CrLf;

  for i := 0 to pDataSet.Count - 1 do
  begin
    lsLine := '';
    for j := 0 to pDataSet.Items[i].Count-1 do
    begin
      lsLine := tiAddTrailingValue(lsLine, ',');
      lsLine := lsLine + pDataSet.Items[i].Items[j].ValueAsString;
    end;
    Result := Result + CrLf;
    Result := Result + lsLine;
  end;

end;

procedure tiDataSetToTextFile(pDataSet : TtiDataBuffer; AFileName : TFileName);
var
  ls : string;
begin
  ls := tiDataSetToString(pDataSet);
  tiStringToFile(ls, AFileName);
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// No, I don't know any assembler...
// The following routines (prefixed with st) where pasted from
// TurboPowers SysTools, which is available on http://sourceforge.net
// The aim of using these routines is go get the functionality from
// stExtractTokensL which strips a string into tokens to be used as the
// values in a data set read from a CSV file.
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{.Z+}
type
  stLStrRec = record
    AllocSize : Longint;
    RefCount : Longint;
    Length   : Longint;
  end;

const
  stStrOffset = SizeOf(stLStrRec);
{.Z-}

function stCharExistsL(const S : AnsiString; C : AnsiChar): Boolean; register;
  {-Count the number of a given character in a string. }
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-stStrOffset].stLStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;


function stExtractTokensL(const S : AnsiString;
                           const Delims : AnsiString;
                           QuoteChar : AnsiChar;
                           AllowNulls : Boolean;
                           pExtractTokenEvent : TExtractTokenEvent): Cardinal;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar   : AnsiChar;
  TokenStart : integer;
  Inx       : integer;
  lResult   : string;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or
     (Delims = '') or
     stCharExistsL(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart :
        begin
          {if the current char is the quote character, switch states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanQuotedToken

          {if the current char is a delimiter, output a null token}
          else if stCharExistsL(Delims, CurChar) then begin

            {if allowed to, output a null token}
            if AllowNulls then begin
              Inc(Result);
              lResult := '';
              pExtractTokenEvent(Result,lResult);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);
          end

          {otherwise, the current char is starting a normal token, so
           switch states}
          else
            State := ScanNormalToken
        end;

      ScanQuotedToken :
        begin
          {if the current char is the quote character, switch states}
          if (CurChar = QuoteChar) then
            State := ScanQuotedTokenEnd
        end;

      ScanQuotedTokenEnd :
        begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
          if (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
          else if stCharExistsL(Delims, CurChar) then begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
            if ((Inx - TokenStart) = 2) then begin
              if AllowNulls then begin
                Inc(Result);
                lResult := '';
                pExtractTokenEvent(Result,lResult);
              end
            end

            {else output the token without the quotes}
            else begin
              Inc(Result);
              lResult := Copy(S, succ(TokenStart), Inx - TokenStart - 2);
              pExtractTokenEvent(Result,lResult);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end

          {otherwise it's a (complex) normal token, so switch states}
          else
            State := ScanNormalToken
        end;

      ScanNormalToken :
        begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token}
          else if stCharExistsL(Delims, CurChar) then begin
            Inc(Result);
            lResult := Copy(S, TokenStart, Inx - TokenStart);
            pExtractTokenEvent(Result,lResult);

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end;
        end;

      ScanNormalTokenWithQuote :
        begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
          if (CurChar = QuoteChar) then
            State := ScanNormalToken;
        end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then begin
    inc(TokenStart);
    dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then begin
    Inc(Result);
    lResult := Copy(S, TokenStart, Inx - TokenStart);
    pExtractTokenEvent(Result,lResult);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then begin
    Inc(Result);
    lResult := '';
    pExtractTokenEvent(Result,lResult);
  end;
end;

function  tiDataSetToHTML(const pDataSet : TtiDataBuffer): string;
var
  i,j : integer;
  ls : string;
begin
  result :=
    '<table border=2 align="centre">' +
    '<tr>' +
    '<th align="centre" colspan=' + IntToStr(pDataSet.Fields.Count) + '>' +
    '<p>' + pDataSet.Name + '</p>' +
    '</td>' +
    '</tr>' +
    '<tr>';
  for i := 0 to pDataSet.Fields.Count - 1 do
    result := result + '<th>' + pDataSet.Fields.Items[i].Name;

  for i := 0 to pDataSet.Count - 1 do
  begin
    result := result + '<tr>';
    for j := 0 to pDataSet.Items[i].Count - 1 do
    begin
      if pDataSet.Items[i].Items[j] <> nil then
        ls := pDataSet.Items[i].Items[j].ValueAsString
      else
        ls := '*'; // This will add a '*' for empty rows
      result := result + '<td>' + ls;
    end;
  end;
  result := result + '</table>'
end;

function  tiDataSetToHTMLV(const pDataSet : TtiDataBuffer): string;
var
  i,j : integer;
  ls : string;
begin
  result :=
    '<table border=2 align="centre">' +
    '<tr>' +
    '<th align="centre" colspan=2>' +
    '<p>' + pDataSet.Name + '</p>' +
    '</td>' +
    '</tr>' +
    '<tr>';
  for i := 0 to pDataSet.Fields.Count - 1 do
  begin
    result := result + '<tr>';
    result := result + '<td>' + pDataSet.Fields.Items[i].Name;
    for j := 0 to pDataSet.Count - 1 do
    begin
      if pDataSet.Items[j].Items[i] <> nil then
        ls := pDataSet.Items[j].Items[i].ValueAsString
      else
        ls := '*'; // This will add a '*' for empty rows
      result := result + '<td>' + ls;
    end;
  end;
  result := result + '</table>'
end;

end.

unit tiTextParser;

interface

{$I tiDefines.inc}

uses
  tiBaseObject
  ,Classes
  ,SysUtils
 ;

type

  TtiTextParserCellEndEvent = procedure(const AString: string) of object;
  TtiTextParserNewLineEvent = procedure of object;
  TtiTextParserEndOfTextEvent = procedure of object;
  TtiTextParserEndOfLineEvent = procedure of object;
  TtiTextParserBeforeParseEvent  = procedure of object;
  TtiTextParserStartOfLineEvent = procedure of object;

//
//  Parser behaviour:
//
//  A text line is a sequence of cells. Cells are terminated by commas
//  or end of text line.
//  A text line is terminated by (CR, LF or CRLF) or end of text.
//  Commas cannot be embedded within cell text. This will be interpreted
//  as a new cell.
//
//  Row indexes are zero-based. Column indexes are one-based
//  Cursor does not advance when CR or LF characters enountered.
//  Cursor does not advance when end of text encountered.
//  BeforeParseEvent occurs at row, col (0,0)
//  StartOfLineEvent occurs at col 0.
//  First character on line (excluding leading CR, LF or CRLF) occurs at col 1.
//  EndOfCellEvent occurs when first "non-cell" character encountered.
//  or CR or LF or end of text encountered. If "non-cell" character encountered,
//  cursor will be pointing to that character.
//  NewLineEvent occurs only when a new line is encountered: CR, LF or CRLF.
//  NewLineEvent occurs before EndOfLineEvent.
//  EndOfLineEvent occurs before EndOfTextEvent.
//
//  Empty lines (eg a line-ending as the first character in a stream of text,
//  or the second and subsequent line-ending in as sequence of line-endings)
//  are interpreted as a line containing one empty cell.

  TtiTextParser = class(TtiBaseObject)
  private

    FStream: TMemoryStream;
    FBuffer: string;
    FOnBeforeParse: TtiTextParserBeforeParseEvent;
    FOnStartOfLine: TtiTextParserStartOfLineEvent;
    FOnCellEnd: TtiTextParserCellEndEvent;
    FOnNewLine: TtiTextParserNewLineEvent;
    FOnEndOfLine: TtiTextParserEndOfLineEvent;
    FOnEndOfText: TtiTextParserEndOfTextEvent;
    FRow: Integer;
    FCol: Integer;
    FToken: string;
    FEOL: boolean;
    FLineStarted: boolean;
    FCellStarted: boolean;

    procedure Execute;

  public

    constructor Create;
    destructor  Destroy; override;
    procedure   ParseFile(const AFileName: string);
    procedure   ParseString(const AString: string);
    procedure   ParseStream(AStream: TStream);

    property    OnBeforeParse: TtiTextParserBeforeParseEvent read FOnBeforeParse write FOnBeforeParse;
    property    OnStartOfLine: TtiTextParserStartOfLineEvent read FOnStartOfLine write FOnStartOfLine;
    property    OnCellEnd:     TtiTextParserCellEndEvent     read FOnCellEnd     write FOnCellEnd;
    property    OnNewLine:     TtiTextParserNewLineEvent     read FOnNewLine     write FOnNewLine;
    property    OnEndOfLine:   TtiTextParserEndOfLineEvent   read FOnEndOfLine   write FOnEndOfLine;
    property    OnEndOfText:   TtiTextParserEndOfTextEvent   read FOnEndOfText   write FOnEndOfText;
    property    Row:           Integer read FRow;
    property    Col:           Integer read FCol;
    property    Token:         string  read FToken;
  end;

implementation
uses
  tiConstants
  ,tiUtils
 ;

const
  cDefaultBufferLength = 256;

{ TtiTextParser }

constructor TtiTextParser.Create;
begin
  inherited Create;
  FStream:= TMemoryStream.Create;
  SetLength(FBuffer, cDefaultBufferLength);
end;

destructor TtiTextParser.destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TtiTextParser.Execute;

  procedure _DoCellEnd(var AIndex: Integer);
  begin

    if FCellStarted then
    begin
      FToken := Copy(FBuffer,1,AIndex);
      AIndex := 0;
      FCellStarted := false;
      FOnCellEnd(FToken);
    end;

  end;

  procedure _DoLineEnd(var AIndex: Integer);
  begin
    _DoCellEnd(AIndex);

    if FEOL then
      FOnNewLine;

    if FLineStarted then
      FOnEndOfLine;

    FEOL := false;
    FLineStarted := (FStream.Position < FStream.Size);
    FCellStarted := FLineStarted;

    if FLineStarted then
    begin
      Inc(FRow);
      FCol := 0;

      if Assigned(FOnStartOfLine) then
        FOnStartOfLine;

    end

  end;

var
  i : Integer;
  lChar: Char;
  lBufferIndex: Integer;

begin
  Assert(Assigned(FOnNewLine), 'OnLineEnd not assigned');
  Assert(Assigned(FOnEndOfLine), 'OnEndOfLine not assigned');
  Assert(Assigned(FOnCellEnd), 'OnCellEnd not assigned');
  Assert(Assigned(FOnEndOfText),  'OnEndOfText not assigned');
  lBufferIndex := 0;
  FRow:= 0;
  FCol:= 0;
  FEOL := false;

  if Assigned(FOnBeforeParse) then
    FOnBeforeParse;

  FLineStarted := (FStream.Position < FStream.Size);
  FCellStarted := FLineStarted;

  if FLineStarted and Assigned(FOnStartOfLine) then
      FOnStartOfLine;

  for i := 0 to FStream.Size - 1 do
  begin
    FStream.readBuffer(lChar, 1);
    Inc(FCol);

    // ToDo: Cell delim, line delim and quotes around strings should be
    //       params
    case lChar of
    chr(32)..chr(43),
    chr(45)..chr(126)
       : begin

           if FEOL then
           begin
             _DoLineEnd(lBufferIndex);
             FCol := 1;
           end;

           Inc(lBufferIndex);
           FBuffer[lBufferIndex]:= lChar;
         end;
    ',': begin // chr(44)

           if FEOL then
           begin
             _DoLineEnd(lBufferIndex);
             FCol := 1;
           end;

           _DoCellEnd(lBufferIndex);
           FCellStarted := true;
         end;
    #13, #10
       : begin
           FEOL := true;
           Dec(FCol);
         end;
    else
      // Add positional info here
      raise Exception.Create('Invalid character "' + lChar +
                             '" Position in file="' + IntToStr(i) +
                             '" Row="'+ IntToStr(FRow) +
                             '" Col="'+ IntToStr(FCol) + '"');
    end;
  end;

  _DoLineEnd(lBufferIndex);
  FOnEndOfText;
end;

procedure TtiTextParser.ParseFile(const AFileName: string);
begin
  Assert(AFileName <> '', 'AFileName not assigned');
  Assert(FileExists(AFileName), 'File not found <' + AFileName + '>');
  FStream.LoadFromFile(AFileName);
  Execute;
end;

procedure TtiTextParser.ParseStream(AStream: TStream);
begin
  Assert(AStream<>nil, 'AStream not assigned');
  tiCopyStream(AStream, FStream);
  Execute;
end;

procedure TtiTextParser.ParseString(const AString: string);
begin
  tiStringToStream(AString, FStream);
  Execute;
end;

end.


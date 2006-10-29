unit tiTextParser;

interface

{$I tiDefines.inc}

uses
  tiBaseObject
  ,Classes
  ,SysUtils
 ;

type

  TtiTextParserNewLineEvent = procedure of object;
  TtiTextParserCellEndEvent = procedure(const AString: string) of object;

  TtiTextParser = class(TtiBaseObject)
  private
    FStream: TMemoryStream;
    FBuffer: string;
    FOnCellEnd: TtiTextParserCellEndEvent;
    FOnNewLine: TtiTextParserNewLineEvent;
    FOnEndOfText: TtiTextParserNewLineEvent;
    FRow: Integer;
    FCol: Integer;
    FToken: string;
    FOnEndOfLine: TtiTextParserNewLineEvent;
    procedure Execute;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ParseFile(const AFileName: string);
    procedure   ParseString(const AString: string);
    procedure   ParseStream(AStream: TStream);
    property    OnNewLine:       TtiTextParserNewLineEvent read FOnNewLine   Write FOnNewLine;
    property    OnEndOfLine:     TtiTextParserNewLineEvent read FOnEndOfLine Write FOnEndOfLine;
    property    OnEndOfText:     TtiTextParserNewLineEvent read FOnEndOfText Write FOnEndOfText;
    property    OnCellEnd:       TtiTextParserCellEndEvent read FOnCellEnd    Write FOnCellEnd;
    property    Row:             Integer read FRow;
    property    Col:             Integer read FCol;
    property    Token:           string read FToken;
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
    FToken := Copy(FBuffer,1,AIndex);
    AIndex := 0;
    FOnCellEnd(FToken);
  end;
var
  i : Integer;
  lChar: Char;
  lIndex: Integer;
begin
  Assert(Assigned(FOnNewLine), 'OnLineEnd not assigned');
  Assert(Assigned(FOnEndOfLine), 'OnEndOfLine not assigned');
  Assert(Assigned(FOnCellEnd), 'OnCellEnd not assigned');
  Assert(Assigned(FOnEndOfText),  'OnEndOfText not assigned');
  lIndex := 0;
  FRow:= 0;
  FCol:= 0;
  for i := 0 to FStream.Size - 1 do
  begin
    FStream.readBuffer(lChar, 1);
    Inc(FCol);
    case lChar of
    'A'..'Z',
    'a'..'z',
    '0'..'9',
    ' ', '-',
    '_', '/',
    ':', '.',
    '(', ')',
    '&', '''',
    '*', '#',
    '@', '^',
    '%', '?',
    '"', '+',
    '`'
       : begin
            Inc(lIndex);
            FBuffer[lIndex]:= lChar;
          end;
    ',': begin
            _DoCellEnd(lIndex);
          end;
    #13 : begin
            _DoCellEnd(lIndex);
            FOnEndOfLine;
            FOnNewLine;
            Inc(FRow);
            FCol:= 0;
          end;
    #10 : begin
            FCol:= 0;
          end;
    else
      // Add positional info here
      raise Exception.Create('Invalid character "' + lChar + '" at position ' + IntToStr(i));
    end;
  end;
  if lIndex <> 0 then
  begin
    _DoCellEnd(lIndex);
    FOnEndOfLine;
    FOnEndOfText;
  end;
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


unit tiTextParser_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTextParser
  ,tiTestFramework
  ,Classes
 ;


type
  TtiResultItem = record
    Text: string;
    Row, Col: integer;
  end;

  TtiResultItems = array of TTiResultItem;

  TtiResults = class
  private
    FResults: TtiResultItems;
    function GetString(const AIndex: integer): string;
    function GetItem(const AIndex: integer): TtiResultItem;
  public
    procedure Add(const AString: string; const ARow: integer = 0; const ACol: integer = 0);
    function Count: integer;
    property Strings[const AIndex: integer]: string read GetString; default;
    property Items[const AIndex: integer]: TtiResultItem read GetItem;
  end;

  TtiTestParserTests = class(TtiTestCase)
  private
    FParser :TtiTextParser;
    FResults: TtiResults;
    procedure DoOnNewLine;
    procedure DoOnEndOfLine;
    procedure DoOnEndOfText;
    procedure DoOnCellEnd(const AString: string);
    procedure DoOnBeforeParse;
    procedure DoOnStartOfLine;
    procedure CheckCount(ACount: Integer);
    procedure CheckResults(const AValue: string; AIndex: Integer); overload;
    procedure CheckResults(const AValue: string; AIndex, ARow, ACol: Integer); overload;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure ParseEmptyString;
    procedure ParseNewLine1;
    procedure ParseNewLine2;
    procedure ParseNewLine3;
    procedure ParseString1;
    procedure ParseString2;
    procedure ParseString3;
    procedure ParseString4;
    procedure ParseString5;
    procedure ParseString6;
    procedure ParseString7;
    procedure ParseString8;
    procedure ParseMissingArg1;
    procedure ParseMissingArg2;
    procedure ParseMissingArg3;
    procedure ParseMissingArg4;
  end;


procedure RegisterTests;


implementation
uses
  tiTestDependencies
  ,SysUtils
 ;

const
  cNewLine = '<New Line>';
  cEndOfText = '<End of text>';
  cBeforeParse = '<Before Parse>';
  cStartOfLine = '<Start of Line>';
  cEndOfLine = '<End of Line>';

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TtiTestParserTests);
end;


{ TtiTestParserTests }

procedure TtiTestParserTests.CheckCount(ACount: Integer);
begin
  CheckEquals(ACount, FResults.Count, 'FResults.Count');
end;

procedure TtiTestParserTests.CheckResults(const AValue: string; AIndex: Integer);
begin
  CheckEquals(AValue, FResults.Strings[AIndex], '#' + IntToStr(AIndex));
end;

procedure TtiTestParserTests.CheckResults(const AValue: string; AIndex, ARow,
  ACol: Integer);
begin
  CheckEquals(AValue, FResults[AIndex], Format('FResults[%d].String', [AIndex]));
  CheckEquals(ARow, FResults.Items[AIndex].Row, Format('<%s> FResults[%d].Row', [AValue, AIndex]));
  CheckEquals(ACol, FResults.Items[AIndex].Col, Format('<%s> FResults[%d].Col', [AValue, AIndex]));
end;

procedure TtiTestParserTests.DoOnBeforeParse;
begin
  FResults.Add(cBeforeParse, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.DoOnCellEnd(const AString: string);
begin
  FResults.Add(AString, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.DoOnEndOfLine;
begin
  FResults.Add(cEndOfLine, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.DoOnEndOfText;
begin
  FResults.Add(cEndOfText, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.DoOnNewLine;
begin
  FResults.Add(cNewLine, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.DoOnStartOfLine;
begin
  FResults.Add(cStartOfLine, FParser.Row, FParser.Col);
end;

procedure TtiTestParserTests.ParseEmptyString;
begin
  FParser.ParseString('');
  CheckCount(2);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cEndOfText, 1, 0, 0);
end;

procedure TtiTestParserTests.ParseNewLine1;
begin
  FParser.ParseString(#13);
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 0);
  CheckResults(cNewLine, 3, 0, 0);
  CheckResults(cEndOfLine, 4, 0, 0);
  CheckResults(cEndOfText, 5, 0, 0);
end;

procedure TtiTestParserTests.ParseNewLine2;
begin
  FParser.ParseString(#10);
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 0);
  CheckResults(cNewLine, 3, 0, 0);
  CheckResults(cEndOfLine, 4, 0, 0);
  CheckResults(cEndOfText, 5, 0, 0);
end;

procedure TtiTestParserTests.ParseNewLine3;
begin
  FParser.ParseString(#13#10);
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 0);
  CheckResults(cNewLine, 3, 0, 0);
  CheckResults(cEndOfLine, 4, 0, 0);
  CheckResults(cEndOfText, 5, 0, 0);
end;

procedure TtiTestParserTests.ParseString1;
begin
  FParser.ParseString('cell1,cell2,cell3');
  CheckCount(7);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults('cell3', 4, 0, 17);
  CheckResults(cEndOfLine, 5, 0, 17);
  CheckResults(cEndOfText, 6, 0, 17);
end;

procedure TtiTestParserTests.ParseString2;
begin
  FParser.ParseString('cell1,cell2' + #13);
  CheckCount(7);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 11);
  CheckResults(cNewLine, 4, 0, 11);
  CheckResults(cEndOfLine, 5, 0, 11);
  CheckResults(cEndOfText, 6, 0, 11);
end;

procedure TtiTestParserTests.ParseString3;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10);
  CheckCount(7);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 11);
  CheckResults(cNewLine, 4, 0, 11);
  CheckResults(cEndOfLine, 5, 0, 11);
  CheckResults(cEndOfText, 6, 0, 11);
end;

procedure TtiTestParserTests.ParseString4;
begin
  FParser.ParseString('cell1,cell2' + #13 + 'cell3,cell4');
  CheckCount(11);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults(cNewLine, 4, 0, 12);
  CheckResults(cEndOfLine, 5, 0, 12);
  CheckResults(cStartOfLine, 6, 1, 0);
  CheckResults('cell3', 7, 1, 6);
  CheckResults('cell4', 8, 1, 11);
  CheckResults(cEndOfLine, 9, 1, 11);
  CheckResults(cEndOfText, 10, 1, 11);
end;

procedure TtiTestParserTests.ParseString5;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4');
  CheckCount(11);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults(cNewLine, 4, 0, 12);
  CheckResults(cEndOfLine, 5, 0, 12);
  CheckResults(cStartOfLine, 6, 1, 0);
  CheckResults('cell3', 7, 1, 6);
  CheckResults('cell4', 8, 1, 11);
  CheckResults(cEndOfLine, 9, 1, 11);
  CheckResults(cEndOfText, 10, 1, 11);
end;

procedure TtiTestParserTests.ParseString6;
begin
  FParser.ParseString('cell1,cell2'  + #10 + 'cell3,cell4');
  CheckCount(11);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults(cNewLine, 4, 0, 12);
  CheckResults(cEndOfLine, 5, 0, 12);
  CheckResults(cStartOfLine, 6, 1, 0);
  CheckResults('cell3', 7, 1, 6);
  CheckResults('cell4', 8, 1, 11);
  CheckResults(cEndOfLine, 9, 1, 11);
  CheckResults(cEndOfText, 10, 1, 11);
end;

procedure TtiTestParserTests.ParseString7;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4' + #13);
  CheckCount(12);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults(cNewLine, 4, 0, 12);
  CheckResults(cEndOfLine, 5, 0, 12);
  CheckResults(cStartOfLine, 6, 1, 0);
  CheckResults('cell3', 7, 1, 6);
  CheckResults('cell4', 8, 1, 11);
  CheckResults(cNewLine, 9, 1, 11);
  CheckResults(cEndOfLine, 10, 1, 11);
  CheckResults(cEndOfText, 11, 1, 11);
end;

procedure TtiTestParserTests.ParseString8;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4' + #13 + #10);
  CheckCount(12);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('cell2', 3, 0, 12);
  CheckResults(cNewLine, 4, 0, 12);
  CheckResults(cEndOfLine, 5, 0, 12);
  CheckResults(cStartOfLine, 6, 1, 0);
  CheckResults('cell3', 7, 1, 6);
  CheckResults('cell4', 8, 1, 11);
  CheckResults(cNewLine, 9, 1, 11);
  CheckResults(cEndOfLine, 10, 1, 11);
  CheckResults(cEndOfText, 11, 1, 11);
end;

procedure TtiTestParserTests.ParseMissingArg1;
begin
  FParser.ParseString(',');
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 1);
  CheckResults('', 3, 0, 1);
  CheckResults(cEndOfLine, 4, 0, 1);
  CheckResults(cEndOfText, 5, 0, 1);
end;

procedure TtiTestParserTests.ParseMissingArg2;
begin
  FParser.ParseString(',,');
  CheckCount(7);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 1);
  CheckResults('', 3, 0, 2);
  CheckResults('', 4, 0, 2);
  CheckResults(cEndOfLine, 5, 0, 2);
  CheckResults(cEndOfText, 6, 0, 2);
end;

procedure TtiTestParserTests.ParseMissingArg3;
begin
  FParser.ParseString('cell1,');
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('cell1', 2, 0, 6);
  CheckResults('', 3, 0, 6);
  CheckResults(cEndOfLine, 4, 0, 6);
  CheckResults(cEndOfText, 5, 0, 6);
end;

procedure TtiTestParserTests.ParseMissingArg4;
begin
  FParser.ParseString(',cell1');
  CheckCount(6);
  CheckResults(cBeforeParse, 0, 0, 0);
  CheckResults(cStartOfLine, 1, 0, 0);
  CheckResults('', 2, 0, 1);
  CheckResults('cell1', 3, 0, 6);
  CheckResults(cEndOfLine, 4, 0, 6);
  CheckResults(cEndOfText, 5, 0, 6);
end;

procedure TtiTestParserTests.SetUp;
begin
  inherited;
  FParser            := TtiTextParser.Create;
  FParser.OnNewLine  := DoOnNewLine;
  FParser.OnEndOfLine := DoOnEndOfLine;
  FParser.OnCellEnd  := DoOnCellEnd;
  FParser.OnEndOfText := DoOnEndOfText;
  FParser.OnBeforeParse := DoOnBeforeParse;
  FParser.OnStartOfLine := DoOnStartOfLine;
  FResults           := TtiResults.Create;
end;

procedure TtiTestParserTests.TearDown;
begin
  FParser.Free;
  FResults.Free;
  inherited;
end;

{ TtiResults }

procedure TtiResults.Add(const AString: string; const ARow, ACol: integer);
var
  LItem: TtiResultItem;
begin
  LItem.Text := AString;
  LItem.Row := ARow;
  LItem.Col := ACol;
  SetLength(FResults, Length(FResults) + 1);
  FResults[Length(FResults) - 1] := LItem;
end;

function TtiResults.Count: integer;
begin
  Result := Length(FResults);
end;

function TtiResults.GetItem(const AIndex: integer): TtiResultItem;
begin
  Assert((AIndex >= 0) and (AIndex < Count), 'index range error');
  Result := FResults[AIndex];
end;

function TtiResults.GetString(const AIndex: integer): string;
begin
  Assert((AIndex >= 0) and (AIndex < Count), 'index range error');
  Result := FResults[AIndex].Text;
end;

end.

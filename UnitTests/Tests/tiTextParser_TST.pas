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
  TtiTestParserTests = class(TtiTestCase)
  private
    FParser :TtiTextParser;
    FResults: TStringList;
    procedure DoOnNewLine;
    procedure DoOnEndOfLine;
    procedure DoOnEndOfText;
    procedure DoOnCellEnd(const AString: string);
    procedure CheckCount(ACount: Integer);
    procedure CheckResults(const AValue: string; AIndex: Integer);
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure ParseString1;
    procedure ParseString2;
    procedure ParseString3;
    procedure ParseString4;
    procedure ParseString5;
    procedure ParseString6;
    procedure ParseString7;
  end;


procedure RegisterTests;


implementation
uses
  tiDUnitDependencies
  ,SysUtils
 ;

const
  cNewLine = '<New Line>';
  cEndOfText = '<End of text>';

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

procedure TtiTestParserTests.DoOnCellEnd(const AString: string);
begin
  FResults.Add(AString);
end;

procedure TtiTestParserTests.DoOnEndOfLine;
begin

end;

procedure TtiTestParserTests.DoOnEndOfText;
begin
  FResults.Add(cEndOfText);
end;

procedure TtiTestParserTests.DoOnNewLine;
begin
  FResults.Add(cNewLine);
end;

procedure TtiTestParserTests.ParseString1;
begin
  FParser.ParseString('cell1,cell2,cell3');
  CheckCount(4);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults('cell3', 2);
  CheckResults(cEndOfText, 3);
end;

procedure TtiTestParserTests.ParseString2;
begin
  FParser.ParseString('cell1,cell2' + #13);
  CheckCount(3);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
end;

procedure TtiTestParserTests.ParseString3;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10);
  CheckCount(3);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
end;

procedure TtiTestParserTests.ParseString4;
begin
  FParser.ParseString('cell1,cell2' + #13 + 'cell3,cell4');
  CheckCount(6);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
  CheckResults('cell3', 3);
  CheckResults('cell4', 4);
  CheckResults(cEndOfText, 5);
end;

procedure TtiTestParserTests.ParseString5;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4');
  CheckCount(6);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
  CheckResults('cell3', 3);
  CheckResults('cell4', 4);
  CheckResults(cEndOfText, 5);
end;

procedure TtiTestParserTests.ParseString6;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4' + #13);
  CheckCount(6);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
  CheckResults('cell3', 3);
  CheckResults('cell4', 4);
  CheckResults(cNewLine, 5);
end;

procedure TtiTestParserTests.ParseString7;
begin
  FParser.ParseString('cell1,cell2' + #13 + #10 + 'cell3,cell4' + #13 + #10);
  CheckCount(6);
  CheckResults('cell1', 0);
  CheckResults('cell2', 1);
  CheckResults(cNewLine, 2);
  CheckResults('cell3', 3);
  CheckResults('cell4', 4);
  CheckResults(cNewLine, 5);
end;

procedure TtiTestParserTests.SetUp;
begin
  inherited;
  FParser            := TtiTextParser.Create;
  FParser.OnNewLine  := DoOnNewLine;
  FParser.OnEndOfLine := DoOnEndOfLine;
  FParser.OnCellEnd  := DoOnCellEnd;
  FParser.OnEndOfText := DoOnEndOfText;
  FResults           := TStringList.Create;
end;

procedure TtiTestParserTests.TearDown;
begin
  FParser.Free;
  FResults.Free;
  inherited;
end;

end.

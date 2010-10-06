unit tiTokenLibrary_TST; 

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiTestFramework
  ;

type

  TTestTITokenLibrary= class(TtiTestCase)
  published
    procedure TestSimpleTokensMultipleSeparators;
    procedure TestSimpleTokenSingleSeparator;
    procedure TestEscapeParamater;
    procedure TestLeftRightMarkParameters;
    procedure TestMultipleSeparators;
    procedure TestVolumeTokenLib;
    procedure TestPrefixLine;
    procedure TestSimpleFileLine;
    procedure TestComplexFileLine;
    procedure TestDirectoryLine;
    procedure TestSummaryLine;
  end;


procedure RegisterTests;


implementation

uses
  tiTestDependencies
  ,tiTokenLibrary
  ;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTITokenLibrary);
end;


{ TTestTITokenLibrary }

procedure TTestTITokenLibrary.TestSimpleTokensMultipleSeparators;
var
  t: TTokens;
begin
  t := TTokens.Create('', ',', '"', '"', '\');
  try
    CheckEquals('', t.Token(1), 'Failed on 1');
    CheckEquals(0, t.TokenCount, 'Failed on 2');
  finally
    t.Free;
  end;

  t := TTokens.Create('a,b,c', ',', '"', '"', '\');
  try
    CheckEquals('a', t.Token(1), 'Failed on 3');
    CheckEquals('b', t.Token(2), 'Failed on 4');
    CheckEquals('c', t.Token(3), 'Failed on 5');
    CheckEquals('', t.Token(4), 'Failed on 6');
    CheckEquals(3, t.TokenCount, 'Failed on 7');
  finally
    t.Free;
  end;

  t := TTokens.Create('aa,bb,cc', ',', '"', '"', '\');
  try
    CheckEquals('aa', t.Token(1), 'Failed on 8');
    CheckEquals('bb', t.Token(2), 'Failed on 9');
    CheckEquals('cc', t.Token(3), 'Failed on 10');
    CheckEquals('', t.Token(4), 'Failed on 11');
    CheckEquals(3, t.TokenCount, 'Failed on 12');
  finally
    t.Free;
  end;

  t := TTokens.Create('aa,,bb,cc', ',', '"', '"', '\');
  try
    CheckEquals('aa', t.Token(1), 'Failed on 13');
    CheckEquals('bb', t.Token(2), 'Failed on 14');
    CheckEquals('cc', t.Token(3), 'Failed on 15');
    CheckEquals(3, t.TokenCount, 'Failed on 16');
  finally
    t.Free;
  end;
end;

procedure TTestTITokenLibrary.TestSimpleTokenSingleSeparator;
var
  t: TTokens;
begin
  t := TTokens.Create('aa,,bb,cc', ',', '"', '"', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals('aa', t.Token(1), 'Failed on 1');
    CheckEquals('', t.Token(2), 'Failed on 2');
    CheckEquals('bb', t.Token(3), 'Failed on 3');
    CheckEquals('cc', t.Token(4), 'Failed on 4');
    CheckEquals(4, t.TokenCount, 'Failed on 5');
  finally
    t.Free;
  end;
end;

procedure TTestTITokenLibrary.TestEscapeParamater;
const
  cLine = 'F          19838 4971CE8E "f:\Common\Modules\AC044\000.A C044 - Module 7\000~s.swf"';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(4, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('F', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('19838', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('4971CE8E', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals('f:CommonModulesAC044000.A C044 - Module 7000~s.swf', FieldSpec.Token(4), 'Failed on 5');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestLeftRightMarkParameters;
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create('"1" "2" "3"', ' ', '"', '"', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals(3, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('3', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals(True, '"3"' <> FieldSpec.Token(3), 'Failed on 5');
  finally
    FieldSpec.Free;
  end;
  
  FieldSpec := TTokens.Create('<1> <2> <3>', ' ', '<', '>', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals(3, FieldSpec.TokenCount, 'Failed on 5');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 6');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 7');
    CheckEquals('3', FieldSpec.Token(3), 'Failed on 8');
    CheckEquals(True, '<3>' <> FieldSpec.Token(3), 'Failed on 9');
  finally
    FieldSpec.Free;
  end;

  // Lets try a weird one - though it should still work!
  FieldSpec := TTokens.Create('"1> "2> "3>', ' ', '"', '>', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals(3, FieldSpec.TokenCount, 'Failed on 10');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 11');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 12');
    CheckEquals('3', FieldSpec.Token(3), 'Failed on 13');
    CheckEquals(True, '"3>' <> FieldSpec.Token(3), 'Failed on 14');
  finally
    FieldSpec.Free;
  end;

  // Another odd one - if double left/right markers are found, they are
  // treated as tokens on there own
  FieldSpec := TTokens.Create('"1>> "2> "3>>', ' ', '"', '>', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals(5, FieldSpec.TokenCount, 'Failed on 15');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 16');
    CheckEquals('>', FieldSpec.Token(2), 'Failed on 17');
    CheckEquals('2', FieldSpec.Token(3), 'Failed on 18');
    CheckEquals('3', FieldSpec.Token(4), 'Failed on 19');
    CheckEquals('>', FieldSpec.Token(5), 'Failed on 20');
  finally
    FieldSpec.Free;
  end;
end;

// Two separators are specified. A space and a comma.
procedure TTestTITokenLibrary.TestMultipleSeparators;
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create('1,2 3', ' ,', '"', '"', '\', tsSingleSeparatorBetweenTokens);
  try
    CheckEquals(3, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('3', FieldSpec.Token(3), 'Failed on 4');
  finally
    FieldSpec.Free;
  end;
  
  FieldSpec := TTokens.Create('1,,2  3 , 4', ' ,', '"', '"', '\', tsMultipleSeparatorsBetweenTokens);
  try
    CheckEquals(4, FieldSpec.TokenCount, 'Failed on 5');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 6');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 7');
    CheckEquals('3', FieldSpec.Token(3), 'Failed on 8');
    CheckEquals('4', FieldSpec.Token(4), 'Failed on 9');
  finally
    FieldSpec.Free;
  end;

  FieldSpec := TTokens.Create('1,,2  "3 , 4"', ' ,', '"', '"', '\', tsMultipleSeparatorsBetweenTokens);
  try
    CheckEquals(3, FieldSpec.TokenCount, 'Failed on 10');
    CheckEquals('1', FieldSpec.Token(1), 'Failed on 11');
    CheckEquals('2', FieldSpec.Token(2), 'Failed on 12');
    CheckEquals('3 , 4', FieldSpec.Token(3), 'Failed on 13');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestVolumeTokenLib;
const
  cLine = 'V                      Label = UPDMAR2007CD3   VolSer = 9B2FE2A8';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(7, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('V', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('UPDMAR2007CD3', FieldSpec.Token(4), 'Failed on 3');
    CheckEquals('9B2FE2A8', FieldSpec.Token(7), 'Failed on 4');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestPrefixLine;
const
  cLine = 'P                         f:                                   ';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(2, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('P', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('f:', FieldSpec.Token(2), 'Failed on 3');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestSimpleFileLine;
const
  cLine = 'F             50 F4008661 f:\AUTORUN.INF                       ';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(4, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('F', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('50', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('F4008661', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals('f:\AUTORUN.INF', FieldSpec.Token(4), 'Failed on 5');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestComplexFileLine;
const
  cLine = 'F          19838 4971CE8E "f:\Common\Modules\AC044\000.A C044 - Module 7\000~s.swf"';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', #$00);
  try
    CheckEquals(4, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('F', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('19838', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('4971CE8E', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals('f:\Common\Modules\AC044\000.A C044 - Module 7\000~s.swf', FieldSpec.Token(4), 'Failed on 5');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestDirectoryLine;
const
  cLine = 'D            201 2B45CE29 f:\Common\Modules\AC044 1            ';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(5, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('D', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('201', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('2B45CE29', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals('f:\Common\Modules\AC044', FieldSpec.Token(4), 'Failed on 5');
    CheckEquals('1', FieldSpec.Token(5), 'Failed on 6');
  finally
    FieldSpec.Free;
  end;
end;

procedure TTestTITokenLibrary.TestSummaryLine;
const
  cLine = 'S      399613538 68AFDD73 6005 468                              ';
var
  FieldSpec: TTokens;
begin
  FieldSpec := TTokens.Create(cLine, ' ', '"', '"', '\');
  try
    CheckEquals(5, FieldSpec.TokenCount, 'Failed on 1');
    CheckEquals('S', FieldSpec.Token(1), 'Failed on 2');
    CheckEquals('399613538', FieldSpec.Token(2), 'Failed on 3');
    CheckEquals('68AFDD73', FieldSpec.Token(3), 'Failed on 4');
    CheckEquals('6005', FieldSpec.Token(4), 'Failed on 5');
    CheckEquals('468', FieldSpec.Token(5), 'Failed on 6');
  finally
    FieldSpec.Free;
  end;
end;


end.


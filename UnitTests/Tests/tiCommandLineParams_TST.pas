unit tiCommandLineParams_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework,
  tiParams
 ;

type

  TTestTICommandLineParams = class(TtiTestCase)
  private
    FCML: ICMLParams;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure   NoParams;
    procedure   OneName;
    procedure   OneNameValue;
    procedure   OneValue;
    procedure   ThreeValues;
    procedure   TwoNameValues;
  end;

procedure RegisterTests;

implementation
uses
  tiCommandLineParams
//  ,tiUtils
  ,Classes
//  ,SysUtils
  ,tiTestDependencies
//  ,tiConstants
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTICommandLineParams);
end;

type
  ICMLParamSetter = interface
    ['{9B7C1E42-04CA-482A-B3B3-C8DD9ADAD086}']
    function Params: TStrings;
  end;

  TMockCML = class(TInterfacedObject, ICMLParams, ICMLParamSetter)
  private
    FParams: TStrings;
  protected
    function ParamCount: integer;
    function ParamStr(const AIndex: integer): string;
    function Params: TStrings;
  public
    constructor Create;
    destructor Destroy;
  end;

{ TTestTICommandLineParams }

procedure TTestTICommandLineParams.NoParams;
var
  LCLP: TtiCommandLineParams;
begin
  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals(0, LCLP.Params.Count, 'FCML intially empty');
    CheckEquals(false, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.OneName;
var
  LCLP: TtiCommandLineParams;
  LMock: ICMLParamSetter;
begin
    LMock := FCML as ICMLParamSetter;
    LMock.Params.Clear;
    LMock.Params.Add('-a');

  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals('-a', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A='#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(false, LCLP.IsParam('b'), 'LCLP.IsParam(b)');
    CheckEquals('', LCLP.GetParam('b'), 'LCLP.GetParam(b)');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.OneNameValue;
var
  LCLP: TtiCommandLineParams;
  LMock: ICMLParamSetter;
begin
    LMock := FCML as ICMLParamSetter;
    LMock.Params.Clear;
    LMock.Params.Add('-a');
    LMock.Params.Add('b');

  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals('-a b', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A=b'#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('b', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(false, LCLP.IsParam('b'), 'LCLP.IsParam(b)');
    CheckEquals('', LCLP.GetParam('b'), 'LCLP.GetParam(b)');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.OneValue;
var
  LCLP: TtiCommandLineParams;
  LMock: ICMLParamSetter;
begin
    LMock := FCML as ICMLParamSetter;
    LMock.Params.Clear;
    LMock.Params.Add('a');

  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals('a', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A='#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(false, LCLP.IsParam('b'), 'LCLP.IsParam(b)');
    CheckEquals('', LCLP.GetParam('b'), 'LCLP.GetParam(b)');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.SetUp;
begin
  inherited;
  FCML := TMockCML.Create;
end;

procedure TTestTICommandLineParams.TearDown;
begin
  inherited;
  FCML := nil;
end;

procedure TTestTICommandLineParams.ThreeValues;
var
  LCLP: TtiCommandLineParams;
  LMock: ICMLParamSetter;
begin
    LMock := FCML as ICMLParamSetter;
    LMock.Params.Clear;
    LMock.Params.Add('a');
    LMock.Params.Add('b');
    LMock.Params.Add('c');

  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals('a b c', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A=b c'#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('b c', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.TwoNameValues;
var
  LCLP: TtiCommandLineParams;
  LMock: ICMLParamSetter;
begin
    LMock := FCML as ICMLParamSetter;
    LMock.Params.Clear;
    LMock.Params.Add('-a');
    LMock.Params.Add('b');
    LMock.Params.Add('-c');
    LMock.Params.Add('d');

  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals('-a b -c d', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A=b'#13#10'C=d'#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('b', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(true, LCLP.IsParam('c'), 'LCLP.IsParam(c)');
    CheckEquals('d', LCLP.GetParam('c'), 'LCLP.GetParam(d)');
  finally
    LCLP.Free;
  end;

end;

{ TMockCML }

constructor TMockCML.Create;
begin
  inherited;
  FParams := TStringList.Create;
end;

destructor TMockCML.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TMockCML.ParamCount: integer;
begin
  Result := FParams.Count;
end;

function TMockCML.Params: TStrings;
begin
  Result := FParams;
end;

function TMockCML.ParamStr(const AIndex: integer): string;
begin
  Result := FParams[AIndex - 1]
end;

end.

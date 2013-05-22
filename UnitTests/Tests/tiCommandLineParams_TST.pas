unit tiCommandLineParams_TST;

{$I tiDefines.inc}
{$DEFINE NEW_VERSION_CLP}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework,
  tiParams,
  Classes;

type

  // Mock Interface for manipulating command-line params
  IMockCLParams = interface(ICLParams)
    ['{9B7C1E42-04CA-482A-B3B3-C8DD9ADAD086}']
    function Params: TStrings;
  end;

  TTestTICommandLineParams = class(TtiTestCase)
  private
    FCLParams: ICLParams;
    FMockCLParams: IMockCLParams;

  protected
    procedure SetUp; override;

  public
    constructor Create; override;

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
  tiCommandLineParams,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTICommandLineParams);
end;

type
  TMockCLParams = class(TInterfacedObject, ICLParams, IMockCLParams)
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

constructor TTestTICommandLineParams.Create;
begin
  inherited;
  FMockCLParams := TMockCLParams.Create;
  FCLParams := FMockCLParams as ICLParams;
end;

procedure TTestTICommandLineParams.NoParams;
var
  LCLP: TtiCommandLineParams;
begin
  LCLP := TtiCommandLineParams.Create(FCLParams);
  try
    CheckEquals(0, LCLP.Params.Count, 'FCL intially empty');
    CheckEquals(false, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(false, LCLP.IsParam(''), 'LCLP.IsParam("")');
    CheckEquals('', LCLP.GetParam(''), 'LCLP.GetParam("")');
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.OneName;
var
  LCLP: TtiCommandLineParams;
begin
  FMockCLParams.Params.Add('-a');

  LCLP := TtiCommandLineParams.Create(FCLParams);
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
begin
  FMockCLParams.Params.Add('-a');
  FMockCLParams.Params.Add('b');

  LCLP := TtiCommandLineParams.Create(FCLParams);
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
begin
  FMockCLParams.Params.Add('a');

  LCLP := TtiCommandLineParams.Create(FCLParams);
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
  FMockCLParams.Params.Clear;
end;

procedure TTestTICommandLineParams.ThreeValues;
var
  LCLP: TtiCommandLineParams;
begin
  FMockCLParams.Params.Add('a');
  FMockCLParams.Params.Add('b');
  FMockCLParams.Params.Add('c');
  LCLP := nil;

{$IFDEF NEW_VERSION_CLP}
  StartExpectingException(ECLParamValue);
{$ENDIF}
  try
    LCLP := TtiCommandLineParams.Create(FCLParams);
{$IFDEF NEW_VERSION_CLP}
    StopExpectingException('Exception not thrown');
{$ELSE}
    CheckEquals('a b c', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A=b c'#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('b c', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
{$ENDIF}
  finally
    LCLP.Free;
  end;

end;

procedure TTestTICommandLineParams.TwoNameValues;
var
  LCLP: TtiCommandLineParams;
begin
  FMockCLParams.Params.Add('-a');
  FMockCLParams.Params.Add('b');
  FMockCLParams.Params.Add('-c');
  FMockCLParams.Params.Add('d');

  LCLP := TtiCommandLineParams.Create(FCLParams);
  try
    CheckEquals('-a b -c d', LCLP.AsString, 'LCLP.AsString');
    CheckEquals('A=b'#13#10'C=d'#13#10, LCLP.Params.Text, 'LCLP.Params.Text');
    CheckEquals(true, LCLP.IsParam('a'), 'LCLP.IsParam(a)');
    CheckEquals('b', LCLP.GetParam('a'), 'LCLP.GetParam(a)');
    CheckEquals(true, LCLP.IsParam('c'), 'LCLP.IsParam(c)');
    CheckEquals('d', LCLP.GetParam('c'), 'LCLP.GetParam(d)');
    CheckEquals(false, LCLP.IsParam('e'), 'LCLP.IsParam(e)');
    CheckEquals('', LCLP.GetParam('e'), 'LCLP.GetParam(e)');
  finally
    LCLP.Free;
  end;

end;

{ TCLMock }

constructor TMockCLParams.Create;
begin
  inherited;
  FParams := TStringList.Create;
end;

destructor TMockCLParams.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TMockCLParams.ParamCount: integer;
begin
  Result := FParams.Count;
end;

function TMockCLParams.Params: TStrings;
begin
  Result := FParams;
end;

function TMockCLParams.ParamStr(const AIndex: integer): string;
begin
  Result := FParams[AIndex - 1]
end;

end.

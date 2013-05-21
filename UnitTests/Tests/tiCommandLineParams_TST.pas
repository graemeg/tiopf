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
    procedure   ThreeParams;
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

procedure TTestTICommandLineParams.ThreeParams;
var
  LCLP: TtiCommandLineParams;
begin
  LCLP := TtiCommandLineParams.Create(FCML);
  try
    CheckEquals(3, LCLP.Params.Count, 'FCML with 3 params');
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

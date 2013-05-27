unit tiParams;

interface

uses
  Classes;

type
  // Define interface for accessing command line params. This facilitates
  // dependency injection into TtiCommandLineParams,
  // which in turn allows unit testing of TtiCommandLineParams with a
  // mock version of ICLParams
  ICLParams = interface
    ['{5C412079-A69B-4C24-A4E9-77201B5DA0F3}']
    function ParamCount: integer;
    function ParamStr(const AIndex: integer): string;
  end;

function CreateSystemCLParams: ICLParams;

type

  // Mock Interface for manipulating command-line params
  IMockCLParams = interface(ICLParams)
    ['{9B7C1E42-04CA-482A-B3B3-C8DD9ADAD086}']
    function Params: TStrings;
  end;

function CreateMockCLParams: IMockCLParams;

implementation

type
  TSystemCLParams = class(TInterfacedObject, ICLParams)
    protected
    function ParamCount: integer;
    function ParamStr(const AIndex: integer): string;
  end;

function CreateSystemCLParams: ICLParams;
begin
  Result := TSystemCLParams.Create;
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
    destructor Destroy; override;
  end;

function CreateMockCLParams: IMockCLParams;
begin
  Result := TMockCLParams.Create;
end;

{ TCLParams }

function TSystemCLParams.ParamCount: integer;
begin
  Result := System.ParamCount;
end;

function TSystemCLParams.ParamStr(const AIndex: integer): string;
begin
  Result := System.ParamStr(AIndex);
end;

{ TMockCLParams }

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

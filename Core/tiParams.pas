unit tiParams;

interface

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

{ TCLParams }

function TSystemCLParams.ParamCount: integer;
begin
  Result := System.ParamCount;
end;

function TSystemCLParams.ParamStr(const AIndex: integer): string;
begin
  Result := System.ParamStr(AIndex);
end;

end.

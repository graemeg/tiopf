unit tiParams;

interface

type
  // Define interface to allow dependency injection into TtiCommandLineParams,
  // which in turn allows unit testing of  TtiCommandLineParams with a
  // mock version of ICMLParams
  ICMLParams = interface
    ['{5C412079-A69B-4C24-A4E9-77201B5DA0F3}']
    function ParamCount: integer;
    function ParamStr(const AIndex: integer): string;
  end;

  // Default implementation that uses System calls
  function CreateCMLParams: ICMLParams;

implementation

type
  TCMLParams = class(TInterfacedObject, ICMLParams)
    protected
    function ParamCount: integer;
    function ParamStr(const AIndex: integer): string;
  end;

  function CreateCMLParams: ICMLParams;
  begin
    Result := TCMLParams.Create;
  end;

{ TCMLParams }

function TCMLParams.ParamCount: integer;
begin
  Result := System.ParamCount;
end;

function TCMLParams.ParamStr(const AIndex: integer): string;
begin
  Result := System.ParamStr(AIndex);
end;

end.

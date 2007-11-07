unit tiCGIExcept;

interface
uses
  SysUtils
  ;
  
type

  EtiCGIException = class( Exception )
  private
    FExitCode: Integer;
  public
    constructor Create(AExitCode: Integer; const AMessage: string = '');
    property    ExitCode: Integer read FExitCode;
  end ;

implementation

{ EtiCGIException }

constructor EtiCGIException.Create(AExitCode: Integer; const AMessage: string);
var
  LMessage: string;
begin
  if AMessage = '' then
    LMessage := 'CGI Exception ' + IntToStr(AExitCode)
  else
    LMessage := 'CGI Exception ' + IntToStr(AExitCode) + ' "' + AMessage + '"';
  inherited Create(LMessage);
  FExitCode := AExitCode ;
end;


end.

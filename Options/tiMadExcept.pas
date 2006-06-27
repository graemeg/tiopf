unit tiMadExcept;

interface
uses
  SysUtils
  ,Windows
  ;

{$IFDEF MadExcept}
function tiMadExceptBugReport(AException: Exception): string;
{$ENDIF}

implementation
{$IFDEF MadExcept}
uses
  MadExcept
  ;

function tiMadExceptBugReport(AException: Exception): string;
begin
  Result := MadExcept.CreateBugReport(False, AException, nil, GetCurrentThreadID, 0, 0, False, nil);
end;
{$ENDIF}

end.

unit tiMadExcept;

interface
uses
  SysUtils
  ,Windows
 ;

function tiMadExceptBugReport(AException: Exception): string;

implementation
{$IFDEF MadExcept}
uses
  MadExcept
 ;
{$ENDIF}

function tiMadExceptBugReport(AException: Exception): string;
begin
{$ifdef MadExcept}
  Result := MadExcept.CreateBugReport(False, AException, nil, GetCurrentThreadID, 0, 0, False, nil);
{$else}
  Result:= 'MadExcept not linked.'#13#13 + AException.message;
{$endif}
end;

end.

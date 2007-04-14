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
{$IFDEF madexcept3}
  Result := MadExcept.CreateBugReport(etNormal, AException, nil, GetCurrentThreadID);
{$ELSE}
{$IFDEF madexcept}
  Result := MadExcept.CreateBugReport(False, AException, nil, GetCurrentThreadID, 0, 0, False, nil);
{$ELSE}
  Result:= 'MadExcept not linked.'#13#13 + AException.message;
{$ENDIF}
{$ENDIF}
end;

end.

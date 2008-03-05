unit tiMadExcept;

interface
uses
  SysUtils
  ,Windows
 ;

function  tiMadExceptBugReport(AException: Exception): string;
procedure tiMadExceptMailBugReport(const AException: Exception); overload;
procedure tiMadExceptMailBugReport(const AText: string); overload;

implementation
{$IFDEF MadExcept}
uses
  MadExcept
 ;
{$ENDIF}

function tiMadExceptBugReport(AException: Exception): string;
begin
{$IFDEF madexcept}
  Result := MadExcept.CreateBugReport(etNormal, AException, nil, GetCurrentThreadID);
{$ELSE}
  Result:= 'MadExcept not linked.'#13#13 + AException.message;
{$ENDIF}
end;

procedure tiMadExceptMailBugReport(const AException: Exception);
{$IFDEF madexcept}
  var
    ls: string;
{$ENDIF}
begin
  Assert(AException = AException);  // Getting rid of compiler hints, param not used.
  {$IFDEF madexcept}
  ls := MadExcept.CreateBugReport(etNormal, AException, nil, GetCurrentThreadID);
  tiMadExceptMailBugReport(ls);
  {$ENDIF}
end;

procedure tiMadExceptMailBugReport(const AText: string);
begin
  Assert(AText = AText);  // Getting rid of compiler hints, param not used.
  {$IFDEF madexcept}
  MadExcept.AutoSendBugReport(AText, nil);
  {$ENDIF}
end;

end.

program Demo_ReadMe;

{$APPTYPE CONSOLE}

uses
  SysUtils;

begin
  WriteLn('');
  WriteLn('These demos require the following on your Delphi search path:');
  WriteLn('  \Core');
  WriteLn('  \Options');
  WriteLn('  \GUI');
  WriteLn('');
  WriteLn('You must also install the package tiOPFGUIDsgn into the IDE.');
  WriteLn('');
  WriteLn('Press <Enter> to continue.');
  ReadLn;
end.

program AdrsBookUIHTML;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
  AdrsBookUIHTML_BOM in 'AdrsBookUIHTML_BOM.pas';

begin
  // ToDo: Implement Params
  Write(TAdrsBookUIHTML.Execute(''));
end.

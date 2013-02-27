program transform;

{
Author:
Uwe Fechner <ufechner@4commerce.de>
copyright:
4commerce technologies AG
kamerbalken 10-14
22525 Hamburg
}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestXSLT in 'TestXSLT.pas';

begin
  writeln;
  writeln('Demo for libxslt!');
  writeln('=================');
  writeln;
  if paramcount=0 then begin
    writeln('Usage:');
    writeln('transform <xml-filename> <stylesheet-filename> <ouput-filename>');
    writeln;
    writeln('For example:');
    writeln('transform 17-1.xml 17-2.xsl 17-3.html');
  end
  else
    if paramcount = 3 then begin
      test2(paramStr(1),paramStr(2),paramStr(3));
    end else begin
      writeln('Invalid parameter count!')
    end;
end.



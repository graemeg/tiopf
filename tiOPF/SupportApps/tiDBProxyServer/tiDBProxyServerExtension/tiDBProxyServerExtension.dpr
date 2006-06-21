program tiDBProxyServerExtension;

uses
  madExcept,
  madLinkDisAsm,
  Windows,
  SysUtils,
  System;

{$APPTYPE CONSOLE}
var
ls : string ;
begin
  ls := ParamStr(0);
  ls := Trim(Copy(GetCommandLine, Length(ls)+3, Length(GetCommandLine)-Length(ls)));
  WriteLn('<html>You''r in<p>' +
          'The parameters are: ' + ls +
          '</html>');
end.

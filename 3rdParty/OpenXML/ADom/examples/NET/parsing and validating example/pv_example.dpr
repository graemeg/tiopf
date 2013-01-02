program pv_example;

uses
  System.Reflection,
  System.Runtime.CompilerServices,
  System.Runtime.InteropServices,
  SysUtils,
  Forms,
  main in 'main.pas' {Mainpage};

[STAThread]
begin
  Application.Initialize;
  Application.CreateForm(TMainpage, Mainpage);
  Application.Run;
end.

program xpath_example;

uses
  Forms,
  main in 'main.pas' {Mainpage};

[STAThread]
begin
  Application.Initialize;
  Application.CreateForm(TMainpage, Mainpage);
  Application.Run;
end.

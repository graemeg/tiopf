program xpath_example;

uses
  Forms,
  main in 'main.pas' {Mainpage};

begin
  Application.Initialize;
  Application.CreateForm(TMainpage, Mainpage);
  Application.Run;
end.

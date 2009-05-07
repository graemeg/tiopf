program testproject;

uses
  Forms,
  sample in 'sample.pas' {Form1},
  tiDeMediators in '..\tiDeMediators.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

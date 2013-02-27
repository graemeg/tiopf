program MakeBS;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uFields in 'uFields.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'MakeBS';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program tifilter_iterator_demo;

uses
  Forms,
  main_form in 'main_form.pas' {Form3},
  person in 'person.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

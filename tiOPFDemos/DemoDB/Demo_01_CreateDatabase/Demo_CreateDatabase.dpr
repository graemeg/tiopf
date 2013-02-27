program Demo_CreateDatabase;

uses
  Forms,
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FMainCreateDatabase in 'FMainCreateDatabase.pas' {FormMainCreateDatabase};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainCreateDatabase, FormMainCreateDatabase);
  Application.Run;
end.

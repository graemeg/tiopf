program Demo_InsertData;

uses
  Forms,
  FMainInsertData in 'FMainInsertData.pas' {Form2},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase};

{$R *.res}

begin
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

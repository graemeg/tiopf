program Demo_OrdinalTypes;

uses
  tiLog,
  Forms,
  FMainOrdinalTypes in 'FMainOrdinalTypes.pas' {FormMainOrdinalTypes},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in 'Client_BOM.pas',
  FtiPerEditDialog in '..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FClientEdit in 'FClientEdit.pas' {FormClientEdit};

{$R *.RES}

begin
  SetupLogForClient;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  Client_BOM.RegisterMappings;
  
  Application.Initialize;
  Application.CreateForm(TFormMainOrdinalTypes, FormMainOrdinalTypes);
  Application.Run;
end.

program Demo_LookupList;

uses
  tiLog,
  Forms,
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in 'Client_BOM.pas',
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  FMainLookupList in 'FMainLookupList.pas' {FormMainLookupList},
  FtiPerEditDialog in '..\Common\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.RES}

begin
//  SetupLogForClient;
  if not TFormConnectToDatabase.Execute then
    Exit; //==>

  Client_BOM.RegisterMappings;

  Application.Initialize;
  Application.CreateForm(TFormMainLookupList, FormMainLookupList);
  Application.Run;
end.


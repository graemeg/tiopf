program Demo_CollectionWithFilter;

uses
  Forms,
  tiLog,
  Client_BOM in 'Client_BOM.pas',
  Client_Svr in 'Client_Svr.pas',
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FMainCollectionWithFilter in 'FMainCollectionWithFilter.pas' {FormCollectionHardCoded},
  FPopulateDatabase in 'FPopulateDatabase.pas' {FormPopulateDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase};

{$R *.res}

begin
  SetupLogForClient(true);
  
  Application.Initialize;
  if not TFormConnectToDatabase.Execute(True) then
    Exit ; //==>

  TFormPopulateDatabase.Execute ;

  Application.CreateForm(TFormCollectionHardCoded, FormCollectionHardCoded);
  Application.Run;
end.

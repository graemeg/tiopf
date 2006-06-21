program Demo_CollectionHardCoded;

uses
  Forms,
  tiLog,
  Client_Svr in 'Client_Svr.pas',
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_BOM in '..\Demo_05_Collection_Common\Client_BOM.pas',
  FMainCollection in '..\Demo_05_Collection_Common\FMainCollection.pas' {FormCollection};

{$R *.res}

begin
  SetupLogForClient(true);
  
  Application.Initialize;
  if not TFormConnectToDatabase.Execute(True) then
    Exit ; //==>

  Application.CreateForm(TFormCollection, FormCollection);
  Application.Run;
end.

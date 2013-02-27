program Demo_CollectionAutoMap;

uses
  tiLog,
  Forms,
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FMainCollection in '..\Demo_05_Collection_Common\FMainCollection.pas' {FormCollection},
  Client_BOM in '..\Demo_05_Collection_Common\Client_BOM.pas';

{$R *.res}

begin
  SetupLogForClient;

  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>

  // Register the class-database mappings
  Client_BOM.RegisterMappings;

  Application.CreateForm(TFormCollection, FormCollection);
  Application.Run;

end.

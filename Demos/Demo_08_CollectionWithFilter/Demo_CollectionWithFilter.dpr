program Demo_CollectionWithFilter;

uses
  Forms,
  tiLogToGUI,
  Client_BOM in 'Client_BOM.pas',
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FMainCollectionWithFilter in 'FMainCollectionWithFilter.pas' {FormCollectionHardCoded},
  FPopulateDatabase in 'FPopulateDatabase.pas' {FormPopulateDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Client_HardCodedVisitors_Svr in 'Client_HardCodedVisitors_Svr.pas';

{$R *.res}

begin

  Application.Initialize;
  if not TFormConnectToDatabase.Execute(True) then
    Exit ; //==>

  Client_HardCodedVisitors_Svr.RegisterVisitors;

  if not TFormPopulateDatabase.Execute then
    Exit; //==>

  Application.CreateForm(TFormCollectionHardCoded, FormCollectionHardCoded);
  Application.Run;
end.

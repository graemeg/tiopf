program Demo_Collection;

uses
  Forms,
  tiLogToGUI,
  Client_AutoMap_Svr,
  Client_HardCodedVisitors_Svr,
  Client_DBIndependentVisitors_Svr,
  FMainCollection in 'FMainCollection.pas' {FormCollection},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  DemoDBUtils in '..\Common\DemoDBUtils.pas',
  WhichPersistenceMechanism in '..\Common\WhichPersistenceMechanism.pas';

{$R *.res}

begin

  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit; //==>

  if not WhichPersistenceMechanism.Execute(
      Client_AutoMap_Svr.RegisterMappings,
      Client_HardCodedVisitors_Svr.RegisterVisitors,
      Client_DBIndependentVisitors_Svr.RegisterVisitors
   ) then
    Exit; //==>


  Application.CreateForm(TFormCollection, FormCollection);
  Application.Run;

end.



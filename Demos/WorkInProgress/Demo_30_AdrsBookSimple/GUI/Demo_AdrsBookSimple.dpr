program Demo_AdrsBookSimple;

uses
  tiObject,
  tiLog,
  tiLogToGUI,
  tiOPFManager,
  Adrs_Dependencies,
  Forms,
  FMain in 'FMain.pas' {FormMain},
  FtiFormMgrForm in '..\..\..\..\tiOPF2\Trunk\GUI\FtiFormMgrForm.pas' {FormTIFormMgrForm},
  FWorkList in 'FWorkList.pas' {FormWorkList},
  FPersonList in 'FPersonList.pas' {FormPersonList},
  Adrs_Manager in '..\BOM\Adrs_Manager.pas';

{$R *.RES}

begin

  ConnectToDatabase;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.






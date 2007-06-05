program Demo_AdrsBook_TreeView;

uses
  tiObject,
  tiLog,
  tiLogToGUI,
  tiOPFManager,
  Forms,
  AdrsMetaData_BOM,
  FtiAdrsListMain in 'FtiAdrsListMain.pas' {FormMain},
  FtiAdrsListChild_Person in 'FtiAdrsListChild_Person.pas' {FormEditPerson},
  FEdit_Addrs in 'FEdit_Addrs.pas' {FormEdit_Adrs},
  FEdit_EAddrs in 'FEdit_EAddrs.pas' {FormEdit_EAdrs},
  FtiAdrsListChild_Company in 'FtiAdrsListChild_Company.pas' {FormEditCompany},
  FPickDatabase in '..\..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  Adrs_Dependencies in '..\BOM\Adrs_Dependencies.pas',
  FtiDialogAbs in '..\..\..\..\tiOPF2\Trunk\GUI\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\..\..\tiOPF2\Trunk\GUI\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.RES}

begin

  if not TFormConnectToDatabase.Execute(false,1) then
    Exit; //==>

  // Check for required tables and create them if missing
  AdrsMetaData_BOM.CheckDatabaseStructure;

  Application.Initialize;

  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.



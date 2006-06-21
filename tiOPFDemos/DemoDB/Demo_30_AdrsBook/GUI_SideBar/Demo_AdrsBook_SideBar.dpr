program Demo_AdrsBook_SideBar;

uses
  tiObjAbs,
  tiLog,
  tiPersist,
  Forms,
  AdrsMetaData_BOM,
  Adrs_BOM,
  FPickDatabase in '..\..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  FMain in 'FMain.pas' {FormMain},
  FWorkList in 'FWorkList.pas' {FormWorkList},
  FtiAdrsListChild_Person in 'FtiAdrsListChild_Person.pas' {FormEditPerson},
  FtiAdrsListChild_Company in 'FtiAdrsListChild_Company.pas' {FormEditCompany},
  FEdit_Addrs in 'FEdit_Addrs.pas' {FormEdit_Adrs},
  FEdit_EAddrs in 'FEdit_EAddrs.pas' {FormEdit_EAdrs},
  Adrs_Dependencies in '..\BOM\Adrs_Dependencies.pas',
  FtiPerEditDialog in '..\..\..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FtiDialogAbs in '..\..\..\..\tiPerAwareCtrls\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiFormMgrForm in '..\..\..\..\tiPerAwareCtrls\FtiFormMgrForm.pas' {FormTIFormMgrForm},
  tiFormMgr in '..\..\..\..\tiPerAwareCtrls\tiFormMgr.pas';

{$R *.RES}

begin

  // Setup the log framework for visual logging.
  tiLog.SetupLogForClient ;

  if not TFormConnectToDatabase.Execute( false, 1 ) then
    Exit ; //==>

  // Check for required tables and create them if missing
  AdrsMetaData_BOM.CheckDatabaseStructure;
  gAdrsBook.Read ;
  
  Application.Initialize;

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormWorkList, FormWorkList);
  Application.Run;

end.

program tiGroupMail;

uses
  tiLog,
  tiPersist,
  tiQueryXML,
  tiPerObjOIDInteger,
  tiClassToDBMap_BOM,
  tiClassToDBMap_Srv,
  tiCommandLineParams,
  Forms,
  Windows,
  FMain in 'FMain.pas' {FormMain},
  FConfig in 'FConfig.pas' {FormConfig},
  FEditListMember in 'FEditListMember.pas' {FormEditListMember},
  tiGroupMailDependencies in 'tiGroupMailDependencies.pas',
  FtiPerEditDialog in '..\..\..\..\..\TechInsite\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.RES}

begin

  if gCommandLineParams.IsParam('NoTrayIcon') then
    SetupLogForClient
  else
    SetupLogForServer ;

  CreateMutex(nil, False, 'tiGroupMail');
  Application.Initialize;
  gTIPerMgr.LoadDatabaseLayer(
    'XML',
    'tiLstSrvr.xml',
    'null',
    'null' ) ;

  Application.Title := 'TechInsite List Server';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.

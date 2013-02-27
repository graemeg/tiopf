Program DemoApp;

uses
  madExcept,
  madLinkDisAsm,
  tiLog,
  tiPersist,
  Forms,
  Main in 'Main.pas' {frmMain},
  Surgery_BOM in 'Surgery_BOM.pas',
  Surgery_HardCodeSQL in 'Surgery_HardCodeSQL.pas',
  ListBase in 'ListBase.pas' {frmListBase},
  PropForm in 'PROPFORM.pas' {frmPropertySheet},
  DoctorMaint in 'DoctorMaint.pas' {frmDoctorMaint},
  DoctorList in 'DoctorList.pas' {frmDoctorList},
  PatientList in 'PatientList.pas' {frmPatientList},
  PatientMaint in 'PatientMaint.pas' {frmPatientMaint},
  SlotCreate in 'SlotCreate.pas' {frmApptCreate},
  AppoinmentDetails in 'AppoinmentDetails.pas' {frmApptDetails},
  LookupBase in 'LookupBase.pas' {frmLookup};

{$R *.RES}

Begin
  SetupLogForClient;
  Application.Initialize;
  gTiPerMgr.LoadPersistenceFramework;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPatientMaint, frmPatientMaint);
  Application.CreateForm(TfrmApptCreate, frmApptCreate);
  Application.CreateForm(TfrmApptDetails, frmApptDetails);
  Application.Run;
  gTiPerMgr.UnloadPersistenceFramework;
End.


Unit Main;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ComCtrls, ExtCtrls, tiListView, tiPerAwareCtrls,
  tiListViewPlus, ToolWin, Surgery_BOM, Buttons, tiFocusPanel, madExceptVcl;

Type
  TfrmMain = Class(TForm)
    MainMenu1 : TMainMenu;
    File1 : TMenuItem;
    Exit1 : TMenuItem;
    alMain : TActionList;
    sbMain : TStatusBar;
    actExit : TAction;
    actDoctors : TAction;
    Doctors1 : TMenuItem;
    N1 : TMenuItem;
    Help1 : TMenuItem;
    ShowHierarchy1 : TMenuItem;
    Panel1 : TPanel;
    actPatients : TAction;
    Patients1 : TMenuItem;
    cmbDoctor : TtiPerAwareComboBoxDynamic;
    dtpCurrentDate : TtiPerAwareDateTimePicker;
    New1 : TMenuItem;
    AppointmentSlots1 : TMenuItem;
    actNewSlots : TAction;
    lvAppointments : TtiListViewPlus;
    actBook : TAction;
    actCancel : TAction;
    actArrive : TAction;
    actStart : TAction;
    actEnd : TAction;
    actAppDetails : TAction;
    actPatientDetails : TAction;
    Panel2 : TPanel;
    spdBook : TSpeedButton;
    spdCancel : TSpeedButton;
    spdArrive : TSpeedButton;
    spdStart : TSpeedButton;
    spdEnd : TSpeedButton;
    spdAppDetails : TSpeedButton;
    spdPatientDetails : TSpeedButton;
    MadExceptionHandler1: TMadExceptionHandler;
    Procedure actExitExecute(Sender : TObject);
    Procedure actDoctorsExecute(Sender : TObject);
    Procedure FormCreate(Sender : TObject);
    Procedure ShowHierarchy1Click(Sender : TObject);
    Procedure actPatientsExecute(Sender : TObject);
    Procedure actNewSlotsExecute(Sender : TObject);
    Procedure actNewSlotsUpdate(Sender : TObject);
    Procedure cmbDoctorChange(Sender : TObject);
    Procedure lvAppointmentsListColumns3DeriveColumn(
      Const pLV : TtiCustomListView; Const pData : TPersistent;
      Const ptiListColumn : TtiListColumn; Var pResult : String);
    Procedure dtpCurrentDateChange(Sender : TObject);
    Procedure FormDestroy(Sender : TObject);
    Procedure lvAppointmentsDblClick(pLV : TtiCustomListView;
      pData : TPersistent; pItem : TListItem);
    Procedure actBookExecute(Sender : TObject);
    Procedure actAppDetailsExecute(Sender : TObject);
    Procedure actAppDetailsUpdate(Sender : TObject);
    Procedure actPatientDetailsUpdate(Sender : TObject);
    Procedure actPatientDetailsExecute(Sender : TObject);
    Procedure actCancelExecute(Sender : TObject);
    Procedure actArriveExecute(Sender : TObject);
    Procedure actStartExecute(Sender : TObject);
    Procedure actEndExecute(Sender : TObject);
  Private
    Function CurrentAppointment : TAppointment;
  Public
    Procedure RefreshView;
  End;

Var
  frmMain : TfrmMain;

Implementation

Uses StdStuff, tiPtnVisPerObj_Cli, tiPerObjOIDInteger, tiPersist,
  PatientList, PatientMaint, DoctorList, SlotCreate, AppoinmentDetails, LookupBase;

{$R *.DFM}

Procedure TfrmMain.actExitExecute(Sender : TObject);
Begin
  Close;
End;

Procedure TfrmMain.actDoctorsExecute(Sender : TObject);
Begin
  With TfrmDoctorList.Create(Nil) Do
  Begin
    Try
      ShowModal;
    Finally
      Free;
    End;
  End;
End;

Procedure TfrmMain.FormCreate(Sender : TObject);
Begin
  gTiPerMgr.Read(Surgery);
  cmbDoctor.List := Surgery.Doctors.List;
  cmbDoctor.LinkToData(Surgery, 'CurrentDoctor');
  dtpCurrentDate.LinkToData(Surgery, 'CurrentDate');
  lvAppointments.Data := Surgery.Appointments.List;
End;

Procedure TfrmMain.ShowHierarchy1Click(Sender : TObject);
Begin
  tiShowPerObjAbs(Surgery);
End;

Procedure TfrmMain.actPatientsExecute(Sender : TObject);
Begin
  With TfrmPatientList.Create(Nil) Do
  Begin
    Try
      ShowModal;
    Finally
      Free;
    End;
  End;
End;

Procedure TfrmMain.actNewSlotsExecute(Sender : TObject);
Begin
  With TfrmApptCreate.Create(Nil) Do
  Begin
    Try
      ShowModal;
    Finally
      Free;
    End;
  End;
End;

Procedure TfrmMain.actNewSlotsUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := (Trim(Surgery.CurrentDoctor.Name) <> '');
End;

Procedure TfrmMain.cmbDoctorChange(Sender : TObject);
Begin
  RefreshView;
End;

Procedure TfrmMain.lvAppointmentsListColumns3DeriveColumn(
  Const pLV : TtiCustomListView; Const pData : TPersistent;
  Const ptiListColumn : TtiListColumn; Var pResult : String);
Begin
  With (pData As TAppointment) Do
  Begin
    If Patient.OID.IsNull Then
      pResult := ''
    Else
      pResult := Patient.Name
  End;
End;

Procedure TfrmMain.RefreshView;
Begin
  Surgery.Appointments.Empty;
  gTiPerMgr.Read(Surgery.Appointments);
  Surgery.Appointments.SortByProps(['Scheduled']);
  lvAppointments.Refresh;
End;

Procedure TfrmMain.dtpCurrentDateChange(Sender : TObject);
Begin
  RefreshView;
End;

Procedure TfrmMain.FormDestroy(Sender : TObject);
Begin
  lvAppointments.Data := Nil;
End;

Procedure TfrmMain.lvAppointmentsDblClick(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Begin
  actAppDetails.Execute;
End;

Procedure TfrmMain.actBookExecute(Sender : TObject);
Var
  lPatient : TPatient;
  lAppt : TAppointment;
Begin
  lAppt := TAppointment(lvAppointments.SelectedData);
  lPatient := TPatient(TfrmLookup.Lookup(Surgery.Patients, '', ''));
  If (lPatient <> Nil) Then
  Begin
    lAppt.Patient := lPatient;
    lAppt.Dirty := True;
    lAppt.Save;
    lvAppointments.Refresh;
  End;
End;

Procedure TfrmMain.actAppDetailsExecute(Sender : TObject);
Begin
  If (TfrmApptDetails.Edit(CurrentAppointment) = mrOK) Then
    lvAppointments.Refresh;
End;

Procedure TfrmMain.actAppDetailsUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := (lvAppointments.SelectedData <> Nil);
End;

// This was an intentional bug!!!!!! 
Procedure TfrmMain.actPatientDetailsUpdate(Sender : TObject);
Var
  lAppt : TAppointment;
  lCanDo : Boolean;
Begin
  lCanDo := False;
  If lvAppointments.CanEdit Then
  Begin
    lAppt := TAppointment(lvAppointments.SelectedData);
    lCanDo := Not (lAppt.Patient.OID.IsNull);
  End;
  (Sender As TAction).Enabled := lCanDo;
End;

Procedure TfrmMain.actPatientDetailsExecute(Sender : TObject);
Begin
  TfrmPatientMaint.Edit(TAppointment(lvAppointments.SelectedData).Patient);
End;

Function TfrmMain.CurrentAppointment : TAppointment;
Begin
  Result := Nil;
  If (lvAppointments.SelectedData <> Nil) Then
    Result := TAppointment(lvAppointments.SelectedData);
End;

Procedure TfrmMain.actCancelExecute(Sender : TObject);
Var
  lAppointment : TAppointment;
Begin
  With CurrentAppointment Do
  Begin
    If PatientAssigned Then
    Begin
      lAppointment := TAppointment.CreateNew;
      lAppointment.Scheduled := Scheduled;
      lAppointment.Duration := Duration;
      lAppointment.Doctor := Doctor;
      Surgery.Appointments.Add(lAppointment);
      lAppointment.Save;
    End;
    IsCancelled := True;
    Dirty := True;
    Save;
  End;
  RefreshView;
End;

Procedure TfrmMain.actArriveExecute(Sender : TObject);
Begin
  With CurrentAppointment Do
  Begin
    Arrived := Now;
    Dirty := True;
    Save;
  End;
  RefreshView;
End;

Procedure TfrmMain.actStartExecute(Sender : TObject);
Begin
  With CurrentAppointment Do
  Begin
    Started := Now;
    Dirty := True;
    Save;
  End;
  RefreshView;
End;

Procedure TfrmMain.actEndExecute(Sender : TObject);
Begin
  With CurrentAppointment Do
  Begin
    Finished := Now;
    Dirty := True;
    Save;
  End;
  RefreshView;
End;

End.


Unit Surgery_HardCodeSQL;

Interface

Uses Surgery_BOM, tiPtnVisSQL;

Type
  TReadDoctor = Class(TVisOwnedQrySelect)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
    Procedure MapRowToObject; Override;
  End;

  TCreateDoctor = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TUpdateDoctor = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TDeleteDoctor = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TReadPatient = Class(TVisOwnedQrySelect)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
    Procedure MapRowToObject; Override;
  End;

  TUpdatePatient = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TCreatePatient = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TDeletePatient = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TReadAppointment = Class(TVisOwnedQrySelect)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
    Procedure MapRowToObject; Override;
  End;

  TCreateAppointment = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

  TUpdateAppointment = Class(TVisOwnedQryUpdate)
  Protected
    Function AcceptVisitor : Boolean; Override;
    Procedure Init; Override;
    Procedure SetupParams; Override;
  End;

Implementation

Uses tiPtnVisPerObj_Cli, tiPersist, tiUtils, tiPtnVisPerObj, tiLog,
  tiPerObjOIDInteger, SysUtils, StdStuff;

{ TReadPatient }

Function TReadPatient.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TPatientList);
End;

Procedure TReadPatient.Init;
Begin
  Query.SQL.Text := 'Select * From Patients';
End;

Procedure TReadPatient.MapRowToObject;
Var
  lData : TPatient;
  lDocId : Integer;
Begin
  lData := TPatient.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.Name := Query.FieldAsString['Name'];
  lData.Address := Query.FieldAsString['Address'];
  lData.PostCode := Query.FieldAsString['PostCode'];
  lData.DateOfBirth := Query.FieldAsDateTime['DateOfBirth'];
  lDocId := Query.FieldAsInteger['Doctor'];
  If (lDocId <> 0) Then
    lData.Doctor := GetDoctor(lDocID);
  lData.ObjectState := posClean;
  TPerVisList(Visited).Add(lData);
End;

  { TCreateDoctor }

Function TCreateDoctor.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TDoctor) And (TDoctor(Visited).ObjectState = posCreate);
End;

Procedure TCreateDoctor.Init;
Begin
  Query.SQL.Text := 'Insert Into Doctors (OID, Name, NHSNumber) ' +
    'Values (:OID, :Name, :NHSNumber)';
End;

Procedure TCreateDoctor.SetupParams;
Var
  lData : TDoctor;
Begin
  lData := TDoctor(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString['Name'] := lData.Name;
  Query.ParamAsString['NHSNumber'] := lData.NHSNumber;
End;

{ TReadDoctor }

Function TReadDoctor.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TDoctorList);
End;

Procedure TReadDoctor.Init;
Begin
  Query.SQL.Text := 'Select * From Doctors';
End;

Procedure TReadDoctor.MapRowToObject;
Var
  lData : TDoctor;
Begin
  lData := TDoctor.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.Name := Query.FieldAsString['Name'];
  lData.NHSNumber := Query.FieldAsString['NHSNumber'];
  lData.ObjectState := posClean;
  TPerVisList(Visited).Add(lData);
End;

Procedure TReadDoctor.SetupParams;
Begin
// Nothing to do.
End;

Procedure TReadPatient.SetupParams;
Begin
// Nothing to do.
End;

{ TUpdateDoctor }

Function TUpdateDoctor.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TDoctor) And (TDoctor(Visited).ObjectState = posUpdate);
End;

Procedure TUpdateDoctor.Init;
Begin
  Query.SQL.Text := 'Update Doctors Set ' +
    'Name = :Name, ' +
    'NHSNumber = :NHSNumber ' +
    'Where OID = :OID';
End;

Procedure TUpdateDoctor.SetupParams;
Var
  lData : TDoctor;
Begin
  lData := TDoctor(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString['Name'] := lData.Name;
  Query.ParamAsString['NHSNumber'] := lData.NHSNumber;
End;

{ TDeleteDoctor }

Function TDeleteDoctor.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TDoctor) And (TDoctor(Visited).ObjectState = posDelete);
End;

Procedure TDeleteDoctor.Init;
Begin
  Query.SQL.Text := 'Delete From Doctors Where OID = :OID';
End;

Procedure TDeleteDoctor.SetupParams;
Var
  lData : TDoctor;
Begin
  lData := TDoctor(Visited);
  lData.OID.AssignToTIQuery(Query);
End;

{ TUpdatePatient }

Function TUpdatePatient.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TPatient) And (TPatient(Visited).ObjectState = posUpdate);
End;

Procedure TUpdatePatient.Init;
Begin
  Query.SQL.Text := 'Update Patients Set ' +
    'Name = :Name, ' +
    'Address = :Address, ' +
    'PostCode = :PostCode, ' +
    'DateOfBirth = :DateOfBirth, ' +
    'Doctor = :Doctor ' +
    'Where OID = :OID';
End;

Procedure TUpdatePatient.SetupParams;
Var
  lData : TPatient;
Begin
  lData := TPatient(Visited);
  Query.ParamAsString['Name'] := lData.Name;
  Query.ParamAsString['Address'] := lData.Address;
  Query.ParamAsString['PostCode'] := lData.PostCode;
  Query.ParamAsDateTime['DateOfBirth'] := lData.DateOfBirth;
  Query.ParamAsInteger['Doctor'] := lData.DoctorID;
  lData.OID.AssignToTIQuery(Query);
End;

{ TCreatePatient }

Function TCreatePatient.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TPatient) And (TPatient(Visited).ObjectState = posCreate);
End;

Procedure TCreatePatient.Init;
Begin
  Query.SQL.Text := 'Insert Into Patients (OID, Name, Address, PostCode, DateOfBirth, Doctor) ' +
    'Values (:OID, :Name, :Address, :PostCode, :DateOfBirth, :Doctor)';
End;

Procedure TCreatePatient.SetupParams;
Var
  lData : TPatient;
Begin
  lData := TPatient(Visited);
  Query.ParamAsString['Name'] := lData.Name;
  Query.ParamAsString['Address'] := lData.Address;
  Query.ParamAsString['PostCode'] := lData.PostCode;
  Query.ParamAsDateTime['DateOfBirth'] := lData.DateOfBirth;
  Query.ParamAsInteger['Doctor'] := lData.DoctorID;
  lData.OID.AssignToTIQuery(Query);
End;

{ TDeletePatient }

Function TDeletePatient.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TPatient) And (TPatient(Visited).ObjectState = posDelete);
End;

Procedure TDeletePatient.Init;
Begin
  Query.SQL.Text := 'Delete From Patients Where OID = :OID';
End;

Procedure TDeletePatient.SetupParams;
Var
  lData : TPatient;
Begin
  lData := TPatient(Visited);
  lData.OID.AssignToTIQuery(Query);
End;

{ TReadAppointment }

Function TReadAppointment.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TAppointmentList);
End;

Procedure TReadAppointment.Init;
Begin
  Query.SQL.Text := 'Select * From Appointments ' +
    'Where Doctor = :Doctor And Scheduled Between :FromDate And :ToDate';
End;

Procedure TReadAppointment.MapRowToObject;
Var
  lData : TAppointment;
  lInt : Integer;
Begin
  lData := TAppointment.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.Arrived := Query.FieldAsDateTime['Arrived'];
  lData.Comment := Query.FieldAsString['Comment'];
  lData.Finished := Query.FieldAsDateTime['Finished'];
  lInt := Query.FieldAsInteger['Doctor'];
  If (lInt > 0) Then
    lData.Doctor := GetDoctor(lInt);
  lData.Duration := Query.FieldAsInteger['Duration'];
  lData.IsCancelled := (Query.FieldAsInteger['Cancelled'] = 1);
  lData.IsEmergency := (Query.FieldAsInteger['Emergency'] = 1);
  lInt := Query.FieldAsInteger['Patient'];
  If (lInt > 0) Then
    lData.Patient := GetPatient(lInt);
  lData.Scheduled := Query.FieldAsDateTime['Scheduled'];
  lData.Started := Query.FieldAsDateTime['Started'];
  lData.ObjectState := posClean;
  TPerVisList(Visited).Add(lData);
End;

Procedure TReadAppointment.SetupParams;
Begin
  Query.ParamAsInteger['Doctor'] := StrToInt(Surgery.CurrentDoctor.OID.AsString);
  Query.ParamAsDateTime['FromDate'] := Trunc(Surgery.CurrentDate);
  Query.ParamAsDateTime['ToDate'] := Trunc(Surgery.CurrentDate) + 0.999;
End;

{ TCreateAppointment }

Function TCreateAppointment.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TAppointment) And (TAppointment(Visited).ObjectState = posCreate);
End;

Procedure TCreateAppointment.Init;
Begin
  Query.SQL.Text := 'Insert Into Appointments (OID, Arrived, Comment, Doctor, ' +
    'Duration, Finished, Cancelled, Emergency, Patient, Scheduled, Started) Values ' +
    '(:OID, :Arrived, :Comment, :Doctor, :Duration, :Finished, :Cancelled, :Emergency, ' +
    ' :Patient, :Scheduled, :Started)';
End;

Procedure TCreateAppointment.SetupParams;
Var
  lData : TAppointment;
Begin
  lData := TAppointment(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsDateTime['Arrived'] := lData.Arrived;
  Query.ParamAsString['Comment'] := lData.Comment;
  Query.ParamAsDateTime['Finished'] := lData.Finished;
  Query.ParamAsInteger['Duration'] := lData.Duration;
  Query.ParamAsInteger['Doctor'] := StrToInt(lData.Doctor.OID.AsString);
  Query.ParamAsInteger['Cancelled'] := Ord(lData.IsCancelled);
  Query.ParamAsInteger['Emergency'] := Ord(lData.IsEmergency);
  Query.ParamAsInteger['Patient'] := StrToInt(lData.Patient.OID.AsString);
  Query.ParamAsDateTime['Scheduled'] := lData.Scheduled;
  Query.ParamAsDateTime['Started'] := lData.Started;
End;

{ TUpdateAppointment }

Function TUpdateAppointment.AcceptVisitor : Boolean;
Begin
  Result := (Visited Is TAppointment) And (TAppointment(Visited).ObjectState = posUpdate);
End;

Procedure TUpdateAppointment.Init;
Begin
  Query.SQL.Text := 'Update Appointments Set ' +
    'Scheduled = :Scheduled, ' +
    'Arrived = :Arrived, ' +
    'Comment = :Comment, ' +
    'Finished = :Finished, ' +
    'Duration = :Duration, ' +
    'Doctor = :Doctor, ' +
    'Patient = :Patient, ' +
    'Cancelled = :Cancelled, ' +
    'Emergency = :Emergency, ' +
    'Started = :Started ' +
    'Where OID = :OID';
End;

Procedure TUpdateAppointment.SetupParams;
Var
  lData : TAppointment;
Begin
  lData := TAppointment(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsDateTime['Arrived'] := lData.Arrived;
  Query.ParamAsString['Comment'] := lData.Comment;
  Query.ParamAsDateTime['Finished'] := lData.Finished;
  Query.ParamAsInteger['Duration'] := lData.Duration;
  Query.ParamAsInteger['Doctor'] := StrToInt(lData.Doctor.OID.AsString);
  Query.ParamAsInteger['Cancelled'] := Ord(lData.IsCancelled);
  Query.ParamAsInteger['Emergency'] := Ord(lData.IsEmergency);
  Query.ParamAsInteger['Patient'] := StrToInt(lData.Patient.OID.AsString);
  Query.ParamAsDateTime['Scheduled'] := lData.Scheduled;
  Query.ParamAsDateTime['Started'] := lData.Started;
End;

Initialization
  gTiPerMgr.RegReadVisitor(TReadDoctor);
  gTiPerMgr.RegReadVisitor(TReadPatient);
  gTiPerMgr.RegReadVisitor(TReadAppointment);

  gTiPerMgr.RegSaveVisitor(TDeleteDoctor);
  gTiPerMgr.RegSaveVisitor(TDeletePatient);

  gTiPerMgr.RegSaveVisitor(TUpdateDoctor);
  gTiPerMgr.RegSaveVisitor(TUpdatePatient);
  gTiPerMgr.RegSaveVisitor(TUpdateAppointment);

  gTiPerMgr.RegSaveVisitor(TCreateDoctor);
  gTiPerMgr.RegSaveVisitor(TCreatePatient);
  gTiPerMgr.RegSaveVisitor(TCreateAppointment);
End.


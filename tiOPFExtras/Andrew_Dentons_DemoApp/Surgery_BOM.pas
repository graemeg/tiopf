Unit Surgery_BOM;

Interface

Uses tiPtnVisPerObj, tiPerObjOIDAbs, tiPerObjOIDInteger, Classes;

Type
  TDoctor = Class;
  TDoctorList = Class;
  TPatientList = Class;
  TAppointmentList = Class;

  TSurgery = Class(TPerObjAbs)
  Private
    FDoctors : TDoctorList;
    FPatients : TPatientList;
    FAppointments : TAppointmentList;
    FCurrentDoctor : TDoctor;
    FCurrentDate : TDateTime;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
  Published
    Property Doctors : TDoctorList Read FDoctors;
    Property Patients : TPatientList Read FPatients;
    Property Appointments : TAppointmentList Read FAppointments;
    Property CurrentDoctor : TDoctor Read FCurrentDoctor Write FCurrentDoctor;
    Property CurrentDate : TDateTime Read FCurrentDate Write FCurrentDate;
  End;

  TPatient = Class(TPerObjAbs)
  Private
    FDoctorID : Integer;
    FName : String;
    FAddress : String;
    FPostCode : String;
    FDateOfBirth : TDateTime;
    FDoctor : TDoctor;
    Function GetAge : Integer;
    Procedure SetDoctor(Const Value : TDoctor);
  Protected
    Function GetCaption : String; Reintroduce;
    Function GetOwner : TPatientList; Reintroduce;
    Procedure SetOwner(Const Value : TPatientList); Reintroduce;
  Public
    Property Owner : TPatientList Read GetOwner Write SetOwner;
    Property DoctorID : Integer Read FDoctorID;
    Constructor Create; Override;
    Destructor Destroy; Override;
  Published
    Property Name : String Read FName Write FName;
    Property Address : String Read FAddress Write FAddress;
    Property PostCode : String Read FPostCode Write FPostCode;
    Property DateOfBirth : TDateTime Read FDateOfBirth Write FDateOfBirth;
    Property Age : Integer Read GetAge;
    Property Doctor : TDoctor Read FDoctor Write SetDoctor;
  End;

  TPatientList = Class(TPerObjList)
  Private
  Protected
    Function GetItems(I : Integer) : TPatient; Reintroduce;
    Procedure SetItems(I : Integer; Const Value : TPatient); Reintroduce;
    Function GetOwner : TSurgery; Reintroduce;
    Procedure SetOwner(Const Value : TSurgery); Reintroduce;
  Public
    Property Items[I : Integer] : TPatient Read GetItems Write SetItems;
    Procedure Add(pObject : TPatient; pbDefaultDispOrder : Boolean = True); Reintroduce;
    Property Owner : TSurgery Read GetOwner Write SetOwner;
  Published
  End;

  TDoctor = Class(TPerObjAbs)
  Private
    FName : String;
    FNHSNumber : String;
  Protected
    Function GetOwner : TDoctorList; Reintroduce;
    Procedure SetOwner(Const Value : TDoctorList); Reintroduce;
  Public
    Property Owner : TDoctorList Read GetOwner Write SetOwner;
  Published
    Property Name : String Read FName Write FName;
    Property NHSNumber : String Read FNHSNumber Write FNHSNumber;
  End;

  TDoctorList = Class(TPerObjList)
  Private
  Protected
    Function GetItems(I : Integer) : TDoctor; Reintroduce;
    Procedure SetItems(I : Integer; Const Value : TDoctor); Reintroduce;
    Function GetOwner : TSurgery; Reintroduce;
    Procedure SetOwner(Const Value : TSurgery); Reintroduce;
  Public
    Property Items[I : Integer] : TDoctor Read GetItems Write SetItems;
    Procedure Add(pObject : TDoctor; pbDefaultDispOrder : Boolean = True); Reintroduce;
    Property Owner : TSurgery Read GetOwner Write SetOwner;
  Published
  End;

  TAppointment = Class(TPerObjAbs)
  Private
    FArrived : TDateTime;
    FComment : String;
    FDoctor : TDoctor;
    FDuration : Integer;
    FFinished : TDateTime;
    FIsCancelled : Boolean;
    FIsEmergency : Boolean;
    FPatient : TPatient;
    FScheduled : TDateTime;
    FStarted : TDateTime;
    Function GetStatus : String;
    Function GetPatientAssigned : Boolean;
  Protected
    Function GetOwner : TAppointmentList; Reintroduce;
    Procedure SetOwner(Const Value : TAppointmentList); Reintroduce;
  Public
    Property Owner : TAppointmentList Read GetOwner Write SetOwner;
    Constructor Create; Override;
    Destructor Destroy; Override;
    Property PatientAssigned : Boolean Read GetPatientAssigned;
  Published
    Property Arrived : TDateTime Read FArrived Write FArrived;
    Property Comment : String Read FComment Write FComment;
    Property Doctor : TDoctor Read FDoctor Write FDoctor;
    Property Duration : Integer Read FDuration Write FDuration;
    Property Finished : TDateTime Read FFinished Write FFinished;
    Property IsCancelled : Boolean Read FIsCancelled Write FIsCancelled;
    Property IsEmergency : Boolean Read FIsEmergency Write FIsEmergency;
    Property Patient : TPatient Read FPatient Write FPatient;
    Property Scheduled : TDateTime Read FScheduled Write FScheduled;
    Property Started : TDateTime Read FStarted Write FStarted;
    Property Status : String Read GetStatus;
  End;

  TAppointmentList = Class(TPerObjList)
  Private
  Protected
    Function GetItems(I : Integer) : TAppointment; Reintroduce;
    Procedure SetItems(I : Integer; Const Value : TAppointment); Reintroduce;
    Function GetOwner : TSurgery; Reintroduce;
    Procedure SetOwner(Const Value : TSurgery); Reintroduce;
  Public
    Property Items[I : Integer] : TAppointment Read GetItems Write SetItems;
    Procedure Add(pObject : TAppointment; pbDefaultDispOrder : Boolean = True); Reintroduce;
    Property Owner : TSurgery Read GetOwner Write SetOwner;
  Published
  End;

Function Surgery : TSurgery;

Function GetDoctor(pID : Integer) : TDoctor;
Function GetPatient(pID : Integer) : TPatient;

Implementation

Uses StdStuff, SysUtils;

Var
  gSurgery : TSurgery;

Function Surgery : TSurgery;
Begin
  If (gSurgery = Nil) Then
    gSurgery := TSurgery.Create;
  Result := gSurgery;
End;

Function GetDoctor(pID : Integer) : TDoctor;
Begin
  Result := TDoctor(Surgery.Doctors.Find(IntToStr(pID)));
End;

Function GetPatient(pID : Integer) : TPatient;
Begin
  Result := TPatient(Surgery.Patients.Find(IntToStr(pID)));
End;

{ TPatient }

Constructor TPatient.Create;
Begin
  Inherited;
  FDoctor := TDoctor.Create;
End;

Destructor TPatient.Destroy;
Begin
  FDoctor.Free;
  Inherited;
End;

Function TPatient.GetAge : Integer;
Var
  NYear, NMonth, NDay : Word;
  BYear, BMonth, BDay : Word;
Begin
  DecodeDate(Date, NYear, NMonth, NDay);
  DecodeDate(DateOfBirth, BYear, BMonth, BDay);
  Result := NYear - BYear;
  If ((NMonth * 100 + NDay) > (BMonth * 100 + BDay)) Then
    Inc(Result);
End;

Function TPatient.GetCaption : String;
Begin
  Result := Name;
End;

Function TPatient.GetOwner : TPatientList;
Begin
  Result := TPatientList(Inherited GetOwner);
End;

Procedure TPatient.SetDoctor(Const Value : TDoctor);
Begin
  FDoctor := Value;
  FDoctorID := StrToInt(Value.OID.AsString);
End;

Procedure TPatient.SetOwner(Const Value : TPatientList);
Begin
  Inherited SetOwner(Value);
End;

{ TSurgery }

Constructor TSurgery.Create;
Begin
  Inherited;
  FCurrentDoctor := TDoctor.Create;
  FCurrentDate := Now;
  FPatients := TPatientList.Create;
  FPatients.Owner := Self;
  FDoctors := TDoctorList.Create;
  FDoctors.Owner := Self;
  FAppointments := TAppointmentList.Create;
  FAppointments.Owner := Self;
End;

Destructor TSurgery.Destroy;
Begin
  FAppointments.Free;
  FDoctors.Free;
  FPatients.Free;
  FCurrentDoctor.Free;
  Inherited;
End;

{ TPatientList }

Procedure TPatientList.Add(pObject : TPatient; pbDefaultDispOrder : Boolean);
Begin
  Inherited Add(pObject, pbDefaultDispOrder);
End;

Function TPatientList.GetItems(I : Integer) : TPatient;
Begin
  Result := TPatient(Inherited GetItems(I));
End;

Function TPatientList.GetOwner : TSurgery;
Begin
  Result := TSurgery(Inherited GetOwner);
End;

Procedure TPatientList.SetItems(I : Integer; Const Value : TPatient);
Begin
  Inherited SetItems(I, Value);
End;

Procedure TPatientList.SetOwner(Const Value : TSurgery);
Begin
  Inherited SetOwner(Value);
End;

{ TDoctor }

Function TDoctor.GetOwner : TDoctorList;
Begin
  Result := TDoctorList(Inherited GetOwner);
End;

Procedure TDoctor.SetOwner(Const Value : TDoctorList);
Begin
  Inherited SetOwner(Value);
End;

{ TDoctorList }

Procedure TDoctorList.Add(pObject : TDoctor; pbDefaultDispOrder : Boolean);
Begin
  Inherited Add(pObject, pbDefaultDispOrder);
End;

Function TDoctorList.GetItems(I : Integer) : TDoctor;
Begin
  Result := TDoctor(Inherited GetItems(I));
End;

Function TDoctorList.GetOwner : TSurgery;
Begin
  Result := TSurgery(Inherited GetOwner);
End;

Procedure TDoctorList.SetItems(I : Integer; Const Value : TDoctor);
Begin
  Inherited SetItems(I, Value);
End;

Procedure TDoctorList.SetOwner(Const Value : TSurgery);
Begin
  Inherited SetOwner(Value);
End;

{ TAppointment }

Constructor TAppointment.Create;
Begin
  Inherited;
  FDoctor := TDoctor.Create;
  FPatient := TPatient.Create;
End;

Destructor TAppointment.Destroy;
Begin
  Inherited;
  FPatient.Free;
  FDoctor.Free;
End;

Function TAppointment.GetOwner : TAppointmentList;
Begin
  Result := TAppointmentList(Inherited GetOwner);
End;

Function TAppointment.GetPatientAssigned : Boolean;
Begin
  Result := Not (FPatient.OID.IsNull);
End;

Function TAppointment.GetStatus : String;
Begin
  If (Finished <> 0) Then
    Result := 'Seen'
  Else If (Started <> 0) Then
    Result := 'Consulting'
  Else If IsCancelled Then
    Result := 'Cancelled'
  Else If IsEmergency Then
    Result := 'Emergency'
  Else If (Arrived <> 0) Then
    Result := 'Waiting'
  Else If Not (FPatient.OID.IsNull) Then
  Begin
    If (Scheduled < Date) Then
      Result := 'DNA'
    Else If (Scheduled < Now) Then
      Result := 'Late'
    Else
      Result := 'Booked';
  End
  Else
    Result := 'Available';
End;

Procedure TAppointment.SetOwner(Const Value : TAppointmentList);
Begin
  Inherited SetOwner(Value);
End;

{ TAppointmentList }

Procedure TAppointmentList.Add(pObject : TAppointment; pbDefaultDispOrder : Boolean);
Begin
  Inherited Add(pObject, pbDefaultDispOrder);
End;

Function TAppointmentList.GetItems(I : Integer) : TAppointment;
Begin
  Result := TAppointment(Inherited GetItems(I));
End;

Function TAppointmentList.GetOwner : TSurgery;
Begin
  Result := TSurgery(Inherited GetOwner);
End;

Procedure TAppointmentList.SetItems(I : Integer; Const Value : TAppointment);
Begin
  Inherited SetItems(I, Value);
End;

Procedure TAppointmentList.SetOwner(Const Value : TSurgery);
Begin
  Inherited SetOwner(Value);
End;

End.


Unit AppoinmentDetails;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PROPFORM, IvMulti, IvConMod, IvDictio, StdCtrls, ExtCtrls, ComCtrls,
  tiPtnVisPerObj, ovcbase;

Type
  TfrmApptDetails = Class(TfrmPropertySheet)
    edDoctor : TEdit;
    edScheduled : TEdit;
    edDuration : TEdit;
    edPatient : TEdit;
    edArrived : TEdit;
    edStarted : TEdit;
    edFinished : TEdit;
    meComments : TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
  Private
    Procedure RefreshControls;
  Protected
    Procedure LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs); Override;
    Procedure SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs); Override;
  Public
    { Public declarations }
  End;

Var
  frmApptDetails : TfrmApptDetails;

Implementation

{$R *.DFM}

Uses Surgery_BOM;

{ TfrmApptDetails }

Procedure TfrmApptDetails.LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs);

  Function TimeToText(pTime : TDateTime) : String;
  Begin
    If (pTime = 0) Then
      Result := ''
    Else
      Result := FormatDateTime('hh:mm', pTime)
  End;

Begin
  With (ObjectToLoad As TAppointment) Do
  Begin
    edDoctor.Text := Doctor.Name;
    edScheduled.Text := FormatDateTime('dddd d mmmm yyyy hh:mm', Scheduled);
    edDuration.Text := IntToStr(Duration);
    edStarted.Text := TimeToText(Started);
    edPatient.Text := Patient.Name;
    edArrived.Text := TimeToText(Arrived);
    edFinished.Text := TimeToText(Finished);
    meComments.Text := Comment;
    RefreshControls;
  End;
End;

Procedure TfrmApptDetails.RefreshControls;
Begin
  With TAppointment(FEditObject) Do
  Begin
    edArrived.ReadOnly := Patient.OID.IsNull;
    edStarted.ReadOnly := Not edArrived.Enabled Or (Arrived = 0);
    edFinished.ReadOnly := Not edStarted.Enabled Or (Started = 0);
  End;
End;

Procedure TfrmApptDetails.SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs);

  Function TextToTime(Const pText : String) : TDateTime;
  Begin
    If (Trim(pText)='') Then
      Result := 0
    Else
      Result := StrToTime(pText);
  End;

Begin
  With (ObjectToSave As TAppointment) Do
  Begin
    Arrived := TextToTime(edArrived.Text);
    Started := TextToTIme(edStarted.Text);
    Finished := TextToTime(edFinished.Text);
    Comment := meComments.Text;
  End;
End;

End.


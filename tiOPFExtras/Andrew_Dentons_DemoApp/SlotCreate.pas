Unit SlotCreate;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

Type
  TfrmApptCreate = Class(TForm)
    btnOK : TButton;
    btnCancel : TButton;
    Bevel1 : TBevel;
    Image1 : TImage;
    Label1 : TLabel;
    dtpDate : TDateTimePicker;
    edStart : TEdit;
    edNoOfSlots : TEdit;
    edDuration : TEdit;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Procedure edDurationKeyPress(Sender : TObject; Var Key : Char);
    Procedure edStartKeyPress(Sender : TObject; Var Key : Char);
    Procedure btnOKClick(Sender : TObject);
    Procedure btnCancelClick(Sender : TObject);
    Procedure FormCreate(Sender : TObject);
  Private
    { Private declarations }
  Public
    Function CreateSlots : Boolean;
  End;

Var
  frmApptCreate : TfrmApptCreate;

Implementation

Uses StdStuff, Surgery_BOM;

{$R *.DFM}

Function TfrmApptCreate.CreateSlots : Boolean;
Const
  OneDelphiMinute = 1 / (24 * 60);
Var
  StartTime : TDateTime;
  NewSlot : TAppointment;
  NumberOfSlots : Integer;
  SlotDuration : Integer;
Begin
  Result := False;
  SlotDuration := StrToIntDef(edDuration.Text, 0);
  NumberOfSlots := StrToIntDef(edNoOfSlots.Text, 0);
  If SlotDuration <= 0 Then
  Begin
    ErrorBox('You must enter the duration between 1 and 99.');
  End
  Else If (NumberOfSlots <= 0) Then
  Begin
    ErrorBox('You must enter the number of slots between 1 and 99.');
  End
  Else
  Begin
    Try
      StartTime := Trunc(dtpDate.Date) + StrToTime(edStart.Text);
      // Create the new slots
      While (NumberOfSlots > 0) Do
      Begin
        NewSlot := TAppointment.CreateNew;
        NewSlot.Doctor := Surgery.CurrentDoctor;
        NewSlot.Scheduled := StartTime;
        NewSlot.Duration := SlotDuration;
        StartTime := StartTime + OneDelphiMinute * SlotDuration;
        Surgery.Appointments.Add(NewSlot);
        NewSlot.Save;
        //NewSlot.Free;
        Dec(NumberOfSlots);
      End;
      Result := True;
    Except On EConvertError Do
        ErrorBox('You must enter a start time.');
    End;
  End;
End;

Procedure TfrmApptCreate.edDurationKeyPress(Sender : TObject; Var Key : Char);
Begin
  If (Ord(Key) > 32) And ((Key < '0') Or (Key > '9')) Then
    Key := #0;
End;

Procedure TfrmApptCreate.edStartKeyPress(Sender : TObject; Var Key : Char);
Begin
  If (Ord(Key) > 32) And (Key <> ':') And ((Key < '0') Or (Key > '9')) Then
    Key := #0;
End;

Procedure TfrmApptCreate.btnOKClick(Sender : TObject);
Begin
  If CreateSlots Then
    Close;
End;

Procedure TfrmApptCreate.btnCancelClick(Sender : TObject);
Begin
  Close;
End;

Procedure TfrmApptCreate.FormCreate(Sender : TObject);
Begin
  dtpDate.DateTime := Surgery.CurrentDate;
  edStart.Text := '10:00';
  edNoOfSlots.Text := '10';
  edDuration.Text := '10';
End;

End.


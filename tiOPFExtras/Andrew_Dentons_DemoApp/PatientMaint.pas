Unit PatientMaint;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PROPFORM, StdCtrls, ExtCtrls, ComCtrls, tiPerAwareCtrls, tiPtnVisPerObj,
  IvMulti, IvConMod, IvDictio, tiFocusPanel, ovcbase;

Type
  TfrmPatientMaint = Class(TfrmPropertySheet)
    edName : TEdit;
    meAddress : TMemo;
    edPostCode : TEdit;
    dtpDOB : TDateTimePicker;
    cmbDoctor : TtiPerAwareComboBoxDynamic;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Procedure FormShow(Sender : TObject);
  Private
  Protected
    Procedure LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs); Override;
    Procedure SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs); Override;
  Public
    { Public declarations }
  End;

Var
  frmPatientMaint : TfrmPatientMaint;

Implementation

Uses Surgery_BOM;
{$R *.DFM}

{ TfrmPatientMaint }

Procedure TfrmPatientMaint.LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs);
Begin
  With (ObjectToLoad As TPatient) Do
  Begin
    If (TabSheet = tabDetails) Then
    Begin
      edName.Text := Name;
      meAddress.Text := Address;
      edPostCode.Text := PostCode;
      dtpDOB.Date := DateOfBirth;
    End;
  End;
End;

Procedure TfrmPatientMaint.SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs);
Begin
  With (ObjectToSave As TPatient) Do
  Begin
    If (TabSheet = tabDetails) Then
    Begin
      Name := edName.Text;
      Address := meAddress.Text;
      PostCode := edPostCode.Text;
      DateOfBirth := dtpDOB.Date;
      // Doctor is automatically assigned by tiPerAware Combo Box.
    End;
  End;
End;

Procedure TfrmPatientMaint.FormShow(Sender : TObject);
Begin
  Inherited;
  cmbDoctor.FieldNameDisplay := 'Name';
  cmbDoctor.List := Surgery.Doctors.List;
  cmbDoctor.LinkToData(FEditObject, 'Doctor');
End;

End.


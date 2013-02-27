Unit DoctorMaint;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PROPFORM, StdCtrls, ExtCtrls, ComCtrls, tiPerAwareCtrls, tiPtnVisPerObj,
  IvMulti, IvConMod, IvDictio, ovcbase;

Type
  TfrmDoctorMaint = Class(TfrmPropertySheet)
    edName : TEdit;
    edNHSNumber : TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Procedure FormCreate(Sender : TObject);
  Private
  Protected
    Procedure LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs); Override;
    Procedure SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs); Override;
  Public
  End;

Var
  frmDoctorMaint : TfrmDoctorMaint;

Implementation

Uses Surgery_BOM;

{$R *.DFM}

{ TfrmDoctorMaint }

Procedure TfrmDoctorMaint.LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs);
Begin
  With (ObjectToLoad As TDoctor) Do
  Begin
    If (TabSheet = tabDetails) Then
    Begin
      edName.Text := Name;
      edNHSNumber.Text := NHSNumber;
    End;
  End;
End;

Procedure TfrmDoctorMaint.SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs);
Begin
  With (ObjectToSave As TDoctor) Do
  Begin
    If TabSheet = tabDetails Then
    Begin
      Name := edName.Text;
      NHSNumber := edNHSNumber.Text;
    End;
  End;
End;

Procedure TfrmDoctorMaint.FormCreate(Sender : TObject);
Begin
  Inherited;
 // edName.LinkToData(FEditObject, 'Name');
 // edNHSNumber.LinkToData(FEditObject, 'NHSNumber');
End;

End.


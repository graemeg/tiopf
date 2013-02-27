Unit PatientList;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ListBase, ActnList, ImgList, ExtCtrls, tiListView, StdCtrls, ComCtrls,
  ToolWin;

Type
  TfrmPatientList = Class(TfrmListBase)
    Procedure lvMaintItemInsert(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
    Procedure lvMaintItemEdit(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
    Procedure lvMaintItemDelete(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
  Private
    { Private declarations }
  Public
    Procedure ConnectToData; Override;
  End;

Var
  frmPatientList : TfrmPatientList;

Implementation

Uses tiPersist, tiPtnVisPerObj, StdStuff, Surgery_BOM, PatientMaint;
{$R *.DFM}

{ TfrmPatientList }

Procedure TfrmPatientList.ConnectToData;
Begin
  lvMaint.Data := Surgery.Patients.List;
End;

Procedure TfrmPatientList.lvMaintItemInsert(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Var
  lPatient : TPatient;
Begin
  lPatient := TPatient.CreateNew;
  If (TfrmPatientMaint.Edit(lPatient) = mrOK) Then
  Begin
    Surgery.Patients.Add(lPatient);
    lvMaint.Refresh;
  End;
End;

Procedure TfrmPatientList.lvMaintItemEdit(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Begin
  If (pData <> Nil) Then // not strictly necessary, but safer.
  Begin
    If (TfrmPatientMaint.Edit(pData As TPatient) = mrOK) Then
      lvMaint.Refresh;
  End;
End;

Procedure TfrmPatientList.lvMaintItemDelete(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Begin
  If (pData <> Nil) Then
  Begin
    If UserAgrees('Are you sure you want to delete this patient?') Then
    Begin
      TPerObjAbs(pData).Deleted := True;
      gTiPerMgr.Save(Surgery.Patients);
      lvMaint.DeleteData(lvMaint.Selected);
      lvMaint.Refresh(True);
    End;
  End;
End;

End.


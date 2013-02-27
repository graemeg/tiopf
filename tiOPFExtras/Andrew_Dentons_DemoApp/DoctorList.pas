Unit DoctorList;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ListBase, ActnList, ImgList, ExtCtrls, tiListView, StdCtrls, ComCtrls,
  ToolWin, tiFocusPanel;

Type
  TfrmDoctorList = Class(TfrmListBase)
    Procedure lvMaintItemInsert(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
    Procedure lvMaintItemEdit(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
    Procedure lvMaintItemDelete(pLV : TtiCustomListView; pData : TPersistent;
      pItem : TListItem);
  Private
    { Private declarations }
  Public
    Procedure ConnectToData; OverRide;
  End;

Var
  frmDoctorList : TfrmDoctorList;

Implementation

{$R *.DFM}

Uses tiPersist, tiPtnVisPerObj, Surgery_BOM, StdStuff, DoctorMaint;

Procedure TfrmDoctorList.lvMaintItemInsert(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Var
  lDoctor : TDoctor;
Begin
  Inherited;
  lDoctor := TDoctor.CreateNew;
  If (TfrmDoctorMaint.Edit(lDoctor) = mrOK) Then
  Begin
    lvMaint.Refresh(True);
    Surgery.Doctors.Add(lDoctor);
  End;
End;

Procedure TfrmDoctorList.lvMaintItemEdit(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Begin
  If (pData <> Nil) Then
  Begin
    If (TfrmDoctorMaint.Edit(pData As TDoctor) = mrOK) Then
      lvMaint.Refresh(True);
  End;
End;

Procedure TfrmDoctorList.lvMaintItemDelete(pLV : TtiCustomListView;
  pData : TPersistent; pItem : TListItem);
Begin
  If UserAgrees('Are you sure you want to delete this Doctor?') Then
  Begin
    TPerObjAbs(pData).Deleted := True;
    gTiPerMgr.Save(Surgery.Doctors);
    lvMaint.DeleteData(lvMaint.Selected);
    lvMaint.Refresh(True);
  End;
End;

Procedure TfrmDoctorList.ConnectToData;
Begin
  lvMaint.Data := Surgery.Doctors.List;
End;

End.


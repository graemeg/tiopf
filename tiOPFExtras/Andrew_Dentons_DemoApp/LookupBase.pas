Unit LookupBase;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, tiPtnVisPerObj, tiListView, tiListViewPlus,
  tiFocusPanel;

Type
  TfrmLookup = Class(TForm)
    gbCriteria : TGroupBox;
    edCriteria : TEdit;
    btnSearch : TButton;
    gbResults : TGroupBox;
    btnOK : TButton;
    btnCancel : TButton;
    lvMain : TtiListViewPlus;
    Procedure btnOKClick(Sender : TObject);
    Procedure btnCancelClick(Sender : TObject);
    Procedure FormKeyPress(Sender : TObject; Var Key : Char);
    Procedure FormShow(Sender : TObject);
    Procedure btnSearchClick(Sender : TObject);
    Procedure lvMainDblClick(pLV : TtiCustomListView; pData : TPersistent; pItem : TListItem);
    Procedure FormDestroy(Sender : TObject);
    Procedure lvMainFilterData(pData : TPersistent; Var pbInclude : Boolean);
  Private
    FSelectedObject : TPerObjAbs;
    FFilter : TtiLVFilter;
    FSearchProp : String;
    Procedure FilterListView(Const PropStr, SearchStr : String);
  Public
    Class Function Lookup(LookupList : TPerObjList; Const PropStr, SearchStr : String) : TPerObjAbs;
  End;

Implementation

{$R *.DFM}

Uses StdStuff;

{ TfrmLookupBase }

Class Function TfrmLookup.Lookup(LookupList : TPerObjList; Const PropStr, SearchStr : String) : TPerObjAbs;
Var
  frmLookup : TfrmLookup;
Begin
  frmLookup := Self.Create(Nil);
  With frmLookup Do
  Begin
    Try
      FSearchProp := PropStr;
      lvMain.Data := LookupList.List;
      Caption := 'Lookup ' + LookupList.Caption;
      edCriteria.Text := SearchStr;
      // Filter here
      FFilter := TtiLVFilter.Create;
      If (Trim(SearchStr) <> '') Then
        FilterListView(PropStr, SearchStr);
      ShowModal;
      Result := FSelectedObject;
    Finally
      FFilter.Free;
      Free;
    End;
  End;
End;

Procedure TfrmLookup.btnOKClick(Sender : TObject);
Begin
  If (lvMain.Selected <> Nil) Then
    FSelectedObject := TPerObjAbs(lvMain.SelectedData);
End;

Procedure TfrmLookup.btnCancelClick(Sender : TObject);
Begin
  FSelectedObject := Nil;
End;

Procedure TfrmLookup.FormKeyPress(Sender : TObject; Var Key : Char);
Begin
  Case Key Of
    #13 :
      Begin
        btnOKClick(Sender);
        Key := #0;
        ModalResult := mrOK;
      End;
    #27 :
      Begin
        btnCancelClick(Sender);
        Key := #0;
        ModalResult := mrCancel;
      End;
  End; { Case }
End;

Procedure TfrmLookup.FormShow(Sender : TObject);
Begin
  btnSearch.Default := (lvMain.Items.Count = 0);
  btnOK.Default := (lvMain.Items.Count > 0);
  lvMain.SetFocus;
End;

Procedure TfrmLookup.btnSearchClick(Sender : TObject);
Begin
  FilterListView(FSearchProp,edCriteria.Text);
End;

Procedure TfrmLookup.lvMainDblClick(pLV : TtiCustomListView; pData : TPersistent;
  pItem : TListItem);
Begin
  btnOKClick(Nil);
End;

Procedure TfrmLookup.FormDestroy(Sender : TObject);
Begin
  lvMain.Data := Nil;
End;

Procedure TfrmLookup.FilterListView(Const PropStr, SearchStr : String);
Begin
  FFilter.Conj := fcNone;
  FFilter.Field := PropStr;
  FFilter.Operator := foContains;
  FFilter.Value := SearchStr;
{  If (lvMain.Items.Count > 0) Then
  Begin
    lvMain.PositionCursor(0);
    FilterRes := FFilter.ValidateFilter(lvMain.SelectedData);
    If (Trim(FilterRes)<>'') Then
      ErrorBox(FilterRes);
  End;}
End;

Procedure TfrmLookup.lvMainFilterData(pData : TPersistent; Var pbInclude : Boolean);
Begin
  If (Trim(FFilter.Value) <> '') Then
    pbInclude := FFilter.DoesDataPassFilter(pData)
  Else
    pbInclude := True;
End;

End.


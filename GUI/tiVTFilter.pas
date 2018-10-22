Unit tiVTFilter;

{$I tiDefines.inc}

Interface

{$IFDEF VIRTUAL_TREEVIEW}

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ActnList, tiObject, tiVTListView;

Type
  TfrmLVFilter = Class(TForm)
    btnOK : TButton;
    btnCancel : TButton;
    Bevel1 : TBevel;
    lblCaption : TLabel;
    lbCriteria : TListBox;
    btnClear : TButton;
    btnDelete : TButton;
    btnNew : TButton;
    btnEdit : TButton;
    cmbProperties : TComboBox;
    lblProperties : TLabel;
    cmbOperator : TComboBox;
    Label1 : TLabel;
    edValue : TEdit;
    Label2 : TLabel;
    cmbConj : TComboBox;
    Label3 : TLabel;
    btnAdd : TButton;
    alFilter : TActionList;
    actClearAll : TAction;
    actDelete : TAction;
    actNew : TAction;
    actEdit : TAction;
    actAdd : TAction;
    Procedure actClearAllUpdate(Sender : TObject);
    Procedure actNewExecute(Sender : TObject);
    Procedure actAddExecute(Sender : TObject);
    Procedure actClearAllExecute(Sender : TObject);
    Procedure actDeleteExecute(Sender : TObject);
    Procedure actDeleteUpdate(Sender : TObject);
    Procedure actEditExecute(Sender : TObject);
    Procedure FormShow(Sender : TObject);
    Procedure FormCreate(Sender : TObject);
    Procedure btnOKClick(Sender : TObject);
    Procedure actAddUpdate(Sender : TObject);
  Private
    FEditItemIndex : Integer;
    FFilters : TList;
    FCurrFilter : TLVFilter;
    Procedure PropsToControls;
    Procedure ControlsToProps;
    Procedure UpdateFilters;
    Procedure RefreshListBox;
  Public
    FStoredObj : TtiObject;
    Property Filters : TList Read FFilters Write FFilters;
    Class Function EditFilter(pObject : TtiObject; pFilters : TList) : Boolean;
  End;

{$ENDIF}

implementation

{$IFDEF VIRTUAL_TREEVIEW}

Uses TypInfo;

{$R *.DFM}

{ TLVFilter }

Procedure TfrmLVFilter.actClearAllUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := (lbCriteria.Items.Count > 0);
End;

Procedure TfrmLVFilter.actNewExecute(Sender : TObject);
Begin
  FCurrFilter := TLVFilter.Create;
  PropsToControls;
  cmbProperties.SetFocus;
End;

Procedure TfrmLVFilter.actAddExecute(Sender : TObject);
Begin
  With FCurrFilter Do
  Begin
    ControlsToProps;
    If (FEditItemIndex = -1) Then
      lbCriteria.Items.AddObject(FCurrFilter.StringExpression, FCurrFilter)
    Else
    Begin
      lbCriteria.Items.Objects[FEditItemIndex] := FCurrFilter;
      FEditItemIndex := -1;
    End;
  End;
  RefreshListBox;
  actNewExecute(Sender);
End;

Procedure TfrmLVFilter.ControlsToProps;
Var
  lStr : String;
Begin
  With FCurrFilter Do
  Begin
    lStr := cmbProperties.Text;
    PropName := lStr;
    FilterOperator := TFilterOp(cmbOperator.ItemIndex);
    Value := edValue.Text;
    Join := TFilterConj(cmbConj.ItemIndex);
  End;
End;

Procedure TfrmLVFilter.PropsToControls;
Begin
  With FCurrFilter Do
  Begin
    cmbProperties.ItemIndex := cmbProperties.Items.IndexOf(PropName);
    edValue.Text := Value;
    cmbOperator.ItemIndex := Ord(FilterOperator);
    cmbConj.ItemIndex := Ord(Join);
  End;
End;

Procedure TfrmLVFilter.actClearAllExecute(Sender : TObject);
Begin
  lbCriteria.Clear;
End;

Procedure TfrmLVFilter.actDeleteExecute(Sender : TObject);
Begin
  lbCriteria.Items.Delete(lbCriteria.ItemIndex);
End;

Procedure TfrmLVFilter.actDeleteUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := (lbCriteria.ItemIndex > -1);
End;

Procedure TfrmLVFilter.actEditExecute(Sender : TObject);
Begin
  FEditItemIndex := lbCriteria.ItemIndex;
  FCurrFilter := TLVFilter(lbCriteria.Items.Objects[FEditItemIndex]);
  PropsToControls;
End;

Class Function TfrmLVFilter.EditFilter(pObject : TtiObject; pFilters : TList) : Boolean;
Var
  frmLVFilter : TfrmLVFilter;
Begin
  frmLVFilter := TfrmLVFilter.Create(Nil);
  frmLVFilter.FStoredObj := pObject;
  frmLVFilter.FFilters := pFilters;
  Try
    Result := (frmLVFilter.ShowModal = mrOK);
  Finally
    frmLVFilter.Free;
  End;
End;

Procedure TfrmLVFilter.FormShow(Sender : TObject);
Var
  I : Integer;
  lPropCount : Integer;
  lList : PPropList;
  lSize : Integer;
Begin
  For I := Low(FilterOps) To High(FilterOps) Do
    cmbOperator.Items.Add(FilterOps[I]);
  cmbOperator.ItemIndex := Ord(foNone);
  For I := Low(FilterConjs) To High(FilterConjs) Do
    cmbConj.Items.Add(FilterConjs[I]);
  cmbConj.ItemIndex := Ord(fcNone);
  lPropCount := GetPropList(FStoredObj.ClassInfo, ctkSimple, Nil);
  lSize := lPropCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  Try
    GetPropList(FStoredObj.ClassInfo, ctkSimple, lList);
    For I := 0 To lPropCount - 1 Do
      cmbProperties.Items.Add(string(lList[I].Name));
  Finally
    FreeMem(lList, lSize);
  End;
  For I := 0 To FFilters.Count - 1 Do
    lbCriteria.Items.AddObject(TLVFilter(FFilters[I]).StringExpression, FFilters[I]);
End;

Procedure TfrmLVFilter.FormCreate(Sender : TObject);
Begin
  FCurrFilter := TLVFilter.Create;
  FEditItemIndex := -1;
End;

Procedure TfrmLVFilter.btnOKClick(Sender : TObject);
Begin
  UpdateFilters;
End;

Procedure TfrmLVFilter.UpdateFilters;
Var
  I : Integer;
Begin
  FFilters.Clear;
  For I := 0 To lbCriteria.Items.Count - 1 Do
    FFilters.Add(lbCriteria.Items.Objects[I]);
End;

Procedure TfrmLVFilter.actAddUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := ((cmbProperties.Text <> '') And (edValue.Text <> '') And (Trim(cmbOperator.Text) <> ''));
End;

procedure TfrmLVFilter.RefreshListBox;
Var
  I : Integer;
begin
  With lbCriteria.Items Do
  Begin
    For I := 0 To Count - 1 Do
      Strings[I] := TLVFilter(Objects[I]).StringExpression;
  End;
end;

{$ENDIF}

End.


Unit PropForm;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, tiPtnVisPerObj;

Type
  TfrmPropertySheet = Class(TForm)
    pgcDetails : TPageControl;
    tabDetails : TTabSheet;
    btnOK : TButton;
    btnCancel : TButton;
    btnApply : TButton;
    imgDetails : TImage;
    lblDetails : TLabel;
    bvlBottom : TBevel;
    Procedure btnOKClick(Sender : TObject);
    Procedure btnApplyClick(Sender : TObject);
    Procedure pgcDetailsChange(Sender : TObject);
    Procedure FormShow(Sender : TObject);
    Procedure FormKeyPress(Sender : TObject; Var Key : Char);
  Private
    LoadedTabs : Integer;
    Function GetIsLoaded(TabSheet : TTabSheet) : Boolean;
    Procedure SetIsLoaded(TabSheet : TTabSheet; Value : Boolean);
    Procedure SaveDetails;
  Protected
    // Allow control over page loading
    FEditObject : TPerObjAbs;
    Property IsLoaded[TabSheet : TTabSheet] : Boolean Read GetIsLoaded Write SetIsLoaded;
    // Override these methods to populate pages
    Procedure LoadTab(TabSheet : TTabSheet; ObjectToLoad : TPerObjAbs); Virtual; Abstract;
    Procedure SaveTab(TabSheet : TTabSheet; ObjectToSave : TPerObjAbs); Virtual; Abstract;
  Public
    Class Function Edit(ObjectToEdit : TPerObjAbs) : TModalResult;
    Procedure CreateParams(Var Params : TCreateParams); Override;
  End;

Implementation

{$R *.DFM}

Uses tiPersist, JclSysInfo;

Procedure TfrmPropertySheet.btnOKClick(Sender : TObject);
Begin
  SaveDetails;
  ModalResult := mrOK;
End;

Procedure TfrmPropertySheet.btnApplyClick(Sender : TObject);
Begin
  SaveDetails;
  btnApply.Enabled := False;
  btnOK.Enabled := True;
  btnCancel.Enabled := False;
  btnCancel.Cancel := False;
  btnOK.Caption := '&Close';
End;

Procedure TfrmPropertySheet.pgcDetailsChange(Sender : TObject);
Begin
  If Not IsLoaded[pgcDetails.ActivePage] Then
  Begin
    // Populate controls
    LoadTab(pgcDetails.ActivePage, FEditObject);
    IsLoaded[pgcDetails.ActivePage] := True;
    // Reset focus to first control on tab
    pgcDetails.ActivePage.SetFocus;
    SendMessage(Handle, WM_NEXTDLGCTL, 0, 0);
  End;
End;

Procedure TfrmPropertySheet.FormShow(Sender : TObject);
Begin
  pgcDetails.ActivePageIndex := 0;
  pgcDetailsChange(Sender);
  Caption := Caption + ' - ' + FEditObject.Caption;
End;

Class Function TfrmPropertySheet.Edit(ObjectToEdit : TPerObjAbs) : TModalResult;
Var
  dlgPropertySheet : TfrmPropertySheet;
Begin
  dlgPropertySheet := Self.Create(Nil);
  Try
    dlgPropertySheet.FEditObject := ObjectToEdit;
    Result := dlgPropertySheet.ShowModal;
  Finally
    dlgPropertySheet.Free;
  End;
End;

Function TfrmPropertySheet.GetIsLoaded(TabSheet : TTabSheet) : Boolean;
Begin
  Result := (LoadedTabs And (1 Shl TabSheet.PageIndex) <> 0);
End;

Procedure TfrmPropertySheet.SaveDetails;
Var
  ThisPage : Integer;
Begin
  For ThisPage := 0 To pgcDetails.PageCount - 1 Do
  Begin
    If IsLoaded[pgcDetails.Pages[ThisPage]] Then
      SaveTab(pgcDetails.Pages[ThisPage], FEditObject);
  End;
  FEditObject.Dirty := True;
  gTiPerMgr.Save(FEditObject);
End;

Procedure TfrmPropertySheet.SetIsLoaded(TabSheet : TTabSheet; Value : Boolean);
Begin
  If Value <> IsLoaded[TabSheet] Then
    LoadedTabs := LoadedTabs Xor (1 Shl TabSheet.PageIndex);
End;

Procedure TfrmPropertySheet.FormKeyPress(Sender : TObject; Var Key : Char);
Begin
  If Key = #13 Then // "Enter-to-Tab" functionality.
  Begin
    SendMessage(Handle, WM_NEXTDLGCTL, 0, 0);
    Key := #0;
  End;
  If Key = #27 Then // Esc closes window
  Begin
    Close;
    Key := #0;
  End;
End;

// This is so the close button always appears where it should. Otherwise it
// gets chopped off or is too close to the bottom of the form when running
// under Windows XP.

Procedure TfrmPropertySheet.CreateParams(Var Params : TCreateParams);
Const
  NonXPCaptionHeight = 19;
Begin
  Inherited;
  If (GetSystemMetrics(SM_CYCAPTION) <> NonXPCaptionHeight) And (BorderStyle = bsSizeable) Then
    Params.Height := Params.Height + GetSystemMetrics(SM_CYCAPTION) - NonXPCaptionHeight;
End;

End.


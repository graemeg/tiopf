Unit ListBase;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ExtCtrls, tiListView, ImgList, ComCtrls, StdCtrls, ToolWin,
  tiFocusPanel;

Type
  TfrmListBase = Class(TForm)
    tbMaint : TToolBar;
    btnClose : TButton;
    bvlBottom : TBevel;
    tbtnNew : TToolButton;
    tbtnProperties : TToolButton;
    tbtnDelete : TToolButton;
    ilmaint : TImageList;
    lvMaint : TtiListView;
    alMaint : TActionList;
    actNew : TAction;
    actProperties : TAction;
    actDelete : TAction;
    Procedure actPropertiesUpdate(Sender : TObject);
    Procedure actDeleteExecute(Sender : TObject);
    Procedure actNewExecute(Sender : TObject);
    Procedure FormKeyPress(Sender : TObject; Var Key : Char);
    Procedure btnCloseClick(Sender : TObject);
    Procedure FormDestroy(Sender : TObject);
    Procedure actNewUpdate(Sender : TObject);
    Procedure actPropertiesExecute(Sender : TObject);
    Procedure actDeleteUpdate(Sender : TObject);
    Procedure FormCreate(Sender : TObject);
  Private
    { Private declarations }
  Public
    Procedure ConnectToData; Virtual; Abstract;
    Procedure DisconnectFromData;
  End;

Var
  frmListBase : TfrmListBase;

Implementation

Uses StdStuff, tiPtnVisPerObj, Surgery_BOM;
{$R *.DFM}

Procedure TfrmListBase.actPropertiesUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := lvMaint.CanEdit;
End;

Procedure TfrmListBase.actDeleteExecute(Sender : TObject);
Begin
  lvMaint.DoDelete;
End;

Procedure TfrmListBase.actNewExecute(Sender : TObject);
Begin
  lvMaint.DoNew;
End;

Procedure TfrmListBase.DisconnectFromData;
Begin
  lvMaint.Data := Nil;
End;

Procedure TfrmListBase.FormKeyPress(Sender : TObject; Var Key : Char);
Begin
  If (Key = #13) Then
    actProperties.Execute;
  If (Key = #27) Then
    btnCloseClick(Sender);
End;

Procedure TfrmListBase.btnCloseClick(Sender : TObject);
Begin
  Close;
End;

Procedure TfrmListBase.FormDestroy(Sender : TObject);
Begin
  DisconnectFromData;
End;

Procedure TfrmListBase.actNewUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := lvMaint.CanInsert;
End;

Procedure TfrmListBase.actPropertiesExecute(Sender : TObject);
Begin
  lvMaint.DoEdit;
End;

Procedure TfrmListBase.actDeleteUpdate(Sender : TObject);
Begin
  (Sender As TAction).Enabled := lvMaint.CanDelete;
End;

Procedure TfrmListBase.FormCreate(Sender : TObject);
Begin
  ConnectToData;
End;

End.


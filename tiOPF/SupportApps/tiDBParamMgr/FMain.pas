{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ActnList, ImgList, ComCtrls, ToolWin, 
  tiDBConnectionPool, tiListView, tiButtons, Menus, tiFocusPanel ;

type
  TForm2 = class(TForm)
    tiToolBar1: TtiToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ActionList1: TActionList;
    ImageList1 : TImageList;
    LV         : TtiListView;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    aSave: TAction;
    aCancel: TAction;
    aClose: TAction;
    aNew: TAction;
    aEdit: TAction;
    aDelete: TAction;
    aOpen: TAction;
    mm: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    aSaveAs: TAction;
    Edit1: TMenuItem;
    Edit2: TMenuItem;
    New1: TMenuItem;
    Delete1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aDeleteExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure LVDblClick(Sender: TObject);
    procedure LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
  private
    FDBConnectParamsList : TDBConnectParamsList ;
    FbDirty : boolean ;
    FFileName : TFileName ;
    procedure Read( const pFileName : TFileName = '' ) ;
    procedure Save( const pFileName : TFileName = '' ) ;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  tiUtils
  ,FEdit
  ,tiDialogs
  ,tiRegINI
  ;

{$R *.DFM}

procedure TForm2.FormCreate(Sender: TObject);
begin
  gReg.ReadFormState( self ) ;
  LV.Align := alClient ;
  FDBConnectParamsList := TDBConnectParamsList.Create ;
  FFileName := gReg.ReadString( name, 'FileName', '' ) ;
  Read( FFileName ) ;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( self ) ;
  gReg.WriteString( name, 'FileName', FFileName ) ;
  FDBConnectParamsList.Free ;
end;

procedure TForm2.aSaveExecute(Sender: TObject);
begin
  Save( FFileName ) ;
end;

procedure TForm2.aCancelExecute(Sender: TObject);
begin
  Read( FFileName ) ;
end;

procedure TForm2.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  liResult : word ;
begin
   if not FbDirty then begin
     CanClose := true ;
     Exit ; //==>
   end;

   liResult := tiYesNoCancel( 'Do you want to save your changes before exiting?' ) ;
   case liResult of
   mrYes    : begin
                Save ;
                CanClose := true ;
              end ;
   mrNo     : CanClose := true ;
   mrCancel : CanClose := false ;
   end ;
end;

procedure TForm2.Read( const pFileName : TFileName = '' ) ;
var
  lSerialiser : TDBConnectParamsListSerialiser ;
begin
  LV.Data := nil ;
  lSerialiser := TDBConnectParamsListSerialiser.Create ;
  try
    if pFileName <> '' then
      lSerialiser.FileName := pFileName ;
    Caption := 'DB param mgr - ' + lSerialiser.FileName ;
    lSerialiser.Read( FDBConnectParamsList ) ;
  finally
    lSerialiser.Free ;
  end ;
  LV.Data := FDBConnectParamsList ;
  FbDirty := false ;
end;

procedure TForm2.Save( const pFileName : TFileName = '' ) ;
var
  lSerialiser : TDBConnectParamsListSerialiser ;
begin
  lSerialiser := TDBConnectParamsListSerialiser.Create ;
  try
    if pFileName <> '' then
      lSerialiser.FileName := pFileName ;
    lSerialiser.Write( FDBConnectParamsList ) ;
    Caption := 'DB param mgr - ' + lSerialiser.FileName ;
  finally
    lSerialiser.Free ;
  end ;
  FbDirty := false ;
end;

procedure TForm2.aDeleteExecute(Sender: TObject);
begin
  LV.DoDelete ;
end;

procedure TForm2.aNewExecute(Sender: TObject);
begin
  LV.DoNew ;
end;

procedure TForm2.aEditExecute(Sender: TObject);
begin
  LV.DoEdit ;
end;

procedure TForm2.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aSave.Enabled   := FbDirty ;
  aCancel.Enabled := FbDirty ;
  aNew.Enabled    := true    ;
  aEdit.Enabled   := LV.SelectedDataList.Count = 1 ;
  aDelete.Enabled := LV.SelectedDataList.Count = 1 ;
  aSaveAs.Enabled := true ;
  Handled := true ;
end;

procedure TForm2.LVDblClick(Sender: TObject);
begin
  LV.DoEdit ;
end;

procedure TForm2.LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
  pItem: TListItem);
var
  lData : TDBConnectParams ;
begin
  if LV.SelectedDataList.Count <> 1 then
    Exit ;

  lData := ( TObject( LV.SelectedDataList[0] ) as TDBConnectParams ) ;

  if not tiAppConfirmation( 'Are you sure you want to delete <' +
                lData.AsString + '> ?' ) then
    Exit ; //==>

  LV.Data := nil ;
  FDBConnectParamsList.Delete( FDBConnectParamsList.IndexOf( lData )) ;
  LV.Data := FDBConnectParamsList ;
  FbDirty := true;
end;

procedure TForm2.LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;
  pItem: TListItem);
var
  lForm : TFormEdit ;
  lData : TDBConnectParams ;
begin
  if LV.SelectedDataList.Count <> 1 then
    Exit ;

  lData := ( TObject( LV.SelectedDataList[0] ) as TDBConnectParams ) ;

  lForm := TFormEdit.Create( nil ) ;
  try
    lForm.Data := lData ;
    if lForm.ShowModal = mrOK then begin
      LV.Refresh( false ) ;
      FbDirty := true;
    end ;
  finally
    lForm.Free ;
  end ;
end;

procedure TForm2.LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
  pItem: TListItem);
var
  lForm : TFormEdit ;
  lData : TDBConnectParams ;
begin

  lData := TDBConnectParams.Create ;

  lForm := TFormEdit.Create( nil ) ;
  try
    lForm.Data := lData ;
    if lForm.ShowModal = mrOK then begin
      LV.Data := nil ;
      FDBConnectParamsList.Add( lData ) ;
      LV.Data := FDBConnectParamsList ;
      FbDirty := true;
    end else
      lData.Free ;
  finally
    lForm.Free ;
  end ;
end;

procedure TForm2.aOpenExecute(Sender: TObject);
var
  lOD : TOpenDialog ;
begin
  lOD := TOpenDialog.Create( nil ) ;
  try
    lOD.FileName := FFileName ;
    lOD.Filter := 'Database connection files|*.dcd|All files|*.*' ;
    lOD.DefaultExt := 'DCD' ;
    if lOD.Execute then
    begin
      FFileName := lOD.FileName ;
      Read( FFileName ) ;
    end ;
  finally
    lOD.Free ;
  end;
end;

procedure TForm2.aSaveAsExecute(Sender: TObject);
var
  lSD : TSaveDialog ;
begin
  lSD := TSaveDialog.Create( nil ) ;
  try
    lSD.FileName := FFileName ;
    lSD.Filter := 'Database connection files|*.dcd|All files|*.*' ;
    lSD.DefaultExt := 'DCD' ;
    if lSD.Execute then
    begin
      FFileName := lSD.FileName ;
      Save( FFileName ) ;
    end ;
  finally
    lSD.Free ;
  end;
end;

end.

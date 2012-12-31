{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
    TechIniste Application Launcher.
    Deploy the latest version of the EXE from the database.
    This is an admin tool to insert a new EXE into the database.

  Command line params:
    Database name:     -d <databaseName>
    Turn logging on:   -l
    Visual logging on: -lv

  Example:
    DeployMgr -lv -d opdd

  Revision History:
    Nov 1999, PWH, Created

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FDeployMgrMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ActnList, ComCtrls, ImgList, tiListView,
  ToolWin  ,tiDeployMgr_BOM, tiButtons, Menus, tiTreeView, tiFocusPanel
  ;

type
  TFormMain = class(TForm)
    tiToolBar1: TtiToolBar;
    ilButtons: TImageList;
    AL: TActionList;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    aSave: TAction;
    aCancelSave: TAction;
    aClose: TAction;
    aSaveAll: TAction;
    ToolButton9: TToolButton;
    SB: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    Refreshandsaveall1: TMenuItem;
    Cancel1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    TV: TtiTreeView;
    ToolButton10: TToolButton;
    ToolButton1: TToolButton;
    Help1: TMenuItem;
    aShowDatabaseConnectionDetails: TAction;
    Showdatabaseconnectiondetails1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ALUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure aCloseExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aSaveExecute(Sender: TObject);
    procedure aCancelSaveExecute(Sender: TObject);
    procedure aSaveAllExecute(Sender: TObject);
    procedure tiTVMappingtiDeployAppsOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingtiDeployAppOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingtiDeployAppOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure TVFilterData(pData: TObject; var pbInclude: Boolean);
    procedure TVSelectNode(ptiTreeView: TtiTreeView; pNode: TTreeNode;
      pData: TObject; pParentNode: TTreeNode; pParentData: TObject);
    procedure TVAfterGetChildForm(pChildForm: TForm; pData: TObject;
      pNode: TTreeNode);
    procedure ToolButton1Click(Sender: TObject);
    procedure aShowDatabaseConnectionDetailsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FData : TtiDeployApps ;
    procedure Read ;
    procedure Save ;
    procedure DoNewApplication(pData: TtiDeployApps; pTV: TtiTreeView);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiUtils
  ,tiLog
  ,tiDBConnectionPool
  ,tiCommandLineParams
  ,ctiDeployMgr
  ,tiObject
  ,tiGUIUtils

  ,FDeployMgrChild_App
  ,FDeployMgrChild_Launch
  ,tiDeployMgr_Cli
  ,tiOPFManager
  ,tiDialogs
  ,tiINI
  ;

{$R *.DFM}
// -----------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title ;
  gINI.ReadFormState( self ) ;

  FData := TtiDeployApps.Create ;
  TV.RegisterChildForm( TtiDeployApps, TFormTIDeployChild_Launch ) ;
  TV.RegisterChildForm( TtiDeployApp, TFormTIDeployChild_App ) ;
  Read ;

  TV.Align := alClient ;
  TV.SplitterPos := gINI.ReadInteger( Name, 'SplitterPos', TV.SplitterPos ) ;
  TV.SelectedAddress := gINI.ReadString( Name, 'SelectedAddress', '' ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
  gINI.WriteInteger( Name, 'SplitterPos',     TV.SplitterPos ) ;
  gINI.WriteString(  Name, 'SelectedAddress', TV.SelectedAddress ) ;
  FData.Free ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.Button2Click(Sender: TObject);
begin
end;

// -----------------------------------------------------------------------------
procedure TFormMain.ALUpdate(Action: TBasicAction;
  var Handled: Boolean);
//var
//  lbDirty : boolean ;
begin

  aSave.Enabled       := FData.Dirty ;
  aCancelSave.Enabled := aSave.Enabled ;
  Handled := true ;


{
  lbDirty := FDeployFiles.Dirty ;


  aEdit.Enabled       := LV.SelectedDataList.Count = 1 ;
  aDelete.Enabled     := LV.SelectedDataList.Count = 1 ;

  aSave.Enabled       := lbDirty ;
  aCancelSave.Enabled := lbDirty ;
}
end;

procedure TFormMain.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

procedure TFormMain.Read;
begin
  TV.Data := nil ;
  FData.Clear ;
  gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadPK, FData ) ;
  //FDeployFiles.ReadDeployFromDirtyStatus ;
  TV.Data := FData ;

end;

procedure TFormMain.Save;
begin
  ReadDeployFromFileDetails( FData ) ;
  gTIOPFManager.VisitorManager.Execute(     cgsDeployMgrSave, FData ) ;
  ReadDeployFromDirty( FData ) ;
  if ( TV.CurrentChildForm is TFormTIDeployChild_App ) then
    TFormTIDeployChild_App( TV.CurrentChildForm ).RefreshData ;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if tiPerObjAbsSaveAndClose( FData, CanClose ) then
    Save ;
end;

procedure TFormMain.aSaveExecute(Sender: TObject);
begin
  Save ;
end;

procedure TFormMain.aCancelSaveExecute(Sender: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to cancel your changes?' ) then
    Read ;
end;

procedure TFormMain.aSaveAllExecute(Sender: TObject);
//var
//  i : integer ;
begin

{
  for i := 0 to FDeployFiles.Count - 1 do
    TtiDeployFile( FDeployFiles.Items[i] ).Dirty := true ;
  LV.Refresh ;
  Application.ProcessMessages ;
  Save ;
}  
end;

procedure TFormMain.tiTVMappingtiDeployAppsOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  Assert( pData is TtiDeployApps ) ;
  DoNewApplication( TtiDeployApps( pData ), ptiTreeView ) ;
end;

procedure TFormMain.tiTVMappingtiDeployAppOnDelete(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  if tiPerObjAbsConfirmAndDelete( TtiObject( pData )) then
    ptiTreeView.Refresh ;
end;

procedure TFormMain.tiTVMappingtiDeployAppOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  Assert( pParentData is TtiDeployApps ) ;
  DoNewApplication( TtiDeployApps( pParentData ), ptiTreeView ) ;
end;

procedure TFormMain.DoNewApplication( pData : TtiDeployApps ; pTV : TtiTreeView ) ;
var
  lData : TtiDeployApp ;
begin
  lData := TtiDeployApp.CreateNew;
  lData.AppName := 'New_Application_' + lData.OID.AsString ;
  lData.Compression := 'No compression' ; 
  pData.Add( lData ) ;
  pTV.Refresh ;
end ;

procedure TFormMain.ToolButton10Click(Sender: TObject);
begin
  tiShowPerObjAbs( FData, true ) ;
end;

procedure TFormMain.TVFilterData(pData: TObject; var pbInclude: Boolean);
begin
  pbInclude := not TtiObject( pData ).Deleted ;
end;

procedure TFormMain.TVSelectNode(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadAppDetails, TtiObject( pData )) ;
  ReadDeployFromDirty( TtiObject( pData )) ;
end;

procedure TFormMain.TVAfterGetChildForm(pChildForm: TForm; pData: TObject;
  pNode: TTreeNode);
begin
  if pChildForm is TFormTIDeployChild_Launch then
    TFormTIDeployChild_Launch( pChildForm ).DatabaseName :=
      gCommandLineParams.GetParam( 'd' ) ;
end;

procedure TFormMain.ToolButton1Click(Sender: TObject);
begin
  showmessage( tiGetWindowsSysDir ) ;
end;

procedure TFormMain.aShowDatabaseConnectionDetailsExecute(Sender: TObject);
var
  ls : string ;
begin
  ls := gTIOPFManager.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
  tiShowMessage( ls ) ;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if gCommandLineParams.IsParam('autostart') or
     gCommandLineParams.IsParam('autorun') then
  begin
    gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadAppDetails, FData ) ;
    ReadDeployFromDirty( FData ) ;
    Save ;
    Close ;
  end ;
end;

end.

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
    November 2000, Peter Hinrichsen, Made open source
    DEC 2000, DJW, implemented tree sort, and locked query code

  Purpose:
    Main form of the SQLManager front end application.

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMainSQLManager;

{ TODO 1 : Implement functionality to test all queries }
{ TODO 2 : Write Delphi Expert to find query from a right click in the IDE. }
{ TODO 2 : Confirm SQL Create script will create the database correctly }
{ TODO 2 : Check queries in the database for changes before factory creating new instance }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, DBGrids, Db, ComCtrls,
  ExtCtrls, ToolWin, ImgList, Menus, ActnList,
  FChildDatabase
  ,FChildQuery
  ,FChildGroup
  ,tiTreeView
  ,tiSQLMgr_BOM
  ;

resourcestring
  crsDropConfirm = 'Do you want Copy or Move <%s>' ;
  crsCopy        = 'Copy' ;
  crsMove        = 'Move' ;
  crsEnterNewQueryName = 'Enter a new query name' ;
  crsQueryNameNotUnique = 'QueryName <%s> not unique.' ;

type

  // ---------------------------------------------------------------------------
  TFormMain = class(TForm)
    ilMenu: TImageList;
    alMain: TActionList;
    aDelete: TAction;
    aFileSave: TAction;
    aFileClose: TAction;
    tbMain: TToolBar;
    tbNew: TToolButton;
    tbsSeparator3: TToolButton;
    tbDelete: TToolButton;
    tbSave: TToolButton;
    tbsSeparator2: TToolButton;
    tbClose: TToolButton;
    aExecute: TAction;
    tbExecute: TToolButton;
    tbsSeparator4: TToolButton;
    ilTV: TImageList;
    mm: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    New1: TMenuItem;
    Newquery2: TMenuItem;
    Newgroup3: TMenuItem;
    Delete2: TMenuItem;
    N7: TMenuItem;
    N9: TMenuItem;
    Validatemodel2: TMenuItem;
    Query1: TMenuItem;
    Runquery2: TMenuItem;
    tbCancelSave: TToolButton;
    aFileCancelChanges: TAction;
    aConstantsFile: TAction;
    TV: TtiTreeView;
    aInsert: TAction;
    tbAdHocQuery: TToolButton;
    aAdhocQuery: TAction;
    tbFind: TToolButton;
    aFindQuery: TAction;
    aFindQuery1: TMenuItem;
    tbSort: TToolButton;
    test1: TMenuItem;
    aSort: TAction;
    ToolButton1: TToolButton;
    aExecuteThenSave: TAction;
    Runquerythensave1: TMenuItem;
    StatusBar1: TStatusBar;
    aDatabaseConnectionDetails: TAction;
    N1: TMenuItem;
    Showdatabaseconnectiondetails1: TMenuItem;
    aShowCurrentObject: TAction;
    aShowCurrentObject1: TMenuItem;
    aFileSaveAs: TAction;
    aFileOpen: TAction;
    Open1: TMenuItem;
    Savetree1: TMenuItem;
    Saveas1: TMenuItem;
    N2: TMenuItem;
    Cancelchanges1: TMenuItem;
    Close1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure aFileSaveExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aExecuteExecute(Sender: TObject);
    procedure aNewQueryExecute(Sender: TObject);
    procedure aFileCloseExecute(Sender: TObject);
    //procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure alMainUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure aFileCancelChangesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
//    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
//      Shift: TShiftState; X, Y: Integer);
//    procedure TVDragOver(Sender, Source: TObject; X, Y: Integer;
//      State: TDragState; var Accept: Boolean);
    procedure About1Click(Sender: TObject);
    procedure aConstantsFileExecute(Sender: TObject);
    procedure TVSelectNode(ptiTreeView: TtiTreeView; pNode: TTreeNode;
      pData: TObject; pParentNode: TTreeNode; pParentData: TObject);
    procedure tiTVMappingSQLMgrGroupOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingSQLMgrQueryOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingSQLMgrQueryOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingSQLMgrOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure aInsertExecute(Sender: TObject);
    procedure TVFilterData(pData: TObject; var pbInclude: Boolean);
    procedure tiTVMappingSQLMgrGroupOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure TVDataMappings2CanAcceptDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
    procedure TVDataMappings1CanAcceptDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
    procedure TVDataMappings0CanAcceptDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
    procedure aAdhocQueryExecute(Sender: TObject);
    procedure aFindQueryExecute(Sender: TObject);
    procedure tiTVMappingSQLMgrQueryOnCanDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject; var pbConfirm: Boolean);
    procedure test1Click(Sender: TObject);
    procedure aSortExecute(Sender: TObject);
    procedure tiTVMappingSQLMgrOnDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
    procedure tiTVMappingSQLMgrGroupOnDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
    procedure tiTVMappingSQLMgrQueryOnDrop(ptiTVSource: TtiTreeView;
      pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
      pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
    procedure aExecuteThenSaveExecute(Sender: TObject);
    procedure aDatabaseConnectionDetailsExecute(Sender: TObject);
    procedure aShowCurrentObjectExecute(Sender: TObject);
    procedure aFileSaveAsExecute(Sender: TObject);
    procedure aFileOpenExecute(Sender: TObject);
  private
    FSQLMgr : TSQLMgr ;
    procedure Read ;
    procedure Save ;
    procedure SaveConstantsFile(const psFileName: string);
    function  GetDBNameAsRegKey: string;
  public
    function FindQueryInTreeView(const psQueryName: String): Boolean;
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiSQLMgr_Cli
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,tiDialogs
  ,tiDBConnectionPool
  ,tiPtnVisMgr
  ,tiPtnVis
  ,FAdhocQuery
  ,tiCommandLineParams
  ,FFindQuery
  ,FAbout
  ,tiPersist
  ,tiRegINI
  ,tiLog
  ,cTIPersist
  ,tiSQLManagerDependencies
  ;

const
    cuConstantsFileRegKey = 'ConstantsFile' ;


{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin

  gINI.ReadFormState( self ) ;

  //TV.RegisterChildForm( TSQLMgr,      TFormChildDatabase ) ;
  TV.RegisterChildForm( TSQLMgrGroup, TFormChildGroup ) ;
  TV.RegisterChildForm( TSQLMgrQuery, TFormChildQuery ) ;

  TV.Align        := alClient ;
  TV.SplitterPos  := gINI.ReadInteger( Name, 'SplitterPos', 180 ) ;

  Read ;

  if gCommandLineParams.GetParam( 'q' ) <> EmptyStr then
    FindQueryInTreeView(gCommandLineParams.GetParam( 'q' ))
  else
    TV.SelectedAddress := gINI.ReadString( Name, 'SelectedAddress', '' ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
  gINI.WriteInteger( Name, 'SplitterPos', TV.SplitterPos ) ;
  gINI.WriteString( Name, 'SelectedAddress', TV.SelectedAddress ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.Read ;
begin
  TV.Data := nil ;
  if FSQLMgr = nil then
    FSQLMgr := gSQLMgrs.FindByDatabaseName( gTIPerMgr.DefaultDBConnectionName )
  else
    FSQLMgr.Clear ;
  FSQLMgr.FileName := GetSQLMgrFileName;
  Assert(FileExists(FSQLMgr.FileName), 'File not found <' + FSQLMgr.FileName + '>');
  FSQLMgr.ReadPK;
  TV.Data := FSQLMgr ;
end ;

// -----------------------------------------------------------------------------
procedure TFormMain.tbCloseClick(Sender: TObject);
begin
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.TVChange(Sender: TObject; Node: TTreeNode);
//r
//lCurrentData : TPerObjAbs ;
begin

(*
  HideCurrentChildForm ;

  lCurrentData := TV.Selected.Data ;

  { ToDo 1 -cGUI: Replace the SQLMgr TreeView's OnChange code with a factory (flyweight) call }

  if lCurrentData is TSQLMgr then begin
    FCurrentChildForm := FFormChildDatabase ;

  end else if lCurrentData is TSQLMgrGroup then begin
    FCurrentChildForm := FFormChildGroup ;

  end else if lCurrentData is TSQLMgrQuery then begin

    // If necessary, read the detail into the object
    gVisMgr.Execute( cgsSQLMgrReadDetail, lCurrentData ) ;
    FCurrentChildForm := FFormChildQuery ;

  end else begin
    FCurrentChildForm := nil ;

  end ;

  if FCurrentChildForm <> nil then begin
    FCurrentChildForm.Read( lCurrentData ) ;
    FCurrentChildForm.Parent := PanelParent ;
    FCurrentChildForm.Align := alClient ;
    FCurrentChildForm.OnUpdateNodeCaption :=
      UpdateNodeCaption ;
    FCurrentChildForm.Visible := true ;

  end ;
*)

end;

//------------------------------------------------------------------------------
procedure TFormMain.aFileSaveExecute(Sender: TObject);
begin
  Save ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aDeleteExecute(Sender: TObject);
begin
  TV.DoDelete ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aExecuteExecute(Sender: TObject);
begin
  if TV.CurrentChildForm is TFormChildQuery then
    TFormChildQuery( TV.CurrentChildForm ).Execute ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aNewQueryExecute(Sender: TObject);
begin
  TV.DoInsert ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aFileCloseExecute(Sender: TObject);
begin
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.alMainUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  aFileSave.Enabled     := FSQLMgr.Dirty ;
  aFileCancelChanges.Enabled   := aFileSave.Enabled ;
  aExecute.Enabled  := ( TV.Selected <> nil ) and
                       ( TObject( TV.Selected.Data ) is TSQLMgrQuery ) ;
  aDelete.Enabled   := TV.CanDeleteSelected ;
  aInsert.Enabled   := TV.CanInsertSelected ;
  aExecute.Enabled  := TV.CurrentChildForm is TFormChildQuery ;
  aExecuteThenSave.Enabled  := aExecute.Enabled ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aFileCancelChangesExecute(Sender: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to cancel your changes?' ) then
    Read ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if tiPerObjAbsSaveAndClose( FSQLMgr, CanClose ) then
    Save ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.About1Click(Sender: TObject);
var
  lForm : TFormAbout ;
begin
  lForm := TFormAbout.Create( nil ) ;
  try
    lForm.ShowModal ;
  finally
    lForm.Free ;
  end ;
end;

function TFormMain.GetDBNameAsRegKey : string ;
begin
  result := FSQLMgr.DatabaseName ;
  result := tiStrTran( result, ':', '' ) ;
  result := tiStrTran( result, '\', '' ) ;
  result := tiStrTran( result, ' ', '' ) ;
  result := tiStrTran( result, '.', '' ) ;
end ;

// -----------------------------------------------------------------------------
procedure TFormMain.aConstantsFileExecute(Sender: TObject);
  function _GetConstantsFileName : string ;
  var
    lSaveDialog : TSaveDialog ;
  begin
    lSaveDialog := TSaveDialog.Create( nil ) ;
    result := '' ;
    try
      lSaveDialog.FileName :=
        gINI.ReadString( cuConstantsFileRegKey,
                         GetDBNameAsRegKey,
                         'cQueryNames.pas' ) ;
      lSaveDialog.DefaultExt := 'pas' ;
      lSaveDialog.Filter := 'Delphi units|*.PAS|All files|*.*' ; ;
      if lSaveDialog.Execute then begin
        result := lSaveDialog.FileName ;
        gINI.WriteString( cuConstantsFileRegKey,
                          GetDBNameAsRegKey,
                          lSaveDialog.FileName ) ;
      end ;
    finally
      lSaveDialog.Free ;
    end ;
  end;

var
  lsFileName  : string ;
begin

  lsFileName := _GetConstantsFileName ;
  if lsFileName = '' then
    exit ;

  SaveConstantsFile( lsFileName ) ;

  tiAppMessage( 'Constants saved to <' +
                lsFileName + '>' ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormMain.SaveConstantsFile( const psFileName : string ) ;
begin
  if psFileName = '' then
    Exit ; //==>
  VisStreamToFile( FSQLMgr,
                   psFileName,
                   TVisConstants );
end ;

procedure TFormMain.TVSelectNode(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  if not ( pData is TSQLMgrQuery ) then
    Exit ; //==>
  TSQLMgrQuery( pData ).read(FSQLMgr.FileName, cTIPersistXMLLight);
end;

procedure TFormMain.tiTVMappingSQLMgrOnInsert(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
var
  lSQLMgrGroup : TSQLMgrGroup ;
begin
  lSQLMgrGroup := TSQLMgrGroup.CreateNew ;
  {$IFDEF OID_AS_INT64}
    lSQLMgrGroup.GroupName := 'New_Group_' + IntToStr(lSQLMgrGroup.OID) ;
  {$ELSE}
    lSQLMgrGroup.GroupName := 'New_Group_' + lSQLMgrGroup.OID.AsString ;
  {$ENDIF}
  TSQLMgr( pData ).Add( lSQLMgrGroup ) ;
end;

procedure TFormMain.tiTVMappingSQLMgrGroupOnDelete(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to delete <' +
                        TSQLMgrGroup( pData ).Caption +
                        '?' ) then
    TSQLMgrGroup( pData ).Deleted := true ;
end;

procedure TFormMain.tiTVMappingSQLMgrQueryOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
var
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  lSQLMgrQuery := TSQLMgrQuery.CreateNew ;
  {$IFDEF OID_AS_INT64}
    lSQLMgrQuery.QueryName := 'New_Query_' + IntToStr(lSQLMgrQuery.OID) ;
  {$ELSE}
    lSQLMgrQuery.QueryName := 'New_Query_' + lSQLMgrQuery.OID.AsString ;
  {$ENDIF}
  TSQLMgrGroup( pParentData ).Add( lSQLMgrQuery ) ;
end;

procedure TFormMain.tiTVMappingSQLMgrQueryOnCanDelete(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject; var pbConfirm: Boolean);
begin
  pbConfirm := not(TSQLMgrQuery( pData ).QueryLocked);
end;

procedure TFormMain.tiTVMappingSQLMgrQueryOnDelete(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to delete <' +
                        TSQLMgrQuery( pData ).Caption +
                        '> ?' ) then
    TSQLMgrQuery( pData ).Deleted := true ;
end;

procedure TFormMain.aInsertExecute(Sender: TObject);
begin
  TV.DoInsert ;
end;

procedure TFormMain.TVFilterData(pData: TObject; var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;

procedure TFormMain.tiTVMappingSQLMgrGroupOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
var
  lSQLMgrQuery : TSQLMgrQuery ;
begin
  lSQLMgrQuery := TSQLMgrQuery.CreateNew ;
  {$IFDEF OID_AS_INT64}
    lSQLMgrQuery.QueryName := 'New_Query_' + IntToStr(lSQLMgrQuery.OID) ;
  {$ELSE}
    lSQLMgrQuery.QueryName := 'New_Query_' + lSQLMgrQuery.OID.AsString ;
  {$ENDIF}
  TSQLMgrGroup( pData ).Add( lSQLMgrQuery ) ;
end;

procedure TFormMain.Save;
var
  lsFileName : string ;
begin
  if TV.IsCurrentChildFormValid then
  begin
    if FSQLMgr.Dirty then
    begin
      lsFileName := gINI.ReadString( cuConstantsFileRegKey, GetDBNameAsRegKey,  '' ) ;
      if lsFileName = '' then
        aConstantsFileExecute( nil )
      else
        SaveConstantsFile( lsFileName ) ;
    end ;
    FSQLMgr.Save(FSQLMgr.FileName, cTIPersistXMLLight);
  end ;
end;

procedure TFormMain.TVDataMappings2CanAcceptDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
begin
  pbConfirm := ( pDataSource is TSQLMgrQuery ) ;
end;

procedure TFormMain.TVDataMappings1CanAcceptDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
begin
  pbConfirm :=
       ( pDataSource is TSQLMgrGroup ) 
    or ( pDataSource is TSQLMgrQuery ) ;
end;

procedure TFormMain.TVDataMappings0CanAcceptDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pbConfirm: Boolean);
begin
  pbConfirm :=
      ( pDataSource is TSQLMgrGroup ) ;
end;

procedure TFormMain.aAdhocQueryExecute(Sender: TObject);
var
  lsEXEName : string ;
  lsParams  : string ;
  lDBConnectParams : TDBConnectParams ;
begin
  lsEXEName := 'tiSQLEditor.exe' ;
  // Better to have this all as a property of the dbConnectionPoolMgr
  lDBConnectParams :=  gTIPerMgr.DefaultDBConnectionPool.DBConnectParams ;
  lsParams  :=
    ' -pl ' + gTIPerMgr.DefaultPerLayerName +
    ' -d '  + lDBConnectParams.DatabaseName +
    ' -u '  + lDBConnectParams.UserName +
    ' -p '  + lDBConnectParams.UserPassword +
    ' -ns' ;
  tiShellExecute( lsEXEName, lsParams ) ;
end;

procedure TFormMain.aFindQueryExecute(Sender: TObject);
var
  lForm : TFormFindQuery ;
begin
  lForm := TFormFindQuery.Create( nil ) ;
  try
    lForm.SQLMgr := FSQLMgr ;
    if lForm.ShowModal = mrOK then
      FindQueryInTreeView( lForm.SQLMgrQuery.QueryName ) ;
  finally
    lForm.Free ;
  end;
end;

function TFormMain.FindQueryInTreeView(const psQueryName: String): Boolean;
var
  lNode: TTreeNode;
  lGroupNode: TTreeNode;
  lSQLMgrQuery: TSQLMgrQuery;
begin
  Result := False;

  lSQLMgrQuery := FSQLMgr.FindQueryByName(psQueryName);

  if lSQLMgrQuery <> nil then
  begin
    lGroupNode := TV.FindNodeByData(lSQLMgrQuery.Owner);
    if lGroupNode <> nil then
    begin
      lGroupNode.Expand(False);

      lNode := TV.FindNodeByData(lSQLMgrQuery);
      if lNode <> nil then
      begin
        TV.Selected := lNode;
        Result := True;
      end;
    end;
  end;
end;

procedure TFormMain.test1Click(Sender: TObject);
//var ThrdCustomQueryFind: TThrdCustomQueryFind;
begin
(*
  ThrdCustomQueryFind := TThrdCustomQueryFind.CreateExt(TVisQueryFind);
  ThrdCustomQueryFind.SQLMgr := FSQLMgr ;
  // Not sure about the try - finally here, because if the line below fails,
  // we probably dont want to resume the thread.
  try
    ThrdCustomQueryFind.SetParams(
      [qfpGroupName{, qfpQueryName, qfpParamName, qfpSQL, qfpQueryLocked, qfpTestInclude}],
      '*EMS*',
      '*A?X*',
      EmptyStr,
      EmptyStr,
      '*OID*',
      True,
      True);
  finally
    ThrdCustomQueryFind.Resume;
  end;//finally
*)

end;

procedure TFormMain.aSortExecute(Sender: TObject);
begin
  if tbSort.Down then TV.TreeSortType := stText
  else begin
    TV.TreeSortType := stNone;
    Read;
  end;{else}
end;

procedure TFormMain.tiTVMappingSQLMgrOnDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
begin
  TSQLMgr( pDataParentSource ).Extract( TPerObjAbs( pDataSource )) ;
  TSQLMgr( pDataTarget ).Insert( 0, TPerObjAbs( pDataSource )) ;
  TPerObjAbs( pDataSource ).Dirty := true ;
end;

procedure TFormMain.tiTVMappingSQLMgrGroupOnDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
begin

  // A TSQLMgrQuery was dropped
  if pDataSource is TSQLMgrQuery then
  begin
    TSQLMgrGroup( pDataParentSource ).Extract( TPerObjAbs( pDataSource )) ;
    TSQLMgrGroup( pDataTarget ).Insert( 0, TPerObjAbs( pDataSource )) ;
    TPerObjAbs( pDataSource ).Dirty := true ;
  end
  // A TSQLMgrGroup was dropped
  else if pDataSource is TSQLMgrGroup then
  begin
    TSQLMgr( pDataParentSource ).Extract( TPerObjAbs( pDataSource )) ;
    TSQLMgr( pDataParentTarget ).Insert( 0, TPerObjAbs( pDataSource )) ;
    TPerObjAbs( pDataSource ).Dirty := true ;
  end ;

end;

procedure TFormMain.tiTVMappingSQLMgrQueryOnDrop(ptiTVSource: TtiTreeView;
  pDataSource, pDataParentSource: TObject; ptiTVTarget: TtiTreeView;
  pDataTarget, pDataParentTarget: TObject; var pSelectedData: TObject);
var
  lsQueryName : string ;
  lSQLMgrQuery : TSQLMgrQuery ;
  i : integer ;
begin
  Assert( pDataSource        is TSQLMgrQuery, 'pDataSource not a TSQLMgrQuery' ) ;
  Assert( pDataParentSource  is TSQLMgrGroup, 'pDataParentSource not a TSQLMgrGroup' ) ;

  Assert( pDataTarget        is TSQLMgrQuery, 'pDataTarget not a TSQLMgrQuery' ) ;
  Assert( pDataParentTarget  is TSQLMgrGroup, 'pDataParentTarget not a TSQLMgrGroup' ) ;


  lsQueryName := TSQLMgrQuery( pDataSource ).QueryName ;

  // Move or copy ?
  if tiMessageDlg( Format( crsDropConfirm,
                           [lsQueryName] ),
                   [crsCopy, crsMove],
                   mtConfirmation,
                   'Move or copy' ) = crsMove then
  // Move
  begin
    TSQLMgrGroup( pDataParentSource ).Extract( TPerObjAbs( pDataSource )) ;
    TSQLMgrGroup( pDataParentTarget ).Insert( TPerObjAbs( pDataTarget ),
                                              TPerObjAbs( pDataSource )) ;
    TPerObjAbs( pDataSource ).Dirty := true ;
  end
  else
  // Copy/Clone
  begin
    lSQLMgrQuery := TSQLMgrQuery( pDataSource ).Clone ;
    // Should do better than this for a default query name
    lsQueryName := lsQueryName + '_New' ;
    tiInputQuery( lsQueryName,
                  crsEnterNewQueryName,
                  crsEnterNewQueryName,
                  50 ) ;
    lSQLMgrQuery.QueryName := lsQueryName ;
    // Should really go into a loop here if the name is not unique.
    if not FSQLMgr.IsUnique( lSQLMgrQuery ) then
    begin
      tiAppError( Format( crsQueryNameNotUnique, [lSQLMgrQuery.QueryName]));
      lSQLMgrQuery.Free ;
    end
    else
    begin
      // There has gotta be a better way than this...
      {$IFDEF OID_AS_INT64}
        lSQLMgrQuery.OID := gTIPerMgr.NextOIDMgr.NextOID ;
      {$ELSE}
        lSQLMgrQuery.OID.GetNextValue(gTIPerMgr.DefaultDBConnectionName, gTIPerMgr.DefaultPerLayerName) ;
      {$ENDIF}
      lSQLMgrQuery.ObjectState := posCreate ;
      for i := 0 to lSQLMgrQuery.Params.Count - 1 do
      begin
        {$IFDEF OID_AS_INT64}
          TSQLMgrParam( lSQLMgrQuery.Params.Items[i] ).OID := gTIPerMgr.NextOIDMgr.NextOID ;
        {$ELSE}
          TSQLMgrParam( lSQLMgrQuery.Params.Items[i] ).OID.GetNextValue(gTIPerMgr.DefaultDBConnectionName, gTIPerMgr.DefaultPerLayerName) ;
        {$ENDIF}
        TSQLMgrParam( lSQLMgrQuery.Params.Items[i] ).ObjectState := posCreate ;
      end ;
      TSQLMgrGroup( pDataParentTarget ).Insert( TPerObjAbs( pDataTarget ),
                                                lSQLMgrQuery) ;
    end ;
  end ;

end;

procedure TFormMain.aExecuteThenSaveExecute(Sender: TObject);
begin
  if TV.CurrentChildForm is TFormChildQuery then
    TFormChildQuery( TV.CurrentChildForm ).ExecuteThenSave ;
end;

procedure TFormMain.aDatabaseConnectionDetailsExecute(Sender: TObject);
var
  ls : string ;
begin
  ls := gTIPerMgr.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
  tiShowMessage( ls ) ;
end;

procedure TFormMain.aShowCurrentObjectExecute(Sender: TObject);
begin
  tiShowPerObjAbs( TPerObjAbs( TV.SelectedData ), true ) ;
end;

procedure TFormMain.aFileSaveAsExecute(Sender: TObject);
begin
  // Not that easy...
  // 1. Create an empty database with the new file name
  // 2. Connect to the new database
  // 3. Clone over all the data
  // 4. Disconnect from the old database
  //    All this will be a little tricky in the context of a FSQLMgr.Save call
  Assert( false, 'Under construction' ) ;
end;

procedure TFormMain.aFileOpenExecute(Sender: TObject);
begin
  // See aFileSaveAsExecute 
  Assert( false, 'Under construction' ) ;
end;

end.

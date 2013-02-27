unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiPerAwareCtrls, ExtCtrls, tiListView, tiListViewPlus,
  tiFileSync_Mgr, tiFileName_BOM, tiFileSyncSetup_BOM, ComCtrls, Buttons,
  tiFocusPanel
  {$IFDEF INTERNAL_HTTP_SERVER}
  ,tiFileSyncReader_Remote_Svr
  {$ENDIF}
  ;

type
  TFormMain = class(TForm)
    sbSyncSourceToTarget: TSpeedButton;
    btnShowFiles: TButton;
    bbClose: TBitBtn;
    PC: TPageControl;
    tsDirToSync: TTabSheet;
    tsFilters: TTabSheet;
    sbSyncTargetToSource: TSpeedButton;
    LV: TtiListView;
    tmrRun: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbSyncSourceToTargetClick(Sender: TObject);
    procedure btnShowFilesClick(Sender: TObject);
    procedure LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVFilterData(pData: TPersistent; var pbInclude: Boolean);
    procedure bbCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PCChange(Sender: TObject);
    procedure sbSyncTargetToSourceClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tmrRunTimer(Sender: TObject);
  private
    FData : TtiFileSyncSetup ;
    {$IFDEF INTERNAL_HTTP_SERVER}
      FFileSyncServer : TtiFileSyncRemoteServer;
    {$ENDIF}
    procedure Read;
    procedure DoSync(pDirection: TtiSyncDirection);
    function AutoRun: boolean;
  public
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiUtils
  ,tiLog
  ,cFileSync
  ,tiPtnVisMgr
  ,tiPtnVisPerObj_Cli
  ,tiFileSyncReader_Abs
  ,FViewFiles
  ,FProgress
  ,tiXML
  ,FFileSyncSetupEdit
  ,tiPtnVisPerObj
  ,tiRegINI
  ,FFileNameFilterEdit
  ,tiDialogs
  ,tiCommandLineParams
  ;

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FData := TtiFileSyncSetup.Create ;
  gReg.ReadFormState( self ) ;
  PC.ActivePageIndex := 0 ;
  Read ;
  {$IFDEF INTERNAL_HTTP_SERVER}
    FFileSyncServer := TtiFileSyncRemoteServer.Create;
    FFileSyncServer.Active:= True ;
  {$ENDIF}
end ;

procedure TFormMain.Read ;
begin
  LV.Data := nil ;
  FData.Read ;
  PCChange( nil ) ;
end ;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FData.Free ;
  gReg.WriteFormState( self ) ;
  {$IFDEF INTERNAL_HTTP_SERVER}
    FFileSyncServer.Free;
  {$ENDIF}
end;

procedure TFormMain.sbSyncSourceToTargetClick(Sender: TObject);
begin
  DoSync( sdSourceToTarget ) ;
end;

procedure TFormMain.DoSync( pDirection : TtiSyncDirection ) ;
var
  lThrd : TthrdFileSyncMgr ;
begin
  lThrd := TthrdFileSyncMgr.CreateExt ;
  lThrd.FileSyncSetups.Assign( FData ) ;
  lThrd.FileSyncSetups.SyncDirection := pDirection ;
  lThrd.FormMain := self ;
  lThrd.CloseAppOnTerminate := AutoRun ;
  lThrd.Resume ;
end;

procedure TFormMain.btnShowFilesClick(Sender: TObject);
var
  lForm : TFormViewFiles ;
  lFSM  : TtiFileSyncMgr;
begin
  Assert(FData.FileSyncDirs.COunt = 1, 'FData.FileSyncDirs.Count <> 1');
  lFSM  := TtiFileSyncMgr.Create;
  try
    Screen.Cursor := crHourGlass ;
    try
      lFSM.AssignFromFileSyncDir(FData.FileSyncDirs.Items[0]);
      lFSM.ReadIndexes ;
    finally
      Screen.Cursor := crDefault ;
    end ;
    lForm := TFormViewFiles.Create( nil ) ;
    try
      lForm.FileSyncMgr := lFSM ;
      lForm.ShowModal ;
    finally
      lForm.Free ;
    end ;
  finally
    lFSM.Free;
  end;
end;

{
procedure TFormMain.SetWorking(const Value: boolean);
begin
  FbWorking       := Value;
  gbLocal.Enabled := not FbWorking ;
  sbSynchroniseLocalToRemote.Enabled := not FbWorking ;
  btnShowFiles.Enabled := not FbWorking ;
  gbRemote.Enabled := not FbWorking ;
  FFormProgress.Visible := FbWorking ;

//    paeLocalRootDir: TtiPerAwareComboBoxHistory;
//    paeArchiveType: TtiPerAwareComboBoxStatic;
//    paeRemoteRootDir: TtiPerAwareComboBoxHistory;
end;
}
procedure TFormMain.LVItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete( TPerObjAbs( pData )) then
    pLV.Refresh ;
end;

procedure TFormMain.LVItemEdit(pLV: TtiCustomListView; pData: TPersistent;  pItem: TListItem);
var
  lData : TPerObjAbs ;
begin
  lData := TPerObjAbs( pData ) ;
  if lData is TtiFileSyncDir then
    TFormFileSyncSetupEdit.Execute( lData )
  else if lData is TtiFileNameFilter then
    TFormFileNameFilterEdit.Execute( lData )
  else
    tiFmtException( 'Invalid class type passed', ClassName, 'LVItemEdit' ) ;
end;

procedure TFormMain.LVItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TPerObjAbs ;
begin
  if PC.ActivePage = tsDirToSync then
  begin
    lData := TtiFileSyncDir.CreateNew ;
    TtiFileSyncDir(lData).SourceReader := cgsDiskFiles ;
    if TFormFileSyncSetupEdit.Execute( lData ) then
      FData.FileSyncDirs.Add( TtiFileSyncDir( lData ))
    else
      lData.Free ;
  end
  else if PC.ActivePage = tsFilters then
  begin
    lData := TtiFileNameFilter.CreateNew ;
    if TFormFileNameFilterEdit.Execute( lData ) then
      FData.FileNameFilters.Add( TtiFileNameFilter(lData ))
    else
      lData.Free ;
  end
  else
    tiFmtException( 'Invalid PC.ActivePage', ClassName, 'LVItemInsert' ) ;

end;

procedure TFormMain.LVFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;

procedure TFormMain.bbCloseClick(Sender: TObject);
begin
  Close ;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false ;
  FData.Save ;
  CanClose := true ;
end;

procedure TFormMain.PCChange(Sender: TObject);
begin
  LV.Data := nil ;
  LV.ClearColumns ;
  LV.Parent := PC.ActivePage ;
  if PC.ActivePage = tsDirToSync then
  begin
    LV.AddColumn( 'SourceReader',   lvtkString, 'Source reader' ) ;
    LV.AddColumn( 'LocalDir',       lvtkString, 'Source file location' ) ;
    LV.AddColumn( 'TargetReader',   lvtkString, 'Target reader' ) ;
    LV.AddColumn( 'TargetLocation', lvtkString, 'Target file location' ) ;
    LV.Data := FData.FileSyncDirs.List;
  end
  else if PC.ActivePage = tsFilters then
  begin
    LV.AddColumn( 'FilterTypeAsString', lvtkString,   'Filter type' ) ;
    LV.AddColumn( 'WildCard',           lvtkString,   'Wild card' ) ;
    LV.Data := FData.FileNameFilters.List;
  end
  else
    tiFmtException( 'Invalid PC.ActivePage', ClassName, 'PCChange' ) ;

end;

procedure TFormMain.sbSyncTargetToSourceClick(Sender: TObject);
begin
  if tiAppConfirmation(
    'Are you sure you want to synchronise from the target to the source?' + Cr(2) +
    'Your source files will be overwritten.' ) then
    DoSync( sdTargetToSource ) ;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LV.SetBounds(
    4,
    4,
    PC.ActivePage.ClientWidth - 8,
    PC.ActivePage.ClientHeight - 8 ) ;
end;

procedure TFormMain.tmrRunTimer(Sender: TObject);
begin
  tmrRun.Enabled := false ;
  if AutoRun then
    DoSync( sdSourceToTarget ) ;
end;

function TFormMain.AutoRun : boolean ;
begin
  result :=
    gCommandLineParams.IsParam('AutoRun') or
    gCommandLineParams.IsParam('AutoStart') or
    gCommandLineParams.IsParam('Run') or
    gCommandLineParams.IsParam('Start') ;
end ;

end.

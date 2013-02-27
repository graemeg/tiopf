{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au
  Mail: peter_hinrichsen@techinsite.com.au

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    October 2001, Peter Hinrichsen, Created

  Purpose:
    A mailing list forwarding application

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IdSMTP, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdMessageClient, IdPOP3, StdCtrls, IdAntiFreezeBase, IdAntiFreeze,
  ExtCtrls, tiLstSrvr_Cli, ComCtrls, ImgList, ActnList, ToolWin, tiButtons,
  Menus, tiTrayIcon, tiLstSrvrCfg_BOM, tiTreeView ;

type
  TFormMain = class(TForm)
    tmr: TTimer;
    sb: TStatusBar;
    tiToolBar1: TtiToolBar;
    ToolButton1: TToolButton;
    AL: TActionList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    pmTrayIcon: TPopupMenu;
    ToolButton4: TToolButton;
    Close1: TMenuItem;
    Checknow1: TMenuItem;
    N1: TMenuItem;
    Configure1 : TMenuItem;
    aCheckNow  : TAction ;
    aClose     : TAction ;
    aViewLog: TAction;
    Viewlog1: TMenuItem;
    TV: TtiTreeView;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    aSave: TAction;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    aNew: TAction;
    aDelete: TAction;
    MM: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    Close2: TMenuItem;
    N2: TMenuItem;
    Checknow2: TMenuItem;
    Edit1: TMenuItem;
    About1: TMenuItem;
    Viewlog2: TMenuItem;
    New1: TMenuItem;
    Deleteselectedmailinglist1: TMenuItem;
    aToggleEnabled: TAction;
    ToggleEnabled1: TMenuItem;
    tbToggleEnabled: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aCheckNowExecute(Sender: TObject);

    procedure TrayIconClick(Sender: TObject; Button: TMouseButton; var DefaultAction: Boolean);
    procedure TrayIconDoubleClick(Sender: TObject; Button: TMouseButton; var DefaultAction: Boolean);
    procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton);
    procedure TrayIconMouseMove(Sender: TObject);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton);

    procedure aMimiseExecute(Sender: TObject);
    procedure aConfigExecute(Sender: TObject);
    procedure ALUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aViewLogExecute(Sender: TObject);
    procedure tiTVMappingtiLstSrvrCfgsOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aToggleEnabledExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure TVFilterData(pData: TObject; var pbInclude: Boolean);
    procedure tiTVMappingtiLstSrvrCfgOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
  private
    FidAntiFreeze : TidAntiFreeze ;
    FTrayIcon : TtiTrayIcon ;
    FCheckEvery : integer ;
    FLastChecked : TDateTime ;
    FProcessing : boolean ;
    FForceClose : boolean ;

    FLstSrvrCfgs : TtiLstSrvrCfgs ;
    FSaveEnabledLstSrvr : boolean ;

    procedure Execute ;
    procedure UpdateStatus ;
    procedure DoThrdLstSrvrTerminate( Sender : TObject ) ;
    function  SecToNextCheck : integer ;
    function  GetEnabledLstSrvr: boolean;
    procedure SetEnabledLstSrvr(const Value: boolean);
    procedure DoOnUpdateProgress( const pTarget : integer ; const pProgress : integer ; const pMessage : string ) ;
    { Private declarations }
  protected
  public
    property EnabledLstSrvr : boolean read GetEnabledLstSrvr write SetEnabledLstSrvr ;
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiUtils
  ,tiLog
  ,FConfig
  ,tiDialogs
  ,tiPtnVisPerObj_Cli
  ,tiPtnVisPerObj
  ,tiRegINI
  ,tiCommandLineParams
  ;

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin

  TV.RegisterChildForm( TtiLstSrvrCfg, TFormConfig ) ;

  FLstSrvrCfgs := TtiLstSrvrCfgs.Create ;
  FLstSrvrCfgs.Read ;
  TV.Data := FLstSrvrCfgs ;
  TV.SelectedAddress := gReg.ReadString( Name, 'SelectedAddress', '' ) ;

  FidAntiFreeze := TidAntiFreeze.Create( Self ) ;
  gReg.ReadFormState( self ) ;


  FTrayIcon := TtiTrayIcon.Create( Self ) ;
  with FTrayIcon do
  begin
    Hint            := Application.Title ;
    PopupMenu       := pmTrayIcon ;
    ShowApplication := False ;
    Visible         := True ;
    OnClick         := TrayIconClick ;
    OnDoubleClick   := TrayIconDoubleClick ;
    OnMouseDown     := TrayIconMouseDown ;
    OnMouseMove     := TrayIconMouseMove ;
    OnMouseUp       := TrayIconMouseUp ;
  end ;

  if gCommandLineParams.IsParam('NoTrayIcon') then
  begin
    FTrayIcon.ShowApplication := True ;
    SetEnabledLstSrvr( false ) ;
  end else
  begin
    FTrayIcon.ShowApplication := False ;
    SetEnabledLstSrvr( true ) ;
  end ;

  FCheckEvery  := gReg.ReadInteger( Name, 'CheckEvery', 60 ) ;
  FLastChecked := Now ;
  FProcessing  := false ;
  FForceClose  := false ;
  FSaveEnabledLstSrvr := EnabledLstSrvr ;

end;

//------------------------------------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin

  gReg.WriteString( Name, 'SelectedAddress', TV.SelectedAddress ) ;
  FLstSrvrCfgs.Free ;
  gReg.WriteFormState( self ) ;

end;

//------------------------------------------------------------------------------
procedure TFormMain.Execute ;
var
  lThrd : TThrdLstSrvr ;
begin
  FProcessing := true ;
  tmr.enabled := false ;
  lThrd := TThrdLstSrvr.Create ;
  lThrd.LstSrvrCfgs := FLstSrvrCfgs ;
  lThrd.OnTerminate := DoThrdLstSrvrTerminate ;
  lThrd.OnUpdateProgress := DoOnUpdateProgress ;
  UpdateStatus ;
  lThrd.Resume ;
end ;

//------------------------------------------------------------------------------
procedure TFormMain.tmrTimer(Sender: TObject);
begin
  if FProcessing then
    Exit ; //==>

  if Now < FLastChecked + cdtOneSecond * FCheckEvery then
  begin
    UpdateStatus ;
    exit ; //==>
  end ;

  Execute ;

end;

//------------------------------------------------------------------------------
procedure TFormMain.UpdateStatus ;
var
  lStatus : string ;
begin
  if FProcessing then
    lStatus := Application.Title + ' - ' +
              'Processing...'
  else
    lStatus := Application.Title + ' - ' +
              'Time to next check: ' +
              IntToStr( SecToNextCheck ) ;
  sb.SimpleText  := lStatus ;
  FTrayIcon.Hint := lStatus ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aCloseExecute(Sender: TObject);
begin
  FForceClose := true ;
  Close ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aCheckNowExecute(Sender: TObject);
begin
  FLastChecked := cdtOneSecond * FCheckEvery - 1 ;
  Tmr.Enabled := true ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.TrayIconClick(Sender: TObject; Button: TMouseButton;
  var DefaultAction: Boolean);
begin
//
end;

//------------------------------------------------------------------------------
procedure TFormMain.TrayIconDoubleClick(Sender: TObject;
  Button: TMouseButton; var DefaultAction: Boolean);
begin
//  aViewLogExecute( nil ) ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.TrayIconMouseDown(Sender: TObject;
  Button: TMouseButton);
begin
  //
end;

//------------------------------------------------------------------------------
procedure TFormMain.TrayIconMouseMove(Sender: TObject);
begin
  UpdateStatus ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.TrayIconMouseUp(Sender: TObject;
  Button: TMouseButton);
begin
//
end;

//------------------------------------------------------------------------------
procedure TFormMain.aMimiseExecute(Sender: TObject);
begin
  FTrayIcon.ShowApplication := false ;
  Visible := false ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.aConfigExecute(Sender: TObject);
begin
  FTrayIcon.ShowApplication := true ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.ALUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  if FLstSrvrCfgs = nil then
    Exit ; //==>

  aNew.Enabled      := TV.CanInsertSelected ;
  aDelete.Enabled   := TV.CanDeleteSelected ;
  aSave.Enabled     := FLstSrvrCfgs.Dirty ;
  aCheckNow.Enabled := not FProcessing   ;
  aClose.Enabled    := aCheckNow.Enabled ;
  Handled := true ;
end;

//------------------------------------------------------------------------------
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  cMinimise = '&Minimise' ;
  cClose  = 'C&lose' ;
  cCancel = 'C&ancel' ;
var
  lResult : string ;
begin

  if not FForceClose then
  begin
    CanClose := false ;
    lResult := tiMessageDlg( 'Do you want to minimise or close?',
                             [cMinimise, cClose, cCancel ] ) ;
    if lResult = cCancel then
      Exit  //==>
    else if lResult = cMinimise then
    begin
      FTrayIcon.ShowApplication := false ;
      Visible := false ;
      Exit ; //==>
    end ;
  end ;

  FForceClose := false ;

  if not tiPerObjAbsSaveAndClose( FLstSrvrCfgs, CanClose ) then
    Exit ; //==>

  CanClose := not FProcessing ;
  if not CanClose then
    tiAppWarning( 'You can not close tiLstSrvr while it is processing messages.' ) ;

end;


//------------------------------------------------------------------------------
procedure TFormMain.DoThrdLstSrvrTerminate( Sender : TObject ) ;
begin
  FProcessing := false ;
  FLastChecked := now ;
  UpdateStatus ;
  tmr.enabled := true ;
end;

//------------------------------------------------------------------------------
function TFormMain.SecToNextCheck: integer;
begin
  result := FCheckEvery - Trunc(( Now - FLastChecked ) * 24 * 60 * 60 ) ;
end;


procedure TFormMain.aViewLogExecute(Sender: TObject);
begin
  tiEditFile( gLog.LogFileName ) ;
end;

procedure TFormMain.tiTVMappingtiLstSrvrCfgsOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
var
  lData : TtiLstSrvrCfg ;
begin
  lData := TtiLstSrvrCfg.CreateNew ;
  FLstSrvrCfgs.Add( lData ) ;
end;

procedure TFormMain.aSaveExecute(Sender: TObject);
begin
  FLstSrvrCfgs.Save ;
end;

procedure TFormMain.ToolButton8Click(Sender: TObject);
begin
  tiShowPerObjAbs( FLstSrvrCfgs ) ;
end;

procedure TFormMain.aNewExecute(Sender: TObject);
begin
  TV.DoInsert ;
end;

procedure TFormMain.aDeleteExecute(Sender: TObject);
begin
  TV.DoDelete ;
end;

function TFormMain.GetEnabledLstSrvr: boolean;
begin
  result := tmr.Enabled ;
end;

procedure TFormMain.SetEnabledLstSrvr(const Value: boolean);
begin
  tmr.Enabled := Value ;
  if Value then
  begin
    aToggleEnabled.Caption := '&Stop';
    aToggleEnabled.Hint := 'Stop checking for email messages' ;
    tbToggleEnabled.Down := true ;
  end else
  begin
    aToggleEnabled.Caption := '&Start' ;
    aToggleEnabled.Hint := 'Start checking for email messages' ;
    tbToggleEnabled.Down := false ;
  end ;
end;

procedure TFormMain.aToggleEnabledExecute(Sender: TObject);
begin
  SetEnabledLstSrvr( not EnabledLstSrvr ) ;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  FSaveEnabledLstSrvr := EnabledLstSrvr ;
  SetEnabledLstSrvr( False ) ;
end;

procedure TFormMain.FormHide(Sender: TObject);
begin
  SetEnabledLstSrvr( FSaveEnabledLstSrvr ) ;
end;

procedure TFormMain.TVFilterData(pData: TObject; var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;

procedure TFormMain.tiTVMappingtiLstSrvrCfgOnDelete(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  if tiAppConfirmation( 'Are you sure you want to delete the mailing list <' +
                        TPerObjAbs( pData ).Caption + '>?' ) then
    TPerObjAbs( pData ).Deleted := true ;
end;

procedure TFormMain.DoOnUpdateProgress(const pTarget, pProgress: integer; const pMessage: string);
var
  lMessage : string ;
begin
  lMessage :=
    Application.Title + ' - ' +
    'Processing... ' +
    IntToStr( pProgress ) +
    '/' +
    IntToStr( pTarget ) +
    ' messages sent.' ;
  sb.SimpleText  := lMessage ;
  FTrayIcon.Hint := lMessage ;
end;

end.

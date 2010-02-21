{
  Purpose:
    Allow the load from file, edit, run, save to file of an adhoc query

  Classes:
    TFormAdHocQuery - the form

}


unit FMainSQLEditor;

{$I tiDefines.inc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ComCtrls, ToolWin, tiButtons, StdCtrls, FSQLEditor
  {$IFDEF FPC}
  , LResources
  {$ENDIF}
  ;

resourcestring
  crsFormCaption = 'tiSQLEditor ' ;
  crsDoYouWantToSave  = 'Do you want to save your changes?' ;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N2: TMenuItem;
    ActionList1: TActionList;
    aFileOpen: TAction;
    aFileSave: TAction;
    aNew: TAction;
    aRun: TAction;
    aClose: TAction;
    ImageList1: TImageList;
    Fileopen1: TMenuItem;
    Filesave1: TMenuItem;
    New1: TMenuItem;
    Run1: TMenuItem;
    Close1: TMenuItem;
    aFileSaveAs: TAction;
    Saveas1: TMenuItem;
    aRunScript: TAction;
    Runasscript1: TMenuItem;
    pmRun: TPopupMenu;
    Runasscript2: TMenuItem;
    miEdit: TMenuItem;
    Query1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    aDatabaseConnectionDetails: TAction;
    Viewdatabaseconnectiondetails1: TMenuItem;
    N1: TMenuItem;
    SB: TStatusBar;
    TB: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aFileOpenExecute(Sender: TObject);
    procedure aFileSaveExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aRunExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aFileSaveAsExecute(Sender: TObject);
    procedure aRunScriptExecute(Sender: TObject);
    procedure ActionList1OnUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure aDatabaseConnectionDetailsExecute(Sender: TObject);
  private
    FSQLEditor : TFormSQLEditor ;
    FsFileName : string ;
    procedure Save ;
    procedure Load( const pFileName : TFileName ) ;
    function  GetSQL: string;
    procedure RunAsScript(const psAppName, psParams : string);
  public
    { Public declarations }
  end;

var
  FormMain : TFormMain ;

implementation
uses
   tiUtils
  ,{FSQLMgrBrowse
  ,}tiSQLMgr_BOM
  ,FRunAsScript
  ,tiLog
  ,{FAbout
  ,}tiOPFManager
  ,tiDialogs
  ,tiCommandLineParams
  ,tiINI
  ,tiGUIINI
  ,tiDBConnectionPool
  ,tiObject
  {$IFDEF FPC}
  ,lcltype
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ,Windows      // Used for GetKeyboardState
  {$ENDIF}
  ,tiQuery
  ;

//{$IFDEF FPC}
  {$R *.dfm}
//{$ENDIF}

const
  cDatabaseName   = '%DatabaseName%';
  cUserName       = '%UserName%';
  cPassword       = '%Password%';
  cScriptFileName = '%ScriptFileName%' ;


procedure TFormMain.FormCreate(Sender: TObject);
{$IFDEF SYNEDIT}
  var
    lMenuItem : TMenuItem ;
{$ENDIF}
begin
  GGUIINI.ReadFormState( self ) ;
  

  FSQLEditor := TFormSQLEditor.Create( self ) ;
  FSQLEditor.Parent := self ;
  FSQLEditor.Align:= alClient;
//  FSQLEditor.Top := TB.Top + TB.Height + 24 ;
//  FSQLEditor.Left := 4 ;
//  FSQLEditor.Height := ClientHeight - TB.Height - 8 - SB.Height ;
//  FSQLEditor.Width  := ClientWidth - 8  ;
//  FSQLEditor.Anchors := [akTop, akLeft, akBottom, akRight ] ;
  Caption := crsFormCaption ;
  FSQLEditor.Visible := true ;
//  FSQLEditor.SetFocus ;
  FsFileName := GINI.ReadString( 'FileName', gCommandLineParams.GetParam( 'd' ), '' ) ;
  if ( FsFileName <> '' ) and
     ( FileExists( FsFileName )) then
    Load( FsFileName )
  else
    FsFileName := '' ;

  {$IFDEF SYNEDIT}
    lMenuItem := TMenuItem.Create( MainMenu1 ) ;
    miEdit.Add( lMenuItem ) ;
    lMenuItem.Action := FSQLEditor.aFindInSQL ;

    lMenuItem := TMenuItem.Create( MainMenu1 ) ;
    miEdit.Add( lMenuItem ) ;
    lMenuItem.Action := FSQLEditor.aFindInSQLAgain ;
  {$ELSE}
      miEdit.Visible := false ;
  {$ENDIF}

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  GGUIINI.WriteFormState( self ) ;
end;

procedure TFormMain.aFileOpenExecute(Sender: TObject);
var
  lOD : TOpenDialog ;
begin
  lOD := TOpenDialog.Create( nil ) ;
  try
    lOD.FileName := GINI.ReadString( 'FileName',
                                     gCommandLineParams.GetParam( 'd' ), '' ) ;
    lOD.Filter   := 'SQL files|*.SQL|All files|*.*' ;
    lOD.DefaultExt := '.SQL' ;
    if lOD.Execute then
      Load( lOD.FileName ) ;
  finally
    lOD.Free ;
  end ;
end;

procedure TFormMain.aFileSaveExecute(Sender: TObject);
begin
  if FsFileName = '' then
    aFileSaveAsExecute( nil )
  else
    Save ;
end;

procedure TFormMain.aFileSaveAsExecute(Sender: TObject);
var
  lSD : TSaveDialog ;
begin
  lSD := TSaveDialog.Create( nil ) ;
  try
    lSD.FileName   := FsFileName ;
    lSD.Filter     := 'SQL files|*.SQL|All files|*.*' ;
    lSD.DefaultExt := '.SQL' ;
    if lSD.Execute then
    begin
      FsFileName := lSD.FileName ;
      GINI.WriteString( 'FileName',
                        gCommandLineParams.GetParam( 'd' ),
                        FsFileName ) ;
      Save ;
    end ;
  finally
    lSD.Free ;
  end ;
end;

procedure TFormMain.aNewExecute(Sender: TObject);
  procedure _DoNew ;
  begin
    FSQLEditor.Clear ;
    FsFileName := '' ;
    Caption := crsFormCaption ;
  end ;
var
  lAction : word ;
begin

  if not FSQLEditor.Dirty then
  begin
    _DoNew ;
    Exit ; //==>
  end ;

  lAction := tiYesNoCancel( crsDoYouWantToSave ) ;

  case lAction of
    mrYes    : begin
                 aFileSaveExecute(nil);
                 _DoNew ;
               end ;
    mrNo     : _DoNew ;
  end ;

end;

procedure TFormMain.aRunExecute(Sender: TObject);
begin
  FSQLEditor.Execute ;
end;

function TFormMain.GetSQL : string ;
begin
  result := FSQLEditor.SQL ;
end ;

procedure TFormMain.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

procedure TFormMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree ;
end;

procedure TFormMain.Save;
begin
  FSQLEditor.SaveToFile( FsFileName ) ;
  Caption := crsFormCaption + ' - ' + FsFileName ;
  FSQLEditor.Dirty := false ;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  lAction : word ;
begin
  if not FSQLEditor.Dirty then
  begin
    CanClose := true ;
    Exit ; //==>
  end ;

  lAction := tiYesNoCancel( crsDoYouWantToSave ) ;

  case lAction of
    mrYes    : begin
                 aFileSaveExecute( nil ) ;
                 CanClose := true ;
               end ;
    mrNo     : CanClose := true ;
    mrCancel : CanClose := false ;
  end ;

end;

procedure TFormMain.aRunScriptExecute(Sender: TObject);
var
  lForm : TFormRunAsScript ;
begin
  lForm := TFormRunAsScript.Create( nil ) ;
  try
    if lForm.ShowModal = mrOK then
      RunAsScript( lForm.AppToRun, lForm.Params ) ;
  finally
    lForm.Free;
  end;
end;

procedure TFormMain.RunAsScript( const psAppName, psParams : string ) ;
var
  lFileName : string ;
  lParams   : string ;
  lDBParams: TtiDBConnectionParams;
begin
  lFileName := tiGetTempFile( 'SQL' ) ;
  lFileName := tiAddTrailingSlash(ExtractFilePath(lFileName)) + 'tiSQLEditor\' + ExtractFileName(lFileName);
  tiForceDirectories(ExtractFilePath(lFileName));
  tiStringToFile( GetSQL, lFileName ) ;
  lDBParams := gTIOPFManager.DefaultDBConnectionPool.DBConnectParams;
  lParams := tiStrTran( psParams, cDatabaseName,   lDBParams.DatabaseName ) ;
  lParams := tiStrTran( lParams,  cUserName,       lDBParams.UserName ) ;
  lParams := tiStrTran( lParams,  cPassword,       lDBParams.Password ) ;
  lParams := tiStrTran( lParams,  cScriptFileName, lFileName ) ;
  Log('About to run ' + psAppName + ' ' + lParams);
//  tiShellExecute( psAppName, lParams ) ;
end ;


procedure TFormMain.ActionList1OnUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  lKeyStates: TKeyboardState;
  ls : string ;
begin

  {$IFDEF MSWINDOWS}
  GetKeyboardState( lKeyStates );
  {$ENDIF}
  {$IFDEF UNIX}
    {$Warning What function can we use? }
//  GetKeyboardState( lKeyStates );
  {$ENDIF}
  ls := '' ;
  if Odd( lKeyStates[VK_NUMLOCK] ) then
    ls := ls + 'NUM' ;

  if Odd( lKeyStates[VK_CAPITAL] ) then
    ls := tiAddTrailingValue( ls, '  ' ) + 'CAPS' ;

  if not Odd( lKeyStates[VK_INSERT] ) then
    ls := tiAddTrailingValue( ls, '  ' ) + 'INSERT' ;

  if Odd( lKeyStates[VK_SCROLL] ) then
    ls := tiAddTrailingValue( ls, '  ' ) + 'SCROLL' ;

  SB.Panels[1].Text := ls ;

  Handled := true ;
  aFileSave.Enabled  := FSQLEditor.Dirty ;
  aRun.Enabled       := GetSQL <> '' ;
  aRunScript.Enabled := aRun.Enabled ;
  Handled := true ;
  
end;

procedure TFormMain.aDatabaseConnectionDetailsExecute(Sender: TObject);
var
  ls : string ;
begin
  ls := gTIOPFManager.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
  tiShowMessage( ls ) ;
end;

procedure TFormMain.Load(const pFileName: TFileName);
begin
  FsFileName := pFileName ;
  GINI.WriteString( 'FileName',
                    gCommandLineParams.GetParam( 'd' ),
                    pFileName ) ;
  FSQLEditor.LoadFromFile( pFileName ) ;
  Caption := crsFormCaption + ' - ' + pFileName ;
end;

initialization
  {$IFDEF FPC}
  {$i FMainSQLEditor.lrs}
  {$ENDIF}

end.

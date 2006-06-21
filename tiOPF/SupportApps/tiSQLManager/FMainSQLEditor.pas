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

  Purpose:
    Allow the load from file, edit, run, save to file of an adhoc query

  Classes:
    TFormAdHocQuery - the form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMainSQLEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ComCtrls, ToolWin, tiButtons, StdCtrls, FSQLEditor
  ;

resourcestring
  crsFormCaption = 'tiSQLEditor ' ;
  crsDoYouWantToSave  = 'Do you want to save your changes?' ;

type
  TFormMain = class(TForm)
    TB: TtiToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
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
    aAbout: TAction;
    About1: TMenuItem;
    aDatabaseConnectionDetails: TAction;
    Viewdatabaseconnectiondetails1: TMenuItem;
    N1: TMenuItem;
    SB: TStatusBar;
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
    procedure aAboutExecute(Sender: TObject);
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
  ,FSQLMgrBrowse
  ,tiSQLMgr_BOM
  ,FRunAsScript
  ,tiLog
  ,FAbout
  ,tiPersist
  ,tiDialogs
  ,tiCommandLineParams
  ,tiRegINI
  ;

const
  cgsFileMacro = '<FILE>' ;

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
{$IFDEF SYNEDIT}
  var
    lMenuItem : TMenuItem ;
{$ENDIF}
begin
  gReg.ReadFormState( self ) ;
  FSQLEditor := TFormSQLEditor.Create( self ) ;
  FSQLEditor.Parent := self ;
  FSQLEditor.Top := TB.Height + 4 ;
  FSQLEditor.Left := 4 ;
  FSQLEditor.Height := ClientHeight - TB.Height - 8 - SB.Height ;
  FSQLEditor.Width  := ClientWidth - 8  ;
  FSQLEditor.Anchors := [akTop, akLeft, akBottom, akRight ] ;
  Caption := crsFormCaption ;
  FSQLEditor.Visible := true ;
  FSQLEditor.SetFocus ;
  FsFileName := gReg.ReadString( 'FileName', gCommandLineParams.GetParam( 'd' ), '' ) ;
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
  gReg.WriteFormState( self ) ;
end;

procedure TFormMain.aFileOpenExecute(Sender: TObject);
var
  lOD : TOpenDialog ;
begin
  lOD := TOpenDialog.Create( nil ) ;
  try
    lOD.FileName := gReg.ReadString( 'FileName',
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
      gReg.WriteString( 'FileName',
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
  lsFileName : string ;
  lsParams   : string ;
begin
  lsFileName := tiGetTempFile( 'SQL' ) ;
  tiStringToFile( GetSQL, lsFileName ) ;
  lsParams := tiStrTran( psParams, cgsFileMacro, lsFileName ) ;
  tiShellExecute( psAppName, lsParams ) ;
end ;


procedure TFormMain.ActionList1OnUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  lKeyStates: TKeyboardState;
  ls : string ;
begin

  GetKeyboardState( lKeyStates );
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

procedure TFormMain.aAboutExecute(Sender: TObject);
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

procedure TFormMain.aDatabaseConnectionDetailsExecute(Sender: TObject);
var
  ls : string ;
begin
  ls := gTIPerMgr.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
  tiShowMessage( ls ) ;
end;

procedure TFormMain.Load(const pFileName: TFileName);
begin
  FsFileName := pFileName ;
  gReg.WriteString( 'FileName',
                    gCommandLineParams.GetParam( 'd' ),
                    pFileName ) ;
  FSQLEditor.LoadFromFile( pFileName ) ;
  Caption := crsFormCaption + ' - ' + pFileName ;
end;

end.

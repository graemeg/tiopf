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

unit FAdhocQuery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ComCtrls, ToolWin, tiButtons, StdCtrls
  {$IFDEF MWEDIT}
    ,wmSQLSyn,
    mwCustomEdit
  {$ENDIF}
  ;

resourcestring
  crsFormCaption = 'tiSQLManager query' ;
  crsCloseQuery  = 'Do you want to save your changes?' ;

type
  TFormAdHocQuery = class(TForm)
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
    N1: TMenuItem;
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
  private
    {$IFDEF MWEDIT}
      FMemoSQL  : TMWCustomEdit ;
      FWMSync   : TWMSQLSyn ;
    {$ELSE}
      FMemoSQL : TMemo ;
    {$ENDIF}
    
    FsFileName : string ;
    FbDirty : boolean ;
    procedure DoMemoOnChange( sender : TObject ) ;
    procedure Save ;
    function GetSQL: string;
    procedure RunAsScript(const psAppName, psParams : string);
  public
    { Public declarations }
  end;

implementation
uses
   tiUtils
  ,FSQLMgrBrowse
  ,tiSQLMgr_BOM
  ,FRunAsScript
  ,tiLog
  ,tiDialogs
  ,tiRegINI
  ;

const
  cgsFileMacro = '<FILE>' ;

{$R *.DFM}

procedure TFormAdHocQuery.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState( self ) ;

  {$IFDEF MWEDIT}
    FMemoSQL := TMWCustomEdit.Create( self ) ;
  {$ELSE}
    FMemoSQL := TMemo.Create( self ) ;
    FMemoSQL.Font.Name := 'Courier New' ;
    FMemoSQL.Font.Size := 10 ;
  {$ENDIF}

  FMemoSQL.Parent := self ;
  FMemoSQL.Top := TB.Height + 4 ;
  FMemoSQL.Left := 4 ;
  FMemoSQL.Height := ClientHeight - TB.Height - 8 ;
  FMemoSQL.Width  := ClientWidth - 8  ;
  FMemoSQL.Anchors := [akTop, akLeft, akBottom, akRight ] ;
  //FMemoSQL.PopupMenu := pmSQL ;
  FMemoSQL.OnChange := DoMemoOnChange ;

  {$IFDEF MWEDIT}
    FMemoSQL.Gutter.ShowLineNumbers := true ;
    FMemoSQL.Gutter.LeftOffset      := 0 ;

    FWMSync := TWMSQLSyn.Create( self ) ;
    with FWMSync do begin
      CommentAttri.Foreground    := clGray     ;
      CommentAttri.Style         := [fsItalic] ;
      IdentifierAttri.Foreground := clNavy     ;
      KeyAttri.Foreground        := clMaroon   ;
      KeyAttri.Style             := [fsBold]   ;
      NumberAttri.Foreground     := clGreen    ;
      StringAttri.Foreground     := clTeal     ;
      SymbolAttri.Foreground     := clRed      ;
    end ;

    FMemoSQL.Highlighter := FWMSync ;
  {$ENDIF}

  Caption := crsFormCaption ;

  FsFileName := '' ;
  FbDirty := false ;
end;

procedure TFormAdHocQuery.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
end;

procedure TFormAdHocQuery.aFileOpenExecute(Sender: TObject);
var
  lOD : TOpenDialog ;
begin
  lOD := TOpenDialog.Create( nil ) ;
  try
    lOD.FileName := gINI.ReadString( Name, 'FileName', '' ) ;
    lOD.Filter   := 'SQL files|*.SQL|All files|*.*' ;
    lOD.DefaultExt := '.SQL' ;
    if lOD.Execute then
    begin
      FsFileName := lOD.FileName ;
      gINI.WriteString( Name, 'FileName', FsFileName ) ;
      FMemoSQL.Lines.LoadFromFile( FsFileName ) ;
      Caption := crsFormCaption + ' - ' + FsFileName ;
      FbDirty := false ;
    end ;
  finally
    lOD.Free ;
  end ;
end;

procedure TFormAdHocQuery.aFileSaveExecute(Sender: TObject);
begin
  if FsFileName = '' then
    aFileSaveAsExecute( nil )
  else
    Save ;
end;

procedure TFormAdHocQuery.aFileSaveAsExecute(Sender: TObject);
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
      gINI.WriteString( Name, 'FileName', FsFileName ) ;
      Save ;
    end ;
  finally
    lSD.Free ;
  end ;
end;

procedure TFormAdHocQuery.aNewExecute(Sender: TObject);
begin
  Save ;
  FMemoSQL.Lines.Clear ;
  FbDirty := false ;
end;

procedure TFormAdHocQuery.aRunExecute(Sender: TObject);
var
  lThrd : TthrdSQLMgrBrowse ;
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery.Create ;
  lData.SQL := GetSQL ;
  lThrd := TthrdSQLMgrBrowse.Create(true) ;
  lThrd.SQLMgrQuery := lData ;
  lThrd.Resume ;
end;

function TFormAdHocQuery.GetSQL : string ;
begin
  if FMemoSQL.SelText <> '' then
    result := FMemoSQL.SelText
  else
    result := FMemoSQL.Lines.Text ;
  result := Trim( result ) ;
end ;

procedure TFormAdHocQuery.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

procedure TFormAdHocQuery.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree ;
end;

procedure TFormAdHocQuery.DoMemoOnChange(sender: TObject);
begin
  if not FbDirty then
    FbDirty := true ;
end;

procedure TFormAdHocQuery.Save;
begin
  FMemoSQL.Lines.SaveToFile( FsFileName ) ;
  Caption := crsFormCaption + ' - ' + FsFileName ;
  FbDirty := false ;
end;

procedure TFormAdHocQuery.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  lAction : word ;
begin
  if not FbDirty then
  begin
    CanClose := true ;
    Exit ; //==>
  end ;

  lAction := tiYesNoCancel( crsCloseQuery ) ;

  case lAction of
    mrYes    : begin
                 aFileSaveExecute( nil ) ;
                 CanClose := true ;
               end ;
    mrNo     : CanClose := true ;
    mrCancel : CanClose := false ;
  end ;

end;

procedure TFormAdHocQuery.aRunScriptExecute(Sender: TObject);
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

procedure TFormAdHocQuery.RunAsScript( const psAppName, psParams : string ) ;
var
  lsFileName : string ;
  lsParams   : string ;
begin
  lsFileName := tiGetTempFile( 'SQL' ) ;
  tiStringToFile( GetSQL, lsFileName ) ;
  lsParams := tiStrTran( psParams, cgsFileMacro, lsFileName ) ;
  tiShellExecute( psAppName, lsParams ) ;
end ;


procedure TFormAdHocQuery.ActionList1OnUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  aFileSave.Enabled := FbDirty ;
  aRun.Enabled      := GetSQL <> '' ;
  aRunScript.Enabled := aRun.Enabled ;
end;

end.

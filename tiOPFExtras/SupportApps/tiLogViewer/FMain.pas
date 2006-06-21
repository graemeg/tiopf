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
    August 2000, Peter Hinrichsen, Created

  Purpose:
    A viewer for the log file created by the tiLog

  Classes:

  ToDo:
    1. Implement file history under the <Open> button, and in the file menu.
    2. Implement as a tray icon app
    3. Implement a 'Quick filter' which will show:
       i)  only events for a single thread
       ii) only a single type of event (eg error)

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, tiListView, tiListViewPlus, ImgList, ActnList, ToolWin, Menus, tiLog,
  StdCtrls, ExtCtrls{, tiTrayIcon} ;

resourcestring
  crsAppTitle      = 'TILog viewer - %s' ;
  crsStatusMessage = '%d logged events of %d showing.' ;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SB: TStatusBar;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    aExit: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ImageList1: TImageList;
    LV: TtiListViewPlus;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    aOpen: TAction;
    aFilter: TAction;
    aFind: TAction;
    Open1: TMenuItem;
    N2: TMenuItem;
    Filter1: TMenuItem;
    Find1: TMenuItem;
    Splitter1: TSplitter;
    pnlDetails: TPanel;
    mMessage: TMemo;
    lblDate: TLabel;
    lblThread: TLabel;
    lblSeverity: TLabel;
    ToolButton7: TToolButton;
    aRefresh: TAction;
    pmTrayIcon: TPopupMenu;
    View1: TMenuItem;
    Close1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aFilterExecute(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure View1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIconDoubleClick(Sender: TObject; Button: TMouseButton;
      var DefaultAction: Boolean);
    procedure FormShow(Sender: TObject);
    procedure LVAfterRefreshData(Sender: TtiCustomListView);
    procedure LVGetFont(Sender: TtiCustomListView; pCanvas: TCanvas;
      pItem: TListItem; pData: TPersistent);
    procedure LVItemArive(Sender: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
  private
    FData : TLogReadFromFile ;
    FsFileName : string ;
    FbCanClose : boolean ;
    procedure Restore;
  public
    procedure Read( const psFileName : string ) ;
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiUtils
  ;

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin
  pnlDetails.Align := alClient ;
  FData := TLogReadFromFile.Create ;
  gReg.ReadFormState( self ) ;
  LV.Height := gReg.ReadInteger( Name, 'LVHeight', Trunc( Height * 0.8 )) ;
  FbCanClose := false ;
  FsFileName := gReg.ReadString( Name, 'FileName', '' ) ;
  if FileExists( FsFileName ) then
    Read( FsFileName ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( self ) ;
  gReg.WriteInteger( Name, 'LVHeight', LV.Height ) ;
  FData.Free ;
  gReg.WriteString( Name, 'FileName', FsFileName ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.Read(const psFileName: string);
begin
  if not fileExists( psFileName ) then
  begin
    FsFileName := '' ;
    Caption := Format( crsAppTitle,
                       ['File not found'] ) ;
    Application.Title := Caption ;
    LV.Data        := nil ;
    LVItemArive( LV, nil, nil ) ;
    Exit ; //==>
  end ;

  FsFileName := psFileName ;
  Caption := Format( crsAppTitle,
                     [psFileName] ) ;
  Application.Title := Caption ;
  LV.Data        := nil ;
  FData.Read( psFileName ) ;
  LV.Data        := FData.List ;
  LV.Last ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aExitExecute(Sender: TObject);
begin
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aOpenExecute(Sender: TObject);
var
  lOpenDialog : TOpenDialog ;
begin
  lOpenDialog := TOpenDialog.Create( nil ) ;
  try
    lOpenDialog.FileName   := gReg.ReadString( name, 'FileName', '' ) ;
    lOpenDialog.DefaultExt := 'Log' ;
    lOpenDialog.Filter     := 'Log files|*.Log|All files|*.*' ;
    if lOpenDialog.Execute then
    begin
      Read( lOpenDialog.FileName ) ;
      gReg.WriteString( name, 'FileName', lOpenDialog.FileName ) ;
    end ;
  finally
    lOpenDialog.Free ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aFilterExecute(Sender: TObject);
begin
  LV.DoQuery ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aFindExecute(Sender: TObject);
begin
  LV.DoFind ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.aRefreshExecute(Sender: TObject);
begin
  Read( FsFileName ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aRefresh.Enabled := FsFileName <> '' ;
  aFilter.Enabled  := LV.Data <> nil ;
  aFind.Enabled    := LV.Data <> nil ;
end;
// -----------------------------------------------------------------------------
procedure TFormMain.View1Click(Sender: TObject);
begin
  Restore ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.Close1Click(Sender: TObject);
begin
  FbCanClose := true ;
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FbCanClose ;
//  TrayIcon.ShowApplication := false ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.TrayIconDoubleClick(Sender: TObject;
  Button: TMouseButton; var DefaultAction: Boolean);
begin
  Restore ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.Restore ;
begin
  aRefreshExecute( nil ) ;
//  TrayIcon.ShowApplication := true ;
end ;

// -----------------------------------------------------------------------------
procedure TFormMain.FormShow(Sender: TObject);
begin
  LV.SetFocus ;
end;

procedure TFormMain.LVAfterRefreshData(Sender: TtiCustomListView);
begin
  SB.Panels[0].Text :=
    Format( crsStatusMessage,
            [LV.Items.Count, LV.Data.Count ]) ;
end;

procedure TFormMain.LVGetFont(Sender: TtiCustomListView; pCanvas: TCanvas;
  pItem: TListItem; pData: TPersistent);
begin
  if TLogEvent( pData ).Severity = lsError then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack ;
end;

procedure TFormMain.LVItemArive(Sender: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TLogEvent ;
begin

  mMessage.Lines.Clear ;
  lblDate.Caption     := '' ;
  lblThread.Caption   := '' ;
  lblSeverity.Caption := '' ;

  if ( pData = nil ) or
     not ( pData  is TLogEvent ) then
    Exit ; //==>

  lData := TLogEvent( pData ) ;
  lblDate.Caption     := 'Date: '      + lData.DateTime ;
  lblThread.Caption   := 'Thread ID: ' + lData.ThreadID ;
  lblSeverity.Caption := 'Severity:  ' + lData.SeverityAsString ;
  mMessage.Lines.Add( lData.LogMessage );
  
end;

end.

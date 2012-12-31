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

unit tiAppLauncherMainImpl1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, tiAppLauncher_TLB, StdVcl, ImgList, ComCtrls, StdCtrls,
  ExtCtrls, tiDeployMgr_BOM, FDeployMgrChild_App ;

type

  TtiAppLauncherMain = class(TActiveForm, ItiAppLauncherMain)
    pnlReading: TPanel;
    lblText: TLabel;
    tmrLoad: TTimer;
    procedure tmrLoadTimer(Sender: TObject);
    procedure ActiveFormDestroy(Sender: TObject);
  private
    FsSoapServerURL : string ;
    FDatabaseName : string ;
    FtiDeployApps : TtiDeployApps ;
    FEvents: ItiAppLauncherMainEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function  GetText: String;
    procedure SetText(const Value: String);
//    procedure Launch( const psDatabaseName, psFileGroup, psCommandLineParams : string ) ;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_Cursor: Smallint; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(const Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(var Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function  Get_URL: WideString; safecall;
    procedure Set_URL(const Value: WideString); safecall;
    function  Get_DatabaseName: WideString; safecall;
    procedure Set_DatabaseName(const Value: WideString); safecall;
  public
    { Public declarations }
    procedure Initialize; override;
    property  Text : String read GetText write SetText ;
  end;

//  ToDo: If there is an error in this thread, the write the error text
//  to the ActiveX.

  TThrdDeployAppsRead = class( TThread )
  private
    FForm: TtiAppLauncherMain;
    FtiDeployApps: TtiDeployApps;
    FURL: string;
    FErrorMessage : String ;
    FDatabaseName: string;
    procedure DoOnTerminate( Sender : TObject ) ;
  public
    Constructor CreateExt ;
    procedure   Execute ; override ;
    property    Form : TtiAppLauncherMain read FForm write FForm ;
    property    DeployApps : TtiDeployApps read FtiDeployApps write FtiDeployApps ;
    property    URL : string read FURL write FURL ;
    property    DatabaseName : string read FDatabaseName write FDatabaseName ;

  end ;

const
  cgDefaultDatabaseName : string = 'OPDD' ;

implementation

uses
  tiOPFManager,
  tiConstants,
  tiQueryHTTP, // tiQueryHTTP must come BEFORE tiDBConnectionPool
  tiDBConnectionPool,
  tiCompressNone,
  tiCompressZLib
  ,FAppLaunch
  ,tiLog
  ,FDeployMgrChild_Launch
  ,cTIDeployMgr
  ,tiUtils
  ,tiGUIUtils
  ,tiExcept
  ,ComObj
  ,ComServ
  ;

{$R *.DFM}

{ TtiAppLauncherMain }

procedure TtiAppLauncherMain.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_tiAppLauncherMainPage); }
end;

procedure TtiAppLauncherMain.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ItiAppLauncherMainEvents;
end;

procedure TtiAppLauncherMain.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;

  // Calling SetupLogForClient will cause the activeX to stay loaded. This
  // will lock up IE and the only way out will be with task manager.
  // Why? Look into this later.
  SetupLogForClient ;

  pnlReading.Align := alClient ;
  pnlReading.Visible := true ;

  FsSoapServerURL := '' ;
  FDatabaseName   := '' ;

end;

function TtiAppLauncherMain.Get_Active: WordBool;
begin
  Result := Active;
end;

function TtiAppLauncherMain.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TtiAppLauncherMain.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TtiAppLauncherMain.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TtiAppLauncherMain.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TtiAppLauncherMain.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TtiAppLauncherMain.Get_Cursor: Smallint;
begin
  Result := Smallint(Cursor);
end;

function TtiAppLauncherMain.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TtiAppLauncherMain.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TtiAppLauncherMain.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TtiAppLauncherMain.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TtiAppLauncherMain.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TtiAppLauncherMain.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TtiAppLauncherMain.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TtiAppLauncherMain.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TtiAppLauncherMain.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TtiAppLauncherMain.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TtiAppLauncherMain.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TtiAppLauncherMain._Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TtiAppLauncherMain.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TtiAppLauncherMain.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TtiAppLauncherMain.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TtiAppLauncherMain.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TtiAppLauncherMain.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TtiAppLauncherMain.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TtiAppLauncherMain.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TtiAppLauncherMain.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TtiAppLauncherMain.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TtiAppLauncherMain.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TtiAppLauncherMain.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TtiAppLauncherMain.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TtiAppLauncherMain.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TtiAppLauncherMain.Set_Cursor(Value: Smallint);
begin
  Cursor := TCursor(Value);
end;

procedure TtiAppLauncherMain.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TtiAppLauncherMain.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TtiAppLauncherMain.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TtiAppLauncherMain.Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TtiAppLauncherMain.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TtiAppLauncherMain.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TtiAppLauncherMain.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TtiAppLauncherMain.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TtiAppLauncherMain.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TtiAppLauncherMain.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TtiAppLauncherMain.tmrLoadTimer(Sender: TObject);
var
  lThrd : TThrdDeployAppsRead ;
begin


  tmrLoad.Enabled := false ;

  if FsSoapServerURL = '' then
    raise Exception.Create( 'URL property not assigned in the HTML that calls ' +
                    ParamStr(0));

  if FDatabaseName = '' then
   raise Exception.Create( 'DatabaseName property not assigned in the HTML that calls ' +
                  ParamStr(0));

  FtiDeployApps := TtiDeployApps.Create ;

  lThrd              := TThrdDeployAppsRead.CreateExt ;
  lThrd.Form         := self ;
  lThrd.DeployApps   := FtiDeployApps ;
  lThrd.URL          := FsSoapServerURL ;
  lThrd.DatabaseName := FDatabaseName ;
  lThrd.Resume ;

end;

function TtiAppLauncherMain.Get_URL: WideString;
begin
  result := FsSoapServerURL ;
end;

procedure TtiAppLauncherMain.Set_URL(const Value: WideString);
begin
  FsSoapServerURL := Value ;
  tmrLoad.Enabled := true ;
end;

function TtiAppLauncherMain.GetText: String;
begin
  result := lblText.Caption ;
end;

procedure TtiAppLauncherMain.SetText(const Value: String);
begin
  lblText.Caption := Value ;
end;

{ TThrdDeployAppsRead }

constructor TThrdDeployAppsRead.CreateExt;
begin
  Create( true ) ;
  FreeOnTerminate := true ;
  OnTerminate := DoOnTerminate ;
  FErrorMessage := '' ;
end;

procedure TThrdDeployAppsRead.DoOnTerminate(Sender: TObject);
var
  lForm : TFormTIDeployChild_Launch ;
begin
  if FErrorMessage = '' then
  begin
    lForm := TFormTIDeployChild_Launch.Create( FForm ) ;
    lForm.DatabaseName := cgDefaultDatabaseName ;
    lForm.Parent := FForm ;
    lForm.BorderStyle := bsNone ;
    lForm.Align := alClient ;
    lForm.Data := FtiDeployApps ;
    lForm.Visible := true ;
  end
  else
    FForm.Text :=
      'Error in ' + ParamStr( 0 ) + Cr( 2 ) +
      'Message:' + Cr +
      FErrorMessage + Cr( 2 ) +
      'URL: ' + FURL + Cr +
      'DatabaseName: ' + FDatabaseName ;

end;

procedure TThrdDeployAppsRead.Execute;
begin
  try
    cgsTIQueryHTTPDefaultURL := FURL ;
    gTIOPFManager.LoadDatabaseLayer( FDatabaseName, 'null', 'null' ) ;
    gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadPK, FtiDeployApps ) ;
  except
    on e:exception do
      FErrorMessage := e.message ;
  end ;
end;

function TtiAppLauncherMain.Get_DatabaseName: WideString;
begin
  result := FDatabaseName ;
end;

procedure TtiAppLauncherMain.Set_DatabaseName(const Value: WideString);
begin
  FDatabaseName := Value ;
  tmrLoad.Enabled := true ;
end;

procedure TtiAppLauncherMain.ActiveFormDestroy(Sender: TObject);
begin
//  FreeDBConnectionPoolMgr ;
end;

initialization

  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TtiAppLauncherMain,
    Class_tiAppLauncherMain,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);

end.

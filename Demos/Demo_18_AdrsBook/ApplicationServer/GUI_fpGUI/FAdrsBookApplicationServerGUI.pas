unit FAdrsBookApplicationServerGUI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_memo,
  tiHTTPIndy, tiDBProxyServer, tiLog;

type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnStartStop: TfpgButton;
    MemoLog: TfpgMemo;
    {@VFD_HEAD_END: MainForm}
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAppServer: TtiDBProxyServer;
    procedure DoLog(ASender: TObject; const ALogEvent: TtiLogEvent);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;


implementation
uses
  tiLogToEventHandler,
//  tiUtils,
  tiOPFManager,
  {$IFDEF FPC}
  tiQuerySqldbIB,
  {$ELSE}
  tiQueryIBX,
  {$ENDIF}
  tiConstants;


procedure TMainForm.btnStartStopClick(Sender: TObject);
begin
  if FAppServer.Active then
  begin
    FAppServer.Stop;
    btnStartStop.Text := 'Start'
  end else
  begin
    FAppServer.Start;
    btnStartStop.Text := 'Stop';
  end;
end;

procedure TMainForm.DoLog(ASender: TObject; const ALogEvent: TtiLogEvent);
var
  sl: TStringlist;
  i: integer;
begin
  MemoLog.BeginUpdate;
  sl := TStringList.Create;
  { so line breaks are handled correctly. This could probably be improved in
    the memo component itself }
  sl.Text := FormatDateTime('yyyy-mm-dd HH:mm:ss  ', Now) + ALogEvent.LogMessage;
  for i := 0 to sl.Count-1 do
    MemoLog.Lines.Add(sl[i]);
  MemoLog.EndUpdate;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 551, 239);
  WindowTitle := 'tiOPF Demo 18 - GUI Application Server';
  Hint := '';

  btnStartStop := TfpgButton.Create(self);
  with btnStartStop do
  begin
    Name := 'btnStartStop';
    SetPosition(4, 4, 80, 24);
    Text := 'Start';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnStartStopClick;
  end;

  MemoLog := TfpgMemo.Create(self);
  with MemoLog do
  begin
    Name := 'MemoLog';
    SetPosition(4, 36, 542, 198);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GLog.RegisterLog(TtiLogToEventHandler.Create(@DoLog));
  FAppServer:= TtiDBProxyServer.Create(8088);
  FAppServer.ReadPageLocationAtStartup:= False;
  // ToDo: These are only accessable through the config object
  //       This change was made to tidy the app server, but it's broken this
  //       demo. Fix.
  //FAppServer.StaticPageLocation:= tiGetEXEPath + '\StaticPages\';
  //FAppServer.CGIBinLocation:= tiGetEXEPath + '\CGI-Bin\';
  GTIOPFManager.DefaultPersistenceLayerName := {$IFDEF FPC}cTIPersistSqldbIB{$ELSE}cTIPersistIBX{$ENDIF};
  GTIOPFManager.ConnectDatabase(
    'adrs', 'localhost:adrs.fdb', 'SYSDBA', 'masterkey', '', '');
  Log('Connected to database ' + LineEnding + GTIOPFManager.DefaultDBConnectionPool.DetailsAsString);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FAppServer.Free;
end;

end.

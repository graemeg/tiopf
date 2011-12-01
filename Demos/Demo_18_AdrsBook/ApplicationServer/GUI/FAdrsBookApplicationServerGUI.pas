unit FAdrsBookApplicationServerGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, tiHTTPIndy, tiDBProxyServer, tiLog;

type
  TForm1 = class(TForm)
    btnStartStop: TButton;
    MemoLog: TMemo;
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAppServer: TtiDBProxyServer;
    procedure DoLog(ASender: TObject; const ALogEvent: TtiLogEvent);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  tiLogToEventHandler,
  tiUtils,
  tiOPFManager,
  tiQueryIBX;

{$R *.dfm}

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if FAppServer.Active then
  begin
    FAppServer.Stop;
    btnStartStop.Caption:= '&Start'
  end else
  begin
    FAppServer.Start;
    btnStartStop.Caption:= '&Stop';
  end;
end;

procedure TForm1.DoLog(ASender: TObject; const ALogEvent: TtiLogEvent);
begin
  MemoLog.Lines.Add(FormatDateTime('yyyy-mm-dd HH:mm:ss  ', Now) + ALogEvent.LogMessage);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLog.RegisterLog(TtiLogToEventHandler.Create(DoLog));
  FAppServer:= TtiDBProxyServer.Create(8088);
  FAppServer.ReadPageLocationAtStartup:= False;
  // ToDo: These are only accessable through the config object
  //       This change was made to tidy the app server, but it's broken this
  //       demo. Fix.
  //FAppServer.StaticPageLocation:= tiGetEXEPath + '\StaticPages\';
  //FAppServer.CGIBinLocation:= tiGetEXEPath + '\CGI-Bin\';
  GTIOPFManager.ConnectDatabase(
    'adrs', 'adrs.fdb', 'SYSDBA', 'masterkey', '', '');
  Log('Connected to database ' + GTIOPFManager.DefaultDBConnectionPool.DetailsAsString);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAppServer.Free;
end;

end.

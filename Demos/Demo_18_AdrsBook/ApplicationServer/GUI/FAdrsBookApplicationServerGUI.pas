unit FAdrsBookApplicationServerGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, tiDBProxyServer, tiLog;

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
    btnStartStop.Caption:= '&Stop'
  end else
  begin
    FAppServer.Start;
    btnStartStop.Caption:= '&Start';
  end;
end;

procedure TForm1.DoLog(ASender: TObject; const ALogEvent: TtiLogEvent);
begin
  MemoLog.Lines.Add(ALogEvent.LogMessage);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLog.RegisterLog(TtiLogToEventHandler.Create(DoLog));
  FAppServer:= TtiDBProxyServer.Create(80);
  FAppServer.ReadPageLocationAtStartup:= False;
  FAppServer.StaticPageLocation:= tiGetEXEPath + '\StaticPages\';
  FAppServer.CGIBinLocation:= tiGetEXEPath + '\CGI-Bin\';

  GTIOPFManager.ConnectDatabase(
    'adrs', 'adrs.fdb', 'SYSDBA', 'masterkey', '', '');
  Log('Connected to database ' + GTIOPFManager.DefaultDBConnectionPool.DetailsAsString);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAppServer.Free;
end;

end.

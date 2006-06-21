program tiIBSQLMonitor;

uses
  Forms,
  FMainSQLMonitor in 'FMainSQLMonitor.pas' {frmIBSQLMonitor},
  TIBSQLEvent_BOM in 'TIBSQLEvent_BOM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Interbase SQL Monitor';
  Application.CreateForm(TfrmIBSQLMonitor, frmIBSQLMonitor);
  Application.Run;
end.

program RtfDemo;

uses
  Forms,
  RtfDemoMain in 'RtfDemoMain.pas' {FrmRtfDemoMain};

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TFrmRtfDemoMain, FrmRtfDemoMain);
  Application.Run;
end.


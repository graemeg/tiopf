program AdrsBookApplicationServerGUI;

{$I tiDefines.inc}

uses
  Forms,
  FAdrsBookApplicationServerGUI in 'FAdrsBookApplicationServerGUI.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  {$ifdef DELPHI2007ORABOVE}
  Application.MainFormOnTaskbar := True;
  {$endif}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

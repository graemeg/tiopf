program tiLogViewer;

uses
  Forms,
  SysUtils,
  FMain in 'FMain.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  if ( ParamStr( 1 ) <> '' ) and
     FileExists( ParamStr( 1 )) then
    FormMain.Read( ParamStr( 1 )) ;
  Application.Run;
end.

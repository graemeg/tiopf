program Demo_CreateTable;

uses
  Forms,
  FMainCreateTable in 'FMainCreateTable.pas' {FormMainCreateTable},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase};

{$R *.res}

begin
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>
  Application.CreateForm(TFormMainCreateTable, FormMainCreateTable);
  Application.Run;
end.

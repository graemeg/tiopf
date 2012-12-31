program tiFileSyncCML;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiFileSyncCML_BOM in 'tiFileSyncCML_BOM.pas';

var
  lFileSyncCML: TtiFileSyncCML;
begin
  lFileSyncCML := TtiFileSyncCML.Create;
  try
    lFileSyncCML.Execute;
  finally
    lFileSyncCML.Free;
  end;
end.

program FormatForBackwardCompatibility;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiUtils,
  DeleteDesignSizeFromDFMFiles_BOM in 'DeleteDesignSizeFromDFMFiles_BOM.pas',
  DeleteExplicitFromDFMFiles_BOM in 'DeleteExplicitFromDFMFiles_BOM.pas',
  DeleteHistoryListsInDofFiles_BOM in 'DeleteHistoryListsInDofFiles_BOM.pas',
  FormatForBackwardCompatibility_BOM in 'FormatForBackwardCompatibility_BOM.pas';

var
  LStartDir: string;
begin
  LStartDir:= ParamStr(1);
  TDeleteDesignSizeFromDFMFiles.Execute(LStartDir);
  TDeleteExplicitFromDFMFiles.Execute(LStartDir);
  TDeleteHistoryListsInDofFiles.Execute(LStartDir);
  {$ifdef debug}
  tiConsoleAppPause;
  {$endif}
end.

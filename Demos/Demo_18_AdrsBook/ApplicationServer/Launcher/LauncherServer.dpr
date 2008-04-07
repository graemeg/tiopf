program LauncherServer;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
  cFileSync,
  Classes,
  tiLog,
  tiLogToFile,
  tiUtils,
  tiCommandLineParams,
  tiFileSyncReader_DiskFiles;

var
  lCommand : string ;
  lData: string ;
  lFSR : TFileSyncReaderDiskFiles;
  ls : string ;
  lParams: TStringList;
begin
  GLog.RegisterLog(TtiLogToFile.CreateWithDateInFileName(1));
  try
  lParams:= TStringList.Create;
  try
    lParams.CommaText := gCommandLineParams.AsString ;
    lCommand:= lParams.Values[cHTTPParamNameCommand];
    lData:= lParams.Values[cHTTPParamNameData];
  finally
    lParams.Free;
  end;
  lFSR := TFileSyncReaderDiskFiles.Create;
  try
    LFSR.Root:= tiGetExePath + '\..\StaticPages';
    ls := lFSR.Execute(lCommand, lData );
    Write(ls);
  finally
    lFSR.Free;
  end;
  ExitCode := 0 ;
  except
    on e:exception do
    begin
      Write(e.message);
      ExitCode:= 1;
    end;
  end;
end.

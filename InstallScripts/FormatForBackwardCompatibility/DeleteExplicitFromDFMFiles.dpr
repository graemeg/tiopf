program DeleteDesignSizeFromDFMFiles;
{$APPTYPE CONSOLE}
uses
  SysUtils
  ,Classes
  ,tiUtils
  ,INIFiles
 ;

var
  lFiles   : TStringList;
  lFile    : TStringList;
  lFileName : string;
  lLine1 : string;
  i, j : integer;
  lFound : boolean;
begin
  try
    WriteLn('Reading *.dfm files');
    lFiles := TStringList.Create;
    try
      lFile := TStringList.Create;
      try
        tiFilesToStringList(ParamStr(1), '*.dfm', lFiles, true);
        WriteLn(IntToStr(lFiles.Count) + ' files to process');
        for i := 0 to lFiles.Count - 1 do
        begin
          lFound := false;
          lFileName := lFiles.Strings[i];
          Write(IntToStr(i+1) + '  ' + lFileName);
          lFile.LoadFromFile(lFileName);
          for j := lFile.Count - 1 downto 0 do
          begin
            lLine1 := lFile.Strings[j];
            if Pos('Explicit', lLine1) > 0 then
            begin
              lFile.Delete(j);
              Write('*');
              lFound := True;
            end;
          end;
          WriteLn;
          if lFound then
            lFile.SaveToFile(lFileName);
        end;
      finally
        lFile.Free;
      end;
    finally
      lFiles.Free;
    end;
  except
    on e:exception do
      WriteLn(e.Message);
  end;
  {$ifdef debug}
  tiConsoleAppPause;
  {$endif}
end.

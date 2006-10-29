program DeleteDesignSizeFromDFMFiles;
{$APPTYPE CONSOLE}
uses
  SysUtils
  ,Classes
  ,tiUtils
  ,tiDialogs
  ,INIFiles
 ;

var
  lFiles   : TStringList;
  lFile    : TStringList;
  lFileName : string;
  lLine1 : string;
  lSubStr1 : string;
  lLine2 : string;
  lSubStr2 : string;
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
        WriteLn(IntToStr(lFiles.Count-1) + ' files to process');
        for i := 0 to lFiles.Count - 1 do
        begin
          lFound := false;
          lFileName := lFiles.Strings[i];
          Write(IntToStr(i) + '  ' + lFileName);
          lFile.LoadFromFile(lFileName);
          for j := lFile.Count - 1 downto 0 do
          begin
            lLine1 := lFile.Strings[j];
            lSubStr1 := Copy(lLine1, Length(lLine1) - 13, 14);
            if (j+2 <= lFile.Count-1) then
              lLine2 := lFile.Strings[j+2]
            else
              Continue; //==>
            lSubStr2 := Copy(lLine2, Length(lLine2), 1);
            if (lSubStr1 = 'DesignSize = (') and
               (lSubStr2 = ')') then
            begin
              lFile.Delete(j+2);
              lFile.Delete(j+1);
              lFile.Delete(j);
              Write('*');
              lFound := true;
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
end.

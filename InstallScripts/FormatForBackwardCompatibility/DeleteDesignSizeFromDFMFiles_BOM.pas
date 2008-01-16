unit DeleteDesignSizeFromDFMFiles_BOM;

interface
uses
  tiBaseObject
  ,FormatForBackwardCompatibility_BOM
  ;

type

  TDeleteDesignSizeFromDFMFiles = class(TFormatForBackwardCompatibility)
  protected
    procedure ProcessOne(const AFileName: string); override;
    function  FileSpec: string; override;
  end;

implementation
uses
   tiUtils
  ,SysUtils
  ,Classes
  ;

{ TDeleteDesignSizeFromDFMFiles }

function TDeleteDesignSizeFromDFMFiles.FileSpec: string;
begin
  result:= '*.dfm';
end;

procedure TDeleteDesignSizeFromDFMFiles.ProcessOne(const AFileName: string);
var
  lFile    : TStringList;
  lLine1 : string;
  lSubStr1 : string;
  lLine2 : string;
  lSubStr2 : string;
  j : integer;
  lFound : boolean;
begin
  lFile := TStringList.Create;
  try
    lFound := false;
    lFile.LoadFromFile(AFileName);
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
    if lFound then
      lFile.SaveToFile(AFileName);
  finally
    lFile.Free;
  end;
end;

end.

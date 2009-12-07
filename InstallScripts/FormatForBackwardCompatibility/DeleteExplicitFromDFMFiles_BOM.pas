unit DeleteExplicitFromDFMFiles_BOM;

interface
uses
   tiBaseObject
  ,FormatForBackwardCompatibility_BOM
  ;

type

  TDeleteExplicitFromDFMFiles = class(TFormatForBackwardCompatibility)
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

{ TDeleteExplicitFromDFMFiles }

function TDeleteExplicitFromDFMFiles.FileSpec: string;
begin
  result:= '*.dfm';
end;

procedure TDeleteExplicitFromDFMFiles.ProcessOne(const AFileName: string);
var
  lFile    : TStringList;
  lLine1 : string;
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
      if Pos('Explicit', lLine1) > 0 then
      begin
        lFile.Delete(j);
        Write('*');
        lFound := True;
      end;
    end;
    if lFound then
      lFile.SaveToFile(AFileName);
  finally
    lFile.Free;
  end;
end;

end.

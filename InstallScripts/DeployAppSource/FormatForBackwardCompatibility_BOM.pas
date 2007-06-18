unit FormatForBackwardCompatibility_BOM;

interface
uses
   tiBaseObject
  ,Classes
  ;

type

  TFormatForBackwardCompatibility = class abstract(TtiBaseObject)
  private
    FFileList: TStringList;
    FLastPath: string;
    FRootDir: string;
  protected
    property  FileList: TStringList read FFileList;
    procedure DoExecute(const ARootDir: string);
    procedure LogFileName(const AFileName: string);

    // ToDo: Refactor Process one so it will find text to extract based on parameters
    procedure ProcessOne(const AFileName: string); virtual; abstract;
    function  FileSpec: string; virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    class procedure Execute(const ARootDir: string);
  end;

implementation
uses
   tiUtils
  ,SysUtils
  ;

{ TFormatForBackwardCompatibility }

constructor TFormatForBackwardCompatibility.Create;
begin
  inherited;
  FFileList:= TStringList.Create;
end;

destructor TFormatForBackwardCompatibility.Destroy;
begin
  FFileList.Free;
  inherited;
end;

procedure TFormatForBackwardCompatibility.DoExecute(
  const ARootDir: string);
var
  i: integer;
begin
  FRootDir:= ARootDir;
  try
    WriteLn('Executing ' + ClassName);
    WriteLn('Start directory: ' + ARootDir);
    WriteLn('Reading ' + FileSpec + ' files');
    tiFilesToStringList(FRootDir, FileSpec, FileList, true);
    WriteLn(IntToStr(FileList.Count-1) + ' files to process');
    for i := 0 to FileList.Count - 1 do
    begin
      LogFileName(FileList.Strings[i]);
      ProcessOne(FileList.Strings[i]);
    end;
  except
    on e:exception do
      WriteLn(e.Message);
  end;
  WriteLn;
end;

class procedure TFormatForBackwardCompatibility.Execute(const ARootDir: string);
var
  lo: TFormatForBackwardCompatibility;
begin
  lo:= Create;
  try
    lo.DoExecute(ARootDir);
  finally
    lo.Free;
  end;
end;

procedure TFormatForBackwardCompatibility.LogFileName(const AFileName: string);
var
  LFileName: string;
  LFilePath: string;
begin
  LFileName:= ExtractFileName(AFileName);
  LFilePath:= tiCIStrTran(ExtractFilePath(AFileName), FRootDir, '');
  if LFilePath <> FLastPath then
  begin
    WriteLn(LFilePath);
    FLastPath:= LFilePath;
  end;
  WriteLn('  ' + LFileName);
end;

end.

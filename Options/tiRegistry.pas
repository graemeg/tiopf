unit tiRegistry;

{$I tiDefines.inc}

interface
uses
   Classes
  ,IniFiles
  ,registry
 ;

type

{$IFDEF MSWINDOWS}
  // Registry manipulation - Registry key with the same name as the application
  TtiRegINIFile = class(TRegINIFile)
  public
    constructor CreateExt;
    function    ReadDate(     const ASection : string; AIndent : string; ADefault : TDateTime): TDateTime;
    procedure   WriteDate(    const ASection : string; AIndent : string; ADateTime : TDateTime);
    procedure   ReadStrings(  const ASection : string; AStrings : TStrings);
    procedure   WriteStrings( const ASection : string; AStrings : TStrings);
  end;

function GReg : TtiRegINIFile;

var
 DefaultRegistryCompany : String = '';//better hierarchy in registry key

{$ENDIF}

implementation
{$IFDEF MSWINDOWS}
uses
   tiUtils
  ,SysUtils
  ,tiConstants
 ;

var
  UReg : TtiRegINIFile;


function GReg : TtiRegINIFile;
begin
  if UReg = nil then
    UReg := TtiRegINIFile.CreateExt;
    result := UReg;
end;

function TtiRegINIFile.ReadDate(const ASection : string; AIndent : string; ADefault : TDateTime): TDateTime;
var
  LDate : string;
begin
  LDate := GReg.readString(ASection, AIndent, DateTimeToStr(ADefault));
  try
    result := StrToDateTime(LDate);
  except
    result := date;
  end;
end;

procedure TtiRegINIFile.WriteDate(const ASection : string; AIndent : string; ADateTime : TDateTime);
var
  LDate : string;
begin
  try
    LDate := formatDateTime(csWinDateTimeFormat, ADateTime);
  except
    LDate := formatDateTime(csWinDateTimeFormat, date);
  end;
  GReg.writeString(ASection, AIndent, LDate);
end;

procedure TtiRegINIFile.WriteStrings(const ASection : string; AStrings : TStrings);
var
  i: integer;
begin
  eraseSection(ASection);
  for i := 0 to AStrings.count - 1 do begin
    writeString(ASection, 'line' + intToStr(i), AStrings.strings[i]);
  end;
end;

procedure TtiRegINIFile.ReadStrings(const ASection : string; AStrings : TStrings);
var
  i: integer;
  LSectionValues: TStringList;
begin
  LSectionValues := TStringList.Create;
  AStrings.clear;
  try
    readSectionValues(ASection, LSectionValues);
    for i := 0 to LSectionValues.count - 1 do begin
      AStrings.add(readString(ASection, 'line' + intToStr(i), ''));
    end;
  finally
    LSectionValues.free;
  end;
end;

constructor TtiRegINIFile.CreateExt;
begin
  if DefaultRegistryCompany <> '' then
    Create('Software\' + DefaultRegistryCompany + '\' + tiApplicationName)
  else
    Create('Software\' + tiApplicationName);
end;

initialization
  UReg := nil;//to be sure
  DefaultRegistryCompany := '';

finalization
  UReg.free;

{$ENDIF}

end.









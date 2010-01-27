unit watchFile;

interface

uses
  Windows      // HKEY_CURRENT_USER
  ,Classes     // TStrings
  ;

const
  cWatchFileInspectionFrequencyMSec = 1000;

function ReadWatchFile(const AFileNameLineNumberStrings: TStrings): boolean;
function WriteWatchFile(const AFileName, ALineNumber: string): boolean;

implementation

uses
  Registry     // TRegistry
  ,IniFiles    // TIniFile
  ,SysUtils    // FileExists
  ;

const
  cRegRootKey = HKEY_CURRENT_USER;
  cRegPluginKey = 'SOFTWARE\DUnitTestInspector';
  cRegWatchFile = 'WatchFile';
  cWatchFileSection = 'FilesToOpen';

function GetWatchFilePath(out AWatchFilePath: string): boolean; forward;

function GetWatchFilePath(out AWatchFilePath: string): boolean;
var
  LRegistry: TRegistry;
begin
  LRegistry := TRegistry.Create;

  try
    LRegistry.RootKey := cRegRootKey;

    if LRegistry.OpenKeyReadOnly(cRegPluginKey) then
    begin
      AWatchFilePath := LRegistry.ReadString(cRegWatchFile);
      Result := true;
    end
    else
      Result := false;

  finally
    LRegistry.Free;
  end;

end;

function ReadWatchFile(const AFileNameLineNumberStrings: TStrings): boolean;
var
  LWatchFile: TIniFile;
  LWatchFilePath: string;
begin
  LWatchFile := nil;
  Result := false;

  try

    if GetWatchFilePath(LWatchFilePath) then
    begin
      LWatchFile := TIniFile.Create(LWatchFilePath);
      AFileNameLineNumberStrings.Clear;
      LWatchFile.ReadSectionValues(cWatchFileSection, AFileNameLineNumberStrings);

      if AFileNameLineNumberStrings.Count > 0 then
        LWatchFile.EraseSection(cWatchFileSection);

      Result := AFileNameLineNumberStrings.Count > 0;
    end;

  finally
    LWatchFile.Free;
  end;

end;

function WriteWatchFile(const AFileName, ALineNumber: string): boolean;
var
  LWatchFilePath: string;
  LWatchFile: TIniFile;

begin
  Result := false;

  if GetWatchFilePath(LWatchFilePath) then
  begin
    LWatchFile := TIniFile.Create(LWatchFilePath);

    try
      LWatchFile.WriteString(cWatchFileSection, AFileName, ALineNumber);
      Result := true;
    finally
      LWatchFile.Free;
    end;

  end;

end;

end.

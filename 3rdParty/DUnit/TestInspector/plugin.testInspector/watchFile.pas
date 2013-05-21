unit watchFile;

interface

//{$I jedi.inc}   // used solely for delphi version definitions
{$DEFINE DELPHIXE2}

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
{$IFDEF DELPHI7} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\Delphi7'; {$ENDIF}  // RINGN:Delphi 7 fix
{$IFDEF DELPHI2007} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\Delphi2007'; {$ENDIF}
{$IFDEF DELPHI2009} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\Delphi2009'; {$ENDIF}
{$IFDEF DELPHI2010} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\Delphi2010'; {$ENDIF}
{$IFDEF DELPHIXE} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\DelphiXE'; {$ENDIF}
{$IFDEF DELPHIXE2} cRegPluginKey = 'SOFTWARE\DUnitTestInspector\DelphiXE2'; {$ENDIF}
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

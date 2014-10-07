unit tiINI;

{$I tiDefines.inc}

interface

uses
   Classes
  ,IniFiles
 ;

type
  TtiINIFile = class;

  ItiINIFileLock = interface
  ['{C5245657-FAB7-4153-854B-A532CF01F190}']
  end;

  // Lock the INI file and auto-unlock on destroy (e.g. out of scope)
  TtiINIFileLock = class(TInterfacedObject, ItiINIFileLock)
  private
    FINIFile: TtiINIFile;
  public
    constructor Create(AINIFile: TtiINIFile);
    destructor Destroy; override;
  end;

  // INI file manipulation - INI file name the same as the application
  // or in the same directory with a file name you specify
  // If a value is not in the INI file and ReadXXX is called, the default
  // value will be written to the file unless ReadOnly is set to true
  TtiINIFile = class(TINIFile)
  private
    FReadOnly : Boolean;
    FAutoLock: boolean;
    function MutexName: string;
    function AcquireLock: ItiINIFileLock;
  public
    constructor CreateExt(const AFileName : string = ''; pReadOnly: Boolean = false);
    // System-wide lock to prevent one app/thread writing while another is reading
    // You can use AutoLock, or manually Lock/Unlock a block of operations that
    // should be performed together
    function Lock: boolean;
    procedure Unlock;

    function ReadString(const ASection, AIdent, ADefault: string): string; override;
    procedure WriteString(const ASection, AIdent, AValue: string); override;
    function ReadInteger(const ASection, AIdent: string; ADefault: Longint): Longint; override;
    function ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean; override;
    function ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function ReadFloat(const ASection, AName: string; ADefault: Double): Double; override;
    function ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    procedure ReadSection(const ASection: string; AStrings: TStrings); override;
    procedure ReadSections(AStrings: TStrings); overload; override;
    procedure ReadSectionValues(const ASection: string; AStrings: TStrings); override;
    procedure EraseSection(const ASection: string); override;
    procedure DeleteKey(const ASection, AIdent: String); override;
    procedure UpdateFile; override;

    // Lock on each read/write operation
    property AutoLock: boolean read FAutoLock write FAutoLock;
  end;

function gINI(const AFileName: string = ''): TtiINIFile;

implementation
uses
   tiUtils
  ,tiSyncObjs
  ,tiLog
  ,SysUtils
 ;

var
  uINI : TtiINIFile;


function gINI(const AFileName: string = ''): TtiINIFile;
begin
  if uINI = nil then
    uINI := TtiINIFile.CreateExt(AFileName);
  result := uINI;
end;

{ TtiINIFileLock }

constructor TtiINIFileLock.Create(AINIFile: TtiINIFile);
begin
  Assert(AINIFile <> nil, 'AINIFile must be assigned');
  inherited Create;

  FINIFile := AINIFile;
  if not FINIFile.Lock then
    raise EtiOPFLockTimedOut.CreateFmt('INI file lock timed out for file: %s',
        [FINIFile.FileName]);
end;

destructor TtiINIFileLock.Destroy;
begin
  FINIFile.Unlock;
end;

{ TtiINIFile }

constructor TtiINIFile.CreateExt(const AFileName : string = ''; pReadOnly: Boolean = false);
var
  lDir     : string;
  lFileName : string;
begin
  FReadOnly := pReadOnly;
  lDir := ExtractFilePath(AFileName);
  lFileName := ExtractFileName(AFileName);

  if lDir = '' then
    lDir := tiGetAppDataDirPrivate;
  lDir := tiAddTrailingSlash(lDir);
  { We used a non-Global config dir, so should be able to create the dir }
  tiForceDirectories(lDir);

  if lFileName = '' then
  begin
    lFileName := tiApplicationName;
    lFileName := tiAddTrailingValue(lFileName, '.', false) + 'ini';
  end
  else
  begin
    if tiExtractExtension(lFileName) = '' then
      lFileName := tiAddTrailingValue(tiRemoveExtension(lFileName), '.', false) + 'ini';
  end;

  lFileName := lDir + lFileName;
  Create(lFileName);
end;

function TtiINIFile.MutexName: string;
begin
  // Use the full and direct path to the ini file as the name but strip
  // backslashes as they have special meaning in mutxes
  //TODO: Better to generate the complete absolute path without any relative
  // portions. For Windows see:
  // http://stackoverflow.com/questions/5329472/conversion-between-absolute-and-relative-paths-in-delphi
  result := tiStrTran(ExpandFileName(FileName), '\', '');
end;

function TtiINIFile.AcquireLock: ItiINIFileLock;
begin
  if FAutoLock then
    result := TtiINIFileLock.Create(Self)
  else
    result := nil;
end;

function TtiINIFile.Lock: boolean;
begin
  //TODO: Resolve deadlock/long held mutex
  //result := tiWaitForMutex(MutexName);
  result := true;
end;

procedure TtiINIFile.Unlock;
begin
  //TODO: Resolve deadlock/long held mutex
  MutexName; // Stop compiler warning
  //tiReleaseMutex(MutexName);
end;

procedure TtiINIFile.DeleteKey(const ASection, AIdent: String);
begin
  AcquireLock;
  inherited;
end;

procedure TtiINIFile.EraseSection(const ASection: string);
begin
  AcquireLock;
  inherited;
end;

function TtiINIFile.ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean;
begin
  AcquireLock;
  if (not ValueExists(ASection, AIdent)) and
     (not FReadOnly) then
    WriteBool(ASection, AIdent, ADefault);
  // 0 = false, else any other number = true, else if match to TrueBoolStrs
  // then true else if match to FalseBoolStrs then false, else default.
  result := StrToBoolDef(inherited ReadString(ASection, AIdent, ''), ADefault);
end;

function TtiINIFile.ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  AcquireLock;
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteDate(ASection, AName, ADefault);
  result := inherited ReadDate(ASection, AName, ADefault);
end;

function TtiINIFile.ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  AcquireLock;
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteDateTime(ASection, AName, ADefault);
  result := inherited ReadDateTime(ASection, AName, ADefault);
end;

function TtiINIFile.ReadFloat(const ASection, AName: string; ADefault: Double): Double;
begin
  AcquireLock;
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteFloat(ASection, AName, ADefault);
  result := inherited ReadFloat(ASection, AName, ADefault);
end;

function TtiINIFile.ReadInteger(const ASection, AIdent: string; ADefault: Longint): Longint;
begin
  AcquireLock;
  if (not ValueExists(ASection, AIdent)) and
     (not FReadOnly) then
    WriteInteger(ASection, AIdent, ADefault);
  result := inherited ReadInteger(ASection, AIdent, ADefault);
end;

function TtiINIFile.ReadString(const ASection, AIdent, ADefault: string): string;
begin
  AcquireLock;
  result := inherited ReadString(ASection, AIdent, ADefault);
  if (not ValueExists(ASection, AIdent)) and
     (not FReadOnly) then
    WriteString(ASection, AIdent, ADefault);
end;

function TtiINIFile.ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  AcquireLock;
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteTime(ASection, AName, ADefault);
  result := inherited ReadTime(ASection, AName, ADefault);
end;

procedure TtiINIFile.WriteString(const ASection, AIdent, AValue: string);
begin
  AcquireLock;
  inherited;
end;

procedure TtiINIFile.ReadSection(const ASection: string; AStrings: TStrings);
begin
  AcquireLock;
  inherited;
end;

procedure TtiINIFile.ReadSections(AStrings: TStrings);
begin
  AcquireLock;
  inherited;
end;

procedure TtiINIFile.ReadSectionValues(const ASection: string;
  AStrings: TStrings);
begin
  AcquireLock;
  inherited;
end;

procedure TtiINIFile.UpdateFile;
begin
  AcquireLock;
  inherited;
end;

initialization
  uINI := nil;

finalization
  uINI.Free;

end.


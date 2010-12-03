unit tiLogToFile;

{$I tiDefines.inc}

interface
uses
  tiLog
  ,Classes
  ,SysUtils
 ;

type

  ELogToFile = class (Exception);

  {: Log to a file }
  TtiLogToFile = class(TtiLogToCacheAbs)
  private
    FFileName : TFileName;
    FOverwriteOldFile: boolean;
    FDateInFileName: boolean;
    FFileCreateAttemptInterval: integer;
    FFileCreateAttempts: integer;
    FMaxFileSize: Int64;
    function  GetDefaultFileName : TFileName;
    function  GetFileName : TFileName;
    procedure ForceLogDirectory;
    function GetLogFileStream(out AFileStream: TFileStream): boolean;
    procedure SetFileCreateAttemptInterval(const Value: integer);
    procedure SetFileCreateAttempts(const Value: integer);
  protected
    procedure WorkingListToOutput; override;
    procedure DeleteOldFileIfRequired;
  public
    // Require param to control max size of file
    constructor Create; override;
    constructor CreateOverwriteOld;
    constructor CreateWithFileName(const AFilePath: string; AFileName: string; AOverwriteOldFile: Boolean); overload;
    constructor CreateWithFileName(const AFilePath: string; AFileName: string; AOverwriteOldFile: Boolean; AMaxFileSize : Int64); Overload;
    constructor CreateWithDateInFileName(const APath: string); overload;
    constructor CreateWithDateInFileName(AUpDirectoryTree: Byte); overload;
    constructor CreateWithDateInFileName; overload;
    destructor  Destroy; override;
    procedure   Terminate; override;
    {: The maximum size (in bytes) of the log file. If this size is exceeded,
      the current log is renamed with a '~.log' extension and a new file created.}
    property    MaxFileSize : Int64 Read FMaxFileSize Write FMaxFileSize;
    property    FileName : TFileName read GetFileName;
    property    OverwriteOldFile : boolean read FOverwriteOldFile;
    property    DateInFileName : boolean read FDateInFileName;
    property    FileCreateAttempts: integer read FFileCreateAttempts write SetFileCreateAttempts;
    property    FileCreateAttemptInterval: integer read FFileCreateAttemptInterval write SetFileCreateAttemptInterval; // ms
  end;


implementation
uses
  tiUtils
  ,tiConstants
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$ifndef Delphi6OrAbove}
  ,FileCtrl
  {$endif}
 ;

const
  CDefaultFileCreateAttempts = 20;
  CDefaultFileCreateAttemptInterval = 250; //ms

constructor TtiLogToFile.Create;
begin
  inherited Create;
  FOverwriteOldFile := false;
  FDateInFileName    := false;
  FFileName          := GetDefaultFileName;
  FFileCreateAttempts := CDefaultFileCreateAttempts;
  FFileCreateAttemptInterval := CDefaultFileCreateAttemptInterval; //ms
  ThrdLog.Resume;
end;


constructor TtiLogToFile.CreateWithFileName(
  const AFilePath: string; AFileName: string; AOverwriteOldFile: Boolean);
var
  LFilePath: string;
  LFileName: string;
begin
  inherited Create;
  FDateInFileName    := False;
  MaxFileSize := 0;
  if AFilePath = '' then
    LFilePath:= ExtractFilePath(GetDefaultFileName)
  else
    LFilePath:= AFilePath;
  if AFileName = '' then
    LFileName:= ExtractFileName(GetDefaultFileName)
  else
    LFileName:= AFileName;
  FFileName:= ExpandFileName(tiAddTrailingSlash(LFilePath) + LFileName);
  FOverwriteOldFile :=  AOverwriteOldFile;
  DeleteOldFileIfRequired;
  FFileCreateAttempts := CDefaultFileCreateAttempts;
  FFileCreateAttemptInterval := CDefaultFileCreateAttemptInterval; //ms
  ThrdLog.Resume;
end;


constructor TtiLogToFile.CreateOverwriteOld;
begin
  Create;
  FOverwriteOldFile:= True;
  DeleteOldFileIfRequired;
end;

constructor TtiLogToFile.CreateWithDateInFileName;
begin
  CreateWithDateInFileName(ExtractFilePath(GetDefaultFileName));
end;

constructor TtiLogToFile.CreateWithDateInFileName(AUpDirectoryTree: Byte);
var
  LUp: string;
  LPath: string;
begin
  LUp:= tiReplicate('..' + PathDelim, AUpDirectoryTree);
  LPath:= tiAddTrailingSlash(tiGetEXEPath) + LUp + 'Log';
  LPath:= ExpandFileName(LPath);
  CreateWithDateInFileName(LPath);
end;

constructor TtiLogToFile.CreateWithDateInFileName(const APath: string);
begin
  inherited Create;
  FOverwriteOldFile := false;
  FDateInFileName    := True;
  FFileName          := ExpandFileName(tiAddTrailingSlash(APath) + ExtractFileName(GetDefaultFileName));
  FFileCreateAttempts := CDefaultFileCreateAttempts;
  FFileCreateAttemptInterval := CDefaultFileCreateAttemptInterval; //ms
  ThrdLog.Resume;
end;


procedure TtiLogToFile.DeleteOldFileIfRequired;
begin
  if FOverwriteOldFile and FileExists(FFileName) then
    SysUtils.DeleteFile(FFileName);
end;

destructor TtiLogToFile.Destroy;
begin
  Terminate;
  inherited;
end;


function TtiLogToFile.GetDefaultFileName: TFileName;
var
  {$IFDEF MSWINDOWS}
  path: array[0..MAX_PATH - 1] of char;
  {$ENDIF}
  lFileName: string;
  lFilePath: string;
begin
  {$IFDEF MSWINDOWS}
  if IsLibrary then
    SetString(lFileName, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
  {$ENDIF}
    lFileName := paramStr(0);
  lFilePath := tiAddTrailingSlash(ExtractFilePath(lFileName)) + 'Log';
  lFileName := ExtractFileName(lFileName);
  lFileName := ChangeFileExt(lFileName, '.Log');
  Result   := tiAddTrailingSlash(lFilePath) + lFileName;
end;


function TtiLogToFile.GetFileName : TFileName;
begin
  if not FDateInFileName then
    result := FFileName
  else
    result :=
      ChangeFileExt(FFileName, '') +
      '_' + FormatDateTime('YYYY-MM-DD', Date) +
      '.Log';
end;

procedure TtiLogToFile.Terminate;
begin
  ThrdLog.Priority := tpHighest;
  inherited Terminate;
  WriteToOutput;
end;

function TtiLogToFile.GetLogFileStream(out AFileStream: TFileStream): boolean;
var
  LFileCreateCount: Integer;
  LFileName: string;
  LFileOpenMode: word;
  LOldFileName : String;
begin
  Result := false;
  LFileName := GetFileName;
  Assert(LFileName<>'', 'FileName not assigned');

  if not DirectoryExists(ExtractFilePath(LFileName)) then
    ForceLogDirectory;

  AFileStream := nil;
  LFileCreateCount := 1;

  while (not Result) and (LFileCreateCount <= FFileCreateAttempts) do
  begin

    try
      LFileOpenMode := fmShareDenyWrite;

      if FileExists(LFileName) then
        LFileOpenMode := LFileOpenMode or fmOpenReadWrite
      else
        LFileOpenMode := LFileOpenMode or fmCreate;

      AFileStream := TFileStream.Create(LFileName, LFileOpenMode);
      If ((FMaxFileSize > 0) And (AFileStream.Size >= FMaxFileSize)) Then
      Begin
        AFileStream.Free;
        lOldFileName := ChangeFileExt(lFilename,'.~log');
        If FileExists(lOldFileName) Then
          DeleteFile(PAnsiChar(lOldFileName));
        RenameFile(lFileName, lOldFileName);
        AFileStream := TFileStream.Create(lFileName, lFileOpenMode);
      End;
      Result := true;
    except
      on EFOpenError do ;
      on EFCreateError do ;
      else
          raise;
    end;

    Inc(LFileCreateCount);
    Sleep(FFileCreateAttemptInterval);
  end;

end;

procedure TtiLogToFile.SetFileCreateAttemptInterval(const Value: integer);
begin
  FFileCreateAttemptInterval := Value;
end;

procedure TtiLogToFile.SetFileCreateAttempts(const Value: integer);
begin
  FFileCreateAttempts := Value;
end;

procedure TtiLogToFile.WorkingListToOutput;
var
  i: integer;
  LLine: string;
  LFileStream: TFileStream;
begin
  if GetLogFileStream(LFileStream) then
  begin
    try
      LFileStream.Seek(0, soFromEnd);
      for i := 0 to WorkingList.Count - 1 do
      begin
        Assert(WorkingList.Items[i].TestValid(TtiLogEvent), CTIErrorInvalidObject);
        LLine := WorkingList.Items[i].AsLeftPaddedString + cLineEnding;
        LFileStream.Write(PChar(LLine)^, Length(LLine));
      end;
    finally
      LFileStream.Free;
    end;
  end
  else
    raise ELogToFile.CreateFmt('Logging timeout trying to access "%s"', [GetFileName]);
end;

procedure TtiLogToFile.ForceLogDirectory;
var
  LLogPath: string;
begin
  LLogPath:= ExtractFilePath(FileName);
  if not DirectoryExists(LLogPath) then
    ForceDirectories(LLogPath);
  if not DirectoryExists(LLogPath) then
    raise Exception.CreateFmt(cErrorCanNotCreateLogDirectory, [LLogPath]);
end;

constructor TtiLogToFile.CreateWithFileName(const AFilePath: string;
  AFileName: string; AOverwriteOldFile: Boolean; AMaxFileSize: Int64);
var
  LFilePath: string;
  LFileName: string;
begin
  inherited Create;
  FDateInFileName    := False;
  MaxFileSize := AMaxFileSize;
  if AFilePath = '' then
    LFilePath:= ExtractFilePath(GetDefaultFileName)
  else
    LFilePath:= AFilePath;
  if AFileName = '' then
    LFileName:= ExtractFileName(GetDefaultFileName)
  else
    LFileName:= AFileName;
  FFileName:= ExpandFileName(tiAddTrailingSlash(LFilePath) + LFileName);
  FOverwriteOldFile :=  AOverwriteOldFile;
  DeleteOldFileIfRequired;
  FFileCreateAttempts := CDefaultFileCreateAttempts;
  FFileCreateAttemptInterval := CDefaultFileCreateAttemptInterval; //ms
  ThrdLog.Resume;
end;

end.

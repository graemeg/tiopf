unit tiLogToFile;

{$I tiDefines.inc}

interface
uses
  tiLog
  ,Classes
  ,SysUtils
 ;

type
  // Log to a file
  TtiLogToFile = class(TtiLogToCacheAbs)
  private
    FFileName : TFileName;
    FOverwriteOldFile: boolean;
    FDateInFileName: boolean;
    function  GetDefaultFileName : TFileName;
    function  GetFileName : TFileName;
    procedure ForceLogDirectory;
  protected
    procedure WriteToOutput; override;
  public
    // Require param to control max size of file
    constructor Create; override;
    constructor CreateWithFileName(const AFilePath: string; AFileName: string; AOverwriteOldFiles: Boolean);
    constructor CreateWithDateInFileName(const APath: string); overload;
    constructor CreateWithDateInFileName(AUpDirectoryTree: Byte); overload;
    constructor CreateWithDateInFileName; overload;
    destructor  Destroy; override;
    property    FileName : TFileName read GetFileName;
    property    OverwriteOldFile : boolean read FOverwriteOldFile;
    property    DateInFileName : boolean read FDateInFileName;
    procedure   Terminate; override;
  end;


implementation
uses
   tiObject
  ,tiUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$ifndef Delphi6OrAbove}
  ,FileCtrl
  {$endif}
 ;

constructor TtiLogToFile.Create;
begin
  inherited;
  FOverwriteOldFile := false;
  FDateInFileName    := false;
  FFileName          := GetDefaultFileName;
  ForceLogDirectory;
  ThrdLog.Resume;
end;


constructor TtiLogToFile.CreateWithFileName(
  const AFilePath: string; AFileName: string; AOverwriteOldFiles: Boolean);
var
  LFilePath: string;
  LFileName: string;
begin
  inherited Create;
  FDateInFileName    := False;
  if AFilePath = '' then
    LFilePath:= ExtractFilePath(GetDefaultFileName)
  else
    LFilePath:= AFilePath;
  if AFileName = '' then
    LFileName:= ExtractFileName(GetDefaultFileName)
  else
    LFileName:= AFileName;
  FFileName:= ExpandFileName(tiAddTrailingSlash(LFilePath) + LFileName);
  ForceLogDirectory;
  if AOverwriteOldFiles and FileExists(FFileName) then
    SysUtils.DeleteFile(FFileName);
  ThrdLog.Resume;
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
  ForceLogDirectory;
  ThrdLog.Resume;
end;


destructor TtiLogToFile.Destroy;
begin
  Terminate;
  inherited;
end;


function TtiLogToFile.GetDefaultFileName: TFileName;
var
  path: array[0..MAX_PATH - 1] of char;
  lFileName : string;
  lFilePath : string;
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
  inherited;
  WriteToOutput;
end;

procedure TtiLogToFile.WriteToOutput;
var
  i        : integer;
  lLine   : string;
  lFileStream : TFileStream;
  lFileName : TFileName;
  LFileCreateCount: Integer;
const
  CFileCreateAttempts = 2;
  CFileCreateAttemptInterval = 250; // ms
begin
  inherited WriteToOutput;
  if ListWorking.Count = 0 then
    Exit; //==>
  lFileName := GetFileName;
  Assert(lFileName<>'', 'FileName not assigned');
  if FileExists(lFileName) then
    lFileStream := TFileStream.Create(lFileName,
                                       fmOpenReadWrite or fmShareDenyNone)
  else
  begin
    lFileStream := nil;
    LFileCreateCount := 1;
    while LFileCreateCount <= CFileCreateAttempts do
    begin
      try
        lFileStream := TFileStream.Create(lFileName,
                                         fmCreate or fmShareDenyNone);
        Break; //==>
      except
        // Perhaps the log directory was deleted.
        on E: EFCreateError do
        begin
          if LFileCreateCount < CFileCreateAttempts then
            ForceLogDirectory
          else
            raise;
        end;
      end;
      Inc(LFileCreateCount);
      Sleep(CFileCreateAttemptInterval);
    end;
  end;

  if Assigned(lFileStream) then
  begin
    try
      lFileStream.Seek(0, soFromEnd);
      for i := 0 to ListWorking.Count - 1 do
      begin
        Assert(ListWorking.Items[i].TestValid(TtiLogEvent), cErrorTIPerObjAbsTestValid);
        lLine := ListWorking.Items[i].AsLeftPaddedString + #13 + #10;
        lFileStream.Write(PChar(lLine)^, Length(lLine));
      end;
    finally
      lFileStream.Free;
    end;
  end;
  
  ListWorking.Clear;
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

end.

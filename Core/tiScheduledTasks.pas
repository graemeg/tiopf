unit tiScheduledTasks;

{$i tiDefines.inc}

interface

uses
  Classes,
  SysUtils,
  DateUtils,
  tiBaseObject,
  tiObject,
  tiThread,
  tiUtils,
  tiINI,
  tiLog,
  SyncObjs;

type
  TtiScheduledFilePurgeDetails = class(TtiObject)
  private
    FDirectory: string;
    FFilenameWildCard: string;
    FFileDaysOld: Cardinal; 
    FRecurseDirectories: Boolean; 
  public
    procedure DeleteOldFiles;
    
    property Directory: string read FDirectory write FDirectory;
    property FilenameWildCard: string read FFilenameWildCard write FFilenameWildCard;
    property FileDaysOld: Cardinal read FFileDaysOld write FFileDaysOld;  
    property RecurseDirectories: Boolean read FRecurseDirectories write FRecurseDirectories;
  end;

  TtiScheduledFilePurgeDetailsList = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TtiScheduledFilePurgeDetails; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiScheduledFilePurgeDetails); reintroduce;
  public
    property    Items[i:integer] : TtiScheduledFilePurgeDetails read GetItems write SetItems;
    procedure   Add(AObject : TtiScheduledFilePurgeDetails); reintroduce;
    procedure DeleteOldFiles;
  end;

  TtiScheduledFilePurgeConfig = class(TtiObject)
  private
    FINIFile: TtiINIFile;
    FINIFileSectionName: string;
    FDetailsList: TtiScheduledFilePurgeDetailsList;
    FTimeToPurge: TDateTime;
  public
    constructor Create(const AINIFile: TtiINIFile;
        const AINIFileSectionName: string); reintroduce;
    destructor Destroy; override;
    procedure Read; override;
    property DetailsList: TtiScheduledFilePurgeDetailsList read FDetailsList;
    property TimeToPurge: TDateTime read FTimeToPurge write FTimeToPurge;
  end;

  TtiScheduledFilePurgeThread = class(TtiThread)
  private
    FConfig: TtiScheduledFilePurgeConfig;
    procedure GetNextScheduledRun(out AScheduledRunTime: TDateTime;
        out ATimeUntilRunInMS: Cardinal);
    procedure DeleteOldFiles;
  public
    constructor Create(const AConfig: TtiScheduledFilePurgeConfig); reintroduce;
    procedure Execute; override;
  end;

  TtiScheduledFilePurge = class(TtiBaseObject)
  private
    FConfig: TtiScheduledFilePurgeConfig;
    FTaskThread: TtiScheduledFilePurgeThread;
  public
    constructor Create(const AINIFile: TtiINIFile;
        const AINIFileSectionName: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  tiConstants;

const
  CINIIdentFilePurge_Time = 'FilePurgeTime';
  CINIDefaultFilePurge_Time = '4:00AM'; //default time not affected by daylight savings
  CINIIdentFilePurgeDetails_Directory = 'Directory';
  CINIDefaultFilePurgeDetails_Directory = '<Enter directory to purge>';
  CINIIdentFilePurgeDetails_WildCard = 'FileNameWildCard';
  CINIDefaultFilePurgeDetails_WildCard = '<Enter wildcard for file names to purge>';
  CINIIdentFilePurgeDetails_DaysOld = 'DaysToKeepFiles';
  CINIDefaultFilePurgeDetails_DaysOld = 20;
  CINIIdentFilePurgeDetails_Recurse = 'RecurseDirectories';
  CINIDefaultFilePurgeDetails_Recurse = false;
    
{ TtiScheduledFilePurgeThread }

constructor TtiScheduledFilePurgeThread.Create(
  const AConfig: TtiScheduledFilePurgeConfig);
begin
  Assert(AConfig.TestValid(TtiScheduledFilePurgeConfig), CTIErrorInvalidObject);
  inherited Create(true {suspended});
  FreeOnTerminate := false;
  FConfig := AConfig;
end;

procedure TtiScheduledFilePurgeThread.Execute;
var
  LScheduledRunTime: TDateTime;
  LSleepTime: Cardinal;
const
  CConfigRefreshIntevalMS = 300000; // 5 mins
begin
  while not Terminated do
  begin
    FConfig.Read; // Pick up any changes
    GetNextScheduledRun(LScheduledRunTime, LSleepTime);
    LSleepTime := Min(LSleepTime, CConfigRefreshIntevalMS);
    if SleepAndCheckTerminated(LSleepTime) and
       (Now >= LScheduledRunTime) then
      DeleteOldFiles;
  end;
end;

procedure TtiScheduledFilePurgeThread.GetNextScheduledRun(
  out AScheduledRunTime: TDateTime; out ATimeUntilRunInMS: Cardinal);
var
  LNow: TDateTime;
  LTimeOfNow: TDateTime;
  LTimeOfRunPurge: TDateTime;
begin
  LNow := Now;
  LTimeOfNow := TimeOf(LNow);
  LTimeOfRunPurge := TimeOf(FConfig.TimeToPurge);

  if LTimeOfNow < LTimeOfRunPurge then
    ATimeUntilRunInMS := MilliSecondsBetween(LTimeOfRunPurge, LTimeOfNow)
  else
    ATimeUntilRunInMS := MSecsPerDay - MilliSecondsBetween(LTimeOfNow, LTimeOfRunPurge);
  AScheduledRunTime := IncMilliSecond(LNow, ATimeUntilRunInMS);
end;

procedure TtiScheduledFilePurgeThread.DeleteOldFiles;
begin
  FConfig.DetailsList.DeleteOldFiles;
end;

{ TtiScheduledFilePurge }

constructor TtiScheduledFilePurge.Create(const AINIFile: TtiINIFile;
  const AINIFileSectionName: string);
begin
  Assert(AINIFile <> nil, 'AINIFile must be assigned');
  inherited Create;

  FConfig := TtiScheduledFilePurgeConfig.Create(AINIFile, AINIFileSectionName);
  FTaskThread := TtiScheduledFilePurgeThread.Create(FConfig);
  FTaskThread.Start;
end;

destructor TtiScheduledFilePurge.Destroy;
begin
  FTaskThread.WakeUpAndTerminate;
  FTaskThread.WaitFor;
  FreeAndNil(FTaskThread);
  FreeAndNil(FConfig);
  inherited;
end;

{ TtiScheduledFilePurgeDetails }

procedure TtiScheduledFilePurgeDetails.DeleteOldFiles;
const
  CDontDeleteEmptyDirectories = false;
begin
  tiDeleteOldFiles(FDirectory, FFilenameWildCard, FFileDaysOld,
      FRecurseDirectories, CDontDeleteEmptyDirectories);
end;

{ TtiScheduledFilePurgeDetailsList }

procedure TtiScheduledFilePurgeDetailsList.Add(
  AObject: TtiScheduledFilePurgeDetails);
begin
  inherited Add(AObject);
end;

procedure TtiScheduledFilePurgeDetailsList.DeleteOldFiles;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].DeleteOldFiles;
end;

function TtiScheduledFilePurgeDetailsList.GetItems(
  i: integer): TtiScheduledFilePurgeDetails;
begin
  Result := inherited GetItems(i) as TtiScheduledFilePurgeDetails;
end;

procedure TtiScheduledFilePurgeDetailsList.SetItems(i: integer;
  const AValue: TtiScheduledFilePurgeDetails);
begin
  inherited SetItems(i, AValue);
end;

{ TtiScheduledFilePurgeConfig }

constructor TtiScheduledFilePurgeConfig.Create(const AINIFile: TtiINIFile;
  const AINIFileSectionName: string);
begin
  Assert(AINIFile <> nil, 'AINIFile must be assigned');
  Assert(AINIFileSectionName <> '', 'INI section must be specified');
  inherited Create;

  FINIFile := AINIFile;
  FINIFileSectionName := AINIFileSectionName;

  FDetailsList := TtiScheduledFilePurgeDetailsList.Create;
  FTimeToPurge := 0;
end;

destructor TtiScheduledFilePurgeConfig.Destroy;
begin
  FDetailsList.Free;
  FINIFile.Free;
  inherited;
end;

procedure TtiScheduledFilePurgeConfig.Read;
var
  LSectionEnabled: Boolean;
  LKeyName: string;
  LFilePurgeDetails: TtiScheduledFilePurgeDetails;
  i: Integer;
  LINIFilePurgeSettingsList: TStringList;
begin
  FDetailsList.Clear;
  LINIFilePurgeSettingsList := nil;
  FINIFile.Lock;
  try
    LINIFilePurgeSettingsList := TStringList.Create;
    if FINIFile.SectionExists(FINIFileSectionName) then
    begin
      FTimeToPurge := FINIFile.ReadTime(FINIFileSectionName, CINIIdentFilePurge_Time, StrToTime(CINIDefaultFilePurge_Time));
      FINIFile.ReadSection(FINIFileSectionName, LINIFilePurgeSettingsList);
      for i := 0 to LINIFilePurgeSettingsList.Count - 1 do
      begin
        LKeyName := LINIFilePurgeSettingsList.Strings[i];
        if LKeyName <> CINIIdentFilePurge_Time then
        begin
          LSectionEnabled := FINIFile.ReadBool(FINIFileSectionName, LKeyName, true);
          if LSectionEnabled then
          begin
            LFilePurgeDetails := TtiScheduledFilePurgeDetails.Create;
            LFilePurgeDetails.Directory := FINIFile.ReadString(LKeyName, CINIIdentFilePurgeDetails_Directory, CINIDefaultFilePurgeDetails_Directory);
            LFilePurgeDetails.FilenameWildCard := FINIFile.ReadString(LKeyName, CINIIdentFilePurgeDetails_WildCard, CINIDefaultFilePurgeDetails_WildCard);
            LFilePurgeDetails.FileDaysOld := FINIFile.ReadInteger(LKeyName, CINIIdentFilePurgeDetails_DaysOld, CINIDefaultFilePurgeDetails_DaysOld);
            LFilePurgeDetails.RecurseDirectories := FINIFile.ReadBool(LKeyName, CINIIdentFilePurgeDetails_Recurse, CINIDefaultFilePurgeDetails_Recurse);
            FDetailsList.Add(LFilePurgeDetails);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(LINIFilePurgeSettingsList);
    FINIFile.Unlock;
  end;
end;

end.


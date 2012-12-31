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
  private
  protected
    function    GetItems(i: integer): TtiScheduledFilePurgeDetails; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiScheduledFilePurgeDetails); reintroduce;
    //function    GetParent: TMyParent; reintroduce;
  public
    property    Items[i:integer] : TtiScheduledFilePurgeDetails read GetItems write SetItems;
    procedure   Add(AObject : TtiScheduledFilePurgeDetails); reintroduce;
    //property    Parent: TMyParent read GetParent;
    procedure DeleteOldFiles;    
  published
  end;
  
  TtiScheduledFilePurgeThread = class(TtiThread)
  private
    FProcessEvent: TEvent;

    FDateTimeLastRun: TDateTime;
    FTimeToRunPurge: TDateTime;

    FScheduledFilePurgeDetailsList: TtiScheduledFilePurgeDetailsList;
    
    function TimeUntilNextScheduledRunInMS: Cardinal;
    procedure DeleteOldFiles;
  public
    constructor Create(
        const ATimeToRunPurge: TDateTime; 
        const AScheduledFilePurgeDetailsList: TtiScheduledFilePurgeDetailsList); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    procedure WakeUp;
  end;
  
  TtiScheduledFilePurge = class(TtiBaseObject)
  private
    FTaskThread: TtiScheduledFilePurgeThread;
    FScheduledFilePurgeDetailsList: TtiScheduledFilePurgeDetailsList;    
  public
    constructor Create(const AINIFile: TtiINIFile;
        const AINIFileSectionName: string); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

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
    
{ TtiScheduledTask }

constructor TtiScheduledFilePurgeThread.Create(const ATimeToRunPurge: TDateTime;
    const AScheduledFilePurgeDetailsList: TtiScheduledFilePurgeDetailsList);
begin
  inherited Create(true {suspended});
  FreeOnTerminate := false;

  FTimeToRunPurge   := TimeOf(ATimeToRunPurge);
  FScheduledFilePurgeDetailsList := AScheduledFilePurgeDetailsList;

  FProcessEvent := TEvent.Create(nil, True, False, '');
end;

destructor TtiScheduledFilePurgeThread.Destroy;
begin
  FreeAndNil(FProcessEvent);
  inherited;
end;

procedure TtiScheduledFilePurgeThread.Execute;
begin
  while not Terminated do
  begin
    case FProcessEvent.WaitFor(TimeUntilNextScheduledRunInMS) of
      wrSignaled: FProcessEvent.ResetEvent;
      wrTimeout: DeleteOldFiles;
        //wrAbandoned, wrError
    end;
  end;
end;

function TtiScheduledFilePurgeThread.TimeUntilNextScheduledRunInMS: Cardinal;
var
  LTimeOfNow: TDateTime;
  LTimeOfRunPurge: TDateTime;
begin
  LTimeOfNow := TimeOf(Now);
  LTimeOfRunPurge := TimeOf(FTimeToRunPurge);

  if LTimeOfNow < LTimeOfRunPurge then
    Result := MilliSecondsBetween(LTimeOfRunPurge, LTimeOfNow)
  else
    Result := MSecsPerDay - MilliSecondsBetween(LTimeOfNow, LTimeOfRunPurge);
end;

procedure TtiScheduledFilePurgeThread.WakeUp;
begin
  FProcessEvent.SetEvent;
end;

procedure TtiScheduledFilePurgeThread.DeleteOldFiles;
begin
  FScheduledFilePurgeDetailsList.DeleteOldFiles;
  FDateTimeLastRun := Now;
end;

{ TtiScheduledFilePurge }

constructor TtiScheduledFilePurge.Create(const AINIFile: TtiINIFile;
  const AINIFileSectionName: string);
var
  i: Integer;
  LINIFilePurgeSettingsList: TStringList;
  LFilePurgeDetails: TtiScheduledFilePurgeDetails;
  LTimeToPurge: TDateTime;
  LKeyName: string;
  LSectionEnabled: Boolean;
begin
  inherited Create;
  Assert(Assigned(AINIFile));
  FScheduledFilePurgeDetailsList := TtiScheduledFilePurgeDetailsList.Create;
  LINIFilePurgeSettingsList := TStringList.Create;
  try
    AINIFile.ReadSection(AINIFileSectionName, LINIFilePurgeSettingsList);

    LTimeToPurge := AINIFile.ReadTime(AINIFileSectionName, CINIIdentFilePurge_Time, StrToTime(CINIDefaultFilePurge_Time));

    for i := 0 to LINIFilePurgeSettingsList.Count - 1 do
    begin
      LKeyName := LINIFilePurgeSettingsList.Strings[i];

      if LKeyName <> CINIIdentFilePurge_Time then
      begin
        LSectionEnabled := AINIFile.ReadBool(AINIFileSectionName, LKeyName, true);

        if LSectionEnabled then
        begin
          LFilePurgeDetails := TtiScheduledFilePurgeDetails.Create;
    
          LFilePurgeDetails.Directory          := AINIFile.ReadString( LKeyName, CINIIdentFilePurgeDetails_Directory, CINIDefaultFilePurgeDetails_Directory);
          LFilePurgeDetails.FilenameWildCard   := AINIFile.ReadString( LKeyName, CINIIdentFilePurgeDetails_WildCard,  CINIDefaultFilePurgeDetails_WildCard);
          LFilePurgeDetails.FileDaysOld        := AINIFile.ReadInteger(LKeyName, CINIIdentFilePurgeDetails_DaysOld,   CINIDefaultFilePurgeDetails_DaysOld);
          LFilePurgeDetails.RecurseDirectories := AINIFile.ReadBool(   LKeyName, CINIIdentFilePurgeDetails_Recurse,   CINIDefaultFilePurgeDetails_Recurse);

          FScheduledFilePurgeDetailsList.Add(LFilePurgeDetails);
        end;
      end;
    end;
  finally
    FreeAndNil(LINIFilePurgeSettingsList);
  end;

  FTaskThread := TtiScheduledFilePurgeThread.Create(
      LTimeToPurge, FScheduledFilePurgeDetailsList);
  FTaskThread.Start;
end;

destructor TtiScheduledFilePurge.Destroy;
begin
  FTaskThread.Terminate;
  FTaskThread.WakeUp;
  FTaskThread.WaitFor;
  FreeAndNil(FTaskThread);
  FreeAndNil(FScheduledFilePurgeDetailsList);
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

end.

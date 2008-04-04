unit tiFileSync_Mgr;

interface
uses
   tiBaseObject
  ,tiFileName_BOM
  ,tiObject
  ,tiFileSyncReader_Abs
  ,tiFileSyncSetup_BOM
  ,Classes
  ,SyncObjs
  ,Forms
  ,SysUtils
  ,Contnrs
 ;

const
  cErrorCreatePathFailed = 'Unable to create path';
  cErrorDeletePathFailed = 'Unable to delete path';
  cErrorCopyFileFailed   = 'Unable to copy file';
  cErrorUpdateFileFailed = 'Unable to update file';
  cErrorDeleteFileFailed = 'Unable to delete file';
  cErrorCanNotGetExcluseUseOfFile = 'Can not not get exclusive use of the file "%s"';

  ctiFSNewFilesToBeCopied      = 'New file(s) to be copied';
  ctiFSExistingFilesToBeUpdated = 'Existing file(s) to be updated';
  cMessageWaitingForTerminate = 'Waiting for download to cancel...';
  
type
  TtiFileSyncAction = (fsaCopy, fsaUpdate, fsaDelete);
  TtiFileSyncActions = set of TtiFileSyncAction;

  TtiFileSyncMgrLogEvent = procedure (const psMessage : string; pAppendToPrevRow: Boolean) of object;
  TtiFileSyncMgrFileEvent = procedure (const psFilename: string;  pAction: TtiFileSyncAction) of object;

  TtiFileSyncTasks = class;
  TtiFileSyncTask = class;

  TtiFileSyncTasks = class(TtiObjectList)
  private
    function    GetItems(i: integer): TtiFileSyncTask; reintroduce;
    procedure   SetItems(i: integer; const Value: TtiFileSyncTask); reintroduce;
  public
    property    Items[i:integer] : TtiFileSyncTask read GetItems write SetItems;
    procedure   Add(pObject : TtiFileSyncTask  ; pDefDispOrdr : boolean = true); reintroduce;
    function    AddInstance(pFileName: TtiFileName; pTargetFileNames: TtiFileNames): TtiFileSyncTask;
  end;

  TtiFileSyncTask = class(TtiObject)
  private
    FSourceFileName: TtiFileName;
    FTargetFileNames: TtiFileNames;
    function GetTargetPathAndName: string;
    function GetDate: TDateTime;
    function GetPathAndName: string;
    function GetSize: Integer;
  protected
    function    GetOwner: TtiFileSyncTasks; reintroduce;
    procedure   SetOwner(const Value: TtiFileSyncTasks); reintroduce;
    property    TargetFileNames: TtiFileNames read FTargetFileNames write FTargetFileNames;
  public
    property    Owner       : TtiFileSyncTasks read GetOwner      write SetOwner;
    property    SourceFileName : TtiFileName read FSourceFileName write FSourceFileName;
    property    TargetPathAndName: string read GetTargetPathAndName;
  published
    property    PathAndName: string read GetPathAndName;
    property    Date: TDateTime read GetDate;
    property    Size: Integer read GetSize;
  end;

  TtiFileNameLockItem = class(TtiBaseObject)
  private
    FFileName: String;
    FHandle: THandle;
  public
    constructor Create(AFileName: String);
    destructor  Destroy; override;
    property    FileName: String Read FFileName;
    function    Lock: Boolean;
    procedure   UnLock;
  end;

  TtiFileNameLockList = class(TtiBaseObject)
  private
    FList: TObjectList;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Lock(AFileName: string): Boolean;
    procedure   UnLock(AFileName: String);
  end;

  TtiFileSyncMgr = class(TtiObject)
  private
    FFileNameFilters  : TtiFileNameFilters;

    FTargetPathNames  : TtiPathNames ;
    FSourcePathNames  : TtiPathNames ;
    FCopyPathNames    : TtiPathNames;
    FDeletePathNames  : TtiPathNames;

    FTargetFileNames : TtiFileNames;
    FSourceFileNames : TtiFileNames;

    FCopyFileNames   : TtiFileSyncTasks;
    FDeleteFileNames : TtiFileNames;
    FUpdateFileNames : TtiFileNames;

    FsTargetReader   : string;
    FsSourceReader   : string;
    FOnLog: TtiFileSyncMgrLogEvent;
    FOnProgressMajor: TtiFileSyncMgrProgressEvent;
    FOnProgressMinor: TtiFileSyncMgrProgressEvent;
    FOnFile: TtiFileSyncMgrFileEvent;
    FiFileCount : integer;
    FiFileNum : integer;
    FTerminated : boolean;
    FFileSyncActions: TtiFileSyncActions;
    FVerboseLogging: Boolean;
    FTargetReaderParams: string;
    FSourceReaderParams: string;
    FError: Boolean;
    FFileNameLockList: TtiFileNameLockList;

    procedure BuildCreatePathList(pData : TtiObject);
    procedure BuildDeletePathList(    pData : TtiObject);
    procedure BuildCopyUpdateFileList(pData : TtiObject);
    procedure BuildDeleteFileList(    pData : TtiObject);
    procedure ReadFileIndex(const pReaderType: string;
                             const pReaderParams: string;
                             const ptiFileNames: TtiFileNames;
                             const pSourceFileNameFilters : TtiFileNameFilters);
    procedure ReadFileIndexByFileName(const AReaderType: string;
                                      const AReaderParams: string;
                                      const ATargetFileNames: TtiFileNames;
                                      const ASourceFileNames: TtiFileNames);
    procedure ReadSingleFile(const AReader: TtiFileSyncReaderAbs;
                             const ATargetFileNames: TtiFileNames;
                             const AFileName: string);
    procedure ReadPathIndex(const psArchiveType: string;
                             const pReaderParams: string;
                             const ptiPathNames: TtiPathNames);
    //procedure CreatePaths; // CreatePaths will be unreliable when RootDir is relative
    procedure DeletePaths;
    procedure CopyFiles;
    procedure DeleteFiles;
    procedure UpdateFiles;
//    procedure CreatePath(const ptiPathName : TtiPathName; const psReader : string; const pReaderParams: string);
    procedure DeletePath(const ptiPathName : TtiPathName; const psReader : string; const pReaderParams: string);
    procedure WriteIndex(const ptiFileNames: TtiFileNames; const psReader: string; const pReaderParams: string);
    procedure ReadFileData(const ptiFileName: TtiFileName; const psReader: string; const pReaderParams: string);
    procedure WriteFileData(const ptiFileName: TtiFileName; const psReader: string; const pReaderParams: string);
    procedure DeleteFileData(const ptiFileName: TtiFileName; const psReader: string; const pReaderParams: string);
    procedure Log(const psMessage : string; pAppendToPrevRow: Boolean = false);
    procedure UpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer); overload;
    procedure UpdateProgressMajor(AFileName: TtiFileName); overload;
    procedure UpdateFile(const pFilename: string; pAction: TtiFileSyncAction);
    procedure DoOnCheckTerminated(var ATerminated: Boolean);
    procedure IncProgress;
    function  LockAllLocalFiles: Boolean;

    // Not sure what to do with this, under construction
    property    FileNameFilters : TtiFileNameFilters read FFileNameFilters;
  protected
    procedure Clear;
    procedure ReadFileLists;
    procedure BuildCopyLists; virtual;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   ReadIndexes; virtual;
    procedure   Execute;
    property    OnLog : TtiFileSyncMgrLogEvent read FOnLog write FOnLog;
    property    OnProgressMajor : TtiFileSyncMgrProgressEvent read FOnProgressMajor write FOnProgressMajor;
    property    OnProgressMinor : TtiFileSyncMgrProgressEvent read FOnProgressMinor write FOnProgressMinor;
    property    OnFile: TtiFileSyncMgrFileEvent read FOnFile write FOnFile;
    procedure   Terminate;

    // Assign values to these before running
    procedure   AssignFromFileSyncDir(const pData: TtiFileSyncDir);
    property    SourceFileNames   : TtiFileNames       read FSourceFileNames;
    property    TargetFileNames   : TtiFileNames       read FTargetFileNames;
    property    SourceReader      : string             read FsSourceReader write FsSourceReader;
    property    SourceReaderParams: string             read FSourceReaderParams Write FSourceReaderParams;
    property    TargetReader      : string             read FsTargetReader write FsTargetReader;
    property    TargetReaderParams: string             read FTargetReaderParams Write FTargetReaderParams;
    property    FileSyncActions   : TtiFileSyncActions read FFileSyncActions Write FFileSyncActions;
    property    VerboseLogging    : Boolean            read FVerboseLogging write FVerboseLogging;

    // Can read these for information about which files are to be copied, updated, deleted
    property    CopyFileNames   : TtiFileSyncTasks read FCopyFileNames;
    property    UpdateFileNames : TtiFileNames read FUpdateFileNames;
    property    DeleteFileNames : TtiFileNames read FDeleteFileNames;
    property    Error: Boolean read FError;

  end;

  TthrdTIFileSyncOneWayCopy = class(TThread)
  private
    FFSM : TtiFileSyncMgr;
    FOnLog: TtiFileSyncMgrLogEvent;
    FOnProgressMajor: TtiFileSyncMgrProgressEvent;
    FOnProgressMinor: TtiFileSyncMgrProgressEvent;
    FOnFile: TtiFileSyncMgrFileEvent;
    FMessage: string;
    FAppendToPrevRow: Boolean;
    FMax: Integer;
    FPos: Integer;
    FFileName: TtiFileName;
    FLastFile: string;
    FLastFileAction: TtiFileSyncAction;
    FFormToShow: TForm;
    FOnTerminateWithError: TNotifyEvent;
    procedure DoLog(const pMessage : string; pAppendToPrevRow: boolean);
    procedure DoUpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer);
    procedure DoUpdateProgressMinor(AFileName: TtiFileName; AMax, APos : integer);
    procedure DoFile(const pFileName: string; pAction: TtiFileSyncAction);
    procedure DoFileSynchronize;
    procedure DoLogSynchronize;
    procedure DoUpdateProgressMajorSynchronize;
    procedure DoUpdateProgressMinorSynchronize;
  protected
  public
    constructor Create(const pSourceReader: string;
                        const pSourceReaderParams: string;
                        const pSourceStartDir: string;
                        const pTargetReader: string;
                        const pTargetReaderParams: string;
                        const pTargetStartDir: string;
                        const pOnLog: TtiFileSyncMgrLogEvent;
                        const AOnProgressMajor: TtiFileSyncMgrProgressEvent;
                        const AOnProgressMinor: TtiFileSyncMgrProgressEvent;
                        const pOnFile: TtiFileSyncMgrFileEvent;
                        const pOnTerminate: TNotifyEvent;
                        const pOnTerminateWithError: TNotifyEvent;
                        const pFormToShow: TForm);

    destructor  Destroy; override;
    procedure   Execute; override;
    procedure   Terminate; reintroduce;
    property    OnLog : TtiFileSyncMgrLogEvent read FOnLog write FOnLog;
    property    OnProgressMajor : TtiFileSyncMgrProgressEvent read FOnProgressMajor write FOnProgressMajor;
    property    OnFile: TtiFileSyncMgrFileEvent read FOnFile write FOnFile;
  end;

  TthrdFileSyncMgr = class(TThread)
  private
    FFormMain: TForm;
    FFileSyncSetup: TtiFileSyncSetup;
    FFileSyncMgr : TtiFileSyncMgr;
    FCloseAppOnTerminate: boolean;
  protected
    procedure HideFormMain;
    procedure ShowFormMain;
  public
    constructor Create(Suspended : Boolean);
    destructor  Destroy; override;
    constructor CreateExt;
    procedure   Execute; override;
    property    FormMain : TForm read FFormMain write FFormMain;
    property    FileSyncSetups : TtiFileSyncSetup read FFileSyncSetup;
    property    CloseAppOnTerminate : boolean read FCloseAppOnTerminate write FCloseAppOnTerminate;
    procedure   CustomTerminate;
  end;


implementation
uses
  cFileSync
  ,tiUtils
  ,tiGUIUtils
  ,tiLog
  ,tiExcept
  ,tiConstants
  ,tiDialogs // Debugging
  ,Windows
 ;

{ TtiFileSyncMgrMgr }

procedure TtiFileSyncMgr.BuildCopyUpdateFileList(pData: TtiObject);
var
  lData : TtiFileName;
begin
  lData := FTargetFileNames.FindLikeRootRemoved(pData as TtiFileName);

  // Copy
  if (lData = nil) then
    FCopyFileNames.AddInstance(pData as TtiFileName,
                                FTargetFileNames)

  // Update
  else if (lData <> nil) and
          ((TtiFileName(pData).Size <> lData.Size) or
           (TtiFileName(pData).CRC <> lData.CRC)) then
    FUpdateFileNames.Add(pData)

end;

procedure TtiFileSyncMgr.BuildDeleteFileList(pData: TtiObject);
var
  lData : TtiFileName;
begin
  lData := FSourceFileNames.FindLikeRootRemoved(TtiFileName(pData));
  if lData = nil then
    FDeleteFileNames.Add(pData);
end;

constructor TtiFileSyncMgr.Create;
begin
  inherited;
  FTargetPathNames  := TtiPathNames.Create ;
  FSourcePathNames  := TtiPathNames.Create ;

  FCopyPathNames   := TtiPathNames.Create;
  FCopyPathNames.OwnsObjects := false;
  FCopyPathNames.AutoSetItemOwner := false;

  FDeletePathNames := TtiPathNames.Create;
  FDeletePathNames.OwnsObjects := false;
  FDeletePathNames.AutoSetItemOwner := false;

  FTargetFileNames := TtiFileNames.Create;
  FSourceFileNames := TtiFileNames.Create;

  FCopyFileNames   := TtiFileSyncTasks.Create;

  FDeleteFileNames := TtiFileNames.Create;
  FDeleteFileNames.OwnsObjects := false;
  FDeleteFileNames.AutoSetItemOwner := false;

  FUpdateFileNames := TtiFileNames.Create;
  FUpdateFileNames.OwnsObjects := false;
  FUpdateFileNames.AutoSetItemOwner := false;

  FFileNameFilters  := TtiFileNameFilters.Create;

  FFileNameLockList:= TtiFileNameLockList.Create;

  FFileSyncActions := [fsaCopy, fsaUpdate, fsaDelete];
  FVerboseLogging := True;
  FError:= False;
  FTerminated := false;
end;

destructor TtiFileSyncMgr.Destroy;
begin
  FFileNameLockList.Free;

  FTargetPathNames.Free ;
  FSourcePathNames.Free ;
  FCopyPathNames.Free   ;
  FDeletePathNames.Free ;

  FTargetFileNames.Free;
  FSourceFileNames.Free;
  FCopyFileNames.Free  ;
  FDeleteFileNames.Free;
  FUpdateFileNames.Free;

  FFileNameFilters.Free;

  inherited;
end;

procedure TtiFileSyncMgr.Clear;
begin
  FCopyPathNames.Clear;
  FDeletePathNames.Clear;
  FUpdateFileNames.Clear;
  FCopyFileNames.Clear;
  FDeleteFileNames.Clear;
  FError:= False;
end;

procedure TtiFileSyncMgr.ReadFileLists;
begin

  // Source directories
  if FVerboseLogging then
    Log('Reading list of source directories');

  if fsaDelete in FFileSyncActions then
  begin
    FSourcePathNames.StartDir := FSourceFileNames.StartDir;
    ReadPathIndex(SourceReader, FSourceReaderParams, FSourcePathNames);
    if FVerboseLogging then
      Log('  Directories in source: ' + IntToStr(FSourcePathNames.Count));

    // Target directories
    if FTerminated then Exit; //==>
    if FVerboseLogging then
      Log('Reading list of target directories');
    FTargetPathNames.StartDir := FTargetFileNames.StartDir;
    ReadPathIndex(TargetReader, FTargetReaderParams, FTargetPathNames);
    if FVerboseLogging then
      Log('  Directories in target: ' + IntToStr(FTargetPathNames.Count));
  end;

  // Source files
  if FTerminated then Exit; //==>
  Log('Reading list of new files...');
  ReadFileIndex(SourceReader, SourceReaderParams, FSourceFileNames, FFileNameFilters);
  if FVerboseLogging then
    Log('  Files in source: ' + IntToStr(FSourceFileNames.Count));

  // Target files
  if FTerminated then Exit; //==>
  Log('Reading list of existing files...');

  // We only need a full list if we want to delete some files
  if fsaDelete in FFileSyncActions then
    ReadFileIndex(TargetReader, TargetReaderParams, FTargetFileNames, nil)
  else
    ReadFileIndexByFileName(TargetReader, TargetReaderParams, FTargetFileNames, FSourceFileNames);

  if FVerboseLogging then
    Log('  Files in target: ' + IntToStr(FTargetFileNames.Count));

end;

procedure TtiFileSyncMgr.ReadIndexes;
begin

  if FTerminated then Exit; //==>
  Clear;

  if FTerminated then Exit; //==>
  ReadFileLists;

  if FTerminated then Exit; //==>
  BuildCopyLists;

  if FTerminated then Exit; //==>
  if fsaCopy in FileSyncActions then
    Inc(FiFileCount, FCopyPathNames.Count);
  if fsaDelete in FileSyncActions then
    Inc(FiFileCount, FDeletePathNames.Count);
  if fsaUpdate in FileSyncActions then
    Inc(FiFileCount, FUpdateFileNames.Count);
  if fsaCopy in FileSyncActions then
    Inc(FiFileCount, FCopyFileNames.Count);
  if fsaDelete in FileSyncActions then
    Inc(FiFileCount, FDeleteFileNames.Count);

  Inc(FiFileCount, 1);
  UpdateProgressMajor(nil, FiFileCount, 1);
  FiFileNum := 1;

end;

procedure TtiFileSyncMgr.BuildCopyLists;
begin
  // Directories to create
  if FVerboseLogging then
    Log('Building list of directories to create');
  FSourcePathNames.ForEach(BuildCreatePathList  );
  if FVerboseLogging then
    Log('  Directories to be created: ' + IntToStr(FCopyPathNames.Count));

  // Directories to delete
  if FTerminated then Exit; //==>
  if FVerboseLogging then
    Log('Building list of directories to delete');
  FTargetPathNames.ForEach(BuildDeletePathList);
  if FVerboseLogging then
    Log('  Directories to be deleted: ' + IntToStr(FDeletePathNames.Count));

  // Files to create or update
  if FTerminated then Exit; //==>
  if FVerboseLogging then
    Log('Building list of files to create or update');
  FSourceFileNames.ForEach(BuildCopyUpdateFileList  );
  if FCopyFileNames.Count <> 0 then
    Log(IntToStr(FCopyFileNames.Count) + ' ' + ctiFSNewFilesToBeCopied);
  if FUpdateFileNames.Count <> 0 then
    Log(IntToStr(FUpdateFileNames.Count) + ' ' + ctiFSExistingFilesToBeUpdated);

  // Files to delete
  if FTerminated then Exit; //==>
  if FVerboseLogging then
    Log('Building list of files to delete');
  FTargetFileNames.ForEach(BuildDeleteFileList);
  if FVerboseLogging then
    Log(IntToStr(FDeleteFileNames.Count) + ' Files to be deleted');
  Log('');
end;

procedure TtiFileSyncMgr.ReadFileIndex(const pReaderType:string;
                                        const pReaderParams: string;
                                        const ptiFileNames : TtiFileNames;
                                        const pSourceFileNameFilters : TtiFileNameFilters);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(pReaderType, pReaderParams);
  try
    lReader.ReadFileIndex(ptiFileNames, pSourceFileNameFilters);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.ReadFileIndexByFileName(
  const AReaderType: string;
  const AReaderParams: string;
  const ATargetFileNames: TtiFileNames;
  const ASourceFileNames: TtiFileNames);
var
  lReader : TtiFileSyncReaderAbs;
  i: Integer;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(AReaderType, AReaderParams);
  try
    for i:= 0 to ASourceFileNames.Count-1 do
      ReadSingleFile(lReader, ATargetFileNames, ASourceFileNames.Items[i].NameWithNewRootDir(ATargetFileNames.StartDir));
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.ReadSingleFile(
  const AReader: TtiFileSyncReaderAbs;
  const ATargetFileNames: TtiFileNames;
  const AFileName: string);
var
  LFilters: TtiFileNameFilters;
  LFiles: TtiFileNames;
  i: Integer;
begin
  Assert(AReader.TestValid, CTIErrorInvalidObject);
  Assert(ATargetFileNames.TestValid, CTIErrorInvalidObject);
  Assert(AFileName<>'', 'AStartDir not assigned');
  LFiles:= TtiFileNames.Create;
  try
    LFiles.StartDir:= ExtractFilePath(AFileName);
    LFilters:= TtiFileNameFilters.Create;
    try
      LFilters.AddFilter(fftInclude, ExtractFileName(AFileName));
      AReader.ReadFileIndex(LFiles, LFilters);
      for i:= 0 to LFiles.Count - 1 do
        ATargetFileNames.Add(LFiles.Items[i].Clone);
    finally
      LFilters.Free;
    end;
  finally
    LFiles.Free;
  end;
end;

procedure TtiFileSyncMgr.Execute;
begin
  if FTerminated then Exit; //==>
  ReadIndexes;

  if FTerminated then Exit; //==>

  // CreatePaths will fail if RootDir is relative. Fix
//  if fsaCopy in FFileSyncActions then
//    CreatePaths;
  if not LockAllLocalFiles then
    Exit ; //==>

  if FTerminated then Exit; //==>
  if fsaCopy in FFileSyncActions then
    CopyFiles;

  if FTerminated then Exit; //==>
  if fsaUpdate in FFileSyncActions then
    UpdateFiles;

  if FTerminated then Exit; //==>
  if fsaDelete in FFileSyncActions then
    DeleteFiles;

  if FTerminated then Exit; //==>
  if fsaDelete in FFileSyncActions then
    DeletePaths;

  if FTerminated then Exit; //==>
  WriteIndex(FSourceFileNames, FsTargetReader, FTargetReaderParams); // Not sure what to do about the list of files to pass here
end;

procedure TtiFileSyncMgr.CopyFiles;
var
  lSource : TtiFileName;
  lTarget : TtiFileName;
  i : integer;
  lPathName: string;
begin
  for i := 0 to CopyFileNames.Count - 1 do
  begin
    if FTerminated then Exit; //==>
    try
      lSource := CopyFileNames.Items[i].SourceFileName;
      UpdateProgressMajor(lSource);
      lPathName := lSource.NameFull;
      Log(Format('Copying %s', [lSource.NameFull]));
      ReadFileData(lSource, FsSourceReader, FSourceReaderParams);
      if not FTerminated then
      begin
        lTarget := lSource.CloneWithNewRootDir(FTargetFileNames.StartDir);
        try
          lPathName := lTarget.PathAndName;
          FFileNameLockList.UnLock(LTarget.PathAndName);
          WriteFileData(lTarget, FsTargetReader, FTargetReaderParams);
          Log('Done.', true);
          if not FileExists(lTarget.PathAndName) then
            LogError('Error - file not written correctly <' + lTarget.PathAndName + '>');
          UpdateFile(lTarget.PathAndName, fsaCopy);
        finally
          lTarget.Free;
        end;
      end;
    except
      on e:exception do
      begin
        Log('Error copying file: ' + e.message + #13#10 +
          'Please close all active applications and try again. ' +
          'If this does not work, please re-start your computer and try again. ' +
          'You may need to confirm with your IT support staff that you have ' +
          'write access to this location.');
        FError:= True;
      end;
    end;
    IncProgress;
  end;
end;

procedure TtiFileSyncMgr.UpdateFiles;
var
  lSource : TtiFileName;
  lTarget : TtiFileName;
  i : integer;
begin
  for i := 0 to UpdateFileNames.Count - 1 do
  begin
    if FTerminated then Exit; //==>
    lSource := UpdateFileNames.Items[i];
    Log('Updating ' + lSource.NameFull);
    UpdateProgressMajor(lSource);
    ReadFileData(lSource, FsSourceReader, FSourceReaderParams);
    if not FTerminated then
    begin
      lTarget := lSource.CloneWithNewRootDir(FTargetFileNames.StartDir);
      try
        try
          FFileNameLockList.UnLock(LTarget.PathAndName);
          WriteFileData(lTarget, FsTargetReader, FTargetReaderParams);
          UpdateFile(lTarget.PathAndName, fsaUpdate);
          Log('Done.', true);
        except
          on e:exception do
          begin
            Log('  ' + e.message);
            FError:= True;
          end;
        end;
      finally
        lTarget.Free;
      end;
    end;
    IncProgress;
  end;
end;

procedure TtiFileSyncMgr.DeleteFiles;
var
  lTarget : TtiFileName;
  i : integer;
begin
  for i := 0 to DeleteFileNames.Count - 1 do
  begin
    if FTerminated then Exit; //==>
    lTarget := DeleteFileNames.Items[i];
    try
      Log('Deleting ' + lTarget.PathAndName);
      FFileNameLockList.UnLock(LTarget.PathAndName);
      DeleteFileData(lTarget, FsTargetReader, FTargetReaderParams);
    except
      on e:exception do
      begin
        Log(cErrorDeleteFileFailed);
        Log('  ' + lTarget.PathAndName);
        Log('  ' + e.message);
        FError:= True;
      end;
    end;
    IncProgress;
  end;
end;

procedure TtiFileSyncMgr.ReadFileData(const ptiFileName : TtiFileName; const psReader : string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
  try
    LReader.OnProgress:= OnProgressMinor;
    LReader.OnCheckTerminated:= DoOnCheckTerminated;
    lReader.ReadFileData(ptiFileName);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.WriteFileData(const ptiFileName : TtiFileName; const psReader : string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
  try
    lReader.WriteFileData(ptiFileName);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.DeleteFileData(const ptiFileName : TtiFileName; const psReader : string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
  try
    lReader.DeleteFileData(ptiFileName);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.WriteIndex(const ptiFileNames : TtiFileNames; const psReader : string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
  try
    lReader.WriteIndex(ptiFileNames);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.Log(const psMessage: string; pAppendToPrevRow: boolean = false);
begin
  if Assigned (FOnLog) then
    FOnLog(psMessage, pAppendToPrevRow);
end;

{ TthrdFileSyncMgr }

constructor TthrdFileSyncMgr.Create(Suspended: Boolean);
begin
  inherited Create(Suspended);
  FreeOnTerminate := true;
  FFileSyncSetup := TtiFileSyncSetup.Create;
  FFileSyncMgr := TtiFileSyncMgr.Create;
  FCloseAppOnTerminate := false;
end;

constructor TthrdFileSyncMgr.CreateExt;
begin
  Create(true);
end;

procedure TthrdFileSyncMgr.CustomTerminate;
begin
  Terminate;
  FFileSyncMgr.Terminate;
end;

destructor TthrdFileSyncMgr.Destroy;
begin
//  FFormProgress.Free;
  FFileSyncSetup.Free;
  FFileSyncMgr.Free;
  inherited;
end;

procedure TthrdFileSyncMgr.Execute;
var
  i : integer;
  lFileSyncDir : TtiFileSyncDir;
begin
  Synchronize(HideFormMain);
  for i := 0 to FFileSyncSetup.FileSyncDirs.Count - 1 do
  begin
    lFileSyncDir := FFileSyncSetup.FileSyncDirs.Items[i];
    FFileSyncMgr.FileNameFilters.Assign(FFileSyncSetup.FileNameFilters);
//    FFileSyncMgr.OnLog := FFormProgress.Log;
//  FFileSyncMgr.OnProgress := FFormProgresspdateProgress;

    if FFileSyncSetup.SyncDirection = sdSourceToTarget then
    begin
      FFileSyncMgr.SourceFileNames.StartDir := lFileSyncDir.LocalDir;
      FFileSyncMgr.SourceReader             := lFileSyncDir.SourceReader;
      FFileSyncMgr.TargetFileNames.StartDir := lFileSyncDir.TargetLocation;
      FFileSyncMgr.TargetReader             := lFileSyncDir.TargetReader;
    end
    else if FFileSyncSetup.SyncDirection = sdTargetToSource then
    begin
      FFileSyncMgr.SourceFileNames.StartDir := lFileSyncDir.TargetLocation;
      FFileSyncMgr.SourceReader             := lFileSyncDir.TargetReader;
      FFileSyncMgr.TargetFileNames.StartDir := lFileSyncDir.LocalDir ;
      FFileSyncMgr.TargetReader             := lFileSyncDir.SourceReader;
    end
    else
      raise Exception.Create('Invalid FFileSyncSetup.SyncDirection');

    FFileSyncMgr.Execute;
    if Terminated then
      Break; //==>
  end;
  Synchronize(ShowFormMain);
end;

procedure TthrdFileSyncMgr.HideFormMain;
begin
  FFormMain.Visible := false;
//FFormProgress.Visible := true;
end;

procedure TthrdFileSyncMgr.ShowFormMain;
begin
  FFormMain.Visible := true;
//FFormProgress.Visible := false;
  if CloseAppOnTerminate then
    FFormMain.Close;
end;

procedure TtiFileSyncMgr.UpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer);
begin
  if Assigned (FOnProgressMajor) then
    FOnProgressMajor(AFileName, AMax, APos);
end;

procedure TtiFileSyncMgr.UpdateFile(const pFilename: string; pAction: TtiFileSyncAction);
begin
  if Assigned(FOnFile) then
    FOnFile(pFileName, pAction);
end;

procedure TtiFileSyncMgr.IncProgress;
begin
  inc(FiFileNum);
end;

{ TtiFileSyncReader }

procedure TtiFileSyncMgr.ReadPathIndex(const psArchiveType: string; const pReaderParams: string; const ptiPathNames: TtiPathNames);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psArchiveType, pReaderParams);
  try
    lReader.ReadPathIndex(ptiPathNames);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.BuildCreatePathList(pData: TtiObject);
var
  lData : TtiPathName;
begin
  lData := FTargetPathNames.FindLikeRootRemoved(TtiPathName(pData));
  if (lData = nil) then
    FCopyPathNames.Add(pData)
end;

procedure TtiFileSyncMgr.BuildDeletePathList(pData: TtiObject);
var
  lData : TtiPathName;
begin
  lData := FSourcePathNames.FindLikeRootRemoved(TtiPathName(pData));
  if lData = nil then
    FDeletePathNames.Add(pData);
end;

{
procedure TtiFileSyncMgr.CreatePaths;
var
  lSource : TtiPathName;
  lTarget : TtiPathName;
  i : integer;
begin
  for i := 0 to FCopyPathNames.Count - 1 do
  begin
    if FTerminated then Exit; //==>
    lSource := FCopyPathNames.Items[i];
    lTarget := lSource.CloneWithNewRootDir(FTargetPathNames.StartDir);
    try
      try
        Log(Format('Creating %s', [lTarget.Path]));
        CreatePath(lTarget, FsTargetReader, FTargetReaderParams);
      except
        on e:exception do
        begin
          Log(cErrorCreatePathFailed);
          Log('  ' + lTarget.Path);
          Log('  ' + e.message);
          FError:= True;
        end;
      end;
    finally
      lTarget.Free;
    end;
    IncProgress;
  end;
end;
}

procedure TtiFileSyncMgr.DeletePaths;
var
  i : integer;
begin
  for i := 0 to FDeletePathNames.Count - 1 do
  begin
    if FTerminated then Exit; //==>
    try
      DeletePath(FDeletePathNames.Items[i], FsTargetReader, FTargetReaderParams);
    except
      on e:Exception do
      begin
        Log(cErrorDeletePathFailed);
        Log('  ' + FDeletePathNames.Items[i].Path);
        Log('  ' + e.message);
        FError:= True;
      end;
    end;
    IncProgress;
  end;
end;

//procedure TtiFileSyncMgr.CreatePath(const ptiPathName: TtiPathName; const psReader: string; const pReaderParams: string);
//var
//  lReader : TtiFileSyncReaderAbs;
//begin
//  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
//  try
//    lReader.CreatePath(ptiPathName);
//  finally
//    lReader.Free;
//  end;
//end;

procedure TtiFileSyncMgr.DeletePath(const ptiPathName: TtiPathName; const psReader: string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs;
begin
  lReader := gFileSyncReaderFactory.CreateInstance(psReader, pReaderParams);
  try
    lReader.DeletePath(ptiPathName);
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.Terminate;
begin
  FTerminated := true;
  Log('');
  Log(cMessageWaitingForTerminate);
end;

procedure TtiFileSyncMgr.AssignFromFileSyncDir(const pData: TtiFileSyncDir);
begin
  Assert(pData.TestValid(TtiFileSyncDir), CTIErrorInvalidObject);
  SourceFileNames.StartDir := pData.LocalDir;
  SourceReader             := pData.SourceReader;
  TargetFileNames.StartDir := pData.TargetLocation;
  TargetReader             := pData.TargetReader;
end;

{ TtiFileSyncTasks }

procedure TtiFileSyncTasks.Add(pObject: TtiFileSyncTask;pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TtiFileSyncTasks.AddInstance(pFileName: TtiFileName;
  pTargetFileNames: TtiFileNames): TtiFileSyncTask;
begin
  result := TtiFileSyncTask.Create;
  result.SourceFileName := pFileName;
  result.TargetFileNames := pTargetFileNames;
  Add(Result);
end;

function TtiFileSyncTasks.GetItems(i: integer): TtiFileSyncTask;
begin
  result := TtiFileSyncTask(inherited GetItems(i));
end;

procedure TtiFileSyncTasks.SetItems(i: integer;const Value: TtiFileSyncTask);
begin
  inherited SetItems(i, Value);
end;

{ TtiFileSyncTask }

function TtiFileSyncTask.GetDate: TDateTime;
begin
  Assert(FSourceFileName.TestValid(TtiFileName), CTIErrorInvalidObject);
  result := FSourceFileName.Date;
end;

function TtiFileSyncTask.GetOwner: TtiFileSyncTasks;
begin
  result := TtiFileSyncTasks(inherited GetOwner);
end;

function TtiFileSyncTask.GetPathAndName: string;
begin
  Assert(FSourceFileName.TestValid(TtiFileName), CTIErrorInvalidObject);
  result := FSourceFileName.PathAndName;
end;

function TtiFileSyncTask.GetSize: Integer;
begin
  Assert(FSourceFileName.TestValid(TtiFileName), CTIErrorInvalidObject);
  result := FSourceFileName.Size;
end;

function TtiFileSyncTask.GetTargetPathAndName: string;
begin
  Assert(FSourceFileName.TestValid(TtiFileName), CTIErrorInvalidObject);
  Assert(FTargetFileNames.TestValid(TtiFileNames), CTIErrorInvalidObject);

  result := tiAddTrailingSlash(FTargetFileNames.StartDir) + FSourceFileName.RootRemoved;
  result := tiStrTran(result, '\\', '\');
end;

procedure TtiFileSyncTask.SetOwner(const Value: TtiFileSyncTasks);
begin
  inherited SetOwner(Value);
end;

{ TthrdTIFileSyncOneWayCopy }

constructor TthrdTIFileSyncOneWayCopy.Create(
                        const pSourceReader: string;
                        const pSourceReaderParams: string;
                        const pSourceStartDir: string;
                        const pTargetReader: string;
                        const pTargetReaderParams: string;
                        const pTargetStartDir: string;
                        const pOnLog: TtiFileSyncMgrLogEvent;
                        const AOnProgressMajor: TtiFileSyncMgrProgressEvent;
                        const AOnProgressMinor: TtiFileSyncMgrProgressEvent;
                        const pOnFile: TtiFileSyncMgrFileEvent;
                        const pOnTerminate: TNotifyEvent;
                        const pOnTerminateWithError: TNotifyEvent;
                        const pFormToShow: TForm);
begin
  inherited Create(True);
  FreeOnTerminate:= False;
  OnTerminate := pOnTerminate;
  FOnLog := pOnLog;
  FOnFile := pOnFile;
  FOnProgressMajor := AOnProgressMajor;
  FOnProgressMinor := AOnProgressMinor;
  FFormToShow := pFormToShow;
  FOnTerminateWithError:= pOnTerminateWithError;

  FFSM := TtiFileSyncMgr.Create;
  FFSM.SourceReader := pSourceReader;
  FFSM.SourceReaderParams := pSourceReaderParams;
  FFSM.SourceFileNames.StartDir := pSourceStartDir;
  FFSM.TargetReader := pTargetReader;
  FFSM.TargetReaderParams := pTargetReaderParams;
  FFSM.TargetFileNames.StartDir := pTargetStartDir;
  FFSM.VerboseLogging := True;
  FFSM.FileSyncActions:= [fsaCopy, fsaUpdate];
  FFSM.OnLog := DoLog;
  FFSM.OnProgressMajor:= DoUpdateProgressMajor;
  FFSM.OnProgressMinor:= DoUpdateProgressMinor;
  FFSM.OnFile := DoFile;
  Resume;
end;

destructor TthrdTIFileSyncOneWayCopy.Destroy;
begin
  FFSM.Free;
  inherited;
end;

procedure TthrdTIFileSyncOneWayCopy.DoLog(const pMessage: string;pAppendToPrevRow: boolean);
begin
  if Assigned(FOnLog) then
  begin
    FMessage:= pMessage;
    FAppendToPrevRow:= pAppendToPrevRow;
    if IsConsole then
      DoLogSynchronize
    else
      Synchronize(DoLogSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoLogSynchronize;
begin
  FOnLog(FMessage, FAppendToPrevRow);
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer);
begin
  if Assigned(FOnProgressMajor) then
  begin
    FFileName:= AFileName;
    FMax:= AMax;
    FPos:= APos;
    Synchronize(DoUpdateProgressMajorSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoFile(const pFileName: string; pAction: TtiFileSyncAction);
begin
  if Assigned(FOnFile) then
  begin
    FLastFile := pFileName;
    FLastFileAction := pAction;
    Synchronize(DoFileSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoFileSynchronize;
begin
  FOnFile(FLastFile, FLastFileAction);
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgressMajorSynchronize;
begin
  if Assigned(FFormToShow) and
     (FMax > 1) and
     (not FFormToShow.Visible) then
    FFormToShow.Visible := True;
  FOnProgressMajor(FFileName, FMax, FPos);
end;

procedure TthrdTIFileSyncOneWayCopy.Execute;
begin
  try
    FFSM.Execute;
    if FFSM.Error then
      OnTerminate := FOnTerminateWithError;
  except
    on e:Exception do
      OnTerminate := FOnTerminateWithError;
  end;
end;

procedure TtiFileSyncMgr.UpdateProgressMajor(AFileName: TtiFileName);
begin
  UpdateProgressMajor(AFileName, FiFileCount, FiFileNum);
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgressMinor(
  AFileName: TtiFileName; AMax, APos: integer);
begin
  if Assigned(FOnProgressMinor) then
  begin
    FFileName:= AFileName;
    FMax:= AMax;
    FPos:= APos;
    Synchronize(DoUpdateProgressMinorSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgressMinorSynchronize;
begin
  FOnProgressMinor(FFileName, FMax, FPos);
end;

procedure TthrdTIFileSyncOneWayCopy.Terminate;
begin
  inherited Terminate;;
  FFSM.Terminate;
end;

procedure TtiFileSyncMgr.DoOnCheckTerminated(var ATerminated: Boolean);
begin
  ATerminated:= FTerminated;
end;

function TtiFileSyncMgr.LockAllLocalFiles: Boolean;
var
  i: Integer;
  LFileList: TStringList;
  LFileName: string;
  LMessage: string;
begin
  LFileList:= TStringList.Create;
  try
    for i:= 0 to FCopyFileNames.Count-1 do
    begin
      LFileName:= FCopyFileNames.Items[i].SourceFileName.NameWithNewRootDir(FTargetFileNames.StartDir);
      if FileExists(LFileName) and
         (not FFileNameLockList.Lock(LFileName)) then
        LFileList.Add(LFileName);
    end;

    for i:= 0 to FUpdateFileNames.Count-1 do
    begin
      LFileName:= FUpdateFileNames.Items[i].NameWithNewRootDir(FTargetFileNames.StartDir);
      if FileExists(LFileName) and
         (not FFileNameLockList.Lock(LFileName)) then
        LFileList.Add(LFileName);
    end;

    for i:= 0 to FDeleteFileNames.Count-1 do
    begin
      LFileName:= FDeleteFileNames.Items[i].NameWithNewRootDir(FTargetFileNames.StartDir);
      if FileExists(LFileName) and
         (not FFileNameLockList.Lock(LFileName)) then
        LFileList.Add(LFileName);
    end;
    Result:= LFileList.Count = 0;
    if not Result then
    begin
      LMessage:= tiIf(LFileList.Count = 1, 'file', 'files');
      Log('The following ' + LMessage + ' appears to be open in another application and could not be locked for update. ' +
          'Please close all active applications and try again. ' +
          'If this does not work, please re-start your computer and try again. ' +
          'You may need to confirm with your IT support staff that you have ' +
          'write access to this location.');
      Log('');
      for i:= 0 to LFileList.Count - 1 do
        Log('  ' + LFileList.Strings[i]);
    end;
  finally
    LFileList.Free;
  end;
  FError:= not Result;
end;

{ TtiFileNameLockList }

constructor TtiFileNameLockList.Create;
begin
  inherited Create;
  FList:= TObjectList.Create(True);
end;

destructor TtiFileNameLockList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiFileNameLockList.Lock(AFileName: string): Boolean;
var
  FLockItem: TtiFileNameLockItem;
begin
  Assert(FileExists(AFileName), 'File not found "' + AFileName + '"');
  FLockItem:= TtiFileNameLockItem.Create(AFileName);
  Result:= FLockItem.Lock;
  if Result then
    FList.Add(FLockItem)
  else begin
    FLockItem.Free;
  end;
end;

procedure TtiFileNameLockList.UnLock(AFileName: String);
var
  i:Integer;
begin
  for i:= 0 to FList.Count-1 do
    if (FList.Items[i] as TtiFileNameLockItem).FileName = AFileName then
    begin
      FList.Delete(i);
      Exit ; //==>
    end;
end;

{ TtiFileNameLockItem }

constructor TtiFileNameLockItem.Create(AFileName: String);
begin
  Assert(FileExists(AFileName), 'File not found "' + AFileName + '"');
  inherited Create;
  FFileName:= AFileName;
  FHandle:= 0;
end;

destructor TtiFileNameLockItem.Destroy;
begin
  Unlock;
  inherited;
end;

function TtiFileNameLockItem.Lock: Boolean;
begin
  try
    FHandle := FileOpen(FFileName, fmOpenWrite or fmShareExclusive);
    Result:= (FHandle > 0) and (FHandle < High(LongWord));
  except
    on e:Exception do
      Result:= False;
  end;
end;

procedure TtiFileNameLockItem.UnLock;
begin
  if FHandle > 0 then
    FileClose(FHandle);
end;

end.

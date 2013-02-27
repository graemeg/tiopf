unit tiFileSync_Mgr;

interface
uses
  Classes
  ,SyncObjs
  ,tiFileName_BOM
  ,tiPtnVisPerObj
  ,tiFileSyncReader_Abs
  ,FProgress
  ,Forms
  ,SysUtils
  ,tiFileSyncSetup_BOM
  ;

const
  cErrorCreatePathFailed = 'Unable to create path';
  cErrorDeletePathFailed = 'Unable to delete path';
  cErrorCopyFileFailed   = 'Unable to copy file';
  cErrorUpdateFileFailed = 'Unable to update file';
  cErrorDeleteFileFailed = 'Unable to delete file';

  ctiFSNewFilesToBeCopied      = 'New file(s) to be copied';
  ctiFSExistingFilesToBeUpdated = 'Existing file(s) to be updated';

type

  TtiFileSyncMgrLogEvent = procedure ( const psMessage : string ; pAppendToPrevRow: Boolean ) of object ;
  TtiFileSyncMgrProgressEvent = procedure ( piMax, piPos : integer ) of object ;

  TtiFileSyncTasks = class;
  TtiFileSyncTask = class;

  TtiFileSyncTasks = class( TPerObjList )
  private
    function    GetItems(i: integer): TtiFileSyncTask ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiFileSyncTask); reintroduce ;
  public
    property    Items[i:integer] : TtiFileSyncTask read GetItems write SetItems ;
    procedure   Add( pObject : TtiFileSyncTask   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    function    AddInstance(pFileName: TtiFileName; pTargetFileNames: TtiFileNames): TtiFileSyncTask;
  end ;

  TtiFileSyncTask = class( TPerObjAbs )
  private
    FSourceFileName: TtiFileName;
    FTargetFileNames: TtiFileNames;
    function GetTargetPathAndName: string;
    function GetDate: TDateTime;
    function GetPathAndName: string;
    function GetSize: Integer;
  protected
    function    GetOwner: TtiFileSyncTasks; reintroduce ;
    procedure   SetOwner(const Value: TtiFileSyncTasks ); reintroduce ;
    property    TargetFileNames: TtiFileNames read FTargetFileNames write FTargetFileNames;
  public
    property    Owner       : TtiFileSyncTasks read GetOwner      write SetOwner ;
    property    SourceFileName : TtiFileName read FSourceFileName write FSourceFileName;
    property    TargetPathAndName: string read GetTargetPathAndName;
  published
    property    PathAndName: string read GetPathAndName;
    property    Date: TDateTime read GetDate;
    property    Size: Integer read GetSize;
  end ;

  TtiFileSyncAction = ( fsaCopy, fsaUpdate, fsaDelete ) ;
  TtiFileSyncActions = set of TtiFileSyncAction;

  TtiFileSyncMgr = class( TPerObjAbs )
  private
    FFileNameFilters  : TtiFileNameFilters ;

    FTargetPathNames  : TtiPathNames  ;
    FSourcePathNames  : TtiPathNames  ;
    FCopyPathNames    : TtiPathNames ;
    FDeletePathNames  : TtiPathNames ;

    FTargetFileNames : TtiFileNames ;
    FSourceFileNames : TtiFileNames ;

    FCopyFileNames   : TtiFileSyncTasks ;
    FDeleteFileNames : TtiFileNames ;
    FUpdateFileNames : TtiFileNames ;

    FsTargetReader   : string ;
    FsSourceReader   : string ;
    FOnLog: TtiFileSyncMgrLogEvent;
    FOnProgress: TtiFileSyncMgrProgressEvent;
    FiFileCount : integer ;
    FiFileNum : integer ;
    FTerminated : boolean ;
    FFileSyncActions: TtiFileSyncActions;
    FVerboseLogging: Boolean;
    FTargetReaderParams: string;
    FSourceReaderParams: string;
    FError: Boolean;

    procedure BuildCreatePathList( pData : TPerObjAbs ) ;
    procedure BuildDeletePathList(     pData : TPerObjAbs ) ;
    procedure BuildCopyUpdateFileList( pData : TPerObjAbs ) ;
    procedure BuildDeleteFileList(     pData : TPerObjAbs ) ;
    procedure ReadFileIndex( const pReaderType: string;
                             const pReaderParams: string ;
                             const ptiFileNames: TtiFileNames;
                             const pSourceFileNameFilters : TtiFileNameFilters);
    procedure ReadPathIndex( const psArchiveType: string;
                             const pReaderParams: string;
                             const ptiPathNames: TtiPathNames);
    procedure CreatePaths ;
    procedure DeletePaths ;
    procedure CopyFiles ;
    procedure DeleteFiles;
    procedure UpdateFiles;
    procedure CreatePath( const ptiPathName : TtiPathName ; const psReader : string ; const pReaderParams: string) ;
    procedure DeletePath( const ptiPathName : TtiPathName ; const psReader : string ; const pReaderParams: string) ;
    procedure WriteIndex(const ptiFileNames: TtiFileNames; const psReader: string; const pReaderParams: string);
    procedure ReadFileData(const ptiFileName: TtiFileName; const psReader: string ; const pReaderParams: string);
    procedure WriteFileData(const ptiFileName: TtiFileName; const psReader: string; const pReaderParams: string);
    procedure DeleteFileData(const ptiFileName: TtiFileName; const psReader: string; const pReaderParams: string);
    procedure Log( const psMessage : string ; pAppendToPrevRow: Boolean = false ) ;
    procedure UpdateProgress( piMax, piPos : integer ) ;
    procedure IncProgress ;

    // Not sure what to do with this, under construction
    property    FileNameFilters : TtiFileNameFilters read FFileNameFilters ;
  protected
    procedure Clear;
    procedure ReadFileLists;
    procedure BuildCopyLists; virtual ;

  public
    constructor Create ; override;
    destructor  Destroy ; override ;
    procedure   ReadIndexes ; virtual;
    procedure   Execute ;
    property    OnLog : TtiFileSyncMgrLogEvent read FOnLog write FOnLog ;
    property    OnProgress : TtiFileSyncMgrProgressEvent read FOnProgress write FOnProgress ;
    procedure   Terminate ;

    // Assign values to these before running
    procedure   AssignFromFileSyncDir(const pData: TtiFileSyncDir);
    property    SourceFileNames   : TtiFileNames       read FSourceFileNames ;
    property    TargetFileNames   : TtiFileNames       read FTargetFileNames ;
    property    SourceReader      : string             read FsSourceReader write FsSourceReader ;
    property    SourceReaderParams: string             read FSourceReaderParams Write FSourceReaderParams;
    property    TargetReader      : string             read FsTargetReader write FsTargetReader ;
    property    TargetReaderParams: string             read FTargetReaderParams Write FTargetReaderParams;
    property    FileSyncActions   : TtiFileSyncActions read FFileSyncActions Write FFileSyncActions;
    property    VerboseLogging    : Boolean            read FVerboseLogging write FVerboseLogging;

    // Can read these for information about which files are to be copied, updated, deleted
    property    CopyFileNames   : TtiFileSyncTasks read FCopyFileNames ;
    property    UpdateFileNames : TtiFileNames read FUpdateFileNames ;
    property    DeleteFileNames : TtiFileNames read FDeleteFileNames ;
    property    Error: Boolean read FError;

  end ;

  TthrdTIFileSyncOneWayCopy = class( TThread )
  private
    FFSM : TtiFileSyncMgr;
    FOnLog: TtiFileSyncMgrLogEvent;
    FOnProgress: TtiFileSyncMgrProgressEvent;
    FMessage: string;
    FAppendToPrevRow: Boolean;
    FMax: Integer;
    FPos: Integer;
    FFormToShow: TForm;
    FOnTerminateWithError: TNotifyEvent;
    procedure DoLog( const pMessage : string; pAppendToPrevRow: boolean);
    procedure DoUpdateProgress( pMax, pPos : integer );
    procedure DoLogSynchronize;
    procedure DoUpdateProgressSynchronize;
  protected
  public
    constructor Create( const pSourceReader: string;
                        const pSourceReaderParams: string;
                        const pSourceStartDir: string;
                        const pTargetReader: string;
                        const pTargetReaderParams: string;
                        const pTargetStartDir: string;
                        const pOnLog: TtiFileSyncMgrLogEvent;
                        const pOnProgress: TtiFileSyncMgrProgressEvent;
                        const pOnTerminate: TNotifyEvent;
                        const pOnTerminateWithError: TNotifyEvent;
                        const pFormToShow: TForm);

    destructor  Destroy; override;
    procedure   Execute; override ;
    property    OnLog : TtiFileSyncMgrLogEvent read FOnLog write FOnLog ;
    property    OnProgress : TtiFileSyncMgrProgressEvent read FOnProgress write FOnProgress ;
  end ;

  TthrdFileSyncMgr = class( TThread )
  private
    FFormProgress :TFormProgress ;
    FFormMain: TForm;
    FFileSyncSetup: TtiFileSyncSetup;
    FFileSyncMgr : TtiFileSyncMgr ;
    FCloseAppOnTerminate: boolean;
  protected
    procedure HideFormMain ;
    procedure ShowFormMain ;
  public
    constructor Create( Suspended : Boolean ) ;
    destructor  Destroy ; override ;
    constructor CreateExt ;
    procedure   Execute ; override ;
    property    FormMain : TForm read FFormMain write FFormMain ;
    property    FileSyncSetups : TtiFileSyncSetup read FFileSyncSetup ;
    property    CloseAppOnTerminate : boolean read FCloseAppOnTerminate write FCloseAppOnTerminate ;
    procedure   CustomTerminate;
  end ;


implementation
uses
  cFileSync
  ,tiUtils
  ,tiPtnVisPerObj_Cli
  ,tiLog
  ,Windows
  ,cTIPersist
  ,tiDialogs // Debugging
  ;


{
procedure TFileSyncReaders.AddItem(const psTextDisplay, psVisName: string);
var
  i : integer ;
  lData : TFileSyncReader ;
begin

  for i := 0 to Count - 1 do
  begin
    lData := TFileSyncReader( Items[i] ) ;
    if SameText( lData.TextDisplay, psTextDisplay ) then
      tiFmtException( 'Duplicate text display <%s>', [psTextDisplay] ) ;
    if SameText( lData.VisName,     psVisName ) then
      tiFmtException( 'Duplicate visitor name <%s>', [psVisName] ) ;
  end ;

  lData             := TFileSyncReader.Create ;
  lData.TextDisplay := psTextDisplay ;
  lData.VisName     := psVisName ;
  Add( lData ) ;

end ;
}

{
procedure TFileSyncReaders.AssignTo(pStrings: TStrings);
var
  i : integer ;
  lData : TFileSyncReader ;
begin
  pStrings.Clear ;
  for i := 0 to Count - 1 do
  begin
    lData := TFileSyncReader( Items[i] )  ;
    pStrings.Add( lData.TextDisplay ) ;
  end ;
end;
}

{
function TFileSyncReaders.VisNameFromTextDisplay(const psTextDisplay: string): string;
var
  i : integer ;
  lData : TFileSyncReader ;
begin
  lData := nil ;
  for i := 0 to Count - 1 do
    if SameText( TFileSyncReader( Items[i] ).TextDisplay, psTextDisplay ) then
    begin
      lData := TFileSyncReader( Items[i] )  ;
      Break ; //==>
    end ;

  Assert( lData <> nil, 'Unable to find visitor for <' + psTextDisplay + '>' ) ;

  result := lData.TextDisplay ;

end;
}

{ TtiFileSyncMgrMgr }

procedure TtiFileSyncMgr.BuildCopyUpdateFileList(pData: TPerObjAbs);
var
  lData : TtiFileName ;
begin
  lData := FTargetFileNames.FindLikeRootRemoved(pData as TtiFileName) ;

  // Copy
  if ( lData = nil ) then
    FCopyFileNames.AddInstance( pData as TtiFileName,
                                FTargetFileNames )

  // Update
  else if ( lData <> nil ) and
          (( TtiFileName( pData ).Size <> lData.Size ) or
           ( TtiFileName( pData ).Date <> lData.Date )) then
    FUpdateFileNames.Add( pData )

end;

procedure TtiFileSyncMgr.BuildDeleteFileList(pData: TPerObjAbs);
var
  lData : TtiFileName ;
begin
  lData := FSourceFileNames.FindLikeRootRemoved( TtiFileName( pData )) ;
  if lData = nil then
    FDeleteFileNames.Add( pData ) ;
end;

constructor TtiFileSyncMgr.Create;
begin
  inherited ;
  FTargetPathNames  := TtiPathNames.Create  ;
  FSourcePathNames  := TtiPathNames.Create  ;

  FCopyPathNames   := TtiPathNames.Create ;
  FCopyPathNames.OwnsObjects := false ;
  FCopyPathNames.AutoSetItemOwner := false ;

  FDeletePathNames := TtiPathNames.Create ;
  FDeletePathNames.OwnsObjects := false ;
  FDeletePathNames.AutoSetItemOwner := false ;

  FTargetFileNames := TtiFileNames.Create ;
  FSourceFileNames := TtiFileNames.Create ;

  FCopyFileNames   := TtiFileSyncTasks.Create ;

  FDeleteFileNames := TtiFileNames.Create ;
  FDeleteFileNames.OwnsObjects := false ;
  FDeleteFileNames.AutoSetItemOwner := false ;

  FUpdateFileNames := TtiFileNames.Create ;
  FUpdateFileNames.OwnsObjects := false ;
  FUpdateFileNames.AutoSetItemOwner := false ;

  FFileNameFilters  := TtiFileNameFilters.Create ;

  FFileSyncActions := [fsaCopy, fsaUpdate, fsaDelete];
  FVerboseLogging := True;
  FError:= False;
  FTerminated := false ;
end;

destructor TtiFileSyncMgr.Destroy;
begin
  FTargetPathNames.Free  ;
  FSourcePathNames.Free  ;
  FCopyPathNames.Free    ;
  FDeletePathNames.Free  ;

  FTargetFileNames.Free ;
  FSourceFileNames.Free ;
  FCopyFileNames.Free   ;
  FDeleteFileNames.Free ;
  FUpdateFileNames.Free ;

  FFileNameFilters.Free;

  inherited;
end;

procedure TtiFileSyncMgr.Clear;
begin
  FCopyPathNames.Clear ;
  FDeletePathNames.Clear ;
  FUpdateFileNames.Clear ;
  FCopyFileNames.Clear ;
  FDeleteFileNames.Clear ;
  FError:= False;
end ;

procedure TtiFileSyncMgr.ReadFileLists;
begin

  // Source directories
  if FVerboseLogging then
    Log( 'Reading list of source directories' );
  FSourcePathNames.StartDir := FSourceFileNames.StartDir ;
  ReadPathIndex( SourceReader, FSourceReaderParams, FSourcePathNames ) ;
  if FVerboseLogging then
    Log( '  Directories in source: ' + IntToStr( FSourcePathNames.Count )) ;

  // Target directories
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Reading list of target directories' );
  FTargetPathNames.StartDir := FTargetFileNames.StartDir ;
  ReadPathIndex( TargetReader, FTargetReaderParams, FTargetPathNames ) ;
  if FVerboseLogging then
    Log( '  Directories in target: ' + IntToStr( FTargetPathNames.Count )) ;

  // Source files
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Reading list of source files' );
  ReadFileIndex( SourceReader, SourceReaderParams, FSourceFileNames, FFileNameFilters ) ;
  if FVerboseLogging then
    Log( '  Files in source: ' + IntToStr( FSourceFileNames.Count )) ;

  // Target files
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Reading list of target files' );
  ReadFileIndex( TargetReader, TargetReaderParams, FTargetFileNames, nil ) ;
  if FVerboseLogging then
    Log( '  Files in target: ' + IntToStr( FTargetFileNames.Count )) ;

end;

procedure TtiFileSyncMgr.ReadIndexes;
begin

  if FTerminated then Exit ; //==>
  Clear ;

  if FTerminated then Exit ; //==>
  ReadFileLists;

  if FTerminated then Exit ; //==>
  BuildCopyLists;

  if FTerminated then Exit ; //==>
  if fsaCopy in FileSyncActions then
    Inc(FiFileCount, FCopyPathNames.Count ) ;
  if fsaDelete in FileSyncActions then
    Inc(FiFileCount, FDeletePathNames.Count );
  if fsaUpdate in FileSyncActions then
    Inc(FiFileCount, FUpdateFileNames.Count );
  if fsaCopy in FileSyncActions then
    Inc(FiFileCount, FCopyFileNames.Count );
  if fsaDelete in FileSyncActions then
    Inc(FiFileCount, FDeleteFileNames.Count );

  Inc(FiFileCount, 1 );
  UpdateProgress( FiFileCount, 1 ) ;
  FiFileNum := 1 ;

end;

procedure TtiFileSyncMgr.BuildCopyLists;
begin
  // Directories to create
  if FVerboseLogging then
    Log( 'Building list of directories to create' );
  FSourcePathNames.ForEach( BuildCreatePathList   ) ;
  if FVerboseLogging then
    Log( '  Directories to be created: ' + IntToStr( FCopyPathNames.Count )) ;

  // Directories to delete
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Building list of directories to delete' );
  FTargetPathNames.ForEach( BuildDeletePathList ) ;
  if FVerboseLogging then
    Log( '  Directories to be deleted: ' + IntToStr( FDeletePathNames.Count )) ;

  // Files to create or update
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Building list of files to create or update' );
  FSourceFileNames.ForEach( BuildCopyUpdateFileList   ) ;
  if FCopyFileNames.Count <> 0 then
    Log( IntToStr( FCopyFileNames.Count ) + ' ' + ctiFSNewFilesToBeCopied) ;
  if FUpdateFileNames.Count <> 0 then
    Log( IntToStr( FUpdateFileNames.Count ) + ' ' + ctiFSExistingFilesToBeUpdated) ;

  // Files to delete
  if FTerminated then Exit ; //==>
  if FVerboseLogging then
    Log( 'Building list of files to delete' );
  FTargetFileNames.ForEach( BuildDeleteFileList ) ;
  if FVerboseLogging then
    Log( IntToStr( FDeleteFileNames.Count ) + ' Files to be deleted') ;
  Log( '' ) ;
end;

procedure TtiFileSyncMgr.ReadFileIndex( const pReaderType:string;
                                        const pReaderParams: string;
                                        const ptiFileNames : TtiFileNames ;
                                        const pSourceFileNameFilters : TtiFileNameFilters) ;
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( pReaderType, pReaderParams ) ;
  try
    lReader.ReadFileIndex( ptiFileNames, pSourceFileNameFilters ) ;
  finally
    lReader.Free;
  end ;
end;

procedure TtiFileSyncMgr.Execute;
begin
  if FTerminated then Exit ; //==>
  ReadIndexes ;

  if FTerminated then Exit ; //==>
  if fsaCopy in FFileSyncActions then
    CreatePaths ;

  if FTerminated then Exit ; //==>
  if fsaCopy in FFileSyncActions then
    CopyFiles ;

  if FTerminated then Exit ; //==>
  if fsaUpdate in FFileSyncActions then
    UpdateFiles ;

  if FTerminated then Exit ; //==>
  if fsaDelete in FFileSyncActions then
    DeleteFiles ;

  if FTerminated then Exit ; //==>
  if fsaDelete in FFileSyncActions then
    DeletePaths ;

  if FTerminated then Exit ; //==>
  WriteIndex( FSourceFileNames, FsTargetReader, FTargetReaderParams ) ; // Not sure what to do about the list of files to pass here
end;

procedure TtiFileSyncMgr.CopyFiles;
var
  lSource : TtiFileName ;
  lTarget : TtiFileName ;
  i : integer ;
  lPathName: string ;
begin
  for i := 0 to CopyFileNames.Count - 1 do
  begin
    if FTerminated then Exit ; //==>
    try
      lSource := CopyFileNames.Items[i].SourceFileName ;
      lPathName := lSource.NameFull;
      Log( Format( 'Copying %s', [lSource.NameFull] )) ;
      ReadFileData( lSource, FsSourceReader, FSourceReaderParams ) ;
      lTarget := lSource.CloneWithNewRootDir(FTargetFileNames.StartDir);
      try
        lPathName := lTarget.PathAndName;
        WriteFileData( lTarget, FsTargetReader, FTargetReaderParams ) ;
        Log('Done.', true) ;
        if not FileExists( lTarget.PathAndName ) then
          LogError( 'Error - file not written correctly <' + lTarget.PathAndName + '>');
      finally
        lTarget.Free ;
      end ;
    except
      on e:exception do
      begin
        Log( cErrorCopyFileFailed ) ;
        Log( '  ' + lPathName ) ;
        Log( '  ' + e.message );
        FError:= True;
      end;
    end ;
    IncProgress ;
  end ;
end;

procedure TtiFileSyncMgr.UpdateFiles;
var
  lSource : TtiFileName ;
  lTarget : TtiFileName ;
  i : integer ;
begin
  for i := 0 to UpdateFileNames.Count - 1 do
  begin
    if FTerminated then Exit ; //==>
    lSource := UpdateFileNames.Items[i] ;
    ReadFileData( lSource, FsSourceReader, FSourceReaderParams ) ;
    lTarget := lSource.CloneWithNewRootDir(FTargetFileNames.StartDir);
    try
      try
        Log( 'Updating ' + lTarget.PathAndName) ;
        WriteFileData( lTarget, FsTargetReader, FTargetReaderParams ) ;
      except
        on e:exception do
        begin
          Log( cErrorUpdateFileFailed ) ;
          Log( '  ' + lTarget.PathAndName );
          Log( '  ' + e.message );
          FError:= True;
        end;
      end ;
    finally
      lTarget.Free ;
    end ;
    IncProgress ;
  end ;
end;

procedure TtiFileSyncMgr.DeleteFiles;
var
  lTarget : TtiFileName ;
  i : integer ;
begin
  for i := 0 to DeleteFileNames.Count - 1 do
  begin
    if FTerminated then Exit ; //==>
    lTarget := DeleteFileNames.Items[i] ;
    try
      Log( 'Deleting ' + lTarget.PathAndName) ;
      DeleteFileData( lTarget, FsTargetReader, FTargetReaderParams ) ;
    except
      on e:exception do
      begin
        Log( cErrorDeleteFileFailed ) ;
        Log( '  ' + lTarget.PathAndName);
        Log( '  ' + e.message );
        FError:= True;
      end;
    end ;
    IncProgress ;
  end ;
end;

procedure TtiFileSyncMgr.ReadFileData( const ptiFileName : TtiFileName ; const psReader : string ; const pReaderParams: string) ;
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.ReadFileData( ptiFileName ) ;
  finally
    lReader.Free ;
  end ;
end ;

procedure TtiFileSyncMgr.WriteFileData( const ptiFileName : TtiFileName ; const psReader : string ; const pReaderParams: string) ;
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.WriteFileData( ptiFileName ) ;
  finally
    lReader.Free;
  end ;
end ;

procedure TtiFileSyncMgr.DeleteFileData( const ptiFileName : TtiFileName ; const psReader : string ; const pReaderParams: string ) ;
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.DeleteFileData( ptiFileName ) ;
  finally
    lReader.Free;
  end;
end ;

procedure TtiFileSyncMgr.WriteIndex( const ptiFileNames : TtiFileNames ; const psReader : string; const pReaderParams: string ) ;
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.WriteIndex( ptiFileNames ) ;
  finally
    lReader.Free;
  end;
end ;

procedure TtiFileSyncMgr.Log(const psMessage: string; pAppendToPrevRow: boolean = false);
begin
  if Assigned ( FOnLog ) then
    FOnLog( psMessage, pAppendToPrevRow ) ;
end;

{ TthrdFileSyncMgr }

constructor TthrdFileSyncMgr.Create(Suspended: Boolean);
begin
  inherited Create( Suspended ) ;
  FreeOnTerminate := true ;
  FFormProgress := TFormProgress.Create( nil ) ;
  FFormProgress.Thread := Self ;
  FFileSyncSetup := TtiFileSyncSetup.Create ;
  FFileSyncMgr := TtiFileSyncMgr.Create ;
  FCloseAppOnTerminate := false ;
end;

constructor TthrdFileSyncMgr.CreateExt;
begin
  Create( true ) ;
end;

procedure TthrdFileSyncMgr.CustomTerminate;
begin
  Terminate ;
  FFileSyncMgr.Terminate ;
end;

destructor TthrdFileSyncMgr.Destroy;
begin
//  FFormProgress.Free ;
  FFileSyncSetup.Free ;
  FFileSyncMgr.Free;
  inherited;
end;

procedure TthrdFileSyncMgr.Execute;
var
  i : integer ;
  lFileSyncDir : TtiFileSyncDir ;
begin
  Synchronize( HideFormMain ) ;
  for i := 0 to FFileSyncSetup.FileSyncDirs.Count - 1 do
  begin
    lFileSyncDir := FFileSyncSetup.FileSyncDirs.Items[i] ;
    FFileSyncMgr.FileNameFilters.Assign( FFileSyncSetup.FileNameFilters ) ;
    FFileSyncMgr.OnLog := FFormProgress.Log ;
    FFileSyncMgr.OnProgress := FFormProgress.UpdateProgress ;

    if FFileSyncSetup.SyncDirection = sdSourceToTarget then
    begin
      FFileSyncMgr.SourceFileNames.StartDir := lFileSyncDir.LocalDir ;
      FFileSyncMgr.SourceReader             := lFileSyncDir.SourceReader ;
      FFileSyncMgr.TargetFileNames.StartDir := lFileSyncDir.TargetLocation ;
      FFileSyncMgr.TargetReader             := lFileSyncDir.TargetReader ;
    end
    else if FFileSyncSetup.SyncDirection = sdTargetToSource then
    begin
      FFileSyncMgr.SourceFileNames.StartDir := lFileSyncDir.TargetLocation;
      FFileSyncMgr.SourceReader             := lFileSyncDir.TargetReader;
      FFileSyncMgr.TargetFileNames.StartDir := lFileSyncDir.LocalDir  ;
      FFileSyncMgr.TargetReader             := lFileSyncDir.SourceReader ;
    end
    else
      tiFmtException( 'Invalid FFileSyncSetup.SyncDirection', ClassName, 'Execute' ) ;

    FFileSyncMgr.Execute ;
    if Terminated then
      Break ; //==>
  end;
  Synchronize( ShowFormMain ) ;
end;

procedure TthrdFileSyncMgr.HideFormMain;
begin
  FFormMain.Visible := false ;
  FFormProgress.Visible := true ;
end;

procedure TthrdFileSyncMgr.ShowFormMain;
begin
  FFormMain.Visible := true ;
  FFormProgress.Visible := false ;
  if CloseAppOnTerminate then
    FFormMain.Close ;
end;

procedure TtiFileSyncMgr.UpdateProgress(piMax, piPos: integer);
begin
  if Assigned ( FOnProgress ) then
    FOnProgress( piMax, piPos ) ;
end;

procedure TtiFileSyncMgr.IncProgress;
begin
  inc( FiFileNum ) ;
  UpdateProgress( FiFileCount, FiFileNum ) ;
end;

{ TtiFileSyncReader }

procedure TtiFileSyncMgr.ReadPathIndex(const psArchiveType: string; const pReaderParams: string; const ptiPathNames: TtiPathNames);
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psArchiveType, pReaderParams ) ;
  try
    lReader.ReadPathIndex( ptiPathNames ) ;
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.BuildCreatePathList(pData: TPerObjAbs);
var
  lData : TtiPathName ;
begin
  lData := FTargetPathNames.FindLikeRootRemoved( TtiPathName( pData )) ;
  if ( lData = nil ) then
    FCopyPathNames.Add( pData )
end;

procedure TtiFileSyncMgr.BuildDeletePathList(pData: TPerObjAbs);
var
  lData : TtiPathName ;
begin
  lData := FSourcePathNames.FindLikeRootRemoved( TtiPathName( pData )) ;
  if lData = nil then
    FDeletePathNames.Add( pData ) ;
end;

procedure TtiFileSyncMgr.CreatePaths;
var
  lSource : TtiPathName ;
  lTarget : TtiPathName ;
  i : integer ;
begin
  for i := 0 to FCopyPathNames.Count - 1 do
  begin
    if FTerminated then Exit ; //==>
    lSource := FCopyPathNames.Items[i] ;
    lTarget := lSource.CloneWithNewRootDir(FTargetPathNames.StartDir);
    try
      try
        Log( Format( 'Copying to %s', [lTarget.Path] )) ;
        Log( '' ) ;
        CreatePath( lTarget, FsTargetReader, FTargetReaderParams ) ;
      except
        on e:exception do
        begin
          Log( cErrorCreatePathFailed );
          Log( '  ' + lTarget.Path );
          Log( '  ' + e.message );
          FError:= True;
        end;
      end ;
    finally
      lTarget.Free ;
    end ;
    IncProgress ;
  end ;
end;

procedure TtiFileSyncMgr.DeletePaths;
var
  i : integer ;
begin
  for i := 0 to FDeletePathNames.Count - 1 do
  begin
    if FTerminated then Exit ; //==>
    try
      DeletePath( FDeletePathNames.Items[i], FsTargetReader, FTargetReaderParams ) ;
    except
      on e:Exception do
      begin
        Log( cErrorDeletePathFailed ) ;
        Log( '  ' + FDeletePathNames.Items[i].Path );
        Log( '  ' + e.message );
        FError:= True;
      end;
    end ;
    IncProgress ;
  end ;
end;

procedure TtiFileSyncMgr.CreatePath(const ptiPathName: TtiPathName; const psReader: string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.CreatePath( ptiPathName ) ;
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.DeletePath(const ptiPathName: TtiPathName; const psReader: string; const pReaderParams: string);
var
  lReader : TtiFileSyncReaderAbs ;
begin
  lReader := gFileSyncReaderFactory.CreateInstance( psReader, pReaderParams ) ;
  try
    lReader.DeletePath( ptiPathName ) ;
  finally
    lReader.Free;
  end;
end;

procedure TtiFileSyncMgr.Terminate;
begin
  FTerminated := true ;
  Log( 'Waiting for process to terminate' ) ;
end;

procedure TtiFileSyncMgr.AssignFromFileSyncDir(const pData: TtiFileSyncDir);
begin
  Assert( pData.TestValid(TtiFileSyncDir), cTIInvalidObjectError);
  SourceFileNames.StartDir := pData.LocalDir ;
  SourceReader             := pData.SourceReader ;
  TargetFileNames.StartDir := pData.TargetLocation ;
  TargetReader             := pData.TargetReader ;
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
  Assert( FSourceFileName.TestValid(TtiFileName), cTIInvalidObjectError);
  result := FSourceFileName.Date;
end;

function TtiFileSyncTask.GetOwner: TtiFileSyncTasks;
begin
  result := TtiFileSyncTasks(inherited GetOwner);
end;

function TtiFileSyncTask.GetPathAndName: string;
begin
  Assert( FSourceFileName.TestValid(TtiFileName), cTIInvalidObjectError);
  result := FSourceFileName.PathAndName;
end;

function TtiFileSyncTask.GetSize: Integer;
begin
  Assert( FSourceFileName.TestValid(TtiFileName), cTIInvalidObjectError);
  result := FSourceFileName.Size;
end;

function TtiFileSyncTask.GetTargetPathAndName: string;
begin
  Assert( FSourceFileName.TestValid(TtiFileName), cTIInvalidObjectError);
  Assert( FTargetFileNames.TestValid(TtiFileNames), cTIInvalidObjectError);

  result := tiAddTrailingSlash(FTargetFileNames.StartDir) + FSourceFileName.RootRemoved ;
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
                        const pOnProgress: TtiFileSyncMgrProgressEvent;
                        const pOnTerminate: TNotifyEvent;
                        const pOnTerminateWithError: TNotifyEvent;
                        const pFormToShow: TForm);
begin
  inherited Create(True);
  FreeOnTerminate:= True ;
  OnTerminate := pOnTerminate;
  FOnLog := pOnLog ;
  FOnProgress := pOnProgress;
  FFormToShow := pFormToShow;
  FOnTerminateWithError:= pOnTerminateWithError;

  FFSM := TtiFileSyncMgr.Create;
  FFSM.SourceReader := pSourceReader;
  FFSM.SourceReaderParams := pSourceReaderParams;
  FFSM.SourceFileNames.StartDir := pSourceStartDir;
  FFSM.TargetReader := pTargetReader;
  FFSM.TargetReaderParams := pTargetReaderParams;
  FFSM.TargetFileNames.StartDir := pTargetStartDir;
  FFSM.VerboseLogging := False ;
  FFSM.FileSyncActions:= [fsaCopy, fsaUpdate];
  FFSM.OnLog := DoLog;
  FFSM.OnProgress:= DoUpdateProgress;
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
    Synchronize(DoLogSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoLogSynchronize;
begin
  FOnLog(FMessage, FAppendToPrevRow);
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgress(pMax, pPos: integer);
begin
  if Assigned(FOnProgress) then
  begin
    FMax:= pMax;
    FPos:= pPos;
    Synchronize(DoUpdateProgressSynchronize);
  end;
end;

procedure TthrdTIFileSyncOneWayCopy.DoUpdateProgressSynchronize;
begin
  if Assigned(FFormToShow) and
     (FMax > 1) and
     (not FFormToShow.Visible) then
    FFormToShow.Visible := True ;
  FOnProgress(FMax, FPos);
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

end.

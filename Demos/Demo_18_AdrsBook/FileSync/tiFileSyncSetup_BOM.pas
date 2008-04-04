unit tiFileSyncSetup_BOM;

interface
uses
  Classes
  ,tiObject
  ,SysUtils
  ;

type

  TtiFileFilterType = ( fftUnassigned, fftInclude, fftExclude ) ;
  TtiSyncDirection = ( sdSourceToTarget, sdTargetToSource ) ;

const
  caFilterTypes : array[TtiFileFilterType] of string =
    ( 'Unassigned', 'Include', 'Exclude' ) ;

type

  TtiFileSyncSetup = class ;
  TtiFileSyncDirs = class ;
  TtiFileSyncDir  = class ;
  TtiFileNameFilters = class ;
  TtiFileNameFilter  = class ;

  TtiFileSyncSetup = class( TtiObject )
  private
    FFileSyncDirs: TtiFileSyncDirs;
    FFileNameFilters: TtiFileNameFilters;
    FSyncDirection: TtiSyncDirection;
  protected
    procedure   AssignClassProps(pSource: TtiObject); override;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    function    Clone : TtiFileSyncSetup ; reintroduce ;
  published
    property FileSyncDirs : TtiFileSyncDirs read FFileSyncDirs ;
    property FileNameFilters : TtiFileNameFilters read FFileNameFilters ;
    property SyncDirection : TtiSyncDirection read FSyncDirection write FSyncDirection ;
  end ;

  TtiFileSyncDirs = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TtiFileSyncDir ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiFileSyncDir); reintroduce ;
  public
    property    Items[i:integer] : TtiFileSyncDir read GetItems write SetItems ;
    procedure   Add( pObject : TtiFileSyncDir ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    function    Clone : TtiFileSyncDirs ; reintroduce ;
    procedure   Assign( pSource : TtiFileSyncDirs ) ; reintroduce ;
  end ;

  TtiFileSyncDir = class( TtiObject )
  private
    FTargetReader: string;
    FSourceReader: string;
    FTargetLocation : TFileName;
    FLocalDir : TFileName;
    procedure SetLocalDir(const Value: TFileName);
    procedure SetTargetLocation(const Value: TFileName);
  protected
    function    GetOwner: TtiFileSyncDirs; reintroduce ;
    procedure   SetOwner(const Value: TtiFileSyncDirs); reintroduce ;
  public
    property    Owner       : TtiFileSyncDirs             read GetOwner      write SetOwner ;
    function    Clone : TtiFileSyncDir ; reintroduce ;
    procedure   Assign( pSource : TtiFileSyncDir ) ; reintroduce ;
  published
    property    LocalDir       : TFileName read FLocalDir       write SetLocalDir;
    property    TargetLocation : TFileName read FTargetLocation write SetTargetLocation ;
    property    SourceReader   : string    read FSourceReader   write FSourceReader ;
    property    TargetReader   : string    read FTargetReader   write FTargetReader ;
  end ;

  TtiFileNameFilters = class( TPerVisList )
  protected
    function    GetItems(i: integer): TtiFileNameFilter ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiFileNameFilter); reintroduce ;
  public
    property    Items[i:integer] : TtiFileNameFilter read GetItems write SetItems ;
    procedure   Add( pObject : TtiFileNameFilter ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    procedure   AddFilter( pFilterType : TtiFileFilterType ;
                           pWildCard   : TFileName ) ;
    function    Clone : TtiFileNameFilters ; reintroduce ;
  end ;

  TtiFileNameFilter = class( TtiObject)
  private
    FWildCard: TFileName;
    FFilterType: TtiFileFilterType;
    function  GetFilterTypeAsString: string;
    procedure SetFilterTypeAsString(const Value: string);
  protected
    function    GetOwner: TtiFileNameFilters; reintroduce ;
    procedure   SetOwner(const Value: TtiFileNameFilters); reintroduce ;
  public
    constructor Create ; override ;
    property    Owner       : TtiFileNameFilters             read GetOwner      write SetOwner ;
    function    Clone : TtiFileNameFilter ; reintroduce ;
    property    FilterType : TtiFileFilterType read FFilterType write FFilterType ;
  published
    property    FilterTypeAsString : string read GetFilterTypeAsString write SetFilterTypeAsString;
    property    WildCard   : TFileName read FWildCard write FWildCard ;
  end ;

procedure RegisterMappings ;
procedure CreateTables;

implementation
uses
  cFileSync
  ,tiFileSyncReader_Abs
  ,tiOPFManager
  ,tiAutoMap
  ,tiUtils
  ,tiQueryXMLLight
  ,tiConstants
  ,tiQuery
  ;

procedure RegisterMappings ;
begin
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileSyncDir, 'Sync_Dir', 'OID',            'OID', [pktDB]    ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileSyncDir, 'Sync_Dir', 'LocalDir',       'Local_Dir'       ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileSyncDir, 'Sync_Dir', 'TargetLocation', 'Target_Location' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileSyncDir, 'Sync_Dir', 'SourceReader',   'Source_Reader'   ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileSyncDir, 'Sync_Dir', 'TargetReader',   'Target_Reader'   ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiFileSyncDirs, TtiFileSyncDir  ) ;

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileNameFilter, 'File_Name_Filter', 'OID', 'OID', [pktDB] ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileNameFilter, 'File_Name_Filter', 'FilterTypeAsString', 'Filter_Type'   ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping( TtiFileNameFilter, 'File_Name_Filter', 'WildCard', 'Wild_Card' ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection( TtiFileNameFilters, TtiFileNameFilter  ) ;
end;

procedure CreateTables;
var
  lMD : TtiDBMetaDataTable;
begin
  lMD :=TtiDBMetaDataTable.Create;
  try
    lMD.AddInstance('oid', qfkString, 36);
    lMD.AddInstance('Local_Dir', qfkString, 255);
    lMD.AddInstance('Target_Location', qfkString, 255);
    lMD.AddInstance('Source_Reader', qfkString, 20);
    lMD.AddInstance('Target_Reader', qfkString, 20);
    lMD.Name := 'sync_dir';
    gTIOPFManager.CreateTable(lMD);
  finally
    lMD.Free;
  end;

  lMD :=TtiDBMetaDataTable.Create;
  try
    lMD.AddInstance('oid', qfkString, 36);
    lMD.AddInstance('Filter_Type', qfkString, 255);
    lMD.AddInstance('Wild_Card', qfkString, 255);
    lMD.Name := 'File_Name_Filter';
    gTIOPFManager.CreateTable(lMD);
  finally
    lMD.Free;
  end;

end ;

{ TtiFileSyncDirs }

procedure TtiFileSyncDirs.Add(pObject: TtiFileSyncDir;
  pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

procedure TtiFileSyncDirs.Assign(pSource: TtiFileSyncDirs);
var
  i : integer ;
begin
  Clear ;
  for i := 0 to pSource.Count - 1 do
    Add( pSource.Items[i].Clone ) ;
end;

function TtiFileSyncDirs.Clone: TtiFileSyncDirs;
begin
  result := TtiFileSyncDirs( inherited Clone ) ;
end;

function TtiFileSyncDirs.GetItems(i: integer): TtiFileSyncDir;
begin
  result := TtiFileSyncDir( inherited GetItems( i )) ;
end;

(*
procedure TtiFileSyncSetups.Read;
var
  lFileSyncSetup  : TtiFileSyncSetup ;
begin

{
  lFileSyncSetup  := TtiFileSyncSetup.Create ;
  lFileSyncSetup.LocalDir := 'c:\temp\SyncFrom' ;
  lFileSyncSetup.SourceReader := cgsDiskFiles ;
  lFileSyncSetup.TargetLocation := 'c:\temp\SyncTo' ;
  lFileSyncSetup.TargetReader := cgsDiskFiles ;
  Add( lFileSyncSetup ) ;
}

  lFileSyncSetup  := TtiFileSyncSetup.Create ;
  lFileSyncSetup.LocalDir := 'c:\techinsite' ;
  lFileSyncSetup.SourceReader := cgsDiskFiles ;
  lFileSyncSetup.TargetLocation := 'h:\backup\techinsite' ;
  lFileSyncSetup.TargetReader := cgsDiskFiles ;
  Add( lFileSyncSetup ) ;

  lFileSyncSetup  := TtiFileSyncSetup.Create ;
  lFileSyncSetup.LocalDir := 'd:\Nemmco' ;
  lFileSyncSetup.SourceReader := cgsDiskFiles ;
  lFileSyncSetup.TargetLocation := 'h:\backup\Nemmco' ;
  lFileSyncSetup.TargetReader := cgsDiskFiles ;
  Add( lFileSyncSetup ) ;

end;

procedure TtiFileSyncSetups.Save;
begin
//  inherited;

end;
*)

procedure TtiFileSyncDirs.SetItems(i: integer; const Value: TtiFileSyncDir);
begin
  inherited SetItems( i, Value ) ;
end;

{ TtiFileSyncDir }

procedure TtiFileSyncDir.Assign(pSource: TtiFileSyncDir);
begin
  inherited Assign( pSource ) ;
end;

function TtiFileSyncDir.Clone: TtiFileSyncDir;
begin
  result := TtiFileSyncDir( inherited Clone ) ;
end;

function TtiFileSyncDir.GetOwner: TtiFileSyncDirs;
begin
  result := TtiFileSyncDirs( inherited GetOwner ) ;
end;

procedure TtiFileSyncDir.SetLocalDir(const Value: TFileName);
begin
  FLocalDir := Trim(Value);
end;

procedure TtiFileSyncDir.SetOwner(const Value: TtiFileSyncDirs);
begin
  inherited SetOwner( Value ) ;
end;



procedure TtiFileSyncDir.SetTargetLocation(const Value: TFileName);
begin
  FTargetLocation := Trim(Value);
end;

{ TtiFileSyncSetup }

procedure TtiFileSyncSetup.AssignClassProps(pSource: TtiObject);
begin
  Assert( pSource is TtiFileSyncSetup, 'pSource not a TtiFileSyncSetup' ) ;
  FileSyncDirs.Assign( TtiFileSyncSetup( pSource ).FileSyncDirs ) ;
  FFileNameFilters.Assign(TtiFileSyncSetup( pSource ).FileNameFilters ) ;
end;

function TtiFileSyncSetup.Clone: TtiFileSyncSetup;
begin
  result := TtiFileSyncSetup( inherited Clone ) ;
end;

constructor TtiFileSyncSetup.create;
begin
  inherited;
  FFileSyncDirs:= TtiFileSyncDirs.Create;
  FFileNameFilters := TtiFileNameFilters.Create ;
end;

destructor TtiFileSyncSetup.destroy;
begin
  FFileSyncDirs.Free;
  FFileNameFilters.Free;
  inherited;
end;

{ TtiFileNameFilters }

procedure TtiFileNameFilters.Add(pObject: TtiFileNameFilter;
  pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

procedure TtiFileNameFilters.AddFilter(pFilterType: TtiFileFilterType; pWildCard: TFileName);
var
  lData : TtiFileNameFilter ;
begin
  lData := TtiFileNameFilter.Create ;
  lData.FilterType := pFilterType ;
  lData.WildCard   := pWildCard ;
  Add( lData ) ;
end;

function TtiFileNameFilters.Clone: TtiFileNameFilters;
begin
  result := TtiFileNameFilters( inherited Clone ) 
end;

function TtiFileNameFilters.GetItems(i: integer): TtiFileNameFilter;
begin
  result := TtiFileNameFilter( inherited GetItems( i )) ;
end;

procedure TtiFileNameFilters.SetItems(i: integer;
  const Value: TtiFileNameFilter);
begin
  inherited SetItems( i, Value ) ;
end;

{ TtiFileNameFilter }

function TtiFileNameFilter.Clone: TtiFileNameFilter;
begin
  result := TtiFileNameFilter( inherited Clone ) ;
end;

constructor TtiFileNameFilter.create;
begin
  inherited;
  FFilterType := fftUnassigned ;
end;

function TtiFileNameFilter.GetFilterTypeAsString: string;
begin
  result := caFilterTypes[FFilterType];
end;

function TtiFileNameFilter.GetOwner: TtiFileNameFilters;
begin
  result := TtiFileNameFilters( inherited GetOwner ) ;
end;

procedure TtiFileNameFilter.SetFilterTypeAsString(const Value: string);
var
  i : TtiFileFilterType ;
begin
  for i := Low( TtiFileFilterType ) to High( TtiFileFilterType ) do
    if SameText( caFilterTypes[i], Value ) then
    begin
      FFilterType := i ;
      Break ; //==>
    end; 
end;

procedure TtiFileNameFilter.SetOwner(const Value: TtiFileNameFilters);
begin
  inherited SetOwner( Value ) ;
end;

end.

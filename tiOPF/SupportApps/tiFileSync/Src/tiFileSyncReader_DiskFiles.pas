unit tiFileSyncReader_DiskFiles;

interface
uses
  tiFileName_BOM
  ,tiFileSyncReader_Abs
  ,tiFileSyncSetup_BOM
  ;

type

  TFileSyncReaderDiskFiles = class( TtiFileSyncReaderAbs )
  private
  public
    procedure ReadFileIndex(  pFileNames : TtiFileNames ;
                              pSourceFileNameFilters : TtiFileNameFilters =  nil ) ; override ;
    procedure ReadPathIndex(  pPathNames : TtiPathNames ;
                              pSourceFileNameFilters : TtiFileNameFilters = nil ) ; override ;
    procedure CreatePath(     pPathName  : TtiPathName   ) ; override ;
    procedure DeletePath(     pPathName  : TtiPathName   ) ; override ;
    procedure ReadFileData(   pFileName  : TtiFileName  ) ; override ;
    procedure WriteFileData(  pFileName  : TtiFileName  ) ; override ;
    procedure DeleteFileData( pFileName  : TtiFileName  ) ; override ;
    procedure WriteIndex(     pFileNames : TtiFileNames ) ; override ;
  end ;

implementation
uses
  Classes
  ,cFileSync
  ,tiUtils
  ,SysUtils
  ,FileCtrl
  ,tiLog
  ;

{ TFileSyncReaderDiskFiles }

procedure TFileSyncReaderDiskFiles.CreatePath(pPathName: TtiPathName);
begin
//  try
    if not DirectoryExists( pPathName.Path ) then
      ForceDirectories( pPathName.Path ) ;
//  except
//    on e:exception do
//      tiFmtException( e, ClassName, 'CreatePath' ) ;
//  end ;
end;

procedure TFileSyncReaderDiskFiles.DeleteFileData(pFileName: TtiFileName);
begin
//  try
    if tiIsFileReadOnly( pFileName.PathAndName ) then
      tiSetFileReadOnly( pFileName.PathAndName, false ) ;
    if not SysUtils.DeleteFile( pFileName.PathAndName ) then
      raise exception.CreateFmt( 'Can not delete %s', [pFileName.PathAndName ]);
//  except
//    on e:exception do
//      tiFmtException( e, ClassName, 'DeleteFileData' ) ;
//  end ;
end;

procedure TFileSyncReaderDiskFiles.DeletePath(pPathName: TtiPathName);
begin
//  try
    if not tiForceRemoveDir( pPathName.Path ) then
      raise exception.CreateFmt( 'Can not delete %s', [pPathName.Path ]);
//  except
//    on e:exception do
//      tiFmtException( e, ClassName, 'DeletePath' ) ;
//  end ;
end;

procedure TFileSyncReaderDiskFiles.ReadFileData(pFileName: TtiFileName);
begin
//  try
    pFileName.Data.LoadFromFile( pFileName.PathAndName ) ;
//  except
//    on e:exception do
//      tiFmtException( e, ClassName, 'ReadFileData' ) ;
//  end ;
end;

procedure TFileSyncReaderDiskFiles.ReadFileIndex( pFileNames: TtiFileNames ;
                                                  pSourceFileNameFilters : TtiFileNameFilters =  nil );
  procedure _IncludeFiles( pFileNames : TtiFileNames ;
                           pStartDir : TFileName ;
                           pWildCard : TFileName  ) ;
  var
    lsl : TStringList ;
    i : integer ;
  begin
    lsl := TStringList.Create ;
    try
      tiFilesToStringList( pStartDir,
                           pWildCard,
                           lsl,
                           pFileNames.Recurse ) ;
      for i := 0 to lsl.Count-1 do
        pFileNames.Add( TtiFileName.CreateExt( lsl.Strings[i] ) ) ;
    finally
      lsl.Free ;
    end ;
  end;

  procedure _ReadDateSize( pFileNames : TtiFileNames ) ;
  var
    pDateTime : TDateTime ;
    piFileSize : integer ;
    i : integer ;
  begin
    for i := 0 to pFileNames.Count - 1 do
    begin
      tiReadFileDateSize( pFileNames.Items[i].PathAndName,
                          pDateTime,
                          piFileSize ) ;
      pFileNames.Items[i].Date := pDateTime ;
      pFileNames.Items[i].Size := piFileSize ;
    end ;
  end ;

  procedure _ExcludeFiles( pFileNames : TtiFileNames ;
                           pStartDir : TFileName ;
                           pWildCard : TFileName  ) ;
  var
    i : integer ;
  begin
    for i := pFileNames.Count - 1 downto 0 do
      if tiWildCardMatch( pFileNames.Items[i].NameFull,
                          pWildCard ) then
        pFileNames.Delete( i ) ;
  end ;
var
  i : integer ;
  lStartDir : string ;
begin
  pFileNames.Clear ;
  lStartDir := pFileNames.StartDir ;
  LogArray(['ReadFileIndex', lStartDir]);

  // Read files to include
  if (pSourceFileNameFilters = nil) or
     (pSourceFileNameFilters.CountNotDeleted = 0 ) then
    _IncludeFiles( pFileNames, lStartDir, '*.*' )
  else
  begin
    for i := 0 to pSourceFileNameFilters.Count - 1 do
      if pSourceFileNameFilters.Items[i].FilterType = fftInclude then
        _IncludeFiles( pFileNames,
                       lStartDir,
                       pSourceFileNameFilters.Items[i].WildCard ) ;
    // Remove files to exclude
    for i := 0 to pSourceFileNameFilters.Count - 1 do
      if pSourceFileNameFilters.Items[i].FilterType = fftExclude then
        _ExcludeFiles( pFileNames,
                       lStartDir,
                       pSourceFileNameFilters.Items[i].WildCard ) ;
  end;

  _ReadDateSize( pFileNames ) ;

end;

procedure TFileSyncReaderDiskFiles.ReadPathIndex(pPathNames: TtiPathNames;
  pSourceFileNameFilters: TtiFileNameFilters);
  procedure _IncludePaths( pPathNames : TtiPathNames ;
                           pStartDir : TFileName ) ;
  var
    lsl : TStringList ;
    i : integer ;
  begin
    lsl := TStringList.Create ;
    try
      tiDirectoryTreeToStringList( pStartDir,
                                   lsl,
                                   pPathNames.Recurse ) ;
      for i := 0 to lsl.Count-1 do
        pPathNames.Add( TtiPathName.CreateExt( lsl.Strings[i] ) ) ;
    finally
      lsl.Free ;
    end ;
  end;
var
  lStartDir: string ;
begin
  pPathNames.Clear ;
  lStartDir := pPathNames.StartDir;
  LogArray(['ReadPathIndex', lStartDir]);
  
  // Read files to include
  if pSourceFileNameFilters = nil then
    _IncludePaths( pPathNames, lStartDir)
  else
     _IncludePaths( pPathNames,
                    lStartDir) ;
end;

procedure TFileSyncReaderDiskFiles.WriteFileData(pFileName: TtiFileName);
begin
//  try
    if not DirectoryExists( pFileName.Path ) then
      ForceDirectories( pFileName.Path ) ;
    pFileName.Data.SaveToFile( pFileName.PathAndName ) ;
    tiSetFileDate( pFileName.PathAndName, pFileName.Date ) ;
//  except
//    on e:exception do
//      tiFmtException( e, ClassName, 'WriteFileData' ) ;
//  end ;
end;

procedure TFileSyncReaderDiskFiles.WriteIndex(pFileNames: TtiFileNames);
begin
  // Do nothing
end;

initialization

  gFileSyncReaderFactory.RegisterClass( cgsDiskFiles, TFileSyncReaderDiskFiles ) ;


end.

unit tiFileSyncReader_DiskFiles;

interface

{$I tiDefines.inc}

uses
  tiFileName_BOM
  ,tiFileSyncReader_Abs
  ,tiFileSyncSetup_BOM
 ;

const
  // On a big file, the CRC check will be very slow, so we only check the first 'cCountBytesToCRC'
  // This introduces a risk that there is a difference between two files that is not detected, but
  // FileSize is also checked so the probability of this will be low.
  // This value should really be controlled by what ever's calling the TFileSyncReader so it can be
  cCountBytesToCRC = 1024;

type

  TFileSyncReaderDiskFiles = class(TtiFileSyncReaderAbs)
  private
    procedure ReadFileFingerPrints(AFileNames: TtiFileNames);
    procedure ReadFileFingerPrint(AFileName: TtiFileName);
  public
    procedure ReadFileIndex( pFileNames : TtiFileNames;
                              pSourceFileNameFilters : TtiFileNameFilters =  nil); override;
    procedure ReadPathIndex( pPathNames : TtiPathNames;
                              pSourceFileNameFilters : TtiFileNameFilters = nil); override;
    procedure CreatePath(    pPathName  : TtiPathName  ); override;
    procedure DeletePath(    pPathName  : TtiPathName  ); override;
    procedure ReadFileData(  pFileName  : TtiFileName  ); override;
    procedure WriteFileData( pFileName  : TtiFileName ); override;
    procedure DeleteFileData(pFileName  : TtiFileName ); override;
    procedure WriteIndex(    pFileNames : TtiFileNames); override;
  end;

implementation
uses
   tiUtils
  ,tiObject
  ,tiCRC32
  ,tiConstants
  ,Classes
  ,Windows
  ,SysUtils
  ,FileCtrl
  ,cFileSync
 ;

{ TFileSyncReaderDiskFiles }

procedure TFileSyncReaderDiskFiles.CreatePath(pPathName: TtiPathName);
begin
  if not DirectoryExists(pPathName.Path) then
    ForceDirectories(pPathName.Path);
end;

procedure TFileSyncReaderDiskFiles.DeleteFileData(pFileName: TtiFileName);
begin
  if tiIsFileReadOnly(pFileName.PathAndName) then
    tiSetFileReadOnly(pFileName.PathAndName, false);
  if not SysUtils.DeleteFile(pFileName.PathAndName) then
    raise exception.CreateFmt('Can not delete %s', [pFileName.PathAndName ]);
end;

procedure TFileSyncReaderDiskFiles.DeletePath(pPathName: TtiPathName);
begin
  if not tiForceRemoveDir(pPathName.Path) then
    raise exception.CreateFmt('Can not delete %s', [pPathName.Path ]);
end;

procedure TFileSyncReaderDiskFiles.ReadFileData(pFileName: TtiFileName);
begin
  pFileName.Data.LoadFromFile(pFileName.PathAndName);
end;

procedure TFileSyncReaderDiskFiles.ReadFileFingerPrint(AFileName: TtiFileName);
var
  LStream: TFileStream;
begin
  Assert(AFileName.TestValid, CTIErrorInvalidObject);
  LStream := TFileStream.Create(AFileName.PathAndName, fmOpenRead or fmShareDenyWrite);
  try
    AFileName.Size := LStream.Size;
    AFileName.CRC  := tiCRC32FromStreamFirstNBytes(LStream, cCountBytesToCRC);
  finally
    LStream.Free;
  end;
  AFileName.Date := tiReadFileDate(AFileName.PathAndName);
end;

procedure TFileSyncReaderDiskFiles.ReadFileFingerPrints(AFileNames : TtiFileNames);
var
  i: Integer;
begin
  for i := 0 to AFileNames.Count - 1 do
    ReadFileFingerPrint(AFileNames.Items[i]);
end;

procedure TFileSyncReaderDiskFiles.ReadFileIndex(pFileNames: TtiFileNames;
                                                  pSourceFileNameFilters : TtiFileNameFilters =  nil);
  procedure _IncludeFiles(pFileNames : TtiFileNames;
                           pStartDir : TFileName;
                           pWildCard : TFileName );
  var
    lsl : TStringList;
    i : integer;
  begin
    lsl := TStringList.Create;
    try
      tiFilesToStringList(pStartDir,
                           pWildCard,
                           lsl,
                           pFileNames.Recurse);
      for i := 0 to lsl.Count-1 do
        pFileNames.Add(TtiFileName.CreateExt(lsl.Strings[i]));
    finally
      lsl.Free;
    end;
  end;

  procedure _ExcludeFiles(pFileNames : TtiFileNames;
                           pStartDir : TFileName;
                           pWildCard : TFileName );
  var
    i : integer;
  begin
    for i := pFileNames.Count - 1 downto 0 do
      if tiWildCardMatch(pFileNames.Items[i].NameFull,
                          pWildCard) then
        pFileNames.Delete(i);
  end;
var
  i : integer;
  lStartDir : string;
begin
  lStartDir := pFileNames.StartDir;

  // Read files to include
  if (pSourceFileNameFilters = nil) or
     (pSourceFileNameFilters.CountNotDeleted = 0) then
    _IncludeFiles(pFileNames, lStartDir, '*.*')
  else
  begin
    for i := 0 to pSourceFileNameFilters.Count - 1 do
      if pSourceFileNameFilters.Items[i].FilterType = fftInclude then
        _IncludeFiles(pFileNames,
                       lStartDir,
                       pSourceFileNameFilters.Items[i].WildCard);
    // Remove files to exclude
    for i := 0 to pSourceFileNameFilters.Count - 1 do
      if pSourceFileNameFilters.Items[i].FilterType = fftExclude then
        _ExcludeFiles(pFileNames,
                       lStartDir,
                       pSourceFileNameFilters.Items[i].WildCard);
  end;

  ReadFileFingerPrints(pFileNames);

end;

procedure TFileSyncReaderDiskFiles.ReadPathIndex(pPathNames: TtiPathNames;
  pSourceFileNameFilters: TtiFileNameFilters);
  procedure _IncludePaths(pPathNames : TtiPathNames;
                           pStartDir : TFileName);
  var
    lsl : TStringList;
    i : integer;
  begin
    lsl := TStringList.Create;
    try
      tiDirectoryTreeToStringList(pStartDir,
                                   lsl,
                                   pPathNames.Recurse);
      for i := 0 to lsl.Count-1 do
        pPathNames.Add(TtiPathName.CreateExt(lsl.Strings[i]));
    finally
      lsl.Free;
    end;
  end;
var
  lStartDir: string;
begin
  lStartDir := pPathNames.StartDir;

  // Read files to include
  if pSourceFileNameFilters = nil then
    _IncludePaths(pPathNames, lStartDir)
  else
     _IncludePaths(pPathNames,
                    lStartDir);
end;

procedure TFileSyncReaderDiskFiles.WriteFileData(pFileName: TtiFileName);
begin
  if not DirectoryExists(pFileName.Path) then
    ForceDirectories(pFileName.Path);
  pFileName.Data.SaveToFile(pFileName.PathAndName);
  tiSetFileDate(pFileName.PathAndName, pFileName.Date);
end;

procedure TFileSyncReaderDiskFiles.WriteIndex(pFileNames: TtiFileNames);
begin
  // Do nothing
end;

initialization

  gFileSyncReaderFactory.RegisterClass(cgsDiskFiles, TFileSyncReaderDiskFiles);

end.

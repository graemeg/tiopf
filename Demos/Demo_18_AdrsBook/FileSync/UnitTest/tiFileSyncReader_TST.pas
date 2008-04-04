unit tiFileSyncReader_TST;

interface
uses
  TestFramework
  ,tiFileSyncReader_Abs
  ,tiFileSyncReader_Remote_Svr
 ;

const
  cHTTPPort = 81;

type

  TTestFileReaderAbs = class(TTestCase)
  private
    FReaderName  : string;
    FReaderClass : TtiFileSyncReaderClass;
  protected
    FRootLocal: string;
    FRootReader: string;
    FParams: string;
    procedure Setup; override;
  published
    procedure FileSyncReader_Factory;
    procedure FileSyncReader_ReadPathIndex;
    procedure FileSyncReader_ReadFileIndex;
    procedure FileSyncReader_CreatePath;
    procedure FileSyncReader_DeletePath;
    procedure FileSyncReader_ReadFileData;
    procedure FileSyncReader_WriteFileData;
    procedure FileSyncReader_DeleteFileData;
  public
    procedure FileSyncReader_WriteIndex;
  end;

  TTestFileReaderDiskFiles = class(TTestFileReaderAbs)
  protected
    procedure Setup; override;
  public
    constructor Create(MethodName: string); override;
  end;

  TTestFileReaderRemote = class(TTestFileReaderAbs)
  private
    FFileSyncServer : TtiFileSyncRemoteServer;
  protected
    procedure   Setup; override;
    procedure   TearDown; override;
  public
    constructor Create(MethodName: string); override;
    destructor  Destroy; override;
  end;

procedure RegisterTests;

implementation
uses
   tiConstants
  ,tiUtils
  ,tiFileSyncReader_DiskFiles
  ,tiFileSyncReader_Remote
  ,tiFileName_BOM
  ,tiCRC32
  ,cFileSync
  ,FileCtrl
  ,SysUtils
 ;

procedure RegisterTests;
begin
  RegisterTest(TTestFileReaderDiskFiles.Suite);
  RegisterTest(TTestFileReaderRemote.Suite);
end;

{ TTestFileReaderAbs }

procedure TTestFileReaderAbs.FileSyncReader_CreatePath;
var
  lReader : TtiFileSyncReaderAbs;
  lPathNames : TtiPathNames;
  lPathName  : TtiPathName ;
begin
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lPathNames := TtiPathNames.Create;
    try
      lPathNames.StartDir := FRootReader;
      Check(Not DirectoryExists(FRootLocal + '\Dir1'), '#1');
      lPathName  := TtiPathName.Create ;
      lPathName.Path := FRootReader + '\Dir1';
      lPathNames.Add(lPathName);
      lReader.CreatePath(lPathName);
      Check(DirectoryExists(FRootLocal + '\Dir1'), '#2');

      Check(Not DirectoryExists(FRootLocal + '\Dir2\Dir3'), '#3');
      lPathName  := TtiPathName.Create ;
      lPathName.Path := FRootReader + '\Dir2\Dir3';
      lPathNames.Add(lPathName);
      lReader.CreatePath(lPathName);
      Check(DirectoryExists(FRootLocal + '\Dir2\Dir3'), '#4');

    finally
      lPathNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_DeleteFileData;
var
  lReader : TtiFileSyncReaderAbs;
  lFileNames : TtiFileNames;
  lFileName  : TtiFileName;
begin
  ForceDirectories(FRootLocal);
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lFileNames := TtiFileNames.Create;
    try
      lFileNames.StartDir := FRootReader;

      lFileName  := TtiFileName.Create;
      lFileName.PathAndName := FRootReader + '\File1.txt';
      lFileNames.Add(lFileName);
      tiStringToFile('test', FRootLocal + '\File1.txt');
      Check(FileExists(FRootLocal + '\File1.txt'), '#1');
      lReader.DeleteFileData(lFileName);
      Check(not FileExists(FRootLocal + '\File1.txt'), '#2');

    finally
      lFileNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_DeletePath;
var
  lReader : TtiFileSyncReaderAbs;
  lPathNames : TtiPathNames;
  lPathName  : TtiPathName ;
begin
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lPathNames := TtiPathNames.Create;
    try
      lPathNames.StartDir := FRootReader;

      ForceDirectories(FRootLocal + '\Dir1');
      Check(DirectoryExists(FRootLocal + '\Dir1'), '#1');
      lPathName  := TtiPathName.Create ;
      lPathName.Path := FRootReader + '\Dir1';
      lPathNames.Add(lPathName);
      lReader.DeletePath(lPathName);
      Check(not DirectoryExists(FRootLocal + '\Dir1'), '#2');

      ForceDirectories(FRootLocal + '\Dir2\Dir3');
      Check(DirectoryExists(FRootLocal + '\Dir2\Dir3'), '#3');
      lPathName  := TtiPathName.Create ;
      lPathName.Path := FRootReader + '\Dir2\Dir3';
      lPathNames.Add(lPathName);
      lReader.DeletePath(lPathName);
      Check(not DirectoryExists(FRootLocal + '\Dir2\Dir3'), '#4');

      ForceDirectories(FRootLocal + '\Dir1');
      Check(DirectoryExists(FRootLocal + '\Dir1'), '#5');
      tiStringToFile('test', FRootLocal + '\Dir1\File1.txt');
      lPathName  := TtiPathName.Create ;
      lPathName.Path := FRootReader + '\Dir1';
      lPathNames.Add(lPathName);
      lReader.DeletePath(lPathName);
      Check(not DirectoryExists(FRootLocal + '\Dir1'), '#6');

    finally
      lPathNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_Factory;
var
  lFactory : TFileSyncReaderFactory;
  lFSR1 : TtiFileSyncReaderAbs;
  lFSR2 : TtiFileSyncReaderAbs;
begin
  lFactory := TFileSyncReaderFactory.Create;
  try
    lFactory.RegisterClass(FReaderName, FReaderClass);
    lFSR1 := lFactory.CreateInstance(FReaderName, FParams);
    try
      CheckNotNull(lFSR1);
      CheckIs(lFSR1, FReaderClass);
      lFSR2 := lFactory.CreateInstance(FReaderName, FParams);
      try
        CheckNotNull(lFSR2);
        CheckIs(lFSR2, FReaderClass);
        Check(lFSR1 <> lFSR2, 'Same object but should be different');
      finally
        lFSR2.Free;
      end;
    finally
      lFSR1.Free;
    end;
  finally
    lFactory.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_ReadFileData;
var
  lReader : TtiFileSyncReaderAbs;
  lFileNames : TtiFileNames;
  lFileName  : TtiFileName;
begin
  ForceDirectories(FRootLocal);
  tiStringToFile('test', FRootLocal + '\File1.txt');
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lFileNames := TtiFileNames.Create;
    try
      lFileNames.StartDir := FRootReader;

      lFileName  := TtiFileName.Create;
      lFileName.PathAndName := FRootReader + '\File1.txt';
      lFileNames.Add(lFileName);

      lReader.ReadFileData(lFileName);
      lFileName.Data.SaveToFile('c:\temp\temp.txt');
      CheckEquals('test', Trim(tiFileToString('c:\temp\temp.txt')));

    finally
      lFileNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_ReadFileIndex;
var
  lReader : TtiFileSyncReaderAbs;
  lFileNames : TtiFileNames;
  lNow : TDateTime;
begin
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lFileNames := TtiFileNames.Create;
    try

      lFileNames.StartDir := FRootReader;
      lReader.ReadFileIndex(lFileNames);
      CheckEquals(0, lFileNames.Count, '#1');

      ForceDirectories(FRootLocal);
      lReader.ReadFileIndex(lFileNames);
      CheckEquals(0, lFileNames.Count, '#2');

      tiStringToFile('test', FRootLocal + '\file1.txt');
      lNow := Now;
      lReader.ReadFileIndex(lFileNames);
      CheckEquals(1, lFileNames.Count, '#3');
      CheckEquals(FRootReader + '\file1.txt', lFileNames.Items[0].PathAndName, '#4');
      CheckEquals(lNow, lFileNames.Items[0].Date, cdtOneSecond*2, '#4a');
      CheckEquals(4,    lFileNames.Items[0].Size, '#4b');
      CheckEquals(tiGetFileCrc32(FRootLocal + '\file1.txt'), LFileNames.Items[0].CRC, '#4c');

      tiStringToFile('test', FRootLocal + '\file2.txt');
      lNow := Now;
      lReader.ReadFileIndex(lFileNames);
      CheckEquals(2, lFileNames.Count, '#5');
      CheckEquals(FRootReader + '\file2.txt', lFileNames.Items[1].PathAndName, '#6');
      CheckEquals(lNow, lFileNames.Items[1].Date, cdtOneSecond*2, '#6a');
      CheckEquals(4,    lFileNames.Items[1].Size, '#6b');
      CheckEquals(tiGetFileCrc32(FRootLocal + '\file2.txt'), LFileNames.Items[1].CRC, '#6c');

      lReader.ReadFileIndex(lFileNames);
      CheckEquals(2, lFileNames.Count, '#7');

      ForceDirectories(FRootLocal + '\Dir1');
      tiStringToFile('test', FRootLocal + '\Dir1\file2.txt');
      lNow := Now;
      lReader.ReadFileIndex(lFileNames);
      CheckEquals(3, lFileNames.Count, '#8');
      CheckEquals(FRootReader + '\Dir1\file2.txt', lFileNames.Items[2].PathAndName, '#9');
      CheckEquals(lNow, lFileNames.Items[0].Date, cdtOneSecond*2, '#9a');
      CheckEquals(4,    lFileNames.Items[0].Size, '#9b');
      CheckEquals(tiGetFileCrc32(FRootLocal + '\Dir1\file2.txt'), LFileNames.Items[0].CRC, '#9c');
    finally
      lFileNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_ReadPathIndex;
var
  lReader : TtiFileSyncReaderAbs;
  lPathNames : TtiPathNames;
begin
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lPathNames := TtiPathNames.Create;
    try
      lPathNames.StartDir := FRootReader;
      lReader.ReadPathIndex(lPathNames);
      CheckEquals(0, lPathNames.Count, '#1');

      ForceDirectories(FRootLocal);
      lPathNames.StartDir := FRootReader;
      lReader.ReadPathIndex(lPathNames);
      CheckEquals(1, lPathNames.Count, '#2');
      CheckEquals(FRootReader, lPathNames.Items[0].Path, '#3');

      ForceDirectories(FRootLocal + '\Dir1');
      lReader.ReadPathIndex(lPathNames);
      CheckEquals(2, lPathNames.Count, '#4');
      CheckEquals(FRootReader + '\Dir1', lPathNames.Items[1].Path, '#5');

      ForceDirectories(FRootLocal + '\Dir2');
      lReader.ReadPathIndex(lPathNames);
      CheckEquals(3, lPathNames.Count, '#6');
      CheckEquals(FRootReader + '\Dir2', lPathNames.Items[2].Path, '#7');

      ForceDirectories(FRootLocal + '\Dir2\DirA');
      lReader.ReadPathIndex(lPathNames);
      CheckEquals(4, lPathNames.Count, '#8');
      CheckEquals(FRootReader + '\Dir2\DirA', lPathNames.Items[3].Path, '#9');

    finally
      lPathNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_WriteFileData;
var
  lReader : TtiFileSyncReaderAbs;
  lFileNames : TtiFileNames;
  lFileName  : TtiFileName;
begin
  ForceDirectories(FRootLocal);
  lReader := FReaderClass.Create;
  try
    lReader.ParamsAsString:= FParams;
    lFileNames := TtiFileNames.Create;
    try
      lFileNames.StartDir := FRootReader;

      lFileName  := TtiFileName.Create;
      lFileName.PathAndName := FRootReader + '\File1.txt';
      lFileNames.Add(lFileName);
      tiStringToFile('test', FRootLocal + '\File1.txt');
      lFileName.Data.LoadFromFile(FRootLocal + '\File1.txt');
      SysUtils.DeleteFile(FRootLocal + '\File1.txt');

      lReader.WriteFileData(lFileName);
      CheckEquals('test', Trim(tiFileToString('c:\temp\temp.txt')));

    finally
      lFileNames.Free;
    end;
  finally
    lReader.Free;
  end;
end;

procedure TTestFileReaderAbs.FileSyncReader_WriteIndex;
begin
  Assert(false, 'Under construction');
end;

{ TTestFileReaderDiskFiles }

procedure TTestFileReaderAbs.Setup;
begin
  inherited;
  tiForceRemoveDir(FRootLocal);
end;

{ TTestFileReaderDiskFiles }

constructor TTestFileReaderDiskFiles.Create(MethodName: string);
begin
  inherited;
  FReaderName  := cgsDiskFiles;
  FReaderClass := TFileSyncReaderDiskFiles;
end;

procedure TTestFileReaderDiskFiles.Setup;
begin
  FRootReader := tiAddTrailingSlash(tiGetEXEPath) + 'test';;
  FRootLocal := FRootReader;
  FParams:= '';
  inherited;
end;

{ TTestFileReaderRemote }

constructor TTestFileReaderRemote.Create(MethodName: string);
begin
  inherited;
  FReaderName  := cgsRemote;
  FReaderClass := TFileSyncReaderRemote;
  FFileSyncServer := TtiFileSyncRemoteServer.Create(cHTTPPort)
end;

destructor TTestFileReaderRemote.Destroy;
begin
  FFileSyncServer.Destroy;
  inherited;
end;

procedure TTestFileReaderRemote.Setup;
begin
  FFileSyncServer.Active := True;
  FRootReader := 'test';;
  FRootLocal :=
    ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) + '.\' + FRootReader);
  FParams:= cHTTPURL + '=http://localhost:' + IntToStr(cHTTPPort);
  inherited;
end;

procedure TTestFileReaderRemote.TearDown;
begin
  inherited;
  FFileSyncServer.Active := False;
end;

end.

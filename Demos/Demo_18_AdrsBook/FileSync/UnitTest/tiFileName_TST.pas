unit tiFileName_TST;

interface
uses
  TestFramework
  ;

type

  TTestTIFileName = class( TTestCase )
  protected
  published
    procedure PathNames_AsXML;
    procedure PathName_AsXML;
    procedure FileNames_AsXML;
    procedure FileName_AsXML;

    procedure PathNames_FindLikeRootRemoved ;
    procedure PathNames_Clone  ;
    procedure PathName_NameProperties  ;
    procedure PathName_Clone  ;
    procedure PathName_CloneWithNewRootDir;

    procedure FileNames_FindLikeRootRemoved ;
    procedure FileName_NameProperties  ;
    procedure FileName_DataProperties  ;
    procedure FileName_Clone  ;
    procedure FileName_CloneWithNewRootDir;

  end ;

procedure RegisterTests ;

implementation
uses
  tiFileName_BOM
  ,tiUtils
  ,tiCRC32
  ,SysUtils
  ,FileCtrl
  ;

const
  cRoot = 'c:\temp' ;
    
procedure RegisterTests ;
begin
  RegisterTest( TTestTIFileName.Suite );
end ;

{ TTestTIFileName }

procedure TTestTIFileName.FileName_NameProperties;
var
  lFileNames : TtiFileNames ;
  lFileName  : TtiFileName ;
begin
  lFileNames := TtiFileNames.Create ;
  try
    lFileNames.StartDir := 'c:\temp' ;
    lFileName := TtiFileName.Create ;
    lFileNames.Add( lFileName ) ;

    lFileName.PathAndName := 'c:\temp\dir1\dir2\dir3\file.ext' ;
    CheckEquals( 'c:',                              lFileName.Drive ) ;
    CheckEquals( '\temp\dir1\dir2\dir3',            lFileName.Directory ) ;
    CheckEquals( 'c:\temp',                         lFileName.RootDir ) ;
    CheckEquals( '\dir1\dir2\dir3\file.ext',        lFileName.RootRemoved ) ;

    CheckEquals( 'file',                            lFileName.NameOnly ) ;
    CheckEquals( 'ext',                             lFileName.Ext ) ;
    CheckEquals( 'file.ext',                        lFileName.NameFull ) ;
    CheckEquals( 'c:\temp\dir1\dir2\dir3\file.ext', lFileName.PathAndName ) ;

  finally
    lFileNames.Free ;
  end ;

end;

procedure TTestTIFileName.PathName_NameProperties;
var
  lPathNames  : TtiPathNames ;
  lPathName   : TtiPathName ;
begin
  lPathNames := TtiPathNames.Create ;
  try
    lPathNames.StartDir := 'c:\temp' ;
    lPathName := TtiPathName.Create ;
    lPathName.Path := 'c:\temp\dir1\dir2\dir3' ;
    lPathNames.Add( lPathName ) ;

    CheckEquals( 'c:', lPathName.Drive ) ;
    CheckEquals( '\temp\dir1\dir2\dir3', lPathName.Directory ) ;
    CheckEquals( 'c:\temp', lPathName.RootDir ) ;
    CheckEquals( '\dir1\dir2\dir3', lPathName.RootRemoved ) ;

  finally
    lPathNames.Free ;
  end ;

end;

procedure TTestTIFileName.PathNames_FindLikeRootRemoved;
  procedure _Do(const pStartDir: string ;
                const pFNFound:   string ;
                const pFNNotFound: string);
  var
    lPathNames : TtiPathNames ;
    lPathName  : TtiPathName ;
    lPathName1 : TtiPathName ;
    lPathName2 : TtiPathName ;
  begin
    lPathNames := TtiPathNames.Create ;
    try
      lPathNames.StartDir := pStartDir ;

      lPathName := TtiPathName.Create ;
      lPathName.Path := pFNNotFound ;
      lPathNames.Add( lPathName ) ;

      lPathName1 := TtiPathName.Create ;
      lPathName1.Path := pFNFound ;
      lPathNames.Add( lPathName1 ) ;
      lPathName2 := lPathNames.FindLikeRootRemoved( lPathName ) ;
      CheckNotNull( lPathName2, 'TtiPathNames.FindLikeRootRemoved failed' ) ;
      CheckSame( lPathName, lPathName2, 'TtiFileNames.FindLikeRootRemoved failed' ) ;

    finally
      lPathNames.Free ;
    end ;
  end;
begin
  _Do('c:\hos','c:\hos\dir1\dir2\dir4', 'c:\hos\dir1\dir2\dir3');
  _Do('hos',   'hos\dir1\dir2\dir4',    'hos\dir1\dir2\dir3');
  _Do('..\hos','..\hos\dir1\dir2\dir4', '..\hos\dir1\dir2\dir3');
end ;

procedure TTestTIFileName.PathNames_Clone;
var
  lPathNamesFrom : TtiPathNames ;
  lPathName1 : TtiPathName ;
  lPathName2 : TtiPathName ;
  lPathNamesTo : TtiPathNames ;
begin
  lPathNamesFrom := TtiPathNames.Create ;
  try
    lPathNamesFrom.StartDir := 'c:\hos' ;
    lPathName1 := TtiPathName.Create ;
    lPathName1.Path := 'c:\hos\dir1\dir2\dir3' ;
    lPathNamesFrom.Add( lPathName1 ) ;
    lPathName2 := TtiPathName.Create ;
    lPathName2.Path := 'c:\hos\dir1\dir2\dir4' ;
    lPathNamesFrom.Add( lPathName2 ) ;

    lPathNamesTo := lPathNamesFrom.Clone ;
    try
      CheckEquals( lPathNamesFrom.StartDir, lPathNamesTo.StartDir, 'StartDir' ) ;
      CheckEquals( 2, lPathNamesTo.Count, 'Count' ) ;
      CheckEquals( lPathName1.Path, lPathNamesTo.Items[0].Path, 'Path #1');
      CheckEquals( lPathName2.Path, lPathNamesTo.Items[1].Path, 'Path #2');
    finally
      lPathNamesTo.Free;
    end;
  finally
    lPathNamesFrom.Free ;
  end ;
end;

procedure TTestTIFileName.FileName_DataProperties;
var
  lFileNames : TtiFileNames ;
  lFileName  : TtiFileName ;
  lDate : TDateTime ;
const
  cFileName  = 'c:\temp\dir1\dir2\dir3\temp.txt' ;
  cTestString = 'testing' ;
begin
  lDate := Now ;
  lFileNames := TtiFileNames.Create ;
  try
    lFileNames.StartDir := 'c:\temp' ;
    lFileName := TtiFileName.Create ;
    lFileNames.Add( lFileName ) ;
    lFileName.PathAndName := cFileName ;
    ForceDirectories( ExtractFilePath( cFileName )) ;
    tiStringToFile( cTestString, cFileName ) ;
    lFileName.Data.LoadFromFile( cFileName ) ;
    lFileName.Date := lDate ;
    lFileName.Size := Length( cTestString ) ;

    CheckEquals( lDate, lFileName.Date, 'Date') ;
    CheckEquals( Length( cTestString ), lFileName.Size, 'Size' ) ;
    SysUtils.DeleteFile( cFileName ) ;
    lFileName.Data.SaveToFile( cFileName ) ;
    CheckEquals( cTestString, tiFileToString( cFileName ), 'Data' ) ;
    
  finally
    lFileNames.Free ;
  end ;
end;

procedure TTestTIFileName.FileNames_FindLikeRootRemoved;
  procedure _Do(const pStartDir: string;
                const pFNFound: string;
                const pFNNotFound: string);
  var
    lFileNames : TtiFileNames ;
    lFileName  : TtiFileName ;
    lFileName1 : TtiFileName ;
    lFileName2 : TtiFileName ;
  begin
    lFileNames := TtiFileNames.Create ;
    try
      lFileNames.StartDir := pStartDir ;
      lFileName := TtiFileName.Create ;
      lFileNames.Add( lFileName ) ;
      lFileName.PathAndName := pFNNotFound ;
      lFileName1 := TtiFileName.Create ;
      lFileNames.Add( lFileName1 ) ;
      lFileName1.PathAndName := pFNFound ;
      lFileName2 := lFileNames.FindLikeRootRemoved( lFileName ) ;
      CheckNotNull( lFileName2, 'TtiFileNames.FindLikeRootRemoved failed' ) ;
      CheckSame( lFileName, lFileName2, 'TtiFileNames.FindLikeRootRemoved failed' ) ;
    finally
      lFileNames.Free ;
    end ;
  end;
begin
  _Do('c:\hos','c:\hos\dir1\dir2\dir3\file1.ext','c:\hos\dir1\dir2\dir3\file.ext');
end ;

procedure TTestTIFileName.FileName_Clone;
var
  lFileNames    : TtiFileNames ;
  lFileNameFrom : TtiFileName ;
  lFileNameTo   : TtiFileName ;
const
  cFileName  = 'c:\temp\dir1\dir2\dir3\temp.txt' ;
  cTestString = 'testing' ;
begin
  lFileNames := TtiFileNames.Create ;
  try
    lFileNames.StartDir := 'c:\temp' ;
    lFileNameFrom := TtiFileName.Create ;
    lFileNames.Add( lFileNameFrom ) ;
    lFileNameFrom.PathAndName := cFileName ;
    ForceDirectories( ExtractFilePath( cFileName )) ;
    tiStringToFile( cTestString, cFileName ) ;
    lFileNameFrom.Data.LoadFromFile( cFileName ) ;
    lFileNameFrom.Date := Now ;
    lFileNameFrom.Size := Length( cTestString ) ;
    lFileNameFrom.CRC  := tiStringToCRC32(cTestString);

    lFileNameTo := lFileNameFrom.Clone ;
    try
      CheckEquals( lFileNameFrom.PathAndName, lFileNameTo.PathAndName, 'PathAndName' ) ;
      CheckEquals( lFileNameFrom.Date, lFileNameTo.Date, 'Date' ) ;
      CheckEquals( lFileNameFrom.Size, lFileNameTo.Size, 'Size' ) ;
      SysUtils.DeleteFile( cFileName ) ;
      lFileNameTo.Data.SaveToFile( cFileName ) ;
      CheckEquals( cTestString, tiFileToString( cFileName ), 'Data' ) ;
      CheckEquals( tiStringToCRC32(cTestString), LFileNameTo.CRC, 'CRC' ) ;

    finally
      lFileNameTo.Free ;
    end;
  finally
    lFileNames.Free ;
  end ;

end;

procedure TTestTIFileName.PathName_Clone;
var
  lPathNames : TtiPathNames ;
  lPathNameFrom : TtiPathName ;
  lPathNameTo : TtiPathName ;
begin
  lPathNames := TtiPathNames.Create ;
  try
    lPathNames.StartDir := 'c:\hos' ;
    lPathNameFrom := TtiPathName.Create ;
    lPathNameFrom.Path := 'c:\hos\dir1\dir2\dir3' ;
    lPathNames.Add( lPathNameFrom ) ;

    lPathNameTo := lPathNameFrom.Clone ;
    try
      CheckEquals( lPathNameFrom.Path, lPathNameTo.Path, 'Path' ) ;
      CheckSame( lPathNameFrom.Owner, lPathNameTo.Owner, 'Owner' ) ;
      CheckEquals( -1, lPathNames.IndexOf(lPathNameTo), 'IndexOf' ) ;
    finally
      lPathNameTo.Free;
    end;
  finally
    lPathNames.Free ;
  end ;
end;


procedure TTestTIFileName.FileNames_AsXML;
  procedure _AddFile(const pFileNames: TtiFileNames; pIndex: Integer);
  var
    lFileName: TtiFileName;
  begin
    lFileName:= TtiFileName.Create;
    lFileName.PathAndName := 'c:\temp\testfile' + IntToStr(pIndex) + '.txt';
    lFileName.Size := 100 + pIndex ;
    lFileName.Date := Date + pIndex ;
    pFileNames.Add(lFileName);
  end ;

  procedure _CheckFile(const pFile: TtiFileName; pIndex: Integer);
  begin
    CheckEquals('c:\temp\testfile' + IntToStr(pIndex) + '.txt', pFile.PathAndName, 'PathAndName');
    CheckEquals(100 + pIndex, pFile.Size, 'Size');
    CheckEquals(Date + pIndex, pFile.Date, 'Date');
  end;
var
  lFileNames: TtiFileNames;
  ls : string ;
begin
  lFileNames:= TtiFileNames.Create ;
  try
    lFileNames.StartDir := cRoot ;
    _AddFile(lFileNames, 1);
    _AddFile(lFileNames, 2);
    _AddFile(lFileNames, 3);
    _AddFile(lFileNames, 4);
    _AddFile(lFileNames, 5);
    ls := lFileNames.AsXML;
  finally
    lFileNames.Free;
  end ;

  lFileNames:= TtiFileNames.Create ;
  try
    lFileNames.AsXML := ls ;
    CheckEquals(cRoot, lFileNames.StartDir, 'StartDir');
    CheckEquals(5, lFileNames.Count, 'Count');
    _CheckFile(lFileNames.Items[0], 1);
    _CheckFile(lFileNames.Items[1], 2);
    _CheckFile(lFileNames.Items[2], 3);
    _CheckFile(lFileNames.Items[3], 4);
    _CheckFile(lFileNames.Items[4], 5);
  finally
    lFileNames.Free;
  end ;
end;

procedure TTestTIFileName.PathNames_AsXML;
var
  lPathNames: TtiPathNames;
  lPathName: TtiPathName;
  ls : string ;
begin
  lPathNames:= TtiPathNames.Create ;
  try
    lPathNames.StartDir := cRoot;
    lPathNames.Recurse  := False ;
    lPathName:= TtiPathName.Create;
    lPathName.Path := 'c:\temp\dir1' ;
    lPathNames.Add(lPathName);
    lPathName:= TtiPathName.Create;
    lPathName.Path := 'c:\temp\dir2' ;
    lPathNames.Add(lPathName);
    lPathName:= TtiPathName.Create;
    lPathName.Path := 'c:\temp\dir3' ;
    lPathNames.Add(lPathName);
    ls := lPathNames.AsXML;
  finally
    lPathNames.Free;
  end ;

  lPathNames:= TtiPathNames.Create ;
  try
    lPathNames.AsXML := ls ;
    CheckEquals( cRoot, lPathNames.StartDir, 'StartDir');
    CheckEquals( False, lPathNames.Recurse, 'Recurse');
    CheckEquals(3, lPathNames.Count, 'Count');
    CheckEquals( 'c:\temp\dir1', lPathNames.Items[0].Path, 'Path');
    CheckEquals( 'c:\temp\dir2', lPathNames.Items[1].Path, 'Path');
    CheckEquals( 'c:\temp\dir3', lPathNames.Items[2].Path, 'Path');
  finally
    lPathNames.Free;
  end ;

end;

procedure TTestTIFileName.FileName_AsXML;
var
  lFileName: TtiFileName;
  ls : string ;
  lFileDate: TDateTime;
const
  cFileData = 'a string to use as the file data';
  cFileName = 'c:\temp\testfile.txt' ;
begin
  lFileDate := Now ;
  lFileName:= TtiFileName.Create ;
  try
    lFileName.PathAndName := cFileName;
    lFileName.Size := Length(cFileData);
    lFileName.Date := lFileDate ;
    tiStringToStream(cFileData,lFileName.Data);
    ls := lFileName.AsXML;
  finally
    lFileName.Free;
  end ;

  lFileName:= TtiFileName.Create ;
  try
    lFileName.AsXML := ls ;
    CheckEquals(cFileName, lFileName.PathAndName, 'FileName');
    CheckEquals(Length(cFileData), lFileName.Size, 'size');
    CheckEquals(lFileDate, lFileName.Date, 'Date');
    CheckEquals(cFileData, tiStreamToString(lFileName.Data), 'Data');
  finally
    lFileName.Free;
  end ;
end;

procedure TTestTIFileName.PathName_AsXML;
var
  lPathName: TtiPathName;
  ls: string;
const
  cFileName = 'c:\temp\testfile.txt' ;
begin
  lPathName:= TtiPathName.Create;
  try
    lPathName.Path := cFileName;
    ls := lPathName.AsXML;
  finally
    lPathName.Free;
  end ;

  lPathName:= TtiPathName.Create ;
  try
    lPathName.AsXML := ls ;
    CheckEquals(cFileName, lPathName.Path, 'File');
  finally
    lPathName.Free;
  end ;
end;

procedure TTestTIFileName.FileName_CloneWithNewRootDir;
  procedure _Do(const pStartDir: string;
                                          const pPathAndName: string;
                                          const pNewRoot: string;
                                          const pResult: string);
  var
    lFileNames    : TtiFileNames ;
    lFileNameFrom : TtiFileName ;
    lFileNameTo   : TtiFileName ;
  begin
    lFileNames := TtiFileNames.Create ;
    try
      lFileNames.StartDir := pStartDir ;
      lFileNameFrom := TtiFileName.Create ;
      lFileNames.Add( lFileNameFrom ) ;
      lFileNameFrom.PathAndName := pPathAndName ;
      lFileNameTo := lFileNameFrom.CloneWithNewRootDir(pNewRoot);
      try
        CheckEquals( pResult, lFileNameTo.PathAndName, 'PathAndName' ) ;
      finally
        lFileNameTo.Free ;
      end;
    finally
      lFileNames.Free ;
    end ;
  end;
begin
 _Do('c:\source\dir', 'c:\source\dir\file.txt','c:\target\dir', 'c:\target\dir\file.txt');
 _Do('c:\source\dir\','c:\source\dir\file.txt','c:\target\dir', 'c:\target\dir\file.txt');
 _Do('c:\source\dir', 'c:\source\dir\file.txt','c:\target\dir\','c:\target\dir\file.txt');
 _Do('c:\source\dir\','c:\source\dir\file.txt','c:\target\dir\','c:\target\dir\file.txt');
 _Do('source\dir\',   'source\dir\file.txt',   'c:\target\dir\','c:\target\dir\file.txt');
 _Do('..\source\dir\','..\source\dir\file.txt','c:\target\dir\','c:\target\dir\file.txt');
 _Do('c:\source\dir\','c:\source\dir\file.txt','target\dir\',   'target\dir\file.txt');
 _Do('c:\source\dir\','c:\source\dir\file.txt','..\target\dir\','..\target\dir\file.txt');
end ;

procedure TTestTIFileName.PathName_CloneWithNewRootDir;
  procedure _Do(const pStartDir: string;
                const pPath: string;
                const pNewRoot: string;
                const pResult: string);
  var
    lPathNames : TtiPathNames ;
    lPathNameFrom : TtiPathName ;
    lPathNameTo : TtiPathName ;
  begin
    lPathNames := TtiPathNames.Create ;
    try
      lPathNames.StartDir := pStartDir ;
      lPathNameFrom := TtiPathName.Create ;
      lPathNameFrom.Path := pPath ;
      lPathNames.Add( lPathNameFrom ) ;
      lPathNameTo := lPathNameFrom.CloneWithNewRootDir(pNewRoot) ;
      try
        CheckEquals( pResult, lPathNameTo.Path, 'Path' ) ;
      finally
        lPathNameTo.Free;
      end;
    finally
      lPathNames.Free ;
    end ;
  end;
begin
  _Do('c:\source', 'c:\source\dir1\dir2\dir3','c:\target', 'c:\target\dir1\dir2\dir3');
  _Do('c:\source\','c:\source\dir1\dir2\dir3','c:\target\','c:\target\dir1\dir2\dir3');
  _Do('c:\source\','c:\source\dir1\dir2\dir3','c:\target', 'c:\target\dir1\dir2\dir3');
  _Do('c:\source', 'c:\source\dir1\dir2\dir3','c:\target\','c:\target\dir1\dir2\dir3');
  _Do('source',    'source\dir1\dir2\dir3',   'c:\target\','c:\target\dir1\dir2\dir3');
  _Do('..\source', '..\source\dir1\dir2\dir3','c:\target\','c:\target\dir1\dir2\dir3');
  _Do('c:\source', 'c:\source\dir1\dir2\dir3','target\',   'target\dir1\dir2\dir3');
  _Do('c:\source', 'c:\source\dir1\dir2\dir3','..target\', '..target\dir1\dir2\dir3');
end;

end.

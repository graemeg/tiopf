{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  (c) TechInsite Pty. Ltd. 1999
  PO Box 429, Abbotsford, Melbourne. 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682

  Created: 15/01/1999

  Notes:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit tiFileName_BOM;

{
    TFilePath properties
      Path      C:\Program Files\Delphi4\Bin
      Drive     C:
      Directory \Program Files\Delphi4\Bin

    TFileName properties
      NameOnly    Delphi
      Ext         EXE
      NameFull    Delphi.EXE
      PathAndName C:\Program Files\Delphi4\Bin\Delphi.EXE
}


interface
uses
  classes
  ,sysUtils
  ,tiObjAbs
  ,tiPtnVisPerObj
  ,tiDataSet_BOM
  ;

type

  TtiPathName        = class ;
  TtiPathNames       = class ;
  TtiFileName        = class ;
  TtiFileNames       = class ;

  //----------------------------------------------------------------------------
  TtiPathNames = class( TPerVisList )
  private
    FbRecurse: boolean;
    FsStartDir: string;
    FsWildCard: string;
    procedure   PathNamesToDataSet(const pDataSet: TtiDataSet);
    function    GetAsXML: string;
  protected
    procedure   SetAsXML(const Value: string);
    procedure   ParamsToDataSet(const pDataSet: TtiDataSet);
    procedure   AssignPropsFromDataSets(const pDataSets: TtiDataSets);
    procedure   AssignItemsFromDataSets(const pDataSets: TtiDataSets); virtual ;
    function    GetItems(i: integer): TtiPathName ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiPathName ); reintroduce ;
  public
    constructor Create ; override ;
    function    Clone           : TtiPathNames ; reintroduce ;
    property    Items[i:integer] : TtiPathName read GetItems write SetItems ;
    function    FindLikeRootRemoved( pFileName : TtiPathName ) : TtiPathName ;
    property    AsXML: string read GetAsXML Write SetAsXML ;
  published
    property    StartDir : string read FsStartDir write FsStartDir ;
    property    Recurse  : boolean read FbRecurse write FbRecurse  ;
  end ;

  //----------------------------------------------------------------------------
  TtiPathName = class( TPerObjAbs )
  private
    procedure   SetDrive( const value : string ) ;
    procedure   SetDirectory( const value : string ) ;
  protected
    FStrPath      : string ;
    FStrDrive     : string ;
    FStrDirectory : string ;
    function    GetAsXML: string; virtual;
    procedure   SetAsXML(const Value: string); virtual ;
    procedure   SetPath( const value : string ) ; virtual ;
    function    GetRootRemoved: string; virtual ;
    function    GetRootDir: string; virtual ;
    function    GetOwner: TtiPathNames; reintroduce ;
    procedure   SetOwner(const Value: TtiPathNames); reintroduce ;
  public
    constructor Create ; override ;
    constructor CreateExt( const pStrPath : string ) ;
    property    Owner           : TtiPathNames read GetOwner write SetOwner ;
    function    Clone           : TtiPathName ; reintroduce ;
    function    CloneWithNewRootDir(const pNewRootDir: string): TtiPathName; virtual ;

    // C:
    property    Drive       : string read FStrDrive     write SetDrive ;
    // \Program files\Borland\Delphi4\Bin
    property    Directory   : string read FStrDirectory write SetDirectory ;
    property    RootRemoved : string read GetRootRemoved ;
    property    RootDir     : string read GetRootDir ;

    property    AsXML: string read GetAsXML Write SetAsXML ;

  published
    // This is registered for persistence
    // C:\Program files\Borland\Delphi4\Bin
    property    Path        : string read FStrPath      write SetPath ;
  end ;

  //----------------------------------------------------------------------------
  TtiFileName = class( TtiPathName )
  private
    FiSize: Integer;
    FDate: TDateTime;
    FData: TMemoryStream;
    procedure   SetNameOnly( const value : string ) ;
    procedure   SetExt( const value : string ) ;
    procedure   SetNameFull( const value : string ) ;
    procedure   SetPathAndName( const value : string ) ;
    function    GetPathAndName : string ;
  protected
    FStrNameOnly    : string ;
    FStrExt         : string ;
    FStrNameFull    : string ;
    FStrPathAndName : string ;
    function    GetAsXML: string; override ;
    procedure   SetAsXML(const Value: string); override ;
    function    GetOwner: TtiFileNames; reintroduce ;
    procedure   SetOwner(const Value: TtiFileNames); reintroduce ;
    function    GetRootRemoved: string; override ;
    procedure   AssignClassProps(pSource: TPerObjAbs); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    constructor CreateExt( const pStrPathAndName : string ) ; overload ;
    constructor CreateExt( const pStrPath, pStrNameFull : string ) ; overload ;
    constructor CreateExt( const pStrPath, pStrName, pStrExt : string ) ; overload ;
    property    Owner           : TtiFileNames read GetOwner write SetOwner ;
    function    Clone           : TtiFileName ; reintroduce ;
    function    CloneWithNewRootDir(const pNewRootDir: string): TtiFileName; reintroduce ;

    property    NameOnly    : string read FStrNameOnly    write SetNameOnly ;
    property    Ext         : string read FStrExt         write SetExt ;
    property    NameFull    : string read FStrNameFull    write SetNameFull ;

  published

    // These are registered cloning
    property    PathAndName : string read GetPathAndName  write SetPathAndName ;
    property    Date        : TDateTime read FDate write FDate ;
    property    Size        : Integer   read FiSize write FiSize ;
    property    Data        : TMemoryStream read FData write FData ;

  end ;

  TtiFileNames = class( TtiPathNames )
  private
    procedure   FileNamesToDataSet(const pDataSet: TtiDataSet);
    function    GetAsXML: string;
  protected
    procedure   AssignItemsFromDataSets(const pDataSets: TtiDataSets); override ;
    function    GetItems(i: integer): TtiFileName ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiFileName ); reintroduce ;
  public
    property    Items[i:integer] : TtiFileName read GetItems write SetItems ;
    function    FindLikeRootRemoved( pFileName : TtiFileName ) : TtiFileName ;
    property    AsXML: string read GetAsXML Write SetAsXML ;
  end ;

implementation
uses
  FileCtrl  // DirectoryExists, FileExists
  ,tiUtils  // tiDirectoryTreeToStringList
  ,tiDialogs
  ,cTIPersist
  ,tiPersist
  ,tiClassToDBMap_BOM
  ,tiXMLToTIDataSet
  ,tiQuery
  ;

const
  cParams_TableName      = 'params';
  cParams_Field_StartDir = 'start_dir' ;
  cParams_Field_Recurse  = 'recurse' ;

  cPathNames_TableName   = 'paths' ;
  cPathNames_Field_Path  = 'path_name';

  cFileNames_TableName   = 'files';
  cFileName_TableName    = 'file';
  cFileNames_Field_Name  = 'file_name';
  cFileNames_Field_Size  = 'file_size';
  cFileNames_Field_Date  = 'file_date';
  cFileNames_Field_Data  = 'file_data';


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TFilePath
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPathName.Create ;
begin
  inherited create ;
  Path := '' ;
end;

constructor TtiPathName.CreateExt(const pStrPath: string);
begin
  inherited create ;
  Path := pStrPath ;
end;

// Directory only \temp\dcu
//------------------------------------------------------------------------------

procedure TtiPathName.SetDirectory(const value: string);
begin
  FStrDirectory := tiRemoveTrailingSlash( value ) ;
  FStrPath  := FStrDrive + FStrDirectory ;
end;

// Drive only, c:
//------------------------------------------------------------------------------

procedure TtiPathName.SetDrive(const value: string);
begin
  FStrDrive := value ;
  FStrPath  := FStrDrive + FStrDirectory ;
end;

// Full path, c:\temp\dcu
//------------------------------------------------------------------------------

procedure TtiPathName.SetPath(const value: string);
begin
  FStrPath := tiRemoveTrailingSlash( value ) ;
  FStrDrive := ExtractFileDrive( value ) ;
  FStrDirectory := copy( FStrPath,
                         length( FStrDrive ) + 1,
                         length( value ) - length( FStrDrive )) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiFileName
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiFileName.Create;
begin
  inherited Create ;
  PathAndName := '' ;
  FData := TMemoryStream.Create ;
end;

//------------------------------------------------------------------------------
destructor TtiFileName.Destroy;
begin
  FData.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
constructor TtiFileName.CreateExt(const pStrPathAndName : string);
begin
  Create ;
  PathAndName := pStrPathAndName ;
end;

//------------------------------------------------------------------------------

constructor TtiFileName.CreateExt( const pStrPath, pStrName, pStrExt : string ) ;
begin
  Create ;
  Path     := pStrPath ;
  NameOnly := pStrName ;
  Ext      := pStrExt ;
end ;

//------------------------------------------------------------------------------
constructor TtiFileName.CreateExt( const pStrPath, pStrNameFull : string ) ;
begin
  Create ;
  Path     := pStrPath ;
  NameFull := pStrNameFull ;
end ;

//------------------------------------------------------------------------------

procedure TtiFileName.SetExt(const value: string);
begin
  FStrExt := tiStrTran( value, '.', '' ) ;
  FStrNameFull := FStrNameOnly + '.' + FStrExt ;
  FStrPathAndName := tiAddTrailingSlash( Path ) + FStrNameFull ;
end;

//------------------------------------------------------------------------------

procedure TtiFileName.SetNameFull(const value: string);
var lStrNameOnly : string ;
begin
  lStrNameOnly    := ChangeFileExt( Value, '' ) ;
  FStrNameOnly    := lStrNameOnly ;
  FStrNameFull    := Value ;
  Ext             := ExtractFileExt( Value ) ;
  FStrPathAndName := tiAddTrailingSlash( Path ) + FStrNameFull ;
end;

//------------------------------------------------------------------------------

procedure TtiFileName.SetNameOnly(const value: string);
begin
  FStrNameOnly    := Value ;
  FStrNameFull    := Value + '.' + FStrExt ;
  FStrPathAndName := tiAddTrailingSlash( Path ) + FStrNameFull ;
end;

//------------------------------------------------------------------------------

procedure TtiFileName.SetPathAndName(const value: string);
begin
  Path := ExtractFilePath( Value ) ;
  NameFull := ExtractFileName( Value ) ;
end;

//------------------------------------------------------------------------------
function TtiFileName.GetPathAndName : string ;
begin
  result := FStrPathAndName ;
end ;

procedure TtiFileNames.AssignItemsFromDataSets(const pDataSets: TtiDataSets);
var
  lDataSet: TtiDataSet;
  i : Integer ;
  lFileName: TtiFileName;
begin
  Assert( pDataSets.TestValid(TtiDataSets), cTIInvalidObjectError );
  lDataSet := pDataSets.FindByName(cFileNames_TableName);
  for i := 0 to lDataSet.Count - 1 do
  begin
    lFileName := TtiFileName.Create;
    lFileName.PathAndName := lDataSet.Items[i].FindByFieldName(cFileNames_Field_Name).ValueAsString;
    lFileName.Size := lDataSet.Items[i].FindByFieldName(cFileNames_Field_Size).ValueAsInteger;
    lFileName.Date := lDataSet.Items[i].FindByFieldName(cFileNames_Field_Date).ValueAsDateTime;
    Add(lFileName);
  end;
end;

procedure TtiFileNames.FileNamesToDataSet(const pDataSet: TtiDataSet);
  procedure _AddMetaData(pDS: TtiDataSet);
  begin
    pDS.Name := cFileNames_TableName;
    pDS.Fields.AddInstance(cFileNames_Field_Name, qfkString, 256);
    pDS.Fields.AddInstance(cFileNames_Field_Size, qfkInteger);
    pDS.Fields.AddInstance(cFileNames_Field_Date, qfkDateTime);
  end;

  procedure _AddData(const pDS: TtiDataSet);
  var
    lRow: TtiDataSetRow;
    i: Integer;
  begin
    for i := 0 to Count - 1 do
    begin
      lRow := pDS.AddInstance;
      lRow.FindByFieldName(cFileNames_Field_Name).ValueAsString   := Items[i].PathAndName ;
      lRow.FindByFieldName(cFileNames_Field_Size).ValueAsInteger  := Items[i].Size ;
      lRow.FindByFieldName(cFileNames_Field_Date).ValueAsDateTime := Items[i].Date ;
    end;
  end;
begin
  Assert( pDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  _AddMetaData(pDataSet);
  _AddData(pDataSet);
end;

function TtiFileNames.FindLikeRootRemoved(pFileName: TtiFileName): TtiFileName;
begin
  result := TtiFileName( inherited FindLikeRootRemoved( pFileName )) ;
end;

function TtiFileNames.GetAsXML: string;
var
  lDSs: TtiDataSets;
  lDS:  TtiDataSet;
begin
  lDSs:= TtiDataSets.Create;
  try
    lDS:= lDSs.AddInstance;
    ParamsToDataSet(lDS);
    lDS:= lDSs.AddInstance;
    FileNamesToDataSet(lDS);
    Result := tiTIDataSetsToXMLString(lDSs);
  finally
    lDSs.Free;
  end;
{
var
  lDS: TtiDataSet;
begin
  lDS:= TtiDataSet.Create;
  try
    _AddMetaData(lDS);
    _AddData(lDS);
    Result := tiTIDataSetToXMLString(lDS);
  finally
    lDS.Free;
  end;
}
end;

function TtiFileNames.GetItems(i: integer): TtiFileName;
begin
  result := TtiFileName( inherited GetItems( i )) ;
end;

procedure TtiFileNames.SetItems(i: integer; const Value: TtiFileName);
begin
  inherited SetItems( i, Value ) ;
end;

function TtiFileName.GetOwner: TtiFileNames;
begin
  result := TtiFileNames( inherited GetOwner ) ;
end;

procedure TtiFileName.SetOwner(const Value: TtiFileNames);
begin
  inherited SetOwner( Value ) ;
end;

function TtiFileName.GetRootRemoved: string;
var
  lsRoot : string ;
begin
  lsRoot := Copy( PathAndName, 1, Length( RootDir )) ;
  if SameText( lsRoot, RootDir ) then
    result := Copy( PathAndName,
                    Length( RootDir ) + 1,
                    Length( PathAndName ) - Length( RootDir ))
  else
    result := PathAndName ;
end ;

function TtiPathName.GetRootDir: string;
begin
  Assert( Owner.TestValid, cTIInvalidObjectError );
  result := Owner.StartDir ;
end;

function TtiFileName.Clone: TtiFileName;
begin
  result := TtiFileName( inherited Clone ) ;
end;

{ TtiPathNames }

function TtiPathNames.Clone: TtiPathNames;
begin
  result := TtiPathNames( inherited clone ) ;
end;

constructor TtiPathNames.Create;
begin
  inherited Create ;
  FbRecurse  := true ;
  FsStartDir := '\' ;
  FsWildCard := '*.*' ;
end;

function TtiPathNames.FindLikeRootRemoved( pFileName: TtiPathName): TtiPathName;
var
  i : integer ;
  ls1: string;
  ls2: string;
  ls3: string;
  ls4: string;
begin
  result := nil ;
  for i := 0 to Count - 1 do
  begin
    ls3:= Items[i].Path;
    ls4:= pFileName.Path;
    ls1 := Items[i].RootRemoved;
    ls2 := pFileName.RootRemoved ;
    if SameText( ls1, ls2 ) then
      result := Items[i] ;
  end;
end;

function TtiPathNames.GetAsXML: string;
var
  lDSs: TtiDataSets;
  lDS:  TtiDataSet;
begin
  lDSs:= TtiDataSets.Create;
  try
    lDS:= lDSs.AddInstance;
    ParamsToDataSet(lDS);
    lDS:= lDSs.AddInstance;
    PathNamesToDataSet(lDS);
    Result := tiTIDataSetsToXMLString(lDSs);
  finally
    lDSs.Free;
  end;
end;

function TtiPathNames.GetItems(i: integer): TtiPathName;
begin
  result := TtiPathName( inherited GetItems( i )) ;
end;

procedure TtiPathNames.ParamsToDataSet(const pDataSet: TtiDataSet);
  procedure _AddMetaData(pDS: TtiDataSet);
  begin
    pDS.Name := cParams_TableName;
    pDS.Fields.AddInstance(cParams_Field_StartDir, qfkString, 256);
    pDS.Fields.AddInstance(cParams_Field_Recurse, qfkLogical);
  end;

  procedure _AddData(const pDS: TtiDataSet);
  var
    lRow: TtiDataSetRow;
  begin
    lRow := pDS.AddInstance;
    lRow.FindByFieldName(cParams_Field_StartDir).ValueAsString := StartDir ;
    lRow.FindByFieldName(cParams_Field_Recurse).ValueAsBool := Recurse ;
  end;
begin
  Assert( pDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  _AddMetaData(pDataSet);
  _AddData(pDataSet);
end;

procedure TtiPathNames.PathNamesToDataSet(const pDataSet: TtiDataSet);
  procedure _AddMetaData(pDS: TtiDataSet);
  begin
    pDS.Name := cPathNames_TableName;
    pDS.Fields.AddInstance(cPathNames_Field_Path, qfkString, 256);
  end;

  procedure _AddData(const pDS: TtiDataSet);
  var
    lRow: TtiDataSetRow;
    i : Integer ;
  begin
    for i := 0 to Count - 1 do
    begin
      lRow := pDS.AddInstance;
      lRow.FindByFieldName(cPathNames_Field_Path).ValueAsString := Items[i].Path ;
    end ;
  end;
begin
  Assert( pDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  _AddMetaData(pDataSet);
  _AddData(pDataSet);
end;

procedure TtiPathNames.SetAsXML(const Value: string);
var
  lDataSets: TtiDataSets;
begin
  Clear;
  lDataSets:= TtiDataSets.Create;
  try
    tiXMLStringToTIDataSets(Value, lDataSets);
    Assert(lDataSets.Count = 2, 'lDataSets.Count <> 2');
    AssignPropsFromDataSets(lDataSets);
    AssignItemsFromDataSets(lDataSets);
  finally
    lDataSets.Free;
  end;
end;

procedure TtiPathNames.SetItems(i: integer; const Value: TtiPathName);
begin
  inherited SetItems( i, Value ) ;
end;

function TtiPathName.GetRootRemoved: string;
var
  lsRoot : string ;
begin
  lsRoot := Copy( Path, 1, Length( RootDir )) ;
  if SameText( lsRoot, RootDir ) then
    result := Copy( Path,
                    Length( RootDir ) + 1,
                    Length( Path ) - Length( RootDir ))
  else
    result := Path ;
end;

function TtiPathName.GetOwner: TtiPathNames;
begin
  result := TtiPathNames( inherited GetOwner ) ;
end;

procedure TtiPathName.SetOwner(const Value: TtiPathNames);
begin
  inherited SetOwner( Value ) ;
end;

procedure TtiFileName.AssignClassProps(pSource: TPerObjAbs);
begin
  Assert( pSource is TtiFileName, 'pSource not a TtiFileName' ) ;
  Data.Size := 0 ;
  TtiFileName( pSource ).Data.Position := 0 ;
  Data.LoadFromStream( TtiFileName( pSource ).Data ) ;

end;

function TtiPathName.Clone: TtiPathName;
begin
  result := TtiPathName( inherited Clone ) ;
end;

function TtiFileName.GetAsXML: string;
  procedure _AddMetaData(pDS: TtiDataSet);
  begin
    pDS.Name := cFileName_TableName;
    pDS.Fields.AddInstance(cFileNames_Field_Name, qfkString, 256);
    pDS.Fields.AddInstance(cFileNames_Field_Size, qfkInteger);
    pDS.Fields.AddInstance(cFileNames_Field_Date, qfkDateTime);
    pDS.Fields.AddInstance(cFileNames_Field_Data, qfkBinary);
  end;

  procedure _AddData(const pDS: TtiDataSet);
  var
    lRow: TtiDataSetRow;
  begin
    lRow := pDS.AddInstance;
    lRow.FindByFieldName(cFileNames_Field_Name).ValueAsString   := PathAndName ;
    lRow.FindByFieldName(cFileNames_Field_Size).ValueAsInteger  := Size ;
    lRow.FindByFieldName(cFileNames_Field_Date).ValueAsDateTime := Date ;
    lRow.FindByFieldName(cFileNames_Field_Data).AssignFromStream(Data);
  end;
var
  lDS: TtiDataSet;
begin
  lDS:= TtiDataSet.Create;
  try
    _AddMetaData(lDS);
    _AddData(lDS);
    Result := tiTIDataSetToXMLString(lDS);
  finally
    lDS.Free;
  end;
end;

procedure TtiFileName.SetAsXML(const Value: string);
var
  lDataSets: TtiDataSets;
  lDataSet: TtiDataSet;
  lRow: TtiDataSetRow;
begin
  lDataSets:= TtiDataSets.Create;
  try
    tiXMLStringToTIDataSets(Value, lDataSets);
    Assert(lDataSets.Count = 1, 'lDataSets.Count <> 1');
    lDataSet := lDataSets.Items[0];
    Assert(lDataSet.Count = 1, 'lDataSet.Count <> 1');
    lRow := lDataSet.Items[0];

    PathAndName := lRow.FindByFieldName(cFileNames_Field_Name).ValueAsString;
    Size := lRow.FindByFieldName(cFileNames_Field_Size).ValueAsInteger;
    Date := lRow.FindByFieldName(cFileNames_Field_Date).ValueAsDateTime;
    lRow.FindByFieldName(cFileNames_Field_Data).AssignToStream(Data);
  finally
    lDataSets.Free;
  end;
end;

procedure TtiPathNames.AssignPropsFromDataSets(const pDataSets: TtiDataSets);
var
  lDataSet: TtiDataSet ;
begin
  Assert( pDataSets.TestValid(TtiDataSets), cTIInvalidObjectError );
  lDataSet := pDataSets.FindByName(cParams_TableName);
  Assert( lDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  Assert( lDataSet.Count = 1, 'lDataSet.Count <> 1');
  StartDir := lDataSet.Items[0].FindByFieldName(cParams_Field_StartDir).ValueAsString;
  Recurse  := lDataSet.Items[0].FindByFieldName(cParams_Field_Recurse).ValueAsBool;
end;

procedure TtiPathNames.AssignItemsFromDataSets(const pDataSets: TtiDataSets);
var
  i : Integer ;
  lDataSet: TtiDataSet ;
  lPathName : TtiPathName;
begin
  Assert( pDataSets.TestValid(TtiDataSets), cTIInvalidObjectError );
  lDataSet := pDataSets.FindByName(cPathNames_TableName);
  Assert( lDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  for i := 0 to lDataSet.Count - 1 do
  begin
    lPathName := TtiPathName.Create;
    lPathName.Path := lDataSet.Items[i].FindByFieldName(cPathNames_Field_Path).ValueAsString;
    Add(lPathName);
  end;
end;

function TtiPathName.GetAsXML: string;
  procedure _AddMetaData(pDS: TtiDataSet);
  begin
    pDS.Name := cFileName_TableName;
    pDS.Fields.AddInstance(cFileNames_Field_Name, qfkString, 256);
  end;

  procedure _AddData(const pDS: TtiDataSet);
  var
    lRow: TtiDataSetRow;
  begin
    lRow := pDS.AddInstance;
    lRow.FindByFieldName(cFileNames_Field_Name).ValueAsString   := Path ;
  end;
var
  lDS: TtiDataSet;
begin
  lDS:= TtiDataSet.Create;
  try
    _AddMetaData(lDS);
    _AddData(lDS);
    Result := tiTIDataSetToXMLString(lDS);
  finally
    lDS.Free;
  end;
end;

procedure TtiPathName.SetAsXML(const Value: string);
var
  lDataSets: TtiDataSets;
  lDataSet: TtiDataSet;
  lRow: TtiDataSetRow;
begin
  lDataSets:= TtiDataSets.Create;
  try
    tiXMLStringToTIDataSets(Value, lDataSets);
    Assert(lDataSets.Count = 1, 'lDataSets.Count <> 1');
    lDataSet := lDataSets.Items[0];
    Assert(lDataSet.Count = 1, 'lDataSet.Count <> 1');
    lRow := lDataSet.Items[0];
    Path := lRow.FindByFieldName(cFileNames_Field_Name).ValueAsString;
  finally
    lDataSets.Free;
  end;
end;

function TtiPathName.CloneWithNewRootDir(const pNewRootDir: string): TtiPathName;
begin
  result := Clone ;
  result.Path := tiAddTrailingSlash(pNewRootDir) + RootRemoved ;
  result.Path := tiStrTran(result.Path, '\\', '\');
end;

function TtiFileName.CloneWithNewRootDir(const pNewRootDir: string): TtiFileName;
begin
  result := Clone ;
  result.PathAndName := tiAddTrailingSlash(pNewRootDir) + RootRemoved ;
  result.PathAndName := tiStrTran(result.PathAndName, '\\', '\');
end;

end.



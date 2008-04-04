unit tiFileSyncReader_Abs;

interface
uses
  tiObject
  ,tiFileName_BOM
  ,Classes
  ,tiFileSyncSetup_BOM
  ;

const
  cErrorInvalidFileSyncCommand = 'Invalid FileSyncCommand <%s>';

  ctiFileSyncCommand_ReadFileIndex  = 'ReadFileIndex';
  ctiFileSyncCommand_ReadPathIndex  = 'ReadPathIndex';
  ctiFileSyncCommand_CreatePath     = 'CreatePath';
  ctiFileSyncCommand_DeletePath     = 'DeletePath';
  ctiFileSyncCommand_ReadFileData   = 'ReadFileData';
  ctiFileSyncCommand_WriteFileData  = 'WriteFileData';
  ctiFileSyncCommand_DeleteFileData = 'DeleteFileData';
  ctiFileSyncCommand_WriteIndex     = 'WriteIndex';

type

  TtiFileSyncMgrProgressEvent = procedure(AFile: TtiFileName; AMax, APos: integer) of object;
  TtiFileSyncCheckTerminatedEvent = procedure(var ATerminated: Boolean) of object;

  TtiFileSyncReaderAbs = class( TtiObject )
  private
    FParams: TStringList;
    FCurrentFile: TtiFileName; // used to pass data to OnProgressEvent
    FOnProgress: TtiFileSyncMgrProgressEvent;
    FOnCheckTerminated: TtiFileSyncCheckTerminatedEvent;
    FRoot: string;
    // These have nothing to do with TtiFileSyncReaderAbs as they contain code
    // that is custom written for the remote server. Move to their own class.
    // (Perhaps the server thingy)
    function    AbsolutePath(const pPath: string): string;
    function    DoReadFileIndex(const pData: string): string;
    function    DoReadPathIndex(const pData: string): string;
    function    DoCreatePath(const pData: string): string;
    function    DoDeletePath(const pData: string): string;
    function    DoReadFileData(const pData: string): string;
    function    DoWriteFileData(const pData: string): string;
    function    DoDeleteFileData(const pData: string): string;
    function    DoWriteIndex(const pData: string): string;
    procedure   SetRoot(const Value: string);
  protected
    function    GetParamsAsString: string; virtual ;
    procedure   SetParamsAsString(const Value: string); virtual ;
    property    Params: TStringList read FParams;
    property    CurrentFile: TtiFileName Read FCurrentFile Write FCurrentFile;
    function    IsTerminated: Boolean;
  public
    constructor Create; override ;
    destructor  Destroy; override ;
    property    Root: string Read FRoot Write SetRoot;
    procedure   ReadFileIndex(  pFileNames : TtiFileNames ;
                                pSourceFileNameFilters : TtiFileNameFilters = nil ) ; virtual ; abstract ;
    procedure   ReadPathIndex(  pPathNames : TtiPathNames ;
                                pSourceFileNameFilters : TtiFileNameFilters = nil ) ; virtual ; abstract ;
    procedure   CreatePath(     pPathName  : TtiPathName  ) ; virtual ; abstract ;
    procedure   DeletePath(     pPathName  : TtiPathName  ) ; virtual ; abstract ;
    procedure   ReadFileData(   pFileName  : TtiFileName ) ; virtual ; abstract ;
    procedure   WriteFileData(  pFileName  : TtiFileName  ) ; virtual ; abstract ;
    procedure   DeleteFileData( pFileName  : TtiFileName  ) ; virtual ; abstract ;
    procedure   WriteIndex(     pFileNames : TtiFileNames ) ; virtual ; abstract ;
    function    Execute(const pCommand: string; pData: string): string ;
    property    ParamsAsString: string read GetParamsAsString Write SetParamsAsString;
    property    OnProgress: TtiFileSyncMgrProgressEvent Read FOnProgress Write FOnProgress;
    property    OnCheckTerminated: TtiFileSyncCheckTerminatedEvent Read FOnCheckTerminated Write FOnCheckTerminated;
  end ;

  TtiFileSyncReaderClass = class of TtiFileSyncReaderAbs ;

  TFileSyncReaderFactory = class( TPerObjFactory )
  public
    Function   CreateInstance( const pStrClassID : string; pParams: string ) : TtiFileSyncReaderAbs ; reintroduce ;
  end ;

function gFileSyncReaderFactory : TFileSyncReaderFactory ;

implementation

uses
  SysUtils
  ,tiXML
  ,tiUtils
  ;

var
  uFileSyncReaderFactory : TFileSyncReaderFactory ;

function gFileSyncReaderFactory : TFileSyncReaderFactory ;
begin
  if uFileSyncReaderFactory = nil then
    uFileSyncReaderFactory := TFileSyncReaderFactory.Create ;
  result := uFileSyncReaderFactory ;
end ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TFileSyncReaderFactory
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TFileSyncReaderFactory.CreateInstance(const pStrClassID: string ; pParams : string ): TtiFileSyncReaderAbs;
begin
  result := TtiFileSyncReaderAbs( inherited CreateInstance( pStrClassID )) ;
  Result.ParamsAsString:= pParams;
end;


{ TtiFileSyncReaderAbs }

function TtiFileSyncReaderAbs.AbsolutePath(const pPath: string): string;
begin
  Result := ExpandFileName(FRoot + '.\' + pPath );
end;

constructor TtiFileSyncReaderAbs.Create;
begin
  inherited;
  FParams:= TStringList.Create;
  Root:= tiGetEXEPath;
end;

destructor TtiFileSyncReaderAbs.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TtiFileSyncReaderAbs.DoCreatePath(const pData: string): string;
var
  lData: TtiPathName;
  lStartDir: string ;
begin
  lData:= TtiPathName.Create;
  try
    lData.AsXML:= pData;
    lStartDir := lData.Path;
    lData.Path := AbsolutePath(lStartDir);
    try
      CreatePath(lData);
    finally
      lData.Path := lStartDir;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoDeleteFileData(const pData: string): string;
var
  lData: TtiFileName;
  lPathAndName: string;
begin
  lData:= TtiFileName.Create;
  try
    lData.AsXML:= pData;
    lPathAndName := lData.PathAndName ;
    lData.PathAndName := AbsolutePath(lPathAndName);
    try
      DeleteFileData(lData);
    finally
      lData.PathAndName := lPathAndName;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoDeletePath(const pData: string): string;
var
  lData: TtiPathName;
  lPath: string ;
begin
  lData:= TtiPathName.Create;
  try
    lData.AsXML:= pData;
    lPath := lData.Path;
    lData.Path := AbsolutePath(lPath);
    try
      DeletePath(lData);
    finally
      lData.Path := lPath;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoReadFileData(const pData: string): string;
var
  lData: TtiFileName;
  lPathAndName : string ;
begin
  lData:= TtiFileName.Create;
  try
    lData.AsXML:= pData;
    lPathAndName := lData.PathAndName ;
    lData.PathAndName := AbsolutePath(lPathAndName);
    try
      ReadFileData(lData);
    finally
      lData.PathAndName := lPathAndName;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoReadFileIndex(const pData: string): string;
var
  lData: TtiFileNames;
  lStartDir: string ;
  i : Integer;
begin
  lData:= TtiFileNames.Create;
  try
    lData.AsXML:= pData;
    lStartDir := lData.StartDir ;
    try
      lData.StartDir := AbsolutePath(lStartDir);
      ReadFileIndex(lData);
      // Now make the paths relative to pData.StartDir
      for i := 0 to lData.Count - 1 do
      begin
        lData.Items[i].PathAndName :=
          tiAddTrailingSlash(lStartDir) + lData.Items[i].RootRemoved ;
        lData.Items[i].PathAndName :=
          tiStrTran(lData.Items[i].PathAndName, '\\', '\' );

{
        lPath := lData.Items[i].PathAndName;
        lPath := Copy(lPath, Length(lData.StartDir)+1, Length(lPath));
        lPath := tiAddTrailingSlash(lStartDir) + lPath;
        lPath := tiStrTran(lPath,'\\', '\');
        lData.Items[i].PathAndName := lPath;
}
      end ;
    finally
      lData.StartDir := lStartDir;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoReadPathIndex(const pData: string): string;
var
  lData: TtiPathNames;
  lStartDir: string ;
  i : integer ;
begin
  lData:= TtiPathNames.Create;
  try
    lData.AsXML:= pData;
    lStartDir := lData.StartDir;
    lData.StartDir := AbsolutePath(lStartDir);
    try
      ReadPathIndex(lData);
      // Now return to relative paths if required
      for i := 0 to lData.Count - 1 do
      begin
        lData.Items[i].Path :=
          tiAddTrailingSlash(lStartDir) + lData.Items[i].RootRemoved ;
        lData.Items[i].Path :=
          tiStrTran(lData.Items[i].Path, '\\', '\' );
      end;
    finally
      lData.StartDir := lStartDir;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoWriteFileData(const pData: string): string;
var
  lData: TtiFileName;
  lPath: string ;
begin
  lData:= TtiFileName.Create;
  try
    lData.AsXML:= pData;
    lPath := lData.Path;
    lData.Path := AbsolutePath(lPath);
    try
      WriteFileData(lData);
    finally
      lData.Path := lPath;
    end;
    Result:= lData.AsXML;
  finally
    lData.Free;
  end;
end;

function TtiFileSyncReaderAbs.DoWriteIndex(const pData: string): string;
begin
    Assert( false, 'Under construction' ) ;
end;

function TtiFileSyncReaderAbs.Execute(const pCommand: string; pData: string): string;
var
  lData : string ;
  lResult: string;
begin
  lData := tiDecompressDecode(pData);

  if pCommand = ctiFileSyncCommand_ReadFileIndex then
    lResult := DoReadFileIndex(  lData )
  else if pCommand = ctiFileSyncCommand_ReadPathIndex then
    lResult := DoReadPathIndex(  lData )
  else if pCommand = ctiFileSyncCommand_CreatePath then
    lResult := DoCreatePath(  lData )
  else if pCommand = ctiFileSyncCommand_DeletePath then
    lResult := DoDeletePath(  lData )
  else if pCommand = ctiFileSyncCommand_ReadFileData then
    lResult := DoReadFileData(   lData  )
  else if pCommand = ctiFileSyncCommand_WriteFileData then
    lResult := DoWriteFileData(   lData  )
  else if pCommand = ctiFileSyncCommand_DeleteFileData then
    lResult := DoDeleteFileData(   lData  )
  else if pCommand = ctiFileSyncCommand_WriteIndex then
    lResult := DoWriteIndex(   lData  )
  else
    raise Exception.CreateFmt(cErrorInvalidFileSyncCommand, [pCommand]);
  Result:= tiCompressEncode(lResult);
  
end;

function TtiFileSyncReaderAbs.GetParamsAsString: string;
begin
  Result := FParams.CommaText;
end;

function TtiFileSyncReaderAbs.IsTerminated: Boolean;
begin
  if Assigned(FOnCheckTerminated) then
    FOnCheckTerminated(Result)
  else
    Result:= False;
end;

procedure TtiFileSyncReaderAbs.SetParamsAsString(const Value: string);
begin
  FParams.CommaText := Value;
end;

procedure TtiFileSyncReaderAbs.SetRoot(const Value: string);
begin
  FRoot := tiAddTrailingSlash(Value);
end;

initialization

finalization
  uFileSyncReaderFactory.Free ;


end.

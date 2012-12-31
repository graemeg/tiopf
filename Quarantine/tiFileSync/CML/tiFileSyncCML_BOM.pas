unit tiFileSyncCML_BOM;

interface
uses
  tiBaseObject
  ,tiFileSync_Mgr
  ;

type

  TtiFileSyncCML = class( TtiBaseObject )
  private
    FFSM : TtiFileSyncMgr;
    procedure DoLog( const pMessage : string; pAppendToPrevRow: boolean);
    procedure DoUpdateProgress( pMax, pPos : integer );
    procedure DoTerminate(Sender: TObject ) ;
    procedure DoTerminateWithError(Sender: TObject ) ;
    procedure ShowHelp;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute ;
  end;

implementation
uses
   tiFileSyncReader_Abs
  ,tiFileSyncReader_DiskFiles
  ,tiFileSyncReader_Remote
  ,tiCompressZLib
  ,tiOIDGUID
  ,cFileSync
  ,tiUtils
  ,tiConstants
  ,tiCommandLineParams
  ,SysUtils
  ;

const
  cSourceReader   = 'sr';
  cTargetReader   = 'tr';
  cSourceLocation = 'sl';
  cTargetLocation = 'tl';
  cSourceURL      = 'su';
  cTargetURL      = 'tu';

procedure TtiFileSyncCML.Execute;
var
  lParams: string;
  lSourceReader: string;
  lTargetReader: string;
  lSourceLocation: string;
  lTargetLocation: string;
  lSourceURL: string;
  lTargetURL: string;
begin
  if gCommandLineParams.IsParam('?') or
     gCommandLineParams.IsParam('help') then
  begin
    ShowHelp;
    Exit ; //==>
  end;

  try
    lSourceReader   := gCommandLineParams.GetParam(cSourceReader);
    lTargetReader   := gCommandLineParams.GetParam(cTargetReader);
    lSourceLocation := gCommandLineParams.GetParam(cSourceLocation);
    lTargetLocation := gCommandLineParams.GetParam(cTargetLocation);
    lSourceURL      := gCommandLineParams.GetParam(cSourceURL);
    lTargetURL      := gCommandLineParams.GetParam(cTargetURL);

    if lSourceReader = cRemoteShort then
    begin
      lParams   := cHTTPURL + '=' + tiAddTrailingValue(lSourceURL ,'/' ) + 'OPDMSLauncher.exe';
      lSourceReader := cgsRemote;
    end else
    if lSourceReader = cDiskFilesShort then
    begin
      lParams   := '';
      lSourceReader := cgsDiskFiles;
    end
    else begin
      WriteLn('Invalid SourceReader: <' + lSourceReader + '>');
      ShowHelp;
      Exit ; //==>
    end;

    FFSM.SourceReader             := lSourceReader;
    FFSM.SourceReaderParams       := lParams;
    FFSM.SourceFileNames.StartDir := lSourceLocation;

    if lTargetReader = cRemoteShort then
    begin
      lParams   := cHTTPURL + '=' + tiAddTrailingValue(lTargetURL ,'/' ) + 'OPDMSLauncher.exe';
      lTargetReader := cgsRemote;
    end else
    if lTargetReader = cDiskFilesShort then
    begin
      lParams   := '';
      lTargetReader := cgsDiskFiles;
    end
    else begin
      WriteLn('Invalid TargetReader: <' + lTargetReader + '>');
      ShowHelp;
      Exit ; //==>
    end;

    FFSM.TargetReader := lTargetReader;
    FFSM.TargetReaderParams := lParams;
    FFSM.TargetFileNames.StartDir := lTargetLocation;
    WriteLn('Source reader:   ' + lSourceReader) ;
    WriteLn('Source URL:      ' + lSourceURL) ;
    WriteLn('Source location: ' + lSourceLocation) ;
    WriteLn('Target reader:   ' + lTargetReader) ;
    WriteLn('Target URL:      ' + lTargetURL) ;
    WriteLn('Target location: ' + lTargetLocation) ;
    WriteLn;

    FFSM.Execute;
  except
    on e:Exception do
      WriteLn(e.message);
  end;

  if gCommandLineParams.IsParam('i') then
  begin
    WriteLn;
    WriteLn;
    WriteLn('Press <Enter> to continue');
    ReadLn;
  end;

end;

procedure TtiFileSyncCML.DoLog(const pMessage: string; pAppendToPrevRow: boolean);
begin
  if not pAppendToPrevRow then
    WriteLn
  else
    Write('  ');
  Write(pMessage);
end;

procedure TtiFileSyncCML.DoUpdateProgress(pMax, pPos: integer);
begin
end;

procedure TtiFileSyncCML.DoTerminate(Sender: TObject);
begin
end;

procedure TtiFileSyncCML.DoTerminateWithError(Sender: TObject);
begin
end;

constructor TtiFileSyncCML.Create;
begin
  inherited Create;
  FFSM := TtiFileSyncMgr.Create;
  FFSM.VerboseLogging := True ;
  FFSM.FileSyncActions:= [fsaCopy, fsaUpdate];
  FFSM.OnLog := DoLog;
  FFSM.OnProgress:= DoUpdateProgress;
end;

destructor TtiFileSyncCML.Destroy;
begin
  FFSM.Free;
  inherited;
end;

procedure TtiFileSyncCML.ShowHelp;
begin
  WriteLn('Parameters:');
  WriteLn('  -' + cSourceReader + ' The source reader. May be ''r'' (remote) or ''df'' (disk files)');
  WriteLn('  -' + cTargetReader + ' The target reader. May be ''r'' (remote) or ''df'' (disk files)');
  WriteLn('  -' + cSourceLocation + ' The location of the source files');
  WriteLn('  -' + cTargetLocation + ' The location of the target');
  WriteLn('  -' + cSourceURL + ' The URL to the source reader (if remote)');
  WriteLn('  -' + cTargetURL + ' The URL to the target reader (if remote)');
  WriteLn('  -i' + ' Interactive mode. Pause the screen when finished.');
  WriteLn('   Source and target location may be in the form:');
  WriteLn('     C:\MyDirectory  or');
  WriteLn('     HTTP:\\PathToRemoteServer');
  WriteLn;
  WriteLn('For example:');
  WriteLn('tiFileSyncCML -sr r -su -tr df -sl .\MyFiles -tl c:\temp\myfiles');

end;

end.

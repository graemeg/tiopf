{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: Application launcher, client side objects

  Revision History:
    Nov 1999, PWH, Created

  Classes:
    TThrdRunDownload - A TThread descendent to run the download in.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiDeployMgr_Cli ;

interface
uses
   tiDeployMgr_BOM
  ,Classes
  ,StdCtrls
  ,ComCtrls
  ,tiVisitor
  ,tiObject
  ;

const
  cTIOPFExcMsgNoFilesFoundToDeploy = 'Can not find any application files to deploy with the tag name <%s>';


type


  TNoThread = class( TObject )
  protected
    procedure Synchronize( pMethod : TThreadMethod ) ;
  public
    constructor Create( Suspend : boolean ) ;
    procedure Execute ; virtual ; abstract ;
    procedure Resume ;
  end ;

  // A thread to run the download process in.
  // I can't make Synchronize work, it hangs for some reason. Have tried using
  // a critical section, and Canvas.Lock, but they all hang - possibly a
  // deadlock. Running out of time so used TNoThread as a work around for the
  // time being.
  TThrdRunDownload = class( TThread )
  private
    FDeployApp : TtiDeployApp ;
    FsErrorMessage : string ;
    FsLog : string ;
    FMemo: TMemo;
    FsDatabaseName: string;

    // Shows an error message inside a synchronize( ) method
    procedure ShowError ;

    procedure RefreshFiles ;

    procedure Log( const psMessage : string ) ;
    procedure DoLog ;
    function  GetCommandLineParams: string;
    procedure SetCommandLineParams(const Value: string);
    procedure SetDatabaseName(const Value: string);

  public
    constructor Create( Suspend : boolean ) ;
    destructor  Destroy ; override ;
    constructor CreateExt ;
    procedure   Execute ; override ;

    property    MemoLog     : TMemo        read FMemo        write FMemo ;
    property    DeployApp   : TtiDeployApp read FDeployApp   write FDeployApp ;
    property    DatabaseName : string      read FsDatabaseName write SetDatabaseName ;
    property    CommandLineParams : string read GetCommandLineParams write SetCommandLineParams ;
  end;

  TVisReadDeployDirtyAbs = class( TtiVisitor )
  protected
    function  AcceptVisitor : boolean ; override ;
    function  IsDirty( pFileName : string ) : boolean ;
  end ;

  TVisReadDeployFromDirty = class( TVisReadDeployDirtyAbs )
  protected
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  TVisReadDeployToDirty = class( TVisReadDeployDirtyAbs )
  protected
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  TVisReadDeployFromReadIcon = class( TtiVisitor )
  protected
    function  AcceptVisitor : boolean ; override ;
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  TVisReadDeployFromFileDetails = class( TVisReadDeployDirtyAbs )
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  TVisWriteDeployToFile = class( TtiVisitor )
  protected
    function  AcceptVisitor : boolean ; override ;
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  // A visitor to launch files flagged for launching
  TDeployMgrLaunch = class( TtiVisitor )
  protected
    function  AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
  end ;

procedure DeployAndLaunch(            pData : TtiDeployLaunchApp ; pDatabaseName : string ) ;
procedure ReadDeployToDirty(          pData : TtiObject ) ;
procedure ReadDeployFromDirty(        pData : TtiObjectTtiObject ) ;
procedure ReadDeployFromFileDetails(  pData : TtiObject ) ;
procedure WriteDeployToFile(          pData : TtiObject ) ;
procedure LaunchFiles(                pData : TtiObject ) ;
procedure ReadDeployFromIcon(         pData : TtiObject ) ;

implementation
uses
   tiDeployMgr_Srv
  ,tiUtils
  ,tiDialogs
  ,tiLog
  ,tiExcept
  ,tiDBConnectionPool
  ,cTIDeployMgr
  ,FAppLaunch
  ,tiCommandLineParams
  ,tiOPFManager
  ,tiGUIUtils
  ,Forms
  ,Controls
  ,Windows
  ,SyncObjs
  ,Graphics
  ,ShellAPI
  ,SysUtils
  ;

procedure ReadDeployToDirty( pData : TtiObject ) ;
var
  lVis : TVisReadDeployToDirty ;
begin
  lVis := TVisReadDeployToDirty.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;

procedure ReadDeployFromDirty( pData : TtiObject ) ;
var
  lVis : TVisReadDeployFromDirty ;
begin
  lVis := TVisReadDeployFromDirty.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;

procedure ReadDeployFromFileDetails( pData : TtiObject ) ;
var
  lVis : TVisReadDeployFromFileDetails ;
begin
  lVis := TVisReadDeployFromFileDetails.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;

procedure WriteDeployToFile( pData : TtiObject ) ;
var
  lVis : TVisWriteDeployToFile ;
begin
  lVis := TVisWriteDeployToFile.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end;

procedure LaunchFiles(               pData : TtiObject ) ;
var
  lVis : TDeployMgrLaunch ;
begin
  lVis := TDeployMgrLaunch.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;

procedure ReadDeployFromIcon(        pData : TtiObject ) ;
var
  lVis : TVisReadDeployFromReadIcon ;
begin
  lVis := TVisReadDeployFromReadIcon.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;

procedure DeployAndLaunch( pData : TtiDeployLaunchApp ; pDatabaseName : string ) ;
var
  lForm : TFormAppLaunch ;
begin
  lForm := TFormAppLaunch.Create( Application ) ;
  lForm.DatabaseName      := pDatabaseName ;
  lForm.FileGroup         := pData.DeployApp.AppName ;
  if pData.DeployParam <> nil then
    lForm.CommandLineParams := pData.DeployParam.ParamStr ;
  lForm.Show ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdRunDownload
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TThrdRunDownload.Execute;
var
  liStart      : DWord ;
begin

  try
    // Confirm the file group was passed on the command line
    if FDeployApp.AppName = '' then
      raise exception.Create(
        'Please pass a file group on the command line.' + #13 +
        'Example: AppLaunch -d opdp -run my_application_name' ) ;

    if not gTIOPFManager.DefaultPerLayer.DBConnectionPools.IsConnected( FsDatabaseName ) then
    begin
      Self.Log( 'Connecting to  ' + FsDatabaseName + '...' ) ;
      gTIOPFManager.DefaultPerLayer.DBConnectionPools.Connect( FsDatabaseName ) ;
      Self.Log( 'Connected.' ) ;
    end else
      Self.Log( 'Already connected to  ' + FsDatabaseName ) ;


    // Read the compression type used for the application
    gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadAppDetails, FDeployApp ) ;

    Self.Log( 'Reading list of files to deploy for ' + FDeployApp.AppName ) ;

    if FDeployApp.Count = 1 then
      Self.Log( Format( 'There is %d file in %s',
                   [FDeployApp.Count, FDeployApp.AppName] ))
    else
      Self.Log( Format( 'There are %d files in %s',
                   [FDeployApp.Count, FDeployApp.AppName] )) ;

    // Make sure there are files to version check
    if FDeployApp.Count = 0 then
      raise exception.Create( Format( cTIOPFExcMsgNoFilesFoundToDeploy, [FDeployApp.AppName])) ;

    liStart := GetTickCount ;

    ReadDeployToDirty( FDeployApp ) ;

    RefreshFiles ;

    LaunchFiles( FDeployApp ) ;

    Self.Log( Format( 'Download took %sms',
                 [ IntToStr( GetTickCount - liStart ) ])) ;
  except
    on e:exception do
    begin
      FsErrorMessage := 'Error in Application Launcher.' + Cr(2) +
                        'Message: ' + Cr(2) +
                        e.message ;
      Synchronize( ShowError ) ;
    end ;
  end ;

end ;

// Show an error dialog
{
procedure TThrdRunDownload.ShowError;
begin
  tiAppError( FsErrorMessage ) ;
  Terminate ;
end;
}
procedure TThrdRunDownload.DoLog;
begin
  tiLog.Log( FsLog ) ;
  if FMemo <> nil then
    FMemo.Lines.Add( FsLog ) ;
end;

procedure TThrdRunDownload.Log( const psMessage : string ) ;
begin
  FsLog := psMessage ;
  Synchronize( DoLog ) ;
end;

constructor TThrdRunDownload.CreateExt;
begin
  Create( true ) ;
end;

constructor TThrdRunDownload.Create(Suspend: boolean);
begin
  inherited Create( Suspend ) ;
  FDeployApp := TtiDeployApp.Create ;
  FDeployApp.ObjectState := posPK ;
  FreeOnTerminate := false ;
end;

destructor TThrdRunDownload.Destroy;
begin
  FDeployApp.Free ;
  inherited;
end;

procedure TThrdRunDownload.RefreshFiles;
var
  i : integer ;
  lDeployFile : TtiDeployFile ;
begin
  // We scan the FDeployFiles here (not with the visitor manager as we
  // want logging to the main form (not to the TtiLog)
  for i := 0 to FDeployApp.Count - 1 do
  begin
    lDeployFile := FDeployApp.Items[i] ;
    if lDeployFile.DeployToDirty then
    begin
      Self.Log( Format( '%d. Refreshing: %s',
                   [i+1,
                    lDeployFile.DeployToPathAndName])) ;
      gTIOPFManager.VisitorManager.Execute( cgsDeployMgrReadBin, lDeployFile, FsDatabaseName ) ;
      WriteDeployToFile( lDeployFile ) ;
      
    end else
      Self.Log( Format( '%d. File OK: %s',
                   [ i+1,
                    lDeployFile.DeployToPathAndName])) ;
  end ;
end;

function TThrdRunDownload.GetCommandLineParams: string;
begin
  result := FDeployApp.CommandLineParams ;
end;

procedure TThrdRunDownload.SetCommandLineParams(const Value: string);
begin
  FDeployApp.CommandLineParams := Value ;
end;

{ TNoThread }

constructor TNoThread.Create(Suspend: boolean);
begin
  inherited Create ;
end;

procedure TNoThread.Resume;
begin
  Execute ;
end;

procedure TNoThread.Synchronize(pMethod: TThreadMethod);
begin
  pMethod ;
end;

procedure TThrdRunDownload.SetDatabaseName(const Value: string);
begin
  FsDatabaseName := Value;
  FDeployApp.DatabaseName := FsDatabaseName ;
end;

procedure TThrdRunDownload.ShowError;
begin
  if Application.MainForm <> nil then
    Application.MainForm.Visible := false ;
  tiAppError( FsErrorMessage ) ;
end;

{ TVisReadDeployFromDirty }

procedure TVisReadDeployFromDirty.Execute(const pVisited: TtiVisited);
var
  lData : TtiDeployFile ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lData := TtiDeployFile( Visited ) ;

  if not FileExists( lData.DeployFromPathAndName ) then
  begin
    lData.SourceFileNotFound := true ;
    Exit ; //==>
  end ;

  lData.DeployFromDirty := IsDirty( lData.DeployFromPathAndName ) ;
  if lData.DeployFromDirty then
    lData.Dirty := true ;
end;

{ TVisReadDeployDirtyAbs }

function TVisReadDeployDirtyAbs.AcceptVisitor: boolean;
begin
  result := Visited is TtiDeployFile ;
end;

function TVisReadDeployDirtyAbs.IsDirty(pFileName: string): boolean;
var
  lDT : TDateTime ;
  liSize : integer ;
  lData : TtiDeployFile ;
begin
  lData := TtiDeployFile( Visited ) ;
  try
    tiReadFileDateSize(pFileName, lDT, liSize ) ;
    result :=
        // Work around file system differences
        (tiDateTimeToFileDateTime(lData.FileDate) <> tiDateTimeToFileDateTime(lDT)) or
        (lData.FileSize <> liSize);
  except
    result := true ;
  end ;
end;

{ TVisReadDeployDirtyTo }

procedure TVisReadDeployToDirty.Execute(const pVisited: TtiVisited);
var
  lData : TtiDeployFile ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lData := TtiDeployFile( Visited ) ;

  lData.DeployToDirty :=
    ( Not FileExists( lData.DeployToPathAndName )) or
    ( IsDirty( lData.DeployToPathAndName )) ;

end;

{ TVisReadDeployFromFileDetails }

procedure TVisReadDeployFromFileDetails.Execute(const pVisited: TtiVisited);
var
  lDT : TDateTime ;
  liSize : integer ;
  lData : TtiDeployFile ;
begin
  inherited Execute(pVisited);
  if not AcceptVisitor then
    Exit ; //==>
  lData := TtiDeployFile( pVisited ) ;
  try
    tiReadFileDateSize( lData.DeployFromPathAndName, lDT, liSize ) ;
    lData.FileDate := lDT ;
    lData.FileSize := liSize ;
    lData.LoadFromFile( lData.DeployFromPathAndName ) ;
  except
    lData.FileDate := cgMinDateTime ;
    lData.FileSize := 0 ;
    lData.Size     := 0 ;
  end ;
end;

{ TVisWriteDeployToFile }

function TVisWriteDeployToFile.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployFile ) and
            ( TtiDeployFile( Visited ).DeployToDirty ) ;
end;

procedure TVisWriteDeployToFile.Execute(const pVisited: TtiVisited);
var
  lDeployFile : TtiDeployFile ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lDeployFile := TtiDeployFile( Visited ) ;

  if FileExists( lDeployFile.DeployToPathAndName ) then
    if not SysUtils.DeleteFile( lDeployFile.DeployToPathAndName ) then
      raise exception.Create(
        Format( 'File <%s> already exists and can not be deleted. ' +
                'It may be in use.', [lDeployFile.DeployToPathAndName] )) ;

  if not DirectoryExists( lDeployFile.DeployToPath ) then
    ForceDirectories( lDeployFile.DeployToPath ) ;
    if not DirectoryExists( lDeployFile.DeployToPath ) then
      raise exception.Create(
        Format( 'Unable to create directory <%s>',
                [lDeployFile.DeployToPathAndName] )) ;

  Log( 'File size: %d File name: %s',
       [lDeployFile.FileSize, lDeployFile.DeployToPathAndName]);

  lDeployFile.SaveToFile( lDeployFile.DeployToPathAndName ) ;

  tiSetFileDate( lDeployFile.DeployToPathAndName, lDeployFile.FileDate ) ;
  Log( '<' + lDeployFile.DeployToPathAndName + '> date has been set to ' +
       tiDateTimeToStr( lDeployFile.FileDate )) ;

end;

{ TDeployMgrLaunch }

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDeployMgrLaunch
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TDeployMgrLaunch.AcceptVisitor : boolean;
begin
  result := ( Visited is TtiDeployFile ) and
            TtiDeployFile( Visited ).Launch ;
end;

procedure TDeployMgrLaunch.Execute(const pVisited: TtiVisited);
var
  lsParams : string ;
  lData : TtiDeployFile ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lData := TtiDeployFile( Visited ) ;
  Log( 'About to launch %s', [lData.DeployToPathAndName] ) ;

  //lsParams := TtiDeployApp( TtiDeployFile( Visited ).Owner ).CommandLineParams ;

  // ToDo: Must do better than this: lsParams := '-d ' + gTIOPFManager.DefaultDBConnectionName
  //lsParams := '-lv -d ' + gTIOPFManager.DefaultDBConnectionName ;
  lsParams := '-d ' + gTIOPFManager.DefaultDBConnectionName ;
  Log( 'Launching ' + TtiDeployFile( Visited ).DeployToPathAndName +
       ' ' + lsParams ) ;

  // Launch the application.
  tiShellExecute( TtiDeployFile( Visited ).DeployToPathAndName, lsParams ) ;

end;


{ TVisReadDeployFromReadIcon }

function TVisReadDeployFromReadIcon.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployFile ) ;
end;

procedure TVisReadDeployFromReadIcon.Execute(const pVisited: TtiVisited);
var
  lDummy : word ;
  lIcon : TIcon ;
  lFileName : string ;
  lData : TtiDeployFile ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lData := TtiDeployFile( Visited ) ;

  if not lData.Launch then
  begin
    if not lData.Owner.Image.Empty then
      lData.Owner.Image.Assign( nil ) ;
      lData.Owner.Dirty := true ;
    Exit ; //==>
  end ;

  try
    lFileName := lData.DeployFromPathAndName ;
    lDummy := 65535 ;
    lIcon := TIcon.Create ;
    try
      lIcon.Handle :=
        ExtractAssociatedIcon( hInstance ,
                               PChar( lFileName ),
                               lDummy );
      if lIcon.Handle <> 0 then
      begin
        lData.Owner.Image.Width  := lIcon.Width;
        lData.Owner.Image.Height := lIcon.Height;
        lData.Owner.Image.PixelFormat := pf24bit;
        lData.Owner.Image.Canvas.Draw(0,0, lIcon);
        lData.Owner.Dirty := true ;
        ContinueVisiting := false ;
      end ;

    finally
      lIcon.Free ;
    end ;
  except
    on e:exception do
      raise Exception.Create(
                      'Unable to determine icon for <' +
                      lFileName +
                      '> Message ' + e.message);
  end ;

end;



end.

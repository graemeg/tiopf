{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:


  Classes:


  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiDeployMgr_BOM;

interface
uses
   Classes
  ,tiPtnVisPerObj
  ,tiPtnVis
  ,Graphics

  ;

const
  cgWinSysDir = '$WINSYS' ;

type

  TtiDeployApps   = class ;
  TtiDeployApp    = class ;
  TtiDeployFile   = class ;
  TtiDeployParams = class ;
  TtiDeployParam  = class ;
  TtiDeployLaunchApps = class ;
  TtiDeployLaunchApp  = class ;

  //----------------------------------------------------------------------------
  TtiDeployApps  = class( TPerObjList )
  private
    FDeployLaunchApps: TtiDeployLaunchApps;
  protected
    function    GetItems(i: integer): TtiDeployApp ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDeployApp); reintroduce ;
    procedure   SetOwner(const Value: TtiDeployApp); reintroduce ;
    function    GetCaption : string ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Items[i:integer] : TtiDeployApp read GetItems write SetItems ;
    procedure   Add( pObject : TtiDeployApp ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  published
    property    DeployLaunchApps : TtiDeployLaunchApps read FDeployLaunchApps ;
  end ;

  //----------------------------------------------------------------------------
  TtiDeployParams = class( TPerObjList )
  protected
    function    GetItems(i: integer): TtiDeployParam ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDeployParam); reintroduce ;
    function    GetOwner: TtiDeployApp; reintroduce ;
    procedure   SetOwner(const Value: TtiDeployApp); reintroduce ;
  public
    property    Items[i:integer] : TtiDeployParam read GetItems write SetItems ;
    property    Owner       : TtiDeployApp             read GetOwner      write SetOwner ;
    procedure   Add( pObject : TtiDeployParam ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  end ;

  //----------------------------------------------------------------------------
  TtiDeployParam = class( TPerObjAbs )
  private
    FParamStr: string;
    FDisplayText: string;
    FDescription: string;
  protected
    function    GetOwner: TtiDeployApp; reintroduce ;
    procedure   SetOwner(const Value: TtiDeployApp); reintroduce ;
  public
    property    Owner       : TtiDeployApp             read GetOwner      write SetOwner ;
    //function    Clone : TPerObjAbs ; reintroduce ; // Must override and typecast if to be used
    //procedure   Assign( pSource : TPerObjAbs ) ; reintroduce ;
  published
    property    ParamStr : string read FParamStr write FParamStr ;
    property    DisplayText : string read FDisplayText write FDisplayText ;
    property    Description : string read FDescription write FDescription ;
  end ;

  //----------------------------------------------------------------------------
  TtiDeployApp = class( TPerObjList )
  private
    FAppName : string ;
    FDescription : string ;
    FsCommandLineParams: string;
    FsDatabaseName: string;
    FCompression: string;
    FDeployParams: TtiDeployParams;
    FImage: TBitMap ;
    FDisplayText: string;
  protected
    function    GetItems(i: integer): TtiDeployFile; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDeployFile); reintroduce ;
    function    GetOwner: TtiDeployApps ; reintroduce ;
    procedure   SetOwner(const Value: TtiDeployApps); reintroduce ;
    function    GetCaption : string ; override ;
  published
    property    AppName        : string read FAppName        write FAppName ;
    property    DisplayText : string read FDisplayText write FDisplayText ;
    property    Description : string read FDescription write FDescription ;
    property    Compression    : string read FCompression    write FCompression ;
    property    DeployParams   : TtiDeployParams read FDeployParams ;
    property    Image          : TBitMap read FImage ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   ReadDeployToDirtyStatus ;

    property    CommandLineParams : string read FsCommandLineParams write FsCommandLineParams ;
    property    DatabaseName : string read FsDatabaseName write FsDatabaseName ;

    property    Items[ i : integer ] : TtiDeployFile read GetItems write SetItems ;
    procedure   Add( pObject : TtiDeployFile ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner        : TtiDeployApps      read GetOwner      write SetOwner ;
  end ;

  // A file to be deployed
  //----------------------------------------------------------------------------
  TtiDeployFile = class( TPerStream )
  private
    FiFileSize: integer;
    FsFileVersion: string;
    FdtFileDate: TDateTime;
    FbLaunch: boolean;
    FsDeployToRoot: string;
    FDeployFrom: string;
    FDeployFromDirty: boolean;
    FDeployToDirty: boolean;
    FSourceFileNotFound: boolean;
    function GetDeployToPath : string ;
    function GetDeployFromPath: string;
    function GetFileName: string;
    function GetDeployToPathAndName: string;
  protected
    function  GetOwner: TtiDeployApp; reintroduce ;
    procedure SetOwner(const Value: TtiDeployApp); reintroduce ;
  published
    // These are entered in the client gui
    property DeployFromPathAndName : string     read FDeployFrom write FDeployFrom ;
    property DeployToRoot          : string     read FsDeployToRoot   write FsDeployToRoot ;
    property Launch                : boolean    read FbLaunch         write FbLaunch ;

    // These are derived by the bom
    property FileName              : string     read GetFileName ;
    property DeployToPath          : string     read GetDeployToPath ;
    property DeployToPathAndName   : string     read GetDeployToPathAndName ;
    property DeployFromPath        : string     read GetDeployFromPath ;

    // These are read by a client side visitor
    property FileSize              : integer   read FiFileSize       write FiFileSize ;
    property FileDate              : TDateTime read FdtFileDate      write FdtFileDate ;
    property DeployFromDirty       : boolean   read FDeployFromDirty write FDeployFromDirty ;
    property DeployToDirty         : boolean   read FDeployToDirty   write FDeployToDirty ;
    property SourceFileNotFound    : boolean   read FSourceFileNotFound write FSourceFileNotFound ;
    property Dirty ;

  public
    constructor create ; override ;
    property  FileVersion    : string    read FsFileVersion    write FsFileVersion ;
    property  Owner : TtiDeployApp read GetOwner write SetOwner ;
    function  Equals( pData : TPerObjAbs ) : boolean ; override ;
  end ;

  //----------------------------------------------------------------------------
  TtiDeployLaunchApps = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TtiDeployLaunchApp ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDeployLaunchApp); reintroduce ;
    function    GetOwner: TtiDeployApps; reintroduce ;
    procedure   SetOwner(const Value: TtiDeployApps); reintroduce ;
  public
    property    Items[i:integer] : TtiDeployLaunchApp read GetItems write SetItems ;
    property    Owner        : TtiDeployApps           read GetOwner      write SetOwner ;
    procedure   Add( pObject : TtiDeployLaunchApp ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    procedure   Refresh ;
  published
  end ;

  //----------------------------------------------------------------------------
  TtiDeployLaunchApp  = class( TPerObjAbs )
  private
    FDeployApp: TtiDeployApp;
    FDeployParam : TtiDeployParam ;
    function GetDescription: string;
    function GetDisplayText: string;
  protected
    function    GetOwner: TtiDeployLaunchApps; reintroduce ;
    procedure   SetOwner(const Value: TtiDeployLaunchApps); reintroduce ;
  public
    property    Owner        : TtiDeployLaunchApps             read GetOwner      write SetOwner ;
    property    DeployApp    : TtiDeployApp   read FDeployApp   write FDeployApp ;
    property    DeployParam  : TtiDeployParam read FDeployParam write FDeployParam ;
  published
    property    DisplayText : string read GetDisplayText ;
    property    AppDescription : string read GetDescription ;
  end ;

implementation
uses
   tiUtils
  ,SysUtils
  ,tiLog
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDeployFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Read the date and size of the local copy of a file
//------------------------------------------------------------------------------
//function TtiDeployFile.GetDeployFromDirty: boolean;
//begin
//  result := IsDirty( DeployFrom ) ;
//end ;

//------------------------------------------------------------------------------
//function TtiDeployFile.IsDirty( const psFileName : string ) : boolean;
//var
//  lDT : TDateTime ;
//  liSize : integer ;
//begin
//  try
//    tiReadFileDateSize(psFileName, lDT, liSize ) ;
//    result := ( FileDate <> lDT ) or ( FileSize <> liSize ) ;
//{    Log([
//          psFileName,
//          result,
//          tiDateTimeToStr( FileDate ),
//          tiDateTimeToStr( lDT ),
//          FileSize,
//          liSize
//        ]);
//}
//  except
//    result := true ;
//  end ;
//end;

//function TtiDeployFile.GetDeployToDirty: boolean;
//begin
//  result := IsDirty( tiAddTrailingSlash( DeployToPath ) + FileName ) ;
//end;

//procedure TtiDeployFile.RefreshFileDetails(const psFileName: string);
//var
//  lDT : TDateTime ;
//  liSize : integer ;
//begin
//  try
//    tiReadFileDateSize(psFileName, lDT, liSize ) ;
//    FileDate := lDT ;
//    FileSize := liSize ;
//  except
//    FileDate := cgMinDateTime ;
//    FileSize     := 0 ;
//  end ;
//
////Working on reading file date & size
//
//end;

//function TtiDeployFile.GetDeployToName: string;
//begin
//  result := tiAddTrailingSlash( DeployToPath ) + FileName ;
//end;

//procedure TtiDeployApp.ReadDeployFromDirtyStatus;
//var
//  lData : TtiDeployFile ;
//  i : integer ;
//begin
//  for i := 0 to Count - 1 do begin
//    lData := TtiDeployFile( Items[i] ) ;
//    if lData.DeployFromDirty and
//       ( not lData.dirty ) then
//       lData.Dirty := true ;
//  end ;
//end;

procedure TtiDeployApp.ReadDeployToDirtyStatus;
var
  lData : TtiDeployFile ;
  i : integer ;
begin
  for i := 0 to Count - 1 do begin
    lData := TtiDeployFile( Items[i] ) ;
    if lData.DeployToDirty and
       ( not lData.dirty ) then
       lData.Dirty := true ;
  end ;
end;

function TtiDeployFile.GetDeployToPath: string;
begin
  Assert( Owner <> Nil, 'Owner not assigned' ) ;
  if SameText( DeployToRoot, cgWinSysDir ) then
    result := tiAddTrailingSlash( tiGetWindowsSysDir )
  else
  begin
{
    // This will deploy to a directory made up as:
    // DeployToRoot + ApplicationName + Database Name
    Result := tiAddTrailingSlash(
                tiAddTrailingSlash( DeployToRoot ) + Owner.AppName ) +
                Owner.DatabaseName ;
    Result := tiCIStrTran( Result, 'http://', '\' ) ;
    Result := tiStrTran( Result, '/', '\' ) ;
    Result := tiStrTran( Result, '\\', '\' ) ;
}
    Result := tiGetEXEPath ;
  end ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDeployApps
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDeployApps.Add(pObject: TtiDeployApp; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

constructor TtiDeployApps.Create;
begin
  inherited;
  FDeployLaunchApps           := TtiDeployLaunchApps.Create ;
  FDeployLaunchApps.Owner     := Self ;
  FDeployLaunchApps.ItemOwner := Self ;
end;

destructor TtiDeployApps.Destroy;
begin
  FDeployLaunchApps.Free ;
  inherited;
end;

function TtiDeployApps.GetCaption: string;
begin
  result := 'TechInsite deployment manager' ;
end;

function TtiDeployApps.GetItems(i: integer): TtiDeployApp;
begin
  result := TtiDeployApp( inherited GetItems( i )) ;
end;

procedure TtiDeployApps.SetItems(i: integer; const Value: TtiDeployApp);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDeployApps.SetOwner(const Value: TtiDeployApp);
begin
  inherited SetOwner( Value ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDeployApp
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDeployApp.Add(pObject: TtiDeployFile; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiDeployApp.GetCaption: string;
begin
  result := AppName ;
end;

function TtiDeployApp.GetItems(i: integer): TtiDeployFile;
begin
  result := TtiDeployFile( inherited GetItems( i )) ;
end;

function TtiDeployApp.GetOwner: TtiDeployApps;
begin
  result := TtiDeployApps( inherited GetOwner ) ;
end;

procedure TtiDeployApp.SetItems(i: integer; const Value: TtiDeployFile);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDeployApp.SetOwner(const Value: TtiDeployApps);
begin
  inherited SetOwner( Value ) ;
end;

function TtiDeployFile.GetOwner: TtiDeployApp;
begin
  result := TtiDeployApp( inherited GetOwner ) ;
end;

procedure TtiDeployFile.SetOwner(const Value: TtiDeployApp);
begin
  inherited SetOwner( Value ) ;
end;

function TtiDeployFile.Equals(pData: TPerObjAbs): boolean;
var
  lData : TtiDeployFile ;
begin
  lData := TtiDeployFile( pData ) ;
  result :=
    ( lData.DeployFromPathAndName = DeployFromPathAndName ) and
    ( lData.DeployToRoot = DeployToRoot ) and
    ( lData.Launch = Launch ) ;
end;

function TtiDeployFile.GetDeployFromPath: string;
begin
  result := ExtractFilePath( FileName ) ;
end;

function TtiDeployFile.GetFileName: string;
begin
  result := ExtractFileName( DeployFromPathAndName ) ;
end;

function TtiDeployFile.GetDeployToPathAndName: string;
begin
  result := tiAddTrailingSlash( DeployToPath ) + FileName ;
end;

{ TtiDeployParams }

procedure TtiDeployParams.Add(pObject: TtiDeployParam;pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject ) ;
end;

function TtiDeployParams.GetItems(i: integer): TtiDeployParam;
begin
  result := TtiDeployParam( inherited GetItems( i )) ;
end;

function TtiDeployParams.GetOwner: TtiDeployApp;
begin
  result := TtiDeployApp( inherited GetOwner ) ;
end;

procedure TtiDeployParams.SetItems(i: integer; const Value: TtiDeployParam);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDeployParams.SetOwner(const Value: TtiDeployApp);
begin
  inherited SetOwner( Value ) ;
end;

{ TtiDeployParam }

function TtiDeployParam.GetOwner: TtiDeployApp;
begin
  result := TtiDeployApp( inherited GetOwner ) ;
end;

procedure TtiDeployParam.SetOwner(const Value: TtiDeployApp);
begin
  inherited SetOwner( Value ) ;
end;

constructor TtiDeployApp.Create;
begin
  inherited;
  FDeployParams := TtiDeployParams.Create ;
  FDeployParams.Owner := self ;
  FDeployParams.ItemOwner := Self ;
  FImage := TBitMap.Create ;
end;

destructor TtiDeployApp.Destroy;
begin
  FDeployParams.Free;
  FImage.Free ;
  inherited;
end;

{ TtiDeployLaunchApps }

procedure TtiDeployLaunchApps.Add(pObject: TtiDeployLaunchApp; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiDeployLaunchApps.GetItems(i: integer): TtiDeployLaunchApp;
begin
  result := TtiDeployLaunchApp( inherited GetItems( i )) ;
end;

function TtiDeployLaunchApps.GetOwner: TtiDeployApps;
begin
  result := TtiDeployApps( inherited GetOwner ) ;
end;

procedure TtiDeployLaunchApps.Refresh;
var
  lDeployApp   : TtiDeployApp ;
  lDeployParam : TtiDeployParam ;
  lData : TtiDeployLaunchApp ;
  i, j : integer ;
  lbIsData : boolean ;
begin
  Clear ;
  for i := 0 to Owner.Count - 1 do
  begin
    lDeployApp   := Owner.Items[i] ;
    if lDeployApp.Deleted then
      Continue ; //==>
    lbIsData := false ;
    for j := 0 to lDeployApp.DeployParams.Count - 1 do
    begin
      lDeployParam := lDeployApp.DeployParams.Items[j] ;
      if lDeployParam.Deleted then
        Continue ; //==>
      lData := TtiDeployLaunchApp.Create ;
      lData.DeployApp := lDeployApp  ;
      lData.DeployParam := lDeployParam ;
      Add( lData ) ;
      lbIsData := true ;
    end ;
    if not lbIsData then
    begin
      lData := TtiDeployLaunchApp.Create ;
      lData.DeployApp   := lDeployApp  ;
      lData.DeployParam := nil ;
      Add( lData ) ;
    end ;
  end ;
end;

procedure TtiDeployLaunchApps.SetItems(i: integer; const Value: TtiDeployLaunchApp);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDeployLaunchApps.SetOwner(const Value: TtiDeployApps);
begin
  inherited SetOwner( Value ) ;
end;

{ TtiDeployLaunchApp }

function TtiDeployLaunchApp.GetDescription: string;
begin
  result := DeployApp.Description ;
  if ( DeployParam <> nil ) and
     ( DeployParam.Description <> '' ) then
  begin
    result :=
      result + ' ' +
      DeployParam.Description ;
  end ;
end;

function TtiDeployLaunchApp.GetDisplayText: string;
begin
  result := DeployApp.DisplayText ;
  if ( DeployParam <> nil ) and
     ( DeployParam.DisplayText <> '' ) then
  begin
    result :=
      result + ' ' +
      DeployParam.DisplayText ;
  end ;
end;

function TtiDeployLaunchApp.GetOwner: TtiDeployLaunchApps;
begin
  result := TtiDeployLaunchApps( inherited GetOwner ) ;
end;

procedure TtiDeployLaunchApp.SetOwner(const Value: TtiDeployLaunchApps);
begin
  inherited SetOwner( Value ) ;
end;

constructor TtiDeployFile.create;
begin
  inherited;
  FSourceFileNotFound := false ;
end;

end.

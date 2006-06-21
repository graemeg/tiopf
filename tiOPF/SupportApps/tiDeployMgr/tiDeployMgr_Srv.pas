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

  Revision History:
  Nov 1999, PWH, Created

  Purpose: Application deployment manager

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiDeployMgr_Srv;

interface
uses
   tiPtnVis
  ,tiPtnVisSQL
  ,tiDeployMgr_BOM
  ,tiQuery
  ,tiDBConnectionPool
  ,Classes
  ;

type

  //----------------------------------------------------------------------------
  TVisDeployMgrReadPK = class( TVisOwnedQrySelect )
  private
    FLastDeployApp : TtiDeployApp ;
    FStream : TMemoryStream ;
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure MapRowToObject ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  public
    destructor destroy ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrParamsRead= class( TVisOwnedQrySelect )
  private
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure MapRowToObject ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
    procedure Final ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrReadOID = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure MapRowToObject ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
    procedure Final ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrAppCreate = class( TVisOwnedQryUpdate )
  private
    FStream : TMemoryStream ;
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  public
    destructor destroy ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrAppUpdate = class( TVisOwnedQryUpdate )
  private
    FStream : TMemoryStream ;
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  public
    destructor destroy ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrAppDelete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init              ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrFilesReadAbs = class( TVisOwnedQrySelect )
  protected
    procedure MapRowToObject ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrFilesReadByOID = class( TVisDeployMgrFilesReadAbs )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrFileCreate = class( TVisOwnedQryUpdate )
  private
    FCompStream : TMemoryStream ;
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  public
    destructor destroy ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrFileDelete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrParamsCreate = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrParamsUpdate = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrParamsDelete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDeployMgrReadBin = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure MapRowToObject ; override ;
    procedure Init ; override ;
    procedure SetupParams ; override ;
  end ;

  // A visitor to launch files flagged for launching
  // ---------------------------------------------------------------------------
  TDeployMgrLaunch = class( TVisitorAbs )
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

implementation
uses
  tiUtils
  ,tiPtnVisPerObj
  ,tiLog
  ,SysUtils
  ,tiCommandLineParams
  ,FileCtrl
  ,tiPersist
  ,cTIDeployMgr
  ,tiCompressAbs
  ,tiPtnVisPerObj_Cli
  ;

const
  cgQrytiDeployMgrApp_ReadPK =
    'select ' +
    '     a.OID               as OID ' +
    '    ,a.APP_NAME          as App_Name ' +
    '    ,a.DESCRIPTION       as Description ' +
    '    ,a.DisplayText       as DisplayText ' +
    '    ,a.Compression       as Compression ' +
    '    ,a.Image             as Image ' +
    'from ' +
    '     Deploy_Mgr_Apps   a ' +
    'order by ' +
    '    a.APP_NAME ' ;

  cgQryTIDeployMgrFile_Delete =
    'delete from  deploy_mgr_files ' +
    'where ' +
    '   OID             = :OID ' ;

  cgQrytiDeployMgrFile_Create =
    'insert into deploy_mgr_files ' +
    '( ' +
    ' OID, ' +
    ' LastChanged, ' +
    ' LastChanged_By, ' +
    ' OID_Deploy_Mgr_App, ' +
    ' deploy_from, ' +
    ' deploy_to_root, ' +
    ' file_size, ' +
    ' file_date, ' +
    ' file_version, ' +
    ' launch, ' +
    ' file_bin ' +
    ') ' +
    'values ' +
    '( ' +
    ' :OID, ' +
    ' SysDate, ' +
    ' :LastChanged_By, ' +
    ' :OID_Deploy_Mgr_App, ' +
    ' :deploy_from, ' +
    ' :deploy_to_root, ' +
    ' :file_size, ' +
    ' :file_date, ' +
    ' :file_version, ' +
    ' :launch, ' +
    ' :file_bin ' +
    ') ' +
    ' ' ;

  cgQryTIDeployMgr_ReadBin =
    'select ' +
    '  file_bin ' +
    'from ' +
    ' deploy_mgr_files ' +
    'where ' +
    ' OID = :OID ' ;

  cgQrytiDeployMgrApp_Create =
    'insert into Deploy_Mgr_Apps ' +
    '(  OID ' +
    '  ,LastChanged ' +
    '  ,LastChanged_By ' +
    '  ,App_Name ' +
    '  ,Description ' +
    '  ,DisplayText ' +
    '  ,Compression ' +
    '  ,Image ' +
    ') ' +
    'values ' +
    '(  :OID ' +
    '  ,SysDate ' +
    '  ,:LastChanged_By ' +
    '  ,:App_Name ' +
    '  ,:Description ' +
    '  ,:DisplayText ' +
    '  ,:Compression ' +
    '  ,:Image ' +
    ') ' +
    ' ' ;

  cgQrytiDeployMgrApp_Update =
    'update Deploy_Mgr_Apps ' +
    'set ' +
    '   LastChanged     = SysDate ' +
    '  ,LastChanged_By  = :LastChanged_By ' +
    '  ,App_Name        = :App_Name ' +
    '  ,Description     = :Description ' +
    '  ,DisplayText     = :DisplayText ' +
    '  ,Compression     = :Compression ' +
    '  ,Image           = :Image ' +
    'where ' +
    '    OID = :OID ' +
    ' ' +
    ' ' ;

  cgQrytiDeployMgrApp_Delete =
    'delete from  Deploy_Mgr_Apps ' +
    'where ' +
    '   OID             = :OID ' ;

  cgQrytiDeployMgrFiles_ReadByOID =
    'select ' +
    '  OID, ' +
    '  deploy_from, ' +
    '  deploy_to_root, ' +
    '  file_size, ' +
    '  file_date, ' +
    '  file_version, ' +
    '  launch ' +
    'from ' +
    '  deploy_mgr_Files ' +
    'where ' +
    '  OID_Deploy_Mgr_App = :OID_Deploy_Mgr_App ' +
    'order by ' +
    '  deploy_from ' +
    ' ' ;

  cgQrytiDeployMgrApp_ReadOID =
    'select ' +
    '    OID ' +
    '    ,Compression ' +
    'from ' +
    '    Deploy_Mgr_Apps ' +
    'where ' +
    '    Upper( App_Name ) = Upper( :App_Name ) ' ;

  cgQrytiDeployMgrParams_Read =
    'select ' +
    '    OID as      OID ' +
    '    ,Params      as Params ' +
    '    ,Description as Param_Description ' +
    '    ,DisplayText as Param_DisplayText ' +
    'from ' +
    '    Deploy_Mgr_Params  ' +
    'where ' +
    '    OID_Deploy_Mgr_App = :OID_Deploy_Mgr_App ' +
    'order by ' +
    '   Params ' ;

  cgQrytiDeployMgrParams_Create =
    'insert into deploy_mgr_Params ' +
    '( ' +
    '    OID ' +
    '    ,LastChanged ' +
    '    ,LastChanged_By ' +
    '    ,OID_Deploy_Mgr_App ' +
    '    ,Params ' +
    '    ,DisplayText ' +
    '    ,Description ' +
    ') ' +
    'Values ' +
    '( ' +
    '    :OID ' +
    '    ,SysDate ' +
    '    ,:LastChanged_By ' +
    '    ,:OID_Deploy_Mgr_App ' +
    '    ,:Params ' +
    '    ,:DisplayText ' +
    '    ,:Description ' +
    ') ' ;

  cgQrytiDeployMgrParams_Update =
    'update deploy_mgr_Params ' +
    'set ' +
    '    LastChanged     = SysDate ' +
    '    ,LastChanged_By = :LastChanged_By ' +
    '    ,Params         = :Params ' +
    '    ,DisplayText    = :DisplayText ' +
    '    ,Description    = :Description ' +
    'where ' +
    '    OID = :OID ' ;

  cgQrytiDeployMgrParams_Delete =
    'delete from  deploy_mgr_Params ' +
    'where ' +
    '   OID             = :OID ' +
    ' ' ;


//SQL

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDeployMgrAbstract
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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

// If the file is flagged to be launched, then launch it.
//------------------------------------------------------------------------------

{ TDeployMgrReadAll }

{
function TDeployMgrReadAll.AcceptVisitor: boolean;
begin
  result := (  Visited is TtiDeployFiles ) ;
end;

procedure TDeployMgrReadAll.DoExecute;
begin
  Query.SQL.Text := cuSQLReadAll ;
  Query.Open ;
  while not Query.EOF do begin
    MapRowToObject( TtiDeployFiles( Visited )) ;
    Query.Next ;
  end ;
  Query.Close ;
  TtiDeployFiles( Visited ).ObjectState := posClean ;
end;

procedure TDeployMgrReadAll.MapRowToObject(pList: TtiDeployFiles);
var
  lDeployFile : TtiDeployFile ;
begin
  lDeployFile := TtiDeployFile.Create ;
  with lDeployFile do begin
    OID            := Query.FieldAsInteger[  'ID'           ] ;
    FileGroup      := Query.FieldAsString[   'File_Group'   ] ;
    FileName       := Query.FieldAsString[   'File_Name'    ] ;
    DeployToRoot   := Query.FieldAsString[   'Deploy_To_Root'   ] ;
    DeployFromPath := Query.FieldAsString[   'Deploy_From_Path' ] ;
    FileSize       := Query.FieldAsInteger[  'File_Size'    ] ;
    FileDate       := Query.FieldAsDateTime[ 'File_Date'    ] ;
    FileVersion    := Query.FieldAsString[   'File_Version' ] ;
    Compression    := Query.FieldAsString[   'Compression'  ] ;
    Launch         := Query.FieldAsBoolean[  'Launch'       ] ;
    ObjectState    := posClean ;
    pList.Add( lDeployFile ) ;
    Log( 'Reading: ' + FileName ) ;
  end ;
end;
}

{ TVisDeployMgrReadPK }

function TVisDeployMgrReadPK.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApps )  ;
end;

destructor TVisDeployMgrReadPK.destroy;
begin
  FStream.Free ;
  inherited;
end;

procedure TVisDeployMgrReadPK.Init;
begin
  Query.SQLText := cgQrytiDeployMgrApp_ReadPK ;
  FLastDeployApp := nil ;
  FStream := TMemoryStream.Create ;
end;

procedure TVisDeployMgrReadPK.MapRowToObject;
begin

  if ( FLastDeployApp = nil ) or
     ( not FLastDeployApp.OID.EqualsQueryField('OID', Query)) then
  begin
    FLastDeployApp := TtiDeployApp.Create ;
    FLastDeployApp.Oid.AssignFromTIQuery(Query) ;
    FLastDeployApp.AppName         := Query.FieldAsString[   'APP_NAME'        ] ;
    FLastDeployApp.Description     := Query.FieldAsString[   'Description' ] ;
    FLastDeployApp.DisplayText     := Query.FieldAsString[   'DisplayText' ] ;
    FLastDeployApp.Compression     := Query.FieldAsString[   'Compression'     ] ;
    Query.AssignFieldAsStream( 'Image', FStream ) ;
    FLastDeployApp.Image.LoadFromStream( FStream ) ;
    FLastDeployApp.ObjectState     := posPK ;
    TPerObjList( Visited ).Add( FLastDeployApp ) ;
  end ;
end;

procedure TVisDeployMgrReadPK.SetupParams;
begin
  // Do nothing
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisDeployMgrFileDelete
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisDeployMgrFileDelete.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployFile ) and
            ( Visited.ObjectState in [ posDelete, posUpdate ]) ;
end;

procedure TVisDeployMgrFileDelete.Init;
begin
  Query.SQLText := cgQryTIDeployMgrFile_Delete ;
end;

procedure TVisDeployMgrFileDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query); ;
end;

{ TVisDeployMgrFileCreate }

function TVisDeployMgrFileCreate.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployFile ) and
            ( Visited.ObjectState in [ posCreate, posUpdate ]) and
            ( not TtiDeployFile( Visited ).SourceFileNotFound ) ;
end;

destructor TVisDeployMgrFileCreate.destroy;
begin
  FCompStream.Free ;
  inherited;
end;

procedure TVisDeployMgrFileCreate.Init;
begin
  Query.SQLText := cgQrytiDeployMgrFile_Create ;
  if FCompStream = nil then
    FCompStream := TMemoryStream.Create ;
end;

procedure TVisDeployMgrFileCreate.SetupParams;
var
  lComp         : TtiCompressAbs ;
  lrCompRatio   : real ;
  lData : TtiDeployFile ;
begin
  lData := TtiDeployFile( Visited ) ;

  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[   'LastChanged_By'    ] := 'UNKNOWN' ;
  lData.Owner.OID.AssignToTIQuery('OID_Deploy_Mgr_App',Query) ;
  Query.ParamAsString[   'deploy_to_root'   ] := lData.DeployToRoot   ;
  Query.ParamAsString[   'deploy_from'      ] := lData.DeployFromPathAndName ;
  Query.ParamAsDateTime[ 'file_date'        ] := lData.FileDate       ;
  Query.ParamAsInteger[  'file_size'        ] := lData.FileSize       ;
  Query.ParamAsString[   'file_version'     ] := 'UNKNOWN'      ;
  Query.ParamAsBoolean[  'Launch'           ] := lData.Launch         ;

  lComp := gCompressFactory.CreateInstance( lData.Owner.Compression ) ;
  try
    Log( 'Compressing: ' + lData.DeployFromPathAndName ) ;
    lrCompRatio := lComp.CompressStream( lData.Stream, FCompStream ) ;
    LogFmt( 'Compressed size: %f%%', [ lrCompRatio ]) ;
  finally
    lComp.Free ;
  end ;
  Query.AssignParamFromStream( 'File_Bin', FCompStream ) ;
  Log( 'Inserting file into database' ) ;
  Log( 'Done' ) ;

end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisDeployMgrReadBin
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisDeployMgrReadBin.AcceptVisitor: boolean;
var
  lbDirty : boolean ;
begin
  result := ( Visited is TtiDeployFile ) ;
  if not result then
    Exit ; //==>

  lbDirty := TtiDeployFile( Visited ).DeployToDirty ;
  result := lbDirty ;
  
end;

procedure TVisDeployMgrReadBin.Init;
begin
  Query.SQLText := cgQryTIDeployMgr_ReadBin ;
end;

procedure TVisDeployMgrReadBin.MapRowToObject;
var
  lMemoryStream  : TMemoryStream ;
  lComp          : TtiCompressAbs ;
  lDeployFile    : TtiDeployFile ;
begin

  lDeployFile    := TtiDeployFile( Visited ) ;

  Log( 'About to download ' + lDeployFile.DeployToPathAndName ) ;

  lMemoryStream := TMemoryStream.Create ;
  try
    Query.AssignFieldAsStream( 'File_Bin', lMemoryStream ) ;
    lComp := gCompressFactory.CreateInstance( lDeployFile.Owner.Compression ) ;
    try
      Log( Format( 'Decompressing <%s> using <%s',
                   [ lDeployFile.DeployToPathAndName, lDeployFile.Owner.Compression ])) ;
      lComp.DecompressStream( lMemoryStream, lDeployFile.Stream ) ;
    finally
      lComp.Free ;
    end ;
  finally
    lMemoryStream.Free ;
  end;
end;

procedure TVisDeployMgrReadBin.SetupParams;
begin
  TtiDeployFile( Visited ).OID.AssignToTIQuery(Query) ;
end;

procedure TDeployMgrLaunch.Execute(const pVisited: TVisitedAbs);
var
  lsParams : string ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  Log( 'About to run download for' ) ;
  if not AcceptVisitor then
    Exit ; //==>


  Assert( false, 'Under Construction' ) ;
    //  lsParams := TtiDeployFiles( TtiDeployFile( Visited ).Owner ).CommandLineParams ;

  Log( 'Launching ' + TtiDeployFile( Visited ).DeployToPathAndName +
       ' ' + lsParams ) ;

  // Launch the application.
  tiShellExecute( TtiDeployFile( Visited ).DeployToPathAndName, lsParams ) ;

end;

{ TVisDeployMgrReadGroup }

//function TVisDeployMgrReadGroup.AcceptVisitor: boolean;
//begin
//  Assert( false, 'Under Construction' ) ;
////  result := (  Visited is TtiDeployFiles ) ;
//end;
//
//procedure TVisDeployMgrReadGroup.Init;
//begin
//  QueryName := cgQryDeployMgr_ReadGroup ;
//end;
//
//procedure TVisDeployMgrReadGroup.MapRowToObject;
//var
//  lDeployFile : TtiDeployFile ;
//begin
////
////  lDeployFile := TtiDeployFile.Create ;
////  with lDeployFile do begin
////    OID            := Query.FieldAsInteger[  'ID'               ] ;
////    FileGroup      := TtiDeployFiles( Visited ).FileGroup ;
////    FileName       := Query.FieldAsString[   'File_Name'        ] ;
////    DeployToRoot   := Query.FieldAsString[   'Deploy_To_Root'   ] ;
////    DeployFromPath := Query.FieldAsString[   'Deploy_From_Path' ] ;
////    FileSize       := Query.FieldAsInteger[  'File_Size'        ] ;
////    FileDate       := Query.FieldAsDateTime[ 'File_Date'        ] ;
////    FileVersion    := Query.FieldAsString[   'File_Version'     ] ;
////    Compression    := Query.FieldAsString[   'Compression'      ] ;
////    Launch         := Query.FieldAsBoolean[  'Launch'           ] ;
////    TtiDeployFiles( Visited ).Add( lDeployFile ) ;
////  end ;
//  Assert( false, 'Under Construction' ) ;
//end;
//
//procedure TVisDeployMgrReadGroup.SetupParams;
//begin
//  Assert( false, 'Under Construction' ) ;
////  Query.ParamAsString[ 'File_Group' ] := TtiDeployFiles( Visited ).FileGroup ;
//end;
//

{ TVisDeployMgrAppCreate }

function TVisDeployMgrAppCreate.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posCreate ) ;
end;

destructor TVisDeployMgrAppCreate.destroy;
begin
  FStream.Free ;
  inherited;
end;

procedure TVisDeployMgrAppCreate.Init;
begin
  Query.SQLText := cgQrytiDeployMgrApp_Create ;
  FStream := TMemoryStream.Create ;
end;

procedure TVisDeployMgrAppCreate.SetupParams;
var
  lData : TtiDeployApp ;
begin
  lData := TtiDeployApp( Visited ) ;
  lData.OID.AssignToTIQuery(Query) ;
  Query.ParamAsString[  'LastChanged_By' ] := 'UNKNOWN' ;
  Query.ParamAsString[  'App_Name' ] := lData.AppName ;
  Query.ParamAsString[  'Description' ] := lData.Description ;
  Query.ParamAsString[  'DisplayText' ] := lData.DisplayText ;
  Query.ParamAsString[  'Compression' ] := lData.Compression ;
  lData.Image.SaveToStream( FStream ) ;
  Query.AssignParamFromStream( 'Image', FStream ) ;
end;

{ TVisDeployMgrAppUpdate }

function TVisDeployMgrAppUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posUpdate ) ;
end;

destructor TVisDeployMgrAppUpdate.destroy;
begin
  FStream.Free ;
  inherited;
end;

procedure TVisDeployMgrAppUpdate.Init;
begin
  Query.SQLText := cgQrytiDeployMgrApp_Update ;
  FStream := TMemoryStream.Create ;
end;

procedure TVisDeployMgrAppUpdate.SetupParams;
var
  lData : TtiDeployApp ;
begin
  lData := TtiDeployApp( Visited ) ;
  lData.OID.AssignToTIQuery(Query) ;
  Query.ParamAsString[  'LastChanged_By' ] := 'UNKNOWN';
  Query.ParamAsString[  'App_Name' ] := lData.AppName ;
  Query.ParamAsString[  'Description' ] := lData.Description ;
  Query.ParamAsString[  'DisplayText' ] := lData.DisplayText ;
  Query.ParamAsString[  'Compression' ] := lData.Compression ;
  lData.Image.SaveToStream( FStream ) ;
  Query.AssignParamFromStream( 'Image', FStream ) ;
end;

{ TVisDeployMgrAppDelete }

function TVisDeployMgrAppDelete.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posDelete ) ;
end;

procedure TVisDeployMgrAppDelete.Init;
begin
  Query.SQLText := cgQrytiDeployMgrApp_Delete ;
end;

procedure TVisDeployMgrAppDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

{ TVisDeployMgrFilesReadByOID }

function TVisDeployMgrFilesReadByOID.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posPK ) ;
end;

procedure TVisDeployMgrFilesReadByOID.Init;
begin
  Query.SQLText := cgQrytiDeployMgrFiles_ReadByOID ;
end;

procedure TVisDeployMgrFilesReadByOID.SetupParams;
begin
  Visited.OID.AssignToTIQuery('OID_Deploy_Mgr_App', Query)  ;
end;

{ TVisDeployMgrFilesReadAbs }

procedure TVisDeployMgrFilesReadAbs.MapRowToObject;
var
  lData : TtiDeployFile ;
begin
  lData := TtiDeployFile.Create ;
  lData.OID.AssignFromTIQuery(Query);
  lData.DeployFromPathAndName     := Query.FieldAsString[   'Deploy_From'      ] ;
  lData.DeployToRoot   := Query.FieldAsString[   'Deploy_To_Root'   ] ;
  lData.FileSize       := Query.FieldAsInteger[  'File_Size'        ] ;
  lData.FileDate       := Query.FieldAsDateTime[ 'File_Date'        ] ;
  lData.FileVersion    := Query.FieldAsString[   'File_Version'     ] ;
  lData.Launch         := Query.FieldAsBoolean[  'Launch'           ] ;
  lData.ObjectState    := posClean ;
  TtiDeployApp( Visited ).Add( lData ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisDeployMgrReadOID
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisDeployMgrReadOID.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posPK ) ;
end;

procedure TVisDeployMgrReadOID.Final;
begin
  // Do nothing
end;

procedure TVisDeployMgrReadOID.Init;
begin
  Query.SQLText := cgQrytiDeployMgrApp_ReadOID ;
end;

procedure TVisDeployMgrReadOID.MapRowToObject;
var
  lData : TtiDeployApp ;
begin
  lData := TtiDeployApp( Visited ) ;
  lData.OID.AssignFromTIQuery(Query);
  lData.Compression := Query.FieldAsString[ 'Compression' ] ;
end;

procedure TVisDeployMgrReadOID.SetupParams;
begin
  Query.ParamAsString[ 'App_Name' ] := TtiDeployApp( Visited ).AppName ;
end;


{ TVisDeployMgrParamsCreate }

function TVisDeployMgrParamsCreate.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployParam ) and
            ( Visited.ObjectState = posCreate ) ;
end;

procedure TVisDeployMgrParamsCreate.Init;
begin
  Query.SQLText := cgQrytiDeployMgrParams_Create ;
end;

procedure TVisDeployMgrParamsCreate.SetupParams;
var
  lData : TtiDeployParam ;
begin
  lData := TtiDeployParam( Visited ) ;
  lData.OID.AssignToTIQuery(Query)         ;
  Query.ParamAsString[   'LastChanged_By'     ] := 'UNKNOWN'         ;
  lData.Owner.OID.AssignToTIQuery('OID_Deploy_Mgr_App',Query)   ;
  Query.ParamAsString[   'Params'             ] := lData.ParamStr    ;
  Query.ParamAsString[   'Description'        ] := lData.Description ;
  Query.ParamAsString[   'DisplayText'        ] := lData.DisplayText ;
end;

{ TVisDeployMgrParamsUpdate }

function TVisDeployMgrParamsUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployParam ) and
            ( Visited.ObjectState = posUpdate ) ;
end;

procedure TVisDeployMgrParamsUpdate.Init;
begin
  Query.SQLText := cgQrytiDeployMgrParams_Update ;
end;

procedure TVisDeployMgrParamsUpdate.SetupParams;
var
  lData : TtiDeployParam ;
begin
  lData := TtiDeployParam( Visited ) ;
  lData.OID.AssignToTIQuery(Query)      ;
  Query.ParamAsString[   'LastChanged_By' ] := 'UNKNOWN'      ;
  Query.ParamAsString[   'Params'         ] := lData.ParamStr ;
  Query.ParamAsString[   'Description'    ] := lData.Description ;
  Query.ParamAsString[   'DisplayText'    ] := lData.DisplayText ;
end;

{ TVisDeployMgrParamsDelete }

function TVisDeployMgrParamsDelete.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployParam ) and 
            ( Visited.ObjectState = posDelete ) ;
end;

procedure TVisDeployMgrParamsDelete.Init;
begin
  Query.SQLText := cgQrytiDeployMgrParams_Delete ;
end;

procedure TVisDeployMgrParamsDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

{ TVisDeployMgrParamsRead }

function TVisDeployMgrParamsRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDeployApp ) and
            ( Visited.ObjectState = posPK ) ;
end;

procedure TVisDeployMgrParamsRead.Final;
begin
  // Do nothing
end;

procedure TVisDeployMgrParamsRead.Init;
begin
  Query.SQLText := cgQrytiDeployMgrParams_Read ;
end;

procedure TVisDeployMgrParamsRead.MapRowToObject;
var
  lParam : TtiDeployParam ;
begin
   lParam := TtiDeployParam.Create ;
  lParam.OID.AssignFromTIQuery('OID', Query);
  lParam.ParamStr       := Query.FieldAsString[   'Params'            ] ;
  lParam.DisplayText    := Query.FieldAsString[   'Param_DisplayText' ] ;
  lParam.Description    := Query.FieldAsString[   'Param_Description' ] ;
  lParam.ObjectState    := posClean ;
  ( Visited as TtiDeployApp ).DeployParams.Add( lParam ) ;
end;

procedure TVisDeployMgrParamsRead.SetupParams;
begin
  Visited.OID.AssignToTIQuery('OID_Deploy_Mgr_App', Query);
end;

initialization

  // Read visitors:
  // Read all (primary key info)
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrReadPK,    TVisDeployMgrReadPK ) ;

  // Read all by group
  // gVisMgr.RegisterVisitor( cgsDeployMgrReadApp,   TVisDeployMgrFilesReadByAppName ) ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrReadAppDetails, TVisDeployMgrReadOID ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrReadAppDetails, TVisDeployMgrParamsRead ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrReadAppDetails, TVisDeployMgrFilesReadByOID ) ;

  // Download a file which is out of date
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrReadBin, TVisDeployMgrReadBin ) ;
//  gVisMgr.RegisterVisitor( cgsDeployMgrLaunch,   TDeployMgrLaunch ) ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrParamsDelete ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrFileDelete ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrAppDelete ) ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrAppCreate ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrFileCreate ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrParamsCreate ) ;

  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrAppUpdate ) ;
  gTIPerMgr.VisMgr.RegisterVisitor( cgsDeployMgrSave,      TVisDeployMgrParamsUpdate ) ;

end.

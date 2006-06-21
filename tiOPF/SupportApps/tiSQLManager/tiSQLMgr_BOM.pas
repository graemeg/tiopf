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
    Nov 2000, Peter Hinrichsen, Made open source
    Nov 2000, SM, Moved query find method to TSQLMgr
    Nov 2000, SM, Added ParamIsNull property to TSQLMgrParam

  Purpose:
    Family of objects to manage SQL Text as read from database

  Classes:

  ToDo:
    1. Manage relationship between persistence layer and SQLManager objects
    2. Unload SQLManager mappings when unloading a persistence layer

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiSQLMgr_BOM;

interface
uses
   classes
  ,tiPtnVis
  ,tiPtnVisPerObj
  ,tiQuery
  ,Windows
  ,tiExcept
  ,SyncObjs
  ,tiPerObjOIDAbs
  ;

const
  cExcCanNotSetObjectStateDeleteWhenPK = 'Can not set ObjectState to posDelete when ObjectState = posPK' ;

type

  TSQLMgrParamName = string[30];

  // Forward dec of a query
  TSQLMgrs      = class ;
  TSQLMgr       = class ;
  TSQLMgrGroup  = class ;
  TSQLMgrQuery  = class ;
  TSQLMgrParams = class ;
  TSQLMgrParam  = class ;

  // A visitor for finding a group of queries based on a pattern match on the
  // queries name.
  //----------------------------------------------------------------------------
  TVisFindQueriesByName = class ( TVisitorAbs )
  private
    FsPattern: string;
    FList: TList;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    property  List : TList read FList write FList ;
    property  Pattern : string read FsPattern write FsPattern ;
    procedure Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TSQLMgrs = class( TPerObjList )
  private
    FCritSect : TCriticalSection;
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TSQLMgr ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgr); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Items[i:integer] : TSQLMgr read GetItems write SetItems ;
    procedure   Add( pObject : TSQLMgr ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    function    AddDatabase( const pDatabaseName : string ) : TSQLMgr ;
    function    FindByDatabaseName( const pDatabaseName : string ) : TSQLMgr ;
    procedure   Clear ; override ;
  published
  end ;

  // The main container for SQLManager data
  TSQLMgr = class( TPerObjList )
  private
    FDatabaseName: string;
    FQueries : TSQLMgrGroup;
    FCritSect : TCriticalSection;
    FFileName: string;
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TSQLMgrGroup ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgrGroup); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    FileName : string read FFileName Write FFileName;
    class procedure CreateFile(   const pDBConnectionName : string ) ;

    function    FindQueryByName(const pQueryName: String): TSQLMgrQuery;
    function    FindCreateQueryByName(const pQueryName: String): TSQLMgrQuery;
    procedure   FindQueriesByName( const psPattern : String ; pList : TList );
    procedure   ValidateModel ;
    procedure   CreateTables;
    procedure   DropTables;

    //class function  CheckTableStructure( const pDBConnectionName : string = '' ;
    //                          const pPerLayerName     : string = ''): boolean ;
    procedure   Read(     const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; override ;
    procedure   ReadPK(   const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; override ;
    procedure   Save(     const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; override ;
    property    Items[i:integer] : TSQLMgrGroup read GetItems write SetItems ; default ;
    procedure   Add( pObject : TSQLMgrGroup   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Queries : TSQLMgrGroup read FQueries ;
    function    IsGroupNameUnique(const pGroup : TSQLMgrGroup): boolean ;
    function    IsQueryNameUnique(const pQuery : TSQLMgrQuery): boolean ;


  published
    property    DatabaseName : string read FDatabaseName    write FDatabaseName ;
  end ;

  TSQLMgrGroup = class( TPerObjList )
  private
    FStrGroupName : string ;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TSQLMgr; reintroduce ;
    procedure   SetOwner(const Value: TSQLMgr); reintroduce ;
    function    GetItems(i: integer): TSQLMgrQuery; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgrQuery); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner : TSQLMgr read GetOwner write SetOwner ;
    property    Items[i:integer] : TSQLMgrQuery read GetItems write SetItems ; default ;
    procedure   Add( pObject : TPerObjAbs   ; pDefDispOrdr : boolean = true ) ; override ;
  published
    property    GroupName : string read FStrGroupName write FStrGroupName ;
    property    Caption ;
    property    DispOrder;
  end ;

  TSQLMgrQuery = class( TPerObjAbs )
  private
    FStrQueryName : string ;
    FStrQueryDesc : string ;
    FStrSQL       : string ;
    FParams       : TSQLMgrParams ;
    FbQueryLocked : boolean;
    FbTestInclude : boolean;
    FQueryVersion: integer;
    function GetQueryGroupName: string;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TSQLMgrGroup; reintroduce ;
    procedure   SetOwner(const Value: TSQLMgrGroup); reintroduce ;
    procedure   SetObjectState(const Value: TPerObjectState) ; override ;
  public
    constructor create ; override ;
    destructor  destroy ; override ;
    property    Owner : TSQLMgrGroup read GetOwner write SetOwner ;
    procedure   ReadByQueryName( const pQueryName : string ;
                  const pSQLMgrFileName : string ) ;
    function    IsParamNameUnique(const pParam : TSQLMgrParam): boolean ;

  published
    property    DispOrder;
    property    QueryName      : string        read FStrQueryName write FStrQueryName ;
    property    QueryDesc      : string        read FStrQueryDesc write FStrQueryDesc ;
    property    QueryGroupName : string        read GetQueryGroupName ;
    property    QueryVersion   : integer       read FQueryVersion write FQueryVersion ;
    property    SQL            : string        read FStrSQL       write FStrSQL ;
    property    Params         : TSQLMgrParams read FParams       write FParams ;
    property    QueryLocked    : boolean       read FbQueryLocked  write FbQueryLocked;
    property    TestInclude    : boolean       read FbTestInclude  write FbTestInclude;
    property    Caption ;
    function    Clone : TSQLMgrQuery ; reintroduce ;
    procedure   Assign( pData : TSQLMgrQuery ) ; reintroduce ;
  end ;

  TSQLMgrParams = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TSQLMgrParam ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgrParam); reintroduce ;
    function    GetOwner: TSQLMgrQuery; reintroduce ;
    procedure   SetOwner(const Value: TSQLMgrQuery); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    function    AsSetupParamsDelphiCode : string ;
    property    Items[i:integer] : TSQLMgrParam read GetItems write SetItems ;
    procedure   Add( pObject : TSQLMgrParam   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TSQLMgrQuery read GetOwner      write SetOwner ;
  published
  end ;

  TSQLMgrParam = class( TPerObjAbs )
  private
    FStrParamName  : TSQLMgrParamName ;
    FParamType  : TtiQueryFieldKind ;
    FStrParamValue : string ;
    FbIsNull : Boolean;
  protected
    function  GetCaption : string ; override ;
    function  GetParamTypeStr : string;
    procedure SetParamTypeStr(const Value: String);
  public
    function  ParamTypeAsTIQueryParamType : string ;
  published
    property  Caption ;
    property  DispOrder;
    property  ParamName  : TSQLMgrParamName  read FStrParamName  write FStrParamName ;
    property  ParamType  : TtiQueryFieldKind  read FParamType  write FParamType ;
    property  ParamTypeStr : String read GetParamTypeStr write SetParamTypeStr ;
    property  ParamValue : string  read FStrParamValue write FStrParamValue ;
    property  IsNull     : boolean read FbIsNull       write FbIsNull;
    function  Clone : TSQLMgrParam ; reintroduce ;
  end ;


function  gSQLMgrs: TSQLMgrs;
function  SQLParamAsProp( const pParamName : string ; pWidth : integer = 0) : string ;
procedure RegisterMappings;

const
  cTableNameSQLManGroup    = 'sqlman_group';
  cFieldNameGroupOID       = 'oid';
  cFieldNameGroupDispOrder = 'disp_order';
  cFieldNameGroupName      = 'group_name';

  cTableNameSQLManSQL      = 'sqlman_sql';
  cFieldNameSQLOID         = 'oid';
  cFieldNameSQLOIDGroup    = 'group_oid';
  cFieldNameSQLDispOrder   = 'disp_order';
  cFieldNameSQLVersion     = 'query_version';
  cFieldNameSQLName        = 'query_name';
  cFieldNameSQLDesc        = 'query_description';
  cFieldNameSQLLocked      = 'query_locked';
  cFieldNameSQLTestInclude = 'test_include';
  // Use this one for SQL Manager tables created before 12/11/2001
  //cFieldNameSQLSQL : string = 'SQL' ;
  // Use this one for SQL Manager tables created after 12/11/2001
  cFieldNameSQLSQL : string = 'query_sql' ;
  // You can uncomment either line above depending on the structure of your
  // SQLManager tables, or add the following line to the application's DPR file.
  //tiSQLMgr_Svr.cFieldNameSQLSQL := 'SQL' ;

  cTableNameSQLManParam = 'sqlman_param';
  cFieldNameParamOID       = 'oid';
  cFieldNameParamOIDSQL    = 'sql_oid';
  cFieldNameParamDispOrder = 'disp_order';
  cFieldNameParamName      = 'param_name';
  cFieldNameParamType      = 'param_type';
  cFieldNameParamValue     = 'param_value';
  cFieldNameParamIsNull    = 'param_isnull';

  cVisSQLMgrReadPK          = 'VisSQLMgrReadPK';
  cVisSQLMgrReadByQueryName = 'VisSQLMgrReadByQueryName';

implementation
uses
  SysUtils // Exception
  ,tiSQLMgr_Svr // To force visitor registration
  ,tiUtils
  ,tiDialogs
  ,tiPersist
  ,tiLog
  ,Math
  ,cTIPersist
  ,tiClassToDBMap_BOM
  ;

var
  uSQLMgrs: TSQLMgrs;
  uRegisterMappingsCalled : boolean ;

const
  cSemaphoreSQLMgrs = 'SQLMgrs' ;
  cSemaphoreSQLMgr  = 'SQLMgr ' ;

function gSQLMgrs: TSQLMgrs;
begin
  if uSQLMgrs = nil then
    uSQLMgrs:= TSQLMgrs.Create;
  result := uSQLMgrs ;
end;

// Tempting to put this in the Initialization section, but while there is the
// option of overriding cFieldNameSQLSQL, the place that RegisterMappings is
// called must be under control of the programmer.
procedure RegisterMappings;
begin

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup,cTableNameSQLManGroup,'OID',cFieldNameGroupOID,[pktDB]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup,cTableNameSQLManGroup,'GroupName',cFieldNameGroupName,[pktReadable]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrGroup,cTableNameSQLManGroup,'DispOrder',cFieldNameGroupDispOrder,[pktReadable]);
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TSQLMgr,TSQLMgrGroup);

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'OID',         cFieldNameSQLOID,[pktDB]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'Owner.OID',   cFieldNameSQLOIDGroup,[pktFK]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'DispOrder',   cFieldNameSQLDispOrder);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'QueryVersion',cFieldNameSQLVersion);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'QueryName',   cFieldNameSQLName);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'QueryDesc',   cFieldNameSQLDesc);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'QueryLocked', cFieldNameSQLLocked);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'TestInclude', cFieldNameSQLTestInclude);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrQuery,cTableNameSQLManSQL,'SQL',         cFieldNameSQLSQL);
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TSQLMgrGroup,TSQLMgrQuery);

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'OID',         cFieldNameParamOID,[pktDB]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'Owner.OID',   cFieldNameParamOIDSQL,[pktFK]);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'DispOrder',   cFieldNameParamDispOrder);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'ParamName',   cFieldNameParamName);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'ParamTypeStr',cFieldNameParamType);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'ParamValue',  cFieldNameParamValue);
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TSQLMgrParam,cTableNameSQLManParam,'IsNull',      cFieldNameParamIsNull);
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TSQLMgrParams,TSQLMgrParam);

  uRegisterMappingsCalled := true ;

end ;

function SQLParamAsProp( const pParamName : string ; pWidth : integer = 0 ) : string ;
begin
  result := tiStrTran( pParamName, '_', ' ' ) ;
  result := tiMixedCase( result ) ;
  result := tiStrTran( result, ' ', '' ) ;
  if pWidth <> 0 then
    result := tiPadR( result, pWidth ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TSQLMgr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TSQLMgr.GetCaption: string;
begin
  result := 'SQL Manager on ' + DatabaseName ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TSQLMgrGroup
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TSQLMgrGroup.Add(pObject: TPerObjAbs; pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
  if Owner <> nil then
    Owner.Queries.Add(pObject,false);
end;

constructor TSQLMgrGroup.Create;
begin
  inherited;
  OwnsObjects := false ;
  AutoSetItemOwner := true ;
end;

destructor TSQLMgrGroup.Destroy;
begin
  inherited;
end;

function TSQLMgrGroup.getCaption: string;
begin
  result := GroupName ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TSQLMgrQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TSQLMgrQuery.Assign(pData: TSQLMgrQuery);
var
  i : integer ;
begin
  // We can't use the parent classes Assign method because params in an owned
  // list, and the pointer to the list will be overwritten during the assign.
  // Assign must know how to copy owned objects!
  QueryName      := pData.QueryName     ;
  QueryDesc      := pData.QueryDesc     ;
  SQL            := pData.SQL           ;
  QueryLocked    := pData.QueryLocked   ;
  TestInclude    := pData.TestInclude   ;
  Params.Clear ;
  for i := 0 to pData.Params.Count-1 do
    Params.Add( TSQLMgrParam( pData.Params.Items[i] ).Clone ) ;
end;

function TSQLMgrQuery.Clone: TSQLMgrQuery;
begin
  result := TSQLMgrQuery.Create  ;
  result.Assign( Self ) ;
end;

constructor TSQLMgrQuery.create;
begin
  inherited ;
  FParams := TSQLMgrParams.Create  ;
  FParams.Owner := self ;
  FbQueryLocked   := false ;
  FbTestInclude   := false ;
end;

//------------------------------------------------------------------------------
destructor TSQLMgrQuery.destroy;
begin
  FParams.Free ;
  inherited ;
end;

//------------------------------------------------------------------------------
function TSQLMgrQuery.GetCaption: string;
begin
  result := QueryName ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TSQLMgrParam
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TSQLMgrParam.Clone: TSQLMgrParam;
begin
  result := TSQLMgrParam( inherited Clone ) ;
end;

function TSQLMgrParam.GetCaption: string;
begin
  result := ParamName + ', ' +
            cgaQueryFieldKind[ParamType] + ', ' +
            ParamValue ;
end;

function TSQLMgrParam.GetParamTypeStr : string;
begin
  Result := cgaQueryFieldKindSQLMgr[FParamType];
end;

procedure TSQLMgr.FindQueriesByName( const psPattern : String ; pList : TList );
var
  lVis : TVisFindQueriesByName ;
begin

  Assert( pList <> nil, 'List not assigned' ) ;
  Assert( psPattern <> EmptyStr, 'Pattern not assigned' ) ;

  pList.Clear ;

  lVis := TVisFindQueriesByName.Create ;
  try
    lVis.List := pList ;
    lVis.Pattern := psPattern ;
    Iterate( lVis ) ;

  finally
    lVis.Free ;
  end;
end;

//------------------------------------------------------------------------------
function  TSQLMgr.FindQueryByName(const pQueryName: String): TSQLMgrQuery;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Queries.Count - 1 do
    if SameText( Queries.Items[i].QueryName, pQueryName ) and
       ( not Queries.Items[i].Deleted ) then
    begin
      result := Queries.Items[i];
      Exit ; //==>
    end ;
end;

procedure TSQLMgr.ValidateModel;
var
  i, j, k : integer ;
  lSQLMgrParam : TSQLMgrParam ;
  lSQLMgrQuery : TSQLMgrQuery ;
  lGroup         : TSQLMgrGroup ;
  lStringList    : TStringList ;
begin
  lStringList := TStringList.Create ;
  try
    for i := 0 to Count - 1 do begin
      lGroup := TSQLMgrGroup( Items[i] ) ;
      lStringList.Add( 'Group: ' + lGroup.GroupName ) ;
      for j := 0 to lGroup.Count - 1 do begin
        try
          lSQLMgrQuery := TSQLMgrQuery( lGroup.Items[j] ) ;
          if lSQLMgrQuery.ObjectState <> posDelete then begin
            lStringList.Add( '  Query: ' +
                             lSQLMgrQuery.QueryName +
                             ', ' + tiPadR( lSQLMgrQuery.SQL, 20 )) ;
            for k := 0 to lSQLMgrQuery.Params.Count - 1 do begin
              try
                lSQLMgrParam := TSQLMgrParam( lSQLMgrQuery.Params.Items[k] ) ;
                if lSQLMgrParam.ObjectState <> posDelete then
                  lStringList.Add( '    Param: ' + lSQLMgrParam.Caption ) ;
              except
                on e:exception do
                  lStringList.Add( '    Param: Error: ' + e.message ) ;
              end ;
            end ;
          end ;
        except
          on e:exception do
              lStringList.Add( '  Query: Error: ' + e.message ) ;
        end ;
      end ;
    end ;
    tiShowStringList( lStringList ) ;
  finally
    lStringList.Free ;
  end;
end;

function TSQLMgrQuery.GetOwner: TSQLMgrGroup;
begin
  result := TSQLMgrGroup( inherited GetOwner ) ;
end;

function TSQLMgrQuery.GetQueryGroupName: string;
begin
  if Owner = nil then
    result := 'N/A'
  else
    result := TSQLMgrGroup( Owner ).GroupName ;
end;

function TSQLMgrGroup.GetItems(i: integer): TSQLMgrQuery;
begin
  result := TSQLMgrQuery(inherited GetItems(i));
end;

function TSQLMgrGroup.GetOwner: TSQLMgr;
begin
  result := TSQLMgr( inherited GetOwner ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisFindQueriesByName
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisFindQueriesByName.AcceptVisitor: boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            ( not TSQLMgrQuery( Visited ).Deleted ) ;
end;

procedure TVisFindQueriesByName.Execute(const pVisited: TVisitedAbs ) ;
begin
  inherited Execute( pVisited ) ;

  Assert( FList <> nil, 'List not assigned' ) ;
  Assert( FsPattern <> EmptyStr, 'Pattern not assigned' ) ;

  if not AcceptVisitor then
    Exit ; //==>

  if ( tiWildCardMatch( TSQLMgrQuery( pVisited ).QueryName, FsPattern )) or
     ( tiWildCardMatch( TSQLMgrGroup( TSQLMgrQuery( pVisited ).Owner ).GroupName, FsPattern )) then
    FList.Add( pVisited ) ;

end;

function TSQLMgrParam.ParamTypeAsTIQueryParamType: string;
begin
  result := GetParamTypeStr ;
  if result = cgaQueryFieldKind[qfkDateTime] then
    result := 'DateTime'
  else if result = cgaQueryFieldKind[qfkLogical] then
    result := 'Boolean' ;
end;

procedure TSQLMgrParam.SetParamTypeStr(const Value: String);
var
  Index: TtiQueryFieldKind;
begin
  ParamType := Low(TtiQueryFieldKind);
  for Index := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    if SameText( cgaQueryFieldKindSQLMgr[Index], Value ) then
    begin
      ParamType := Index;
      Exit ; //==>
    end;
  tiFmtException( 'Invalid field kind <' + Value + '>',
                  ClassName,
                  'SetParamTypeAsStr');

end;

{ TSQLMgrs }

procedure TSQLMgrs.Add(pObject: TSQLMgr; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TSQLMgrs.AddDatabase(const pDatabaseName: string) : TSQLMgr ;
begin
  result := TSQLMgr.Create ;
  result.DatabaseName := pDatabaseName ;
  Add( result ) ;
end;

procedure TSQLMgrs.Clear;
begin
  inherited;
  ObjectState := posEmpty ;
end;

constructor TSQLMgrs.Create;
begin
  inherited;
  FCritSect := TCriticalSection.Create;
end;

destructor TSQLMgrs.Destroy;
begin
  FCritSect.Free;
  inherited;
end;

function TSQLMgrs.FindByDatabaseName(const pDatabaseName: string): TSQLMgr;
var
  i : integer ;
begin
  result := nil ;
  FCritSect.Enter;
  try
    for i := 0 to Count - 1 do
      if SameText( Items[i].DatabaseName, pDatabaseName ) then
      begin
        result := Items[i] ;
        Break ; //==>
      end ;
    if result = nil then
      result := AddDatabase( pDatabaseName ) ;
  finally
    FCritSect.Leave ;
  end ;
end;

function TSQLMgrs.GetCaption: string;
begin
  result := 'SQLManager Databases' ;
end;

function TSQLMgrs.GetItems(i: integer): TSQLMgr;
begin
  result := TSQLMgr( inherited GetItems( i )) ;
end;

procedure TSQLMgrs.SetItems(i: integer; const Value: TSQLMgr);
begin
  inherited SetItems( i, Value ) ;
end;

function TSQLMgrQuery.IsParamNameUnique(
  const pParam: TSQLMgrParam): boolean;
var
  i : integer;
begin
  result := true ;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Params.Count - 1 do
    if SameText( Params.Items[i].ParamName, pParam.ParamName ) and
       ( not Params.Items[i].OID.Equals(pParam.OID) ) and
       ( not Params.Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ELSE}
  for i := 0 to Params.Count - 1 do
    if SameText( Params.Items[i].ParamName, pParam.ParamName ) and
       ( not Params.Items[i].OID = pParam.OID ) and
       ( not Params.Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ENDIF}
end;

procedure TSQLMgrQuery.ReadByQueryName( const pQueryName : string ;
                                        const pSQLMgrFileName : string ) ;
begin
  Assert(ObjectState = posEmpty, 'ObjectState <> posEmpty');
  Assert(pSQLMgrFileName<>'', 'pSQLMgrFileName not assigned');
  QueryName := pQueryName;
  gTIPerMgr.VisMgr.Execute(cVisSQLMgrReadByQueryName, Self, pSQLMgrFileName, cTIPersistXMLLight);
end;

procedure TSQLMgrQuery.SetObjectState(const Value: TPerObjectState);
begin
  if (ObjectState = posPK) and
     (Value = posDelete) then
    raise EtiOPFProgrammerException.create(cExcCanNotSetObjectStateDeleteWhenPK);
  if ( ObjectState = posDelete ) and
     ( Value = posDeleted ) then
    Params.ObjectState := posDeleted ;
  inherited;
end;

procedure TSQLMgrQuery.SetOwner(const Value: TSQLMgrGroup);
begin
  inherited SetOwner( Value ) ;
end;

procedure TSQLMgrGroup.SetItems(i: integer; const Value: TSQLMgrQuery);
begin
  inherited SetItems(i, Value);
end;

procedure TSQLMgrGroup.SetOwner(const Value: TSQLMgr);
begin
  inherited SetOwner( Value ) ;
end;

{ TSQLMgrParams }

procedure TSQLMgrParams.Add(pObject: TSQLMgrParam; pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TSQLMgrParams.AsSetupParamsDelphiCode: string;
var
  i  : integer ;
  lAreParams : boolean ;
  lWidth : integer ;
begin
  result :=
    'var'                            + Cr +
    '  lData : TMyClassType ;'         + Cr +
    'begin'                          + Cr +
    '  lData := ( Visited as TMyClassType ) ;' + Cr ;

  lWidth := 0 ;
  for i := 0 to Count - 1 do
  begin
    if not Items[i].Deleted then
      lWidth := Max( lWidth, Length(TSQLMgrParam(Items[i]).ParamName));
  end ;

  lAreParams := false ;
  for i := 0 to Count - 1 do
  begin
    if lAreParams then
      result := result + Cr ;
    if not Items[i].Deleted then
    begin
      lAreParams := true ;
      result :=
        result +
        '  ' +
        tiPadR( 'Query.ParamAs' + TSQLMgrParam(Items[i]).ParamTypeAsTIQueryParamType + '[', 22 ) +
        ' ''' +
        tiPadR( TSQLMgrParam(Items[i]).ParamName + '''', lWidth + 2 ) +
        '] := lData.' +
        SQLParamAsProp(TSQLMgrParam(Items[i]).ParamName, lWidth) + ' ;' ;
    end ;
  end ;

end;

procedure TSQLMgr.CreateTables;
  procedure _CreateTableSQLMan_Group;
  var
    lTable : TtiDBMetaDataTable ;
  begin
    lTable := TtiDBMetaDataTable.Create ;
    try
      lTable.Name := cTableNameSQLManGroup ;
      lTable.AddField( cFieldNameGroupOID,        qfkString, 36 ) ; // Should be Not Null & PK
      lTable.AddField( cFieldNameGroupName, qfkString, 50 ) ;
      lTable.AddField( cFieldNameGroupDispOrder, qfkInteger ) ;
      gTIPerMgr.CreateTable( lTable, FFileName, cTIPersistXMLLight) ;
    finally
      lTable.Free ;
    end ;
  end;

  procedure _CreateTableSQLMan_SQL;
  var
    lTable : TtiDBMetaDataTable ;
  begin
    lTable := TtiDBMetaDataTable.Create ;
    try
      lTable.Name := cTableNameSQLManSQL ;
      lTable.AddField( cFieldNameSQLOID,         qfkString, 36 ) ; // Should be Not Null & PK
      lTable.AddField( cFieldNameSQLOIDGroup,    qfkString, 36 ) ;
      lTable.AddField( cFieldNameSQLDispOrder,   qfkInteger ) ;
      lTable.AddField( cFieldNameSQLVersion,     qfkInteger ) ;
      lTable.AddField( cFieldNameSQLName,        qfkString, 50 ) ;
      lTable.AddField( cFieldNameSQLDesc,        qfkLongString ) ;
      lTable.AddField( cFieldNameSQLLocked,      qfkLogical ) ;
      lTable.AddField( cFieldNameSQLTestInclude, qfkLogical ) ;
      lTable.AddField( cFieldNameSQLSQL,         qfkLongString ) ;
      gTIPerMgr.CreateTable( lTable, FFileName, cTIPersistXMLLight ) ;
    finally
      lTable.Free ;
    end ;
  end;

  procedure _CreateTableSQLMan_Param;
  var
    lTable : TtiDBMetaDataTable ;
  begin
    lTable := TtiDBMetaDataTable.Create ;
    try
      lTable.Name := cTableNameSQLManParam ;
      lTable.AddField( cFieldNameParamOID,       qfkString, 36 ) ; // Should be Not Null & PK
      lTable.AddField( cFieldNameParamOIDSQL,    qfkString, 36 ) ;
      lTable.AddField( cFieldNameParamDispOrder, qfkInteger ) ;
      lTable.AddField( cFieldNameParamName,      qfkString, 20 ) ;
      lTable.AddField( cFieldNameParamType,      qfkString, 20  ) ;
      lTable.AddField( cFieldNameParamValue,     qfkString, 50 ) ;
      lTable.AddField( cFieldNameParamIsNull,    qfkLogical ) ;
      gTIPerMgr.CreateTable( lTable, FFileName, cTIPersistXMLLight ) ;
    finally
      lTable.Free ;
    end ;
  end;

begin

  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(FileName <> '', ClassName + '.FileName not assigned.');
  _CreateTableSQLMan_Group;
  _CreateTableSQLMan_SQL;
  _CreateTableSQLMan_Param;

end;

procedure TSQLMgr.DropTables;
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(FileName <> '', ClassName + '.FileName not assigned');
  gTIPerMgr.DropTable( cTableNameSQLManGroup, FFileName, cTIPersistXMLLight);
  gTIPerMgr.DropTable( cTableNameSQLManSQL,   FFileName, cTIPersistXMLLight);
  gTIPerMgr.DropTable( cTableNameSQLManParam, FFileName, cTIPersistXMLLight);
end;

procedure TSQLMgr.Read(const pDBConnectionName: string; pPerLayerName: string);
begin
  Assert(false, 'Don''t call ' + ClassName + '.Read Call ' +
         ClassName + '.ReadPK');
end;

procedure TSQLMgr.Add(pObject: TSQLMgrGroup; pDefDispOrdr: boolean);
begin
  inherited Add(pObject, pDefDispOrdr);
end;

function TSQLMgr.GetItems(i: integer): TSQLMgrGroup;
begin
  result := TSQLMgrGroup(inherited GetItems(i));
end;

procedure TSQLMgr.SetItems(i: integer; const Value: TSQLMgrGroup);
begin
  inherited SetItems(i, Value);
end;

function TSQLMgrParams.GetItems(i: integer): TSQLMgrParam;
begin
  result := TSQLMgrParam(inherited GetItems(i));
end;

function TSQLMgrParams.GetOID: TOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := inherited GetOID ;
end;

function TSQLMgrParams.GetOwner: TSQLMgrQuery;
begin
  result := TSQLMgrQuery(inherited GetOwner);
end;

procedure TSQLMgrParams.SetItems(i: integer; const Value: TSQLMgrParam);
begin
  inherited SetItems(i, Value);
end;

procedure TSQLMgrParams.SetOwner(const Value: TSQLMgrQuery);
begin
  inherited SetOwner(Value);
end;

constructor TSQLMgr.Create;
begin
  inherited;
  FCritSect := TCriticalSection.Create;
  FQueries := TSQLMgrGroup.Create;
  FQueries.OwnsObjects := true ;
  FQueries.AutoSetItemOwner := false ;
end;

destructor TSQLMgr.Destroy;
begin
  FCritSect.Free;
  FQueries.Free;
  inherited;
end;

function TSQLMgr.FindCreateQueryByName(const pQueryName: String): TSQLMgrQuery;
begin
  FCritSect.Enter;
  try
    result := FindQueryByName(pQueryName);
    if result <> nil then
      Exit ; //==>
    result := TSQLMgrQuery.create;
    result.ReadByQueryName(pQueryName, FFileName);
    if result.ObjectState <> posClean then
    begin
      result.free;
      result := nil ;
    end else
      Queries.Add(result);
  finally
    FCritSect.Leave;
  end;
end;

function TSQLMgr.IsGroupNameUnique(const pGroup: TSQLMgrGroup): boolean;
var
  i : integer;
begin
  result := true ;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Count - 1 do
    if SameText( Items[i].GroupName, pGroup.GroupName ) and
       ( not Items[i].OID.Equals(pGroup.OID) ) and
       ( not Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ELSE}
  for i := 0 to Count - 1 do
    if SameText( Items[i].GroupName, pGroup.GroupName ) and
       ( not Items[i].OID = pGroup.OID ) and
       ( not Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ENDIF}
end;

function TSQLMgr.IsQueryNameUnique(const pQuery: TSQLMgrQuery): boolean;
var
  i : integer;
begin
  result := true ;
  {$IFNDEF OID_AS_INT64}
  for i := 0 to Queries.Count - 1 do
    if SameText( Queries.Items[i].QueryName, pQuery.QueryName ) and
       ( not Queries.Items[i].OID.Equals(pQuery.OID) ) and
       ( not Queries.Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ELSE}
  for i := 0 to Queries.Count - 1 do
    if SameText( Queries.Items[i].QueryName, pQuery.QueryName ) and
       ( not Queries.Items[i].OID = pQuery.OID ) and
       ( not Queries.Items[i].Deleted ) then
    begin
      result := false ;
      Exit ; //==>
    end;
  {$ENDIF}
end;

procedure TSQLMgr.ReadPK(const pDBConnectionName: string; pPerLayerName: string);
var
  i : integer ;
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(pDBConnectionName = '', ClassName + '.ReadPK() does not accept parameters.');
  Assert(pPerLayerName = '', ClassName + '.ReadPK() does not accept parameters.');
  Assert(FFileName <> '', ClassName + '.FileName not assigned');
  gTIPerMgr.VisMgr.Execute(cVisSQLMgrReadPK, Self, FFileName, cTIPersistXMLLight);
  SortByDispOrder;
  for i := 0 to Count - 1 do
    Items[i].SortByDispOrder;
end;

procedure TSQLMgr.Save(const pDBConnectionName: string;pPerLayerName: string);
begin
  Assert(uRegisterMappingsCalled, 'tiSQLMgr_BOM.RegisterMappings has not been called.');
  Assert(pDBConnectionName = '', ClassName + '.Save() does not accept parameters.');
  Assert(pPerLayerName = '', ClassName + '.Save() does not accept parameters.');
  Assert(FFileName <> '', ClassName + '.FileName not assigned');
  inherited Save(FFileName, ctiPersistXMLLight);
end;

class procedure TSQLMgr.CreateFile(const pDBConnectionName: string);
begin
  gTIPerMgr.CreateDatabase(pDBConnectionName, 'null', 'null', cTIPersistXMLLight);
end;

initialization
  uRegisterMappingsCalled := false ;

finalization
  uSQLMgrs.Free ;

end.




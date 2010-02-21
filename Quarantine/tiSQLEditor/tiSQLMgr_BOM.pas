unit tiSQLMgr_BOM;

{$I tiDefines.inc}

interface
uses
   classes
  ,tiVisitor
  ,tiVisitorDB
  ,tiObject
  ,tiQuery
  ,Windows
  ,tiExcept
  ,SyncObjs
  ,tiOID
  ;

const
  cExcCanNotSetObjectStateDeleteWhenPK = 'Can not set ObjectState to posDelete when ObjectState = posPK' ;

type

  TSQLMgrParamName = string[30];

  TSQLMgrQuery  = class ;
  TSQLMgrParams = class ;
  TSQLMgrParam  = class ;

  TSQLMgrQuery = class( TtiObject )
  private
    FStrQueryName : string ;
    FStrQueryDesc : string ;
    FStrSQL       : string ;
    FParams       : TSQLMgrParams ;
    FbQueryLocked : boolean;
    FbTestInclude : boolean;
    FQueryVersion: integer;
  protected
    function    GetCaption : string ; override ;
    procedure   SetObjectState(const Value: TPerObjectState) ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   ReadByQueryName( const pQueryName : string ;
                  const pSQLMgrFileName : string ) ;

  published
    property    QueryName      : string        read FStrQueryName write FStrQueryName ;
    property    QueryDesc      : string        read FStrQueryDesc write FStrQueryDesc ;
    property    QueryVersion   : integer       read FQueryVersion write FQueryVersion ;
    property    SQL            : string        read FStrSQL       write FStrSQL ;
    property    Params         : TSQLMgrParams read FParams       write FParams ;
    property    QueryLocked    : boolean       read FbQueryLocked  write FbQueryLocked;
    property    TestInclude    : boolean       read FbTestInclude  write FbTestInclude;
    property    Caption ;
    function    Clone : TSQLMgrQuery ; reintroduce ;
    procedure   Assign( pData : TSQLMgrQuery ) ; reintroduce ;
  end ;

  TSQLMgrParams = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TSQLMgrParam ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgrParam); reintroduce ;
    function    GetOwner: TSQLMgrQuery; reintroduce ;
    procedure   SetOwner(const Value: TSQLMgrQuery); reintroduce ;
    function    GetOID : TtiOID ; override ;
  public
    function    AsSetupParamsDelphiCode : string ;
    property    Items[i:integer] : TSQLMgrParam read GetItems write SetItems ;
    procedure   Add( pObject : TSQLMgrParam   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TSQLMgrQuery read GetOwner      write SetOwner ;
  published
  end ;

  TSQLMgrParam = class( TtiObject )
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
    property  ParamName  : TSQLMgrParamName  read FStrParamName  write FStrParamName ;
    property  ParamType  : TtiQueryFieldKind  read FParamType  write FParamType ;
    property  ParamTypeStr : String read GetParamTypeStr write SetParamTypeStr ;
    property  ParamValue : string  read FStrParamValue write FStrParamValue ;
    property  IsNull     : boolean read FbIsNull       write FbIsNull;
    function  Clone : TSQLMgrParam ; reintroduce ;
  end ;


function  SQLParamAsProp( const pParamName : string ; pWidth : integer = 0) : string ;

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
  ,tiUtils
  ,tiDialogs
  ,tiOPFManager
  ,tiLog
  ,Math
  ,tiConstants
  ;

const
  cSemaphoreSQLMgrs = 'SQLMgrs' ;
  cSemaphoreSQLMgr  = 'SQLMgr ' ;

function SQLParamAsProp( const pParamName : string ; pWidth : integer = 0 ) : string ;
begin
  result := tiStrTran( pParamName, '_', ' ' ) ;
  result := tiMixedCase( result ) ;
  result := tiStrTran( result, ' ', '' ) ;
  if pWidth <> 0 then
    result := tiPadR( result, pWidth ) ;
end;

procedure TSQLMgrQuery.Assign(pData: TSQLMgrQuery);
begin
  // We can't use the parent classes Assign method because params in an owned
  // list, and the pointer to the list will be overwritten during the assign.
  // Assign must know how to copy owned objects!
  QueryName      := pData.QueryName     ;
  QueryDesc      := pData.QueryDesc     ;
  SQL            := pData.SQL           ;
  QueryLocked    := pData.QueryLocked   ;
  TestInclude    := pData.TestInclude   ;
//  Params.Clear ;
//  for i := 0 to pData.Params.Count-1 do
//    Params.Add( TSQLMgrParam( pData.Params.Items[i] ).Clone ) ;
  Assert(False, 'Under construction');
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
//  tiFmtException( 'Invalid field kind <' + Value + '>',
//                  ClassName,
//                  'SetParamTypeAsStr');
Assert(False, 'Under construction');
end;

procedure TSQLMgrQuery.ReadByQueryName( const pQueryName : string ;
                                        const pSQLMgrFileName : string ) ;
begin
  Assert(ObjectState = posEmpty, 'ObjectState <> posEmpty');
  Assert(pSQLMgrFileName<>'', 'pSQLMgrFileName not assigned');
  QueryName := pQueryName;
//  gTIPerMgr.VisMgr.Execute(cVisSQLMgrReadByQueryName, Self, pSQLMgrFileName, cTIPersistXMLLight);
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

{ TSQLMgrParams }

procedure TSQLMgrParams.Add(pObject: TSQLMgrParam; pDefDispOrdr: boolean);
begin
  inherited Add(pObject);
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

function TSQLMgrParams.GetItems(i: integer): TSQLMgrParam;
begin
  result := TSQLMgrParam(inherited GetItems(i));
end;

function TSQLMgrParams.GetOID: TtiOID;
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

end.




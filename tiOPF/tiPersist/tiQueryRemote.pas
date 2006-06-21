unit tiQueryRemote;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  ,tiHTTP
  ,tiQueryXMLLight
  ,tiXMLToTIDataSet
  ,tiXML
  ,Windows
  ;

const

  cTIOPFExcMsgConnectionConfirmationFormatWrong = 'Server did not return correctly formatted connection confirmation Expected <%s> Found <%s>';
  cTIOPFExcMsgCanNotConnectToServer = 'Can not connect to remote server. Some possible reasons may be:' + #13 +
                                      'a) The server address is wrong; or' + #13 +
                                      'b) the server is not available (or running); or' + #13 +
                                      'c) the connection is being blocked by a firewall or network fault.' + #13 + #13 +
                                      'The application will now shut down' ;
  cTIOPFExcMsgErrorOnRemoteServer = 'Error reading response from remote server:' + #13 + '%s' ;
  cTIOPFErrorAttemptedToConnectTo = 'Attempted to connect to';

  cParamProxyServer = 'proxyserver';
  cParamProxyPort   = 'proxyport';

{
  cTableNameQuery              = 'query'           ;
  cFieldNameQuerySQL           = 'query_sql'       ;
  cFieldNameTransactionID      = 'transaction_id'  ;
  cFieldNameCommandType        = 'command_type'    ;
  cFieldNameComputerName       = 'computer_name'   ;
  cFieldNameUserName           = 'user_name'       ;

  // Request Query_Param table
  cTableNameQueryParam         = 'query_param'     ;
  cFieldNameTableName          = 'table_name'      ;
  cFieldNameParamName          = 'param_name'      ;
  cFieldNameParamKind          = 'param_kind'      ;
  cFieldNameParamValue         = 'param_value'     ;

  // Response message table
  cTableNameResultMessage      = 'result_message'  ;
  cFieldNameResultError        = 'error_message'   ;
  cFieldNameResultRowCount     = 'row_count'       ;

  // MetaData structure
  cTableNameMetaData           = 'table_metadata' ;
  cFieldNameMetaDataTableName  = 'md_table_name'   ;
  cFieldNameMetaDataFieldName  = 'md_field_name'   ;
  cFieldNameMetaDataFieldKind  = 'md_field_kind'   ;
  cFieldNameMetaDataFieldWidth = 'md_field_width'  ;

  // Response set table
  cTableNameResultSet          = 'result_set'      ;
}

  cNullTransactionID           = '' ;

type
  TtiRemoteCommandType = (
     rctUnknown
    ,rctStartTransaction
    ,rctCommit
    ,rctRollBack
    ,rctExecSQL
    ,rctCreateTable
    ,rctDropTable
    ,rctReadMetaDataTables
    ,rctReadMetaDataFields
    ) ;

const
  cRemoteCommandTypes : array[TtiRemoteCommandType] of string = (
     'UNKNOWN'
    ,'START_TRANSACTION'
    ,'COMMIT'
    ,'ROLLBACK'
    ,'EXECSQL'
    ,'CREATE_TABLE'
    ,'DROP_TABLE'
    ,'READ_METADATA_TABLES'
    ,'READ_METADATA_FIELDS'
    ) ;

(*
  cRemoteCommandTypes : array[TtiRemoteCommandType] of string = (
     {$IFNDEF OPTIMISE_XMLDB_SIZE} 'UNKNOWN'               {$ELSE} 'u' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'START_TRANSACTION'     {$ELSE} 's' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'COMMIT'                {$ELSE} 'c' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'ROLLBACK'              {$ELSE} 'r' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'EXECSQL'               {$ELSE} 'e' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'CREATE_TABLE'          {$ELSE} 'a' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'DROP_TABLE'            {$ELSE} 'd' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'READ_METADATA_TABLES'  {$ELSE} 'm' {$ENDIF}
    ,{$IFNDEF OPTIMISE_XMLDB_SIZE} 'READ_METADATA_FIELDS'  {$ELSE} 't' {$ENDIF}
    ) ;
*)

function StrToRemoteCommandType( const pValue : string ) : TtiRemoteCommandType ;

type

  TtiQueryTransParams = class ;

  TtiQueryTransParams = class( TtiQueryParams )
  private
    FTransID: integer;
    FSQL: string;
    FQueryKindAsString: string;
  protected
  public
    function    AddInstance( pName, pKind, pValue : string ) : TtiQueryParamAbs ;
    procedure   AssignToQuery(const pQuery : TtiQuery);
  published
    property    SQL : string read FSQL write FSQL ;
    property    TransID : integer read FTransID write FTransID ;
    property    QueryKindAsString : string read FQueryKindAsString write FQueryKindAsString ;
  end ;

  TtiDatabaseRemoteXML = class( TtiDatabase )
  private
    FConnected : boolean ;
    FInTransaction : boolean ;

    FDBResponseXML    : TtiDatabaseXMLLight ;
    FDBRequestXML     : TtiDatabaseXMLLight ;
    FResponse         : string ;
    FRequest          : string ;

    FHTTP: TtiHTTPAbs;
    FTransactionID : string ;
    FHasResultSet  : boolean ;
    FMetaDataTableCreated : boolean ;
    FLastCallTime : DWord ;

    procedure CreateRequestTableQuery ;
    procedure CreateRequestTableQueryParams ;
    procedure ReadMessagesFromResponse ;
    procedure CreateMetaDataTable( const pDatabase : TtiDatabaseXMLLight ) ;
    procedure InsertMetaData( const pDatabase : TtiDatabaseXMLLight ; const pMetaData : TtiDBMetaDataTable ) ;
    procedure InsertMetaDataRow( const pDatabase : TtiDatabaseXMLLight ; const pTableName : string ; const pFieldName : string ; pFieldKind : TtiQueryFieldKind ; pFieldWidth : integer ) ;

    // ToDo: Rename SQLText CommandText, as it is now used for flat info like table names
    procedure InsertRemoteRequest(pRemoteCommandType: TtiRemoteCommandType; const pSQLText: string);

  protected
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
  public
    constructor create ; override ;
    destructor  Destroy ; override ;

    class function DatabaseExists( const psDatabaseName, psUserName, psPassword : string ) : boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;

    procedure   ReadMetaDataTables( pData : TPersistent ) ; override ;
    procedure   ReadMetaDataFields( pData : TPersistent ) ; override ;
    procedure   DropTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
    procedure   CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;

    property    DBResponseXML : TtiDatabaseXMLLight read FDBResponseXML write FDBResponseXML ;
    property    DBRequestXML : TtiDatabaseXMLLight read FDBRequestXML write FDBRequestXML ;
    property    Response : string read FResponse write FResponse ;
    property    Request  : string read FRequest  write FRequest ;
    procedure   ExecuteRemoteCall ;
    property    HasResultSet : boolean read FHasResultSet ;
    procedure   ClearRequest ;
    function    Test : boolean ; override ;

  end ;

  TtiQueryRemoteXML = class(TtiQuerySQL)
  private
    FQueryResponseXML : TtiQueryXMLLight ;

    FParams : TtiQueryTransParams ;
    FSQL : TStringList ;
    FActive : boolean ;

    function DBRemoteXML   : TtiDatabaseRemoteXML ;

  protected

    function    GetFieldAsString(const psName: string): string       ; override ;
    function    GetFieldAsFloat(const psName: string): real          ; override ;
    function    GetFieldAsBoolean(const psName: string): boolean     ; override ;
    function    GetFieldAsInteger(const psName: string): Int64       ; override ;
    function    GetFieldAsDateTime(const psName: string):TDateTime   ; override ;
    function    GetFieldIsNull(const psName: string): Boolean        ; override ;

    function    GetFieldAsStringByIndex(pIndex: Integer): string     ; override;
    function    GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; override;
    function    GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override;
    function    GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override;
    function    GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override;
    function    GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override;

    function    GetSQL: TStrings; override ;
    procedure   SetSQL(const Value: TStrings); override ;
    procedure   SetSQLText(const Value: string);override;
    function    GetActive: boolean; override ;
    procedure   SetActive(const Value: boolean); override ;
    function    GetEOF: boolean; override ;

    function    GetParamAsString( const psName: string): string; override ;
    function    GetParamAsBoolean(const psName: string): boolean; override ;
    function    GetParamAsFloat(const psName: string): real;override ;
    function    GetParamAsInteger(const psName: string): Int64 ;override ;
    function    GetParamAsDateTime(const psName: string): TDateTime ; override ;
    function    GetParamIsNull( const psName: String): Boolean; override;

    procedure   SetParamAsString( const psName, Value: string); override ;
    procedure   SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    procedure   SetParamAsFloat(const psName: string; const Value: real);override ;
    procedure   SetParamAsInteger(const psName: string;const Value: Int64); override ;
    procedure   SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;
    procedure   SetParamAsMacro( const psName: string;
                                 const Value: string); override ;
    procedure   SetParamIsNull( const psName: String; const Value: Boolean); override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    procedure   ExecSQL; override;

    function    ParamCount: integer; override;
    function    ParamName(pIndex: integer): string; override;

    procedure   AssignParamFromStream(      const pName  : string;  const pValue: TStream); override;
    procedure   AssignParamToStream(        const pName  : string;  const pValue: TStream); override;
    procedure   AssignFieldAsStream(        const pName  : string;  const pValue: TStream); override;
    procedure   AssignFieldAsStreamByIndex(       pIndex : integer; const pValue: TStream); override;

    procedure   AttachDatabase(pDatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(pIndex: integer): string; override;
    function    FieldIndex(const psName: string): integer; override;
    function    FieldKind(pIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(pIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean ; override ;

  end ;

// Move these calls to tiXML
function  tiQueryRemoteDecode(const pValue: string; const pCompression : string): string;
function  tiQueryRemoteEncode(const pValue: string; const pCompression : string): string;
function  tiFormatRemoteConnectionErrorString(const pDatabaseList : string): string;

procedure RegisterMappings ;

implementation
uses
  tiPersist
  ,cTIPersist
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  ,SysUtils
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiUtils
  ,tiLog
  ,tiRJMime
  ,tiDialogs
  ,tiDBConnectionPool
  ,tiRegPerLayer
  ,tiCompressAbs
  ,tiCompressNone
  ,tiCompressZLib
  ;

var
  uXMLTags : TtiXMLTags ;

function tiFormatRemoteConnectionErrorString(const pDatabaseList : string): string;
var
  lCount   : integer ;
  i : integer ;
begin
  lCount := tiNumToken(pDatabaseList, cDatabaseNameDelim);
  if lCount <= 1 then
    Result := cTIOPFErrorAttemptedToConnectTo + ' ' + pDatabaseList
  else begin
    for i := 1 to lCount do
    begin
      if Result <> '' then Result := Result + Cr ;
      Result := Result + '  ' + tiToken(pDatabaseList,cDatabaseNameDelim,i);
    end ;
    Result := cTIOPFErrorAttemptedToConnectTo + ':' + Cr + Result ;
  end ;
  Result := Result + Cr(2) + cTIOPFExcMsgCanNotConnectToServer;
end ;

procedure RegisterMappings ;
begin
  if not gTIPerMgr.ClassDBMappingMgr.ClassMaps.IsClassReg(TtiQueryParamAbs) then
  begin
    gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'Name',          'Name', [pktDB]);
    gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'KindAsStr',     'Kind' );
    gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'ValueAsString', 'Value' );
    gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TtiQueryTransParams, TtiQueryParamAbs);
  end;
end ;

function StrToRemoteCommandType( const pValue : string ) : TtiRemoteCommandType ;
var
  i : TtiRemoteCommandType ;
begin
  result := rctUnknown;
  for i := Low(TtiRemoteCommandType) to High(TtiRemoteCommandType) do
    if SameText( cRemoteCommandTypes[i], pValue ) then
    begin
      result := i ;
      Exit ; //==>
    end;
end;

procedure TtiDatabaseRemoteXML.CreateMetaDataTable( const pDatabase : TtiDatabaseXMLLight ) ;
var
  lMetaData : TtiDBMetaDataTable ;
begin
  // Should normalise this into two tables...
  lMetaData := TtiDBMetaDataTable.Create ;
  try
    lMetaData.Name := uXMLTags.TableNameMetaData;
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataTableName,  qfkString, 255 ) ;
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldName,  qfkString, 255 ) ;
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldKind,  qfkString, 20 ) ;
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldWidth, qfkInteger ) ;
    pDatabase.CreateTable(lMetaData);
  finally
    lMetaData.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.InsertMetaData( const pDatabase : TtiDatabaseXMLLight ; const pMetaData : TtiDBMetaDataTable ) ;
var
  i : integer ;
begin
  for i := 0 to pMetaData.Count - 1 do
    InsertMetaDataRow( pDatabase,
                       pMetaData.Name,
                       pMetaData.Items[i].Name,
                       pMetaData.Items[i].Kind,
                       pMetaData.Items[i].Width);
end;

procedure TtiDatabaseRemoteXML.InsertMetaDataRow( const pDatabase : TtiDatabaseXMLLight ;
                             const pTableName : string ;
                             const pFieldName : string ;
                             pFieldKind : TtiQueryFieldKind ;
                             pFieldWidth : integer ) ;
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataTableName, pTableName);
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataFieldName, pFieldName);
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataFieldKind, cgaQueryFieldKind[pFieldKind]);
    lParams.SetValueAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, pFieldWidth);
    pDatabase.InsertRow(uXMLTags.TableNameMetaData, lParams);
  finally
    lParams.Free;
  end;
end ;

function tiQueryRemoteDecode(const pValue: string; const pCompression : string): string;
begin
  Result := tiDecompressDecode(pValue,pCompression);
end;

function tiQueryRemoteEncode(const pValue: string ; const pCompression : string ): string;
begin
  Result := tiCompressEncode(pValue, pCompression);
end;

{ TtiQueryTransParams }

function TtiQueryTransParams.AddInstance(pName, pKind,
  pValue: string): TtiQueryParamAbs;
begin
  result := nil ;
  case StrToQueryFieldKind(pKind) of
  qfkString   : result := TtiQueryParamString.Create;
  qfkInteger  : result := TtiQueryParamInteger.Create;
  qfkFloat    : result := TtiQueryParamFloat.Create;
  qfkDateTime : result := TtiQueryParamDateTime.Create;
  qfkLogical  : result := TtiQueryParamBoolean.Create;
  qfkBinary   : result := TtiQueryParamStream.Create;
  else
    tiFmtException('Invalid ParamKind', ClassName, 'AddInstance' ) ;
  end ;
  result.Name := pName ;
  result.SetValueAsString(pValue);
  Add(result);

end;

procedure TtiQueryTransParams.AssignToQuery(const pQuery: TtiQuery);
var
  i : integer ;
begin
  pQuery.SQL.Text := SQL ;
  for i := 0 to Count - 1 do
    Items[i].AssignToTIQuery(pQuery);
//    qfkBinary,
//    qfkMacro,
end;

{ TtiDatabaseRemoteXML }

procedure TtiDatabaseRemoteXML.Commit;
begin
  FInTransaction := false ;
  ClearRequest ;
  InsertRemoteRequest(rctCommit, '');
  ExecuteRemoteCall ;
end;

constructor TtiDatabaseRemoteXML.create;
begin
  inherited;
  Assert( gTIHTTPClass <> nil, 'gTIHTTPClass not assigned');
  FHTTP := gTIHTTPClass.Create;
  FDBRequestXML := TtiDatabaseXMLLight.Create ;
  FDBRequestXML.PersistToFile := false ;
  FDBRequestXML.OptXMLDBSize := optDBSizeOn ;
  FDBRequestXML.XMLFieldNameStyle := xfnsInteger ;

  CreateRequestTableQuery ;
  CreateRequestTableQueryParams ;

  FDBResponseXML := TtiDatabaseXMLLight.Create ;
  FDBResponseXML.PersistToFile := false ;
  FDBResponseXML.OptXMLDBSize := optDBSizeOn ;
  FDBResponseXML.XMLFieldNameStyle := xfnsInteger ;

  FMetaDataTableCreated := false ;

end;

class procedure TtiDatabaseRemoteXML.CreateDatabase(const psDatabaseName,
  psUserName, psPassword: string);
begin
  Assert( false, 'CreateDatabase not implemented in ' + ClassName);
end;

procedure TtiDatabaseRemoteXML.CreateTable(const pTableMetaData: TtiDBMetaDataTable);
begin
  ClearRequest ;
  // ToDo: Perhaps table name would be better in the second param of InsertRemoteRequest
  InsertRemoteRequest(rctCreateTable, pTableMetaData.Name);
  if not FMetaDataTableCreated then
  begin
    CreateMetadataTable( DBRequestXML ) ;
    FMetaDataTableCreated := true ;
  end ;
  InsertMetaData( DBRequestXML, pTableMetaData ) ;
  ExecuteRemoteCall ;
end;

procedure TtiDatabaseRemoteXML.CreateRequestTableQueryParams;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := uXMLTags.TableNameQueryParam ;
    lTableMetaData.AddField( uXMLTags.FieldNameParamName,       qfkString, 9999 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameParamKind,       qfkString, 9999 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameParamValue,      qfkString, 9999 ) ;
    DBRequestXML.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
end;

procedure TtiDatabaseRemoteXML.CreateRequestTableQuery;
var
  lTableMetaData : TtiDBMetaDataTable ;
begin
  lTableMetaData := TtiDBMetaDataTable.Create ;
  try
    lTableMetaData.Name := uXMLTags.TableNameQuery ;
    lTableMetaData.AddField( uXMLTags.FieldNameQuerySQL,      qfkString, 9999 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameTransactionID, qfkString, 10 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameCommandType,   qfkString, 9999 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameUserName,      qfkString, 9999 ) ;
    lTableMetaData.AddField( uXMLTags.FieldNameComputerName,  qfkString, 9999 ) ;
    DBRequestXML.CreateTable( lTableMetaData ) ;
  finally
    lTableMetaData.Free;
  end ;
end;

class function TtiDatabaseRemoteXML.DatabaseExists(const psDatabaseName,
  psUserName, psPassword: string): boolean;
begin
  result := false ;
  Assert( false, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseRemoteXML.Destroy;
begin
  FHTTP.Free;
  FDBRequestXML.Free;
  FDBResponseXML.Free;
  inherited;
end;

procedure TtiDatabaseRemoteXML.DropTable(const pTableMetaData: TtiDBMetaDataTable);
begin
  ClearRequest ;
  InsertRemoteRequest(rctDropTable, pTableMetaData.Name);
  ExecuteRemoteCall ;
end;

function TtiDatabaseRemoteXML.GetConnected: boolean;
begin
  // ToDo: Must have some kind of PING command to check server can be reached
  Result := FConnected ;
end;

function TtiDatabaseRemoteXML.InTransaction: boolean;
begin
  // ToDo: Implement transaction management
  result := FInTransaction ;
end;

procedure TtiDatabaseRemoteXML.ReadMetaDataFields(pData: TPersistent);
var
  lTableMetaData : TtiDBMetaDataTable ;
  lQuery : TtiQueryXMLLight ;
begin
  ClearRequest ;
  lTableMetaData := pData as TtiDBMetaDataTable ;
  InsertRemoteRequest(rctReadMetaDataFields, lTableMetaData.Name);

  ExecuteRemoteCall ;

  lQuery := TtiQueryXMLLight.Create ;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameMetaData);
    if lQuery.EOF then
      tiFmtException( cTIOPFExcMsgErrorOnRemoteServer, ['No meta data available'],
                      ClassName, 'ReadMetaDataFields' ) ;

   while not lQuery.EOF do
   begin
     lTableMetaData.AddInstance(
       lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldName],
       StrToQueryFieldKind(lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldKind]),
       lQuery.FieldAsInteger[uXMLTags.FieldNameMetaDataFieldWidth]);
     lQuery.Next ;
   end ;

  finally
    lQuery.Free ;
  end ;

end;

procedure TtiDatabaseRemoteXML.ReadMetaDataTables(pData: TPersistent);
var
  lDBMetaData : TtiDBMetaData ;
  lTableMetaData : TtiDBMetaDataTable ;
  lQuery : TtiQueryXMLLight ;
begin
  ClearRequest ;
  InsertRemoteRequest(rctReadMetaDataTables, '');
  ExecuteRemoteCall ;
  // ToDo: Should pass the database name as a param
  lDBMetaData := pData as TtiDBMetaData ;
  // ToDo: Require a custom table structure here, rather than the default meta data struct.
  lQuery := TtiQueryXMLLight.Create ;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameMetaData);
    while not lQuery.EOF do
    begin
      lTableMetaData := TtiDBMetaDataTable.Create ;
      lTableMetaData.Name := lQuery.FieldAsString[uXMLTags.FieldNameMetaDataTableName];
      lDBMetaData.Add(lTableMetaData);
      lQuery.Next ;
    end ;
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabaseRemoteXML.RollBack;
begin
  FInTransaction := false ;
  ClearRequest ;
  InsertRemoteRequest(rctRollBack, '');
  ExecuteRemoteCall ;
end;

procedure TtiDatabaseRemoteXML.SetConnected(pbValue: boolean);
  procedure _TestConnection ;
  var
    ls : string ;
  begin
    FHTTP.Clear ;
    try
      FHTTP.Post(DatabaseName + cgTIDBProxyTestAlive1 );
      ls := FHTTP.Output.DataString ;
    except
      on e:exception do
        raise EtiOPFDBExceptionCanNotConnect.Create( cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgCanNotConnectToServer ) ;
    end ;
    if ls <> uXMLTags.ProxyTestPassed then
      raise exception.createFmt(cTIOPFExcMsgConnectionConfirmationFormatWrong, [uXMLTags.ProxyTestPassed, ls]) ;
  end ;
  procedure _TestServerVersion ;
  var
    ls : string ;
  begin
    FHTTP.Clear ;
    try
      FHTTP.Post(DatabaseName + cgTIDBProxyServerVersion );
      //FHTTP.Post(DatabaseName + 'version' );
      ls := FHTTP.Output.DataString ;
    except
      on e:exception do
        raise EtiOPFDBExceptionCanNotConnect.Create( cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgCanNotConnectToServer ) ;
    end ;
    if ls <> uXMLTags.XMLVersion then
      raise EtiOPFDBExceptionWrongServerVersion.Create( cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgWrongServerVersion ) ;
  end ;

  procedure _AssignParams;
  var
    lProxyServer: string;
    lProxyPort  : Integer ;
    lProxyActive: Boolean;
    lHTTPClassMapping : string ;
  begin
    // Warning: This code is cloned in tiFileSyncReader_Remote
    lHTTPClassMapping := Params.Values[cHTTPConnectWith];
    if not SameText( FHTTP.MappingName, lHTTPClassMapping) then
    begin
      FreeAndNil(FHTTP);
      FHTTP := gTIHTTPFactory.CreateInstance(lHTTPClassMapping);
    end ;
    if not SameText( cHTTPMSXML, FHTTP.MappingName ) then
    begin
      lProxyActive := tiStrToBool(Params.Values[cHTTPProxyServeractive]);
      lProxyServer := Params.Values[cHTTPProxyServerName];
      lProxyPort   := StrToIntDef(Params.Values[cHTTPProxyPort], 0);
      if (lProxyActive ) and
         ( lProxyServer <> '' ) and
         ( lProxyPort <> 0 ) then
      begin
        FHTTP.ProxyServer := lProxyServer;
        FHTTP.ProxyPort   := lProxyPort;
      end;
    end;
  end ;

begin
  if not pbValue then
  begin
    FConnected := false ;
    Exit ; //==>
  end ;

  FConnected := false ;
  DatabaseName := tiAddTrailingValue( DatabaseName, '/' ) ;
  _AssignParams;
  _TestConnection;
  _TestServerVersion;
  FConnected := true ;

end;

procedure TtiDatabaseRemoteXML.StartTransaction;
begin
  ClearRequest ;
  InsertRemoteRequest(rctStartTransaction, '');
  ExecuteRemoteCall ;
  FInTransaction := true ;
end;

procedure TtiDatabaseRemoteXML.ExecuteRemoteCall;
var
  ls : string ;
  lDifTime : DWord ;
begin
  if DBRequestXML.InTransaction then
    DBRequestXML.Commit ;

  ls := DBRequestXML.AsString ;
  ls := tiQueryRemoteEncode(ls,cgsCompressZLib);

  // This is to stop the HTTP Server melting down.
  // Must learn more about why this is happening.
  // Socket Error # 10048 is the error
  // Am only seeing this error on XP machines -
  // Not sure if it's because of XP, or because
  // the machines are faster.
  // Kudzu explains the server is being overloaded
  // Search Google Groups on "Socket Error # 10048 TidHTTP"
  // This may have to be a unit var: FLastCallTime
  // So the blocking is accross threads

  // The problem is described here:
  // http://www.microsoft.com/windows2000/techinfo/reskit/en-us/default.asp?url=/windows2000/techinfo/reskit/en-us/regentry/58791.asp

  // and here
  // http://groups.google.com.au/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&threadm=404cf7a4%241%40newsgroups.borland.com&rnum=3&prev=/groups%3Fq%3DSocket%2BError%2B%2523%2B10048%2BTidHTTP%26hl%3Den%26lr%3D%26ie%3DUTF-8%26oe%3DUTF-8%26selm%3D404cf7a4%25241%2540newsgroups.borland.com%26rnum%3D3

  FHTTP.Clear ;
  FHTTP.Input.WriteString(ls) ;
  lDifTime := GetTickCount - FLastCallTime ;
  if (lDifTime <= 75) and
     (not gTIPerMgr.Terminated) then
    Sleep(75-lDifTime);

  if gTIPerMgr.Terminated then
    Exit ; //==>

  FHTTP.Post(DatabaseName + cgTIDBProxy );
  FLastCallTime := GetTickCount ;

  if gTIPerMgr.Terminated then
    Exit ; //==>

  ls := tiQueryRemoteDecode(FHTTP.Output.DataString,cgsCompressZLib);

  if gTIPerMgr.Terminated then
    Exit ; //==>

  DBResponseXML.AsString := ls ;

  if gTIPerMgr.Terminated then
    Exit ; //==>

  ReadMessagesFromResponse ;

end;

procedure TtiDatabaseRemoteXML.ReadMessagesFromResponse;
var
  lQuery : TtiQueryXMLLight ;
  lRowCount : integer ;
  lErrorMessage : string ;
begin
  lQuery := TtiQueryXMLLight.Create ;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameResultMessage);
    if lQuery.EOF then
      tiFmtException( cTIOPFExcMsgErrorOnRemoteServer,
                      ['Unable to extract response messages'],
                      ClassName, 'ReadMessagesFromResponse' ) ;
    lErrorMessage  := lQuery.FieldAsString[uXMLTags.FieldNameResultError] ;
    if lErrorMessage <> '' then
      tiFmtException( cTIOPFExcMsgErrorOnRemoteServer,
                      [lErrorMessage],
                      ClassName, 'ReadMessagesFromResponse' ) ;
    FTransactionID := lQuery.FieldAsString[uXMLTags.FieldNameTransactionID] ;
    lRowCount := lQuery.FieldAsInteger[uXMLTags.FieldNameResultRowCount] ;
    FHasResultSet := lRowCount > 0 ;  
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabaseRemoteXML.InsertRemoteRequest(
             pRemoteCommandType : TtiRemoteCommandType ;
             const pSQLText : string ) ;
var
  lParams : TtiQueryTransParams ;
  lQuery : TtiQuery ;
begin
  lQuery := TtiQueryXMLLight.Create ;
  try
    lQuery.AttachDatabase(DBRequestXML);
    lParams := TtiQueryTransParams.Create ;
    try
      lParams.SetValueAsString(uXMLTags.FieldNameQuerySQL, pSQLText) ;
      lParams.SetValueAsString(uXMLTags.FieldNameTransactionID, FTransactionID) ;
      lParams.SetValueAsString(uXMLTags.FieldNameCommandType,  cRemoteCommandTypes[pRemoteCommandType]) ;
      lParams.SetValueAsString(uXMLTags.FieldNameComputerName, tiGetComputerName) ;
      lParams.SetValueAsString(uXMLTags.FieldNameUserName,     tiGetUserName) ;
      lQuery.InsertRow(uXMLTags.TableNameQuery, lParams);
    finally
      lParams.Free;
    end;
  finally
    lQuery.Free;
  end ;
end;

procedure TtiDatabaseRemoteXML.ClearRequest;
var
  lQuery : TtiQuery ;
begin
  //ToDo: Remove this hard-coded tiQuery once the framework can handle multiple persistence layers
  lQuery := TtiQueryXMLLight.Create ;
  try
    lQuery.AttachDatabase( DBRequestXML ) ;
    lQuery.DeleteRow(uXMLTags.TableNameQuery, nil);
    lQuery.DeleteRow(uXMLTags.TableNameQueryParam, nil);
    //DBRequestXML.DeleteRow(cTableNameQuery, nil);
    //DBRequestXML.DeleteRow(cTableNameQueryParam, nil);
    if FMetaDataTableCreated then
      lQuery.DeleteRow(uXMLTags.TableNameMetaData, nil);
  finally
    lQuery.Free;
  end;
  DBRequestXML.StartTransaction;
end;

function TtiDatabaseRemoteXML.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;  
end;

{ TtiQueryRemoteXML }

constructor TtiQueryRemoteXML.Create;
begin
  inherited;
  FQueryResponseXML    := TtiQueryXMLLight.Create ;
  FParams := TtiQueryTransParams.Create ;
  FSQL    := TStringList.Create ;
  FActive := false ;
end;


destructor TtiQueryRemoteXML.Destroy;
begin
  FSQL.Free;
  FParams.Free ;
  FQueryResponseXML.Free;
  inherited;
end;

procedure TtiQueryRemoteXML.ExecSQL;
  procedure _SaveParams( const pDatabase : TtiDatabase ) ;
  var
    i : integer ;
    lParams : TtiQueryTransParams ;
    lQuery : TtiQuery ;
  begin
    lQuery := TtiQueryXMLLight.Create ;
    try
      lQuery.AttachDatabase(pDatabase);
      lParams := TtiQueryTransParams.Create ;
      try
        for i := 0 to FParams.Count - 1 do
        begin
          lParams.SetValueAsString(uXMLTags.FieldNameParamName, FParams.Items[i].Name) ;
          lParams.SetValueAsString(uXMLTags.FieldNameParamKind, FParams.Items[i].KindAsStr) ;
          lParams.SetValueAsString(uXMLTags.FieldNameParamValue, FParams.Items[i].GetValueAsString) ;
          lQuery.InsertRow(uXMLTags.TableNameQueryParam, lParams);
        end;
      finally
        lParams.Free;
      end;
    finally
      lQuery.Free;
    end ;
  end;

begin

  DBRemoteXML.ClearRequest ;

  DBRemoteXML.DBRequestXML.StartTransaction;
  DBRemoteXML.InsertRemoteRequest(rctExecSQL, SQLText);
  _SaveParams(DBRemoteXML.DBRequestXML) ;
  DBRemoteXML.DBRequestXML.Commit ;
  DBRemoteXML.ExecuteRemoteCall ;

  if DBRemoteXML.HasResultSet then
    FQueryResponseXML.SelectRow(uXMLTags.TableNameResultSet);

end;

function TtiQueryRemoteXML.FieldCount: integer;
begin
  result := FQueryResponseXML.FieldCount ;
end;

function TtiQueryRemoteXML.FieldIndex(const psName: string): integer;
begin
  result := FQueryResponseXML.FieldIndex(psName);
end;

function TtiQueryRemoteXML.FieldKind(pIndex: integer): TtiQueryFieldKind;
begin
  result := FQueryResponseXML.FieldKind(pIndex);
end;

function TtiQueryRemoteXML.FieldName(pIndex: integer): string;
begin
  result := FQueryResponseXML.FieldName(pIndex);
end;

function TtiQueryRemoteXML.FieldSize(pIndex: integer): integer;
begin
  result := FQueryResponseXML.FieldSize(pIndex);
end;

function TtiQueryRemoteXML.GetActive: boolean;
begin
  result := FActive ;
end;

function TtiQueryRemoteXML.GetEOF: boolean;
begin
  result :=
    not DBRemoteXML.HasResultSet or
    FQueryResponseXML.EOF;
end;

function TtiQueryRemoteXML.GetFieldAsBoolean(const psName: string): boolean;
begin
  result := FQueryResponseXML.FieldAsBoolean[psName];
end;

function TtiQueryRemoteXML.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQueryResponseXML.FieldAsDateTime[psName];
end;

function TtiQueryRemoteXML.GetFieldAsFloat(const psName: string): real;
begin
  result := FQueryResponseXML.FieldAsFloat[psName];
end;

function TtiQueryRemoteXML.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQueryResponseXML.FieldAsInteger[psName];
end;

function TtiQueryRemoteXML.GetFieldAsString(const psName: string): string;
begin
  result := FQueryResponseXML.FieldAsString[psName];
end;

function TtiQueryRemoteXML.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQueryResponseXML.FieldIsNull[psName];
end;

function TtiQueryRemoteXML.GetParamAsBoolean(const psName: string): boolean;
begin
  result := FParams.GetValueAsBoolean( psName ) ;
end;

function TtiQueryRemoteXML.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FParams.GetValueAsDateTime( psName ) ;
end;

function TtiQueryRemoteXML.GetParamAsFloat(const psName: string): real;
begin
  result := FParams.GetValueAsFloat(psName);
end;

function TtiQueryRemoteXML.GetParamAsInteger(const psName: string): Int64;
begin
  result := FParams.GetValueAsInteger(psName);
end;

function TtiQueryRemoteXML.GetParamAsString(const psName: string): string;
begin
  result := FParams.GetValueAsString(psName) ;
end;

function TtiQueryRemoteXML.GetParamIsNull(const psName: String): Boolean;
begin
  result := FParams.ParamIsNull[psName];
end;

function TtiQueryRemoteXML.GetSQL: TStrings;
begin
  result := FSQL ;
end;

function TtiQueryRemoteXML.HasNativeLogicalType: boolean;
begin
  result := FQueryResponseXML.HasNativeLogicalType;
end;

function TtiQueryRemoteXML.ParamCount: integer;
begin
  result := FParams.Count;
end;

function TtiQueryRemoteXML.ParamName(pIndex: integer): string;
begin
  result := FParams.ParamName(pIndex);
end;

procedure TtiQueryRemoteXML.Reset;
begin
  // Not sure what to put in here
end;

procedure TtiQueryRemoteXML.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value ;
    if FActive then
      ExecSQL ;
  end;
end;

procedure TtiQueryRemoteXML.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  FParams.SetValueAsBoolean(psName, Value) ;
end;

procedure TtiQueryRemoteXML.SetParamAsDateTime(const psName: string; const Value: TDateTime);
begin
  FParams.SetValueAsDateTime(psName, Value ) ;
end;

procedure TtiQueryRemoteXML.SetParamAsFloat(const psName: string; const Value: real);
begin
  FParams.SetValueAsFloat(psName, Value) ;
end;

procedure TtiQueryRemoteXML.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  FParams.SetValueAsInteger(psName, Value ) ;
end;

procedure TtiQueryRemoteXML.SetParamAsMacro(const psName, Value: string);
begin
  SQLText :=
    tiCIStrTran( SQLText,
                 cgtiQueryMacroChr + psName,
                 Value ) ;
end;

procedure TtiQueryRemoteXML.SetParamAsString(const psName, Value: string);
begin
  FParams.SetValueAsString(psName, Value );
end;

procedure TtiQueryRemoteXML.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  FParams.ParamIsNull[ psName ] := Value ;
end;

procedure TtiQueryRemoteXML.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
  FParams.Clear ;
end;

procedure TtiQueryRemoteXML.AssignFieldAsStream(const pName: string;const pValue: TStream);
var
  ls : string ;
  lStream : TStringStream ;
begin
  ls := GetFieldAsString(pName);
  lStream := TStringStream.Create(ls) ;
  try
    pValue.Size := 0 ;
    MimeDecodeStream(lStream, pValue);
    pValue.Position := 0 ;
  finally
    lStream.Free;
  end;
end;

procedure TtiQueryRemoteXML.Next;
begin
  FQueryResponseXML.Next;
end;

procedure TtiQueryRemoteXML.AssignParamFromStream(const pName: string;const pValue: TStream);
begin
  FParams.SetValueAsStream(pName, pValue);
end;

procedure TtiQueryRemoteXML.AssignParamToStream(const pName: string; const pValue: TStream);
begin
  FParams.AssignValueToStream(pName, pValue);
end;

procedure TtiQueryRemoteXML.Close;
begin
  Active := false ;
end;

procedure TtiQueryRemoteXML.Open;
begin
  Active := true ;
end;

function TtiQueryRemoteXML.DBRemoteXML: TtiDatabaseRemoteXML;
begin
  result := ( Database as TtiDatabaseRemoteXML ) ;
end;

//var
//  lRegPerLayer : TtiRegPerLayer ;

procedure TtiQueryRemoteXML.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
  if pDatabase <> nil then
    FQueryResponseXML.AttachDatabase((pDatabase as TtiDatabaseRemoteXML).DBResponseXML);
end;

procedure TtiQueryRemoteXML.DetachDatabase;
begin
  inherited;
  FQueryResponseXML.DetachDatabase;
end;

procedure TtiQueryRemoteXML.SetSQLText(const Value: string);
begin
  FSQL.Text := Value ;
  FParams.Clear ;
end;

procedure TtiQueryRemoteXML.AssignFieldAsStreamByIndex(pIndex: Integer; const pValue: TStream);
var
  ls : string ;
  lStream : TStringStream ;
begin
  ls := GetFieldAsStringByIndex(pIndex);
  lStream := TStringStream.Create(ls) ;
  try
    pValue.Size := 0 ;
    MimeDecodeStream(lStream, pValue);
    pValue.Position := 0 ;
  finally
    lStream.Free;
  end;
end;

function TtiQueryRemoteXML.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
begin
  result := FQueryResponseXML.FieldAsBooleanByIndex[pIndex];
end;

function TtiQueryRemoteXML.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQueryResponseXML.FieldAsDateTimeByIndex[pIndex];
end;

function TtiQueryRemoteXML.GetFieldAsFloatByIndex(pIndex: Integer): real;
begin
  result := FQueryResponseXML.FieldAsFloatByIndex[pIndex];
end;

function TtiQueryRemoteXML.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FQueryResponseXML.FieldAsIntegerByIndex[pIndex];
end;

function TtiQueryRemoteXML.GetFieldAsStringByIndex(pIndex: Integer): string;
begin
  result := FQueryResponseXML.FieldAsStringByIndex[pIndex];
end;

function TtiQueryRemoteXML.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQueryResponseXML.FieldIsNullByIndex[pIndex];
end;

Initialization
begin
  uXMLTags := TtiXMLTags.Create ;
  uXMLTags.OptXMLDBSize := optDBSizeOn;

  RegisterMappings ;

  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistRemote,
              TtiDBConnectionPoolDataAbs,
              TtiQueryRemoteXML,
              TtiDatabaseRemoteXML ) ;

  // Change the default package from XML to Remote
  if (gTIPerMgr.RegPerLayers.FindByPerLayerName(cTIPersistXMLLight) <> nil ) and
     (gTIPerMgr.RegPerLayers.DefaultPerLayerName = cTIPersistXMLLight ) then
    gTIPerMgr.RegPerLayers.DefaultPerLayerName := cTIPersistRemote ;

end;

finalization
  uXMLTags.Free ;
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistRemote ) ;
end.


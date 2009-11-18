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
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ,tiUtils
 ;

const

  cTIOPFExcMsgConnectionConfirmationFormatWrong = 'Server did not return correctly formatted connection confirmation Expected <%s> Found <%s>';
  cTIOPFExcMsgCanNotConnectToServer = 'Can not connect to remote server. Some possible reasons may be:' + #13#10 +
                                      'a) The server address is wrong; or' + #13#10 +
                                      'b) the server is not available (or running); or' + #13#10 +
                                      'c) the connection is being blocked by a firewall or network fault.' + #13#10 + #13#10 +
                                      'The application will now shut down';
  cTIOPFExcMsgErrorOnRemoteServer = 'Error reading response from remote server:' + #13#10 + '%s';
  cTIOPFErrorAttemptedToConnectTo = 'Attempted to connect to';

  cParamProxyServer = 'proxyserver';
  cParamProxyPort   = 'proxyport';
  cRetryCount       = 3;
  cRetrySleep       = 2000;

{
  cTableNameQuery              = 'query'          ;
  cFieldNameQuerySQL           = 'query_sql'      ;
  cFieldNameTransactionID      = 'transaction_id' ;
  cFieldNameCommandType        = 'command_type'   ;
  cFieldNameComputerName       = 'computer_name'  ;
  cFieldNameUserName           = 'user_name'      ;

  // Request Query_Param table
  cTableNameQueryParam         = 'query_param'    ;
  cFieldNameTableName          = 'table_name'     ;
  cFieldNameParamName          = 'param_name'     ;
  cFieldNameParamKind          = 'param_kind'     ;
  cFieldNameParamValue         = 'param_value'    ;

  // Response message table
  cTableNameResultMessage      = 'result_message' ;
  cFieldNameResultError        = 'error_message'  ;
  cFieldNameResultRowCount     = 'row_count'      ;

  // MetaData structure
  cTableNameMetaData           = 'table_metadata';
  cFieldNameMetaDataTableName  = 'md_table_name'  ;
  cFieldNameMetaDataFieldName  = 'md_field_name'  ;
  cFieldNameMetaDataFieldKind  = 'md_field_kind'  ;
  cFieldNameMetaDataFieldWidth = 'md_field_width' ;

  // Response set table
  cTableNameResultSet          = 'result_set'     ;
}

  cNullTransactionID           = '';

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
   );

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
   );

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
   );
*)

function StrToRemoteCommandType(const AValue : string): TtiRemoteCommandType;

type

  TtiPersistenceLayerRemoteXML = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiQueryTransParams = class;

  TtiQueryTransParams = class(TtiQueryParams)
  private
    FTransID: integer;
    FSQL: string;
    FQueryKindAsString: string;
  protected
  public
    function    AddInstance(AName, pKind, AValue : string): TtiQueryParamAbs;
    procedure   AssignToQuery(const AQuery : TtiQuery);
  published
    property    SQL : string read FSQL write FSQL;
    property    TransID : integer read FTransID write FTransID;
    property    QueryKindAsString : string read FQueryKindAsString write FQueryKindAsString;
  end;

  TtiDatabaseRemoteXML = class(TtiDatabase)
  private
    FConnected : boolean;
    FInTransaction : boolean;

    FHTTP: TtiHTTPAbs;
    FCurrentThreadID: TThreadID;

    FDBResponseXML   : TtiDatabaseXMLLight;
    FDBRequestXML    : TtiDatabaseXMLLight;
    FResponse        : string;
    FRequest         : string;

    FTransactionID : string;
    FHasResultSet : boolean;
    FMetaDataTableCreated : boolean;

    procedure RefreshHTTPInstance;
    procedure CreateRequestTableQuery;
    procedure CreateRequestTableQueryParams;
    procedure ReadMessagesFromResponse;
    procedure CreateMetaDataTable(const ADatabase : TtiDatabaseXMLLight);
    procedure InsertMetaData(const ADatabase : TtiDatabaseXMLLight; const pMetaData : TtiDBMetaDataTable);
    procedure InsertMetaDataRow(const ADatabase : TtiDatabaseXMLLight; const ATableName : string; const AFieldName : string; AFieldKind : TtiQueryFieldKind; AFieldWidth : integer);

    // ToDo: Rename SQLText CommandText, as it is now used for flat info like table names
    procedure InsertRemoteRequest(pRemoteCommandType: TtiRemoteCommandType; const pSQLText: string);
    procedure TryHTTPPost(const AInput: string);

  protected
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    class function DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;

    procedure   ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    procedure   DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;

    property    DBResponseXML : TtiDatabaseXMLLight read FDBResponseXML write FDBResponseXML;
    property    DBRequestXML : TtiDatabaseXMLLight read FDBRequestXML write FDBRequestXML;
    property    Response : string read FResponse write FResponse;
    property    Request : string read FRequest  write FRequest;
    procedure   ExecuteRemoteCall;
    property    HasResultSet : boolean read FHasResultSet;
    procedure   ClearRequest;
    function    Test : boolean; override;
    function    TIQueryClass: TtiQueryClass; override;

  end;

  TtiQueryRemoteXML = class(TtiQuerySQL)
  private
    FQueryResponseXML : TtiQueryXMLLight;

    FParams : TtiQueryTransParams;
    FSQL : TStringList;
    FActive : boolean;

    function DBRemoteXML  : TtiDatabaseRemoteXML;

  protected

    function    GetFieldAsString(const AName: string): string      ; override;
    function    GetFieldAsFloat(const AName: string): extended         ; override;
    function    GetFieldAsBoolean(const AName: string): boolean    ; override;
    function    GetFieldAsInteger(const AName: string): Int64      ; override;
    function    GetFieldAsDateTime(const AName: string):TDateTime  ; override;
    function    GetFieldIsNull(const AName: string): Boolean       ; override;

    function    GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function    GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
    function    GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function    GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function    GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function    GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    procedure   SetSQLText(const AValue: string);override;
    function    GetActive: boolean; override;
    procedure   SetActive(const AValue: boolean); override;
    function    GetEOF: boolean; override;

    function    GetParamAsString(const AName: string): string; override;
    function    GetParamAsBoolean(const AName: string): boolean; override;
    function    GetParamAsFloat(const AName: string): extended;override;
    function    GetParamAsInteger(const AName: string): Int64;override;
    function    GetParamAsDateTime(const AName: string): TDateTime; override;
    function    GetParamIsNull(const AName: String): Boolean; override;

    procedure   SetParamAsString(const AName, AValue: string); override;
    procedure   SetParamAsBoolean(const AName: string;const AValue: boolean);override;
    procedure   SetParamAsFloat(const AName: string; const AValue: Extended);override;
    procedure   SetParamAsInteger(const AName: string;const AValue: Int64); override;
    procedure   SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;
    procedure   SetParamAsMacro(const AName: string;
                                 const AValue: string); override;
    procedure   SetParamIsNull(const AName: String; const AValue: Boolean); override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    function    ExecSQL: integer; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(     const AName : string;  const AValue: TStream); override;
    procedure   AssignParamToStream(       const AName : string;  const AValue: TStream); override;
    procedure   AssignFieldAsStream(       const AName : string;  const AValue: TStream); override;
    procedure   AssignFieldAsStreamByIndex(      AIndex : integer; const AValue: TStream); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

  end;

// Move these calls to tiXML
function  tiFormatRemoteConnectionErrorString(const pDatabaseList : string): string;

procedure RegisterMappings;

implementation
uses
   tiOPFManager
  ,tiConstants
  ,tiAutoMap
  ,tiObject
  ,tiLog
  ,tiCompress
  ,tiCompressNone
  ,tiCompressZLib
  ,tiExcept
  ,tiStreams
  ,tiWebServerVersion
  ,SysUtils
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
 ;

var
  uXMLTags : TtiXMLTags;

function tiFormatRemoteConnectionErrorString(const pDatabaseList : string): string;
var
  lCount  : integer;
  i : integer;
begin
  lCount := tiNumToken(pDatabaseList, cDatabaseNameDelim);
  if lCount <= 1 then
    Result := cTIOPFErrorAttemptedToConnectTo + ' ' + pDatabaseList
  else begin
    for i := 1 to lCount do
    begin
      if Result <> '' then Result := Result + Cr;
      Result := Result + '  ' + tiToken(pDatabaseList,cDatabaseNameDelim,i);
    end;
    Result := cTIOPFErrorAttemptedToConnectTo + ':' + Cr + Result;
  end;
  Result := Result + Cr(2) + cTIOPFExcMsgCanNotConnectToServer;
end;

procedure RegisterMappings;
begin
  if not GTIOPFManager.ClassDBMappingMgr.ClassMaps.IsClassReg(TtiQueryParamAbs) then
  begin
    GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'Name',          'Name', [pktDB]);
    GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'KindAsStr',     'Kind');
    GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TtiQueryParamAbs, uXMLTags.TableNameQueryParam, 'ValueAsString', 'AValue');
    GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TtiQueryTransParams, TtiQueryParamAbs);
  end;
end;

function StrToRemoteCommandType(const AValue : string): TtiRemoteCommandType;
var
  i : TtiRemoteCommandType;
begin
  result := rctUnknown;
  for i := Low(TtiRemoteCommandType) to High(TtiRemoteCommandType) do
    if SameText(cRemoteCommandTypes[i], AValue) then
    begin
      result := i;
      Exit; //==>
    end;
end;

procedure TtiDatabaseRemoteXML.CreateMetaDataTable(const ADatabase : TtiDatabaseXMLLight);
var
  lMetaData : TtiDBMetaDataTable;
begin
  // Should normalise this into two tables...
  lMetaData := TtiDBMetaDataTable.Create;
  try
    lMetaData.Name := uXMLTags.TableNameMetaData;
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataTableName,  qfkString, 255);
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldName,  qfkString, 255);
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldKind,  qfkString, 20);
    lMetaData.AddInstance(uXMLTags.FieldNameMetaDataFieldWidth, qfkInteger);
    ADatabase.CreateTable(lMetaData);
  finally
    lMetaData.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.InsertMetaData(const ADatabase : TtiDatabaseXMLLight; const pMetaData : TtiDBMetaDataTable);
var
  i : integer;
begin
  for i := 0 to pMetaData.Count - 1 do
    InsertMetaDataRow(ADatabase,
                       pMetaData.Name,
                       pMetaData.Items[i].Name,
                       pMetaData.Items[i].Kind,
                       pMetaData.Items[i].Width);
end;

procedure TtiDatabaseRemoteXML.InsertMetaDataRow(const ADatabase : TtiDatabaseXMLLight;
                             const ATableName : string;
                             const AFieldName : string;
                             AFieldKind : TtiQueryFieldKind;
                             AFieldWidth : integer);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataTableName, ATableName);
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataFieldName, AFieldName);
    lParams.SetValueAsString(uXMLTags.FieldNameMetaDataFieldKind, cgaQueryFieldKind[AFieldKind]);
    lParams.SetValueAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, AFieldWidth);
    ADatabase.InsertRow(uXMLTags.TableNameMetaData, lParams);
  finally
    lParams.Free;
  end;
end;

{ TtiQueryTransParams }

function TtiQueryTransParams.AddInstance(AName, pKind,
  AValue: string): TtiQueryParamAbs;
begin
  case StrToQueryFieldKind(pKind) of
  qfkString  : result := TtiQueryParamString.Create;
  qfkInteger : result := TtiQueryParamInteger.Create;
  qfkFloat   : result := TtiQueryParamFloat.Create;
  qfkDateTime : result := TtiQueryParamDateTime.Create;
  qfkLogical : result := TtiQueryParamBoolean.Create;
  qfkBinary  : result := TtiQueryParamStream.Create;
  else
    raise Exception.Create('Invalid ParamKind');
  end;
  result.Name := AName;
  result.SetValueAsString(AValue);
  Add(result);

end;

procedure TtiQueryTransParams.AssignToQuery(const AQuery: TtiQuery);
var
  i : integer;
begin
  AQuery.SQL.Text := SQL;
  for i := 0 to Count - 1 do
    Items[i].AssignToTIQuery(AQuery);
//    qfkBinary,
//    qfkMacro,
end;

{ TtiDatabaseRemoteXML }

procedure TtiDatabaseRemoteXML.Commit;
begin
  FInTransaction := false;
  ClearRequest;
  InsertRemoteRequest(rctCommit, '');
  ExecuteRemoteCall;
end;

constructor TtiDatabaseRemoteXML.create;
begin
  inherited;

  FDBRequestXML := TtiDatabaseXMLLight.Create;
  FDBRequestXML.PersistToFile := false;
  FDBRequestXML.OptXMLDBSize := optDBSizeOn;
  FDBRequestXML.XMLFieldNameStyle := xfnsInteger;

  CreateRequestTableQuery;
  CreateRequestTableQueryParams;

  FDBResponseXML := TtiDatabaseXMLLight.Create;
  FDBResponseXML.PersistToFile := false;
  FDBResponseXML.OptXMLDBSize := optDBSizeOn;
  FDBResponseXML.XMLFieldNameStyle := xfnsInteger;

  FMetaDataTableCreated := false;
  FCurrentThreadID:= 0;

end;

class procedure TtiDatabaseRemoteXML.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class procedure TtiDatabaseRemoteXML.DropDatabase(const ADatabaseName,
  AUserName, APassword: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

procedure TtiDatabaseRemoteXML.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  ClearRequest;
  // ToDo: Perhaps table name would be better in the second param of InsertRemoteRequest
  InsertRemoteRequest(rctCreateTable, ATableMetaData.Name);
  if not FMetaDataTableCreated then
  begin
    CreateMetadataTable(DBRequestXML);
    FMetaDataTableCreated := true;
  end;
  InsertMetaData(DBRequestXML, ATableMetaData);
  ExecuteRemoteCall;
end;

procedure TtiDatabaseRemoteXML.CreateRequestTableQueryParams;
var
  lTableMetaData : TtiDBMetaDataTable;
begin
  lTableMetaData := TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name := uXMLTags.TableNameQueryParam;
    lTableMetaData.AddInstance(uXMLTags.FieldNameParamName,       qfkString, 9999);
    lTableMetaData.AddInstance(uXMLTags.FieldNameParamKind,       qfkString, 9999);
    lTableMetaData.AddInstance(uXMLTags.FieldNameParamValue,      qfkString, 9999);
    DBRequestXML.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.CreateRequestTableQuery;
var
  lTableMetaData : TtiDBMetaDataTable;
begin
  lTableMetaData := TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name := uXMLTags.TableNameQuery;
    lTableMetaData.AddInstance(uXMLTags.FieldNameQuerySQL,      qfkString, 9999);
    lTableMetaData.AddInstance(uXMLTags.FieldNameTransactionID, qfkString, 10);
    lTableMetaData.AddInstance(uXMLTags.FieldNameCommandType,   qfkString, 9999);
    lTableMetaData.AddInstance(uXMLTags.FieldNameUserName,      qfkString, 9999);
    lTableMetaData.AddInstance(uXMLTags.FieldNameComputerName,  qfkString, 9999);
    DBRequestXML.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
end;

class function TtiDatabaseRemoteXML.DatabaseExists(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  result := false;
  Assert(false, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseRemoteXML.Destroy;
begin
  FDBRequestXML.Free;
  FDBResponseXML.Free;
  FreeAndNil(FHTTP);
  inherited;
end;

procedure TtiDatabaseRemoteXML.DropTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  ClearRequest;
  InsertRemoteRequest(rctDropTable, ATableMetaData.Name);
  ExecuteRemoteCall;
end;

function TtiDatabaseRemoteXML.GetConnected: boolean;
begin
  // ToDo: Must have some kind of PING command to check server can be reached
  Result := FConnected;
end;

function TtiDatabaseRemoteXML.InTransaction: boolean;
begin
  // ToDo: Implement transaction management
  result := FInTransaction;
end;

procedure TtiDatabaseRemoteXML.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableMetaData : TtiDBMetaDataTable;
  lQuery : TtiQueryXMLLight;
begin
  ClearRequest;
  lTableMetaData := AData as TtiDBMetaDataTable;
  InsertRemoteRequest(rctReadMetaDataFields, lTableMetaData.Name);

  ExecuteRemoteCall;

  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameMetaData);
    if lQuery.EOF then
      raise Exception.CreateFmt(cTIOPFExcMsgErrorOnRemoteServer, ['No meta data available']);

   while not lQuery.EOF do
   begin
     lTableMetaData.AddInstance(
       lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldName],
       StrToQueryFieldKind(lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldKind]),
       lQuery.FieldAsInteger[uXMLTags.FieldNameMetaDataFieldWidth]);
     lQuery.Next;
   end;

  finally
    lQuery.Free;
  end;

end;

procedure TtiDatabaseRemoteXML.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lDBMetaData : TtiDBMetaData;
  lTableMetaData : TtiDBMetaDataTable;
  lQuery : TtiQueryXMLLight;
begin
  ClearRequest;
  InsertRemoteRequest(rctReadMetaDataTables, '');
  ExecuteRemoteCall;
  // ToDo: Should pass the database name as a param
  lDBMetaData := AData as TtiDBMetaData;
  // ToDo: Require a custom table structure here, rather than the default meta data struct.
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameMetaData);
    while not lQuery.EOF do
    begin
      lTableMetaData := TtiDBMetaDataTable.Create;
      lTableMetaData.Name := lQuery.FieldAsString[uXMLTags.FieldNameMetaDataTableName];
      lDBMetaData.Add(lTableMetaData);
      lQuery.Next;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.RollBack;
begin
  FInTransaction := false;
  ClearRequest;
  InsertRemoteRequest(rctRollBack, '');
  ExecuteRemoteCall;
end;

procedure TtiDatabaseRemoteXML.SetConnected(AValue: boolean);
  procedure _TestConnection;
  var
    ls : string;
  begin
    FHTTP.Clear;
    try
      FHTTP.Post(DatabaseName + cgTIDBProxyTestAlive1);
      ls := FHTTP.Output.DataString;
    except
      on e:exception do
        raise EtiOPFDBExceptionCanNotConnect.Create(cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgCanNotConnectToServer);
    end;
    if not TtiAppServerVersionAbs.IsXMLValid(LS) then
      raise exception.createFmt(cTIOPFExcMsgConnectionConfirmationFormatWrong, [TtiAppServerVersionAbs.ExpectedAsString, ls]);
  end;
  procedure _TestServerVersion;
  var
    ls : string;
  begin
    FHTTP.Clear;
    try
      FHTTP.Post(DatabaseName + cgTIDBProxyServerVersion);
      //FHTTP.Post(DatabaseName + 'version');
      ls := FHTTP.Output.DataString;
    except
      on e:exception do
        raise EtiOPFDBExceptionCanNotConnect.Create(cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgCanNotConnectToServer);
    end;
    if ls <> uXMLTags.XMLVersion then
      raise EtiOPFDBExceptionWrongServerVersion.Create(cTIPersistRemote, DatabaseName, UserName, Password, cTIOPFExcMsgWrongServerVersion);
  end;
begin
  if not AValue then
  begin
    FConnected := false;
    Exit; //==>
  end;

  RefreshHTTPInstance;
  FConnected := false;
  DatabaseName := tiAddTrailingValue(DatabaseName, '/');
  _TestConnection;
  _TestServerVersion;
  FConnected := true;
end;

procedure TtiDatabaseRemoteXML.StartTransaction;
begin
  ClearRequest;
  InsertRemoteRequest(rctStartTransaction, '');
  ExecuteRemoteCall;
  FInTransaction := true;
end;

procedure TtiDatabaseRemoteXML.ExecuteRemoteCall;
var
  ls : string;
//  lDifTime : DWord;
begin
  if DBRequestXML.InTransaction then
    DBRequestXML.Commit;

  ls := DBRequestXML.AsString;
  ls := tiCompressEncode(ls,cgsCompressZLib);

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

//  lDifTime := GetTickCount - FLastCallTime;
//  if (lDifTime <= 75) and
//     (not GTIOPFManager.Terminated) then
//    Sleep(75-lDifTime);

  if GTIOPFManager.Terminated then
    Exit; //==>

  RefreshHTTPInstance;

//  Log('Request');
//  Log(ls);

  TryHTTPPost(ls);

  if GTIOPFManager.Terminated then
    Exit; //==>

  ls := FHTTP.Output.DataString;
  ls := tiDecompressDecode(ls,cgsCompressZLib);
//  Log('Response');
//  Log(ls);

  if GTIOPFManager.Terminated then
    Exit; //==>

  DBResponseXML.AsString := ls;

  if GTIOPFManager.Terminated then
    Exit; //==>

  ReadMessagesFromResponse;

end;

procedure TtiDatabaseRemoteXML.ReadMessagesFromResponse;
var
  lQuery : TtiQueryXMLLight;
  lRowCount : integer;
  lErrorMessage : string;
begin
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(DBResponseXML);
    lQuery.SelectRow(uXMLTags.TableNameResultMessage);
    if lQuery.EOF then
      raise Exception.CreateFmt(cTIOPFExcMsgErrorOnRemoteServer,
                      ['Unable to extract response messages']);
    lErrorMessage := lQuery.FieldAsString[uXMLTags.FieldNameResultError];
    if lErrorMessage <> '' then
      raise Exception.CreateFmt(cTIOPFExcMsgErrorOnRemoteServer,
                                 [lErrorMessage]);
    FTransactionID := lQuery.FieldAsString[uXMLTags.FieldNameTransactionID];
    lRowCount := lQuery.FieldAsInteger[uXMLTags.FieldNameResultRowCount];
    FHasResultSet := lRowCount > 0;  
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.InsertRemoteRequest(
             pRemoteCommandType : TtiRemoteCommandType;
             const pSQLText : string);
var
  lParams : TtiQueryTransParams;
  lQuery : TtiQuery;
begin
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(DBRequestXML);
    lParams := TtiQueryTransParams.Create;
    try
      lParams.SetValueAsString(uXMLTags.FieldNameQuerySQL, pSQLText);
      lParams.SetValueAsString(uXMLTags.FieldNameTransactionID, FTransactionID);
      lParams.SetValueAsString(uXMLTags.FieldNameCommandType,  cRemoteCommandTypes[pRemoteCommandType]);
      lParams.SetValueAsString(uXMLTags.FieldNameComputerName, tiGetComputerName);
      lParams.SetValueAsString(uXMLTags.FieldNameUserName,     tiGetUserName);
      lQuery.InsertRow(uXMLTags.TableNameQuery, lParams);
    finally
      lParams.Free;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseRemoteXML.ClearRequest;
var
  lQuery : TtiQuery;
begin
  //ToDo: Remove this hard-coded tiQuery once the framework can handle multiple persistence layers
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(DBRequestXML);
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
  result := false;
  Assert(false, 'Under construction');  
end;

function TtiDatabaseRemoteXML.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryRemoteXML;
end;

procedure TtiDatabaseRemoteXML.TryHTTPPost(const AInput: string);
var
  LTryCount: Integer;
  LTrySuccessful: Boolean;
begin
  LTryCount:= 1;
  LTrySuccessful:= False;
  // ToDo: Retry not required in TtiDatabaseRemoteXML.TryHTTPPost
  while (not LTrySuccessful) do
  begin
    try
      FHTTP.Clear;
      FHTTP.Input.WriteString(AInput);
      FHTTP.Post(DatabaseName + cgTIDBProxy);
      LTrySuccessful:= True;
    except
      on e:Exception do
      begin
        if LTryCount < cRetryCount then
        begin
          Inc(LTryCount);
          Sleep(cRetrySleep);
        end else
          raise;
      end;
    end;
  end;
end;

procedure TtiDatabaseRemoteXML.RefreshHTTPInstance;
var
  lProxyServer: string;
  lProxyPort : Integer;
  lProxyActive: Boolean;
  LHTTPClassMapping : string;
  LCurrentThreadID: TThreadID;
begin

  LCurrentThreadID:= GetCurrentThreadID;

  if (FCurrentThreadID<>LCurrentThreadID) then
  begin
    FCurrentThreadID:= LCurrentThreadID;
    if FHTTP<>nil then
      FreeAndNil(FHTTP);

    LHTTPClassMapping := Params.Values[cHTTPConnectWith];
    if LHTTPClassMapping <> '' then
      FHTTP := gTIHTTPFactory.CreateInstance(LHTTPClassMapping)
    else
    begin
      Assert(gTIHTTPClass <> nil, 'gTIHTTPClass not assigned');
      FHTTP:= gTIHTTPClass.Create;
    end;

    FHTTP.BlockSize:= StrToInt64Def(Params.Values[CHTTPBlockSize], 0);
    FHTTP.RetryLimit:= StrToIntDef(Params.Values[CHTTPRetryLimit], 1);

    if not SameText(cHTTPMSXML, FHTTP.MappingName) then
    begin
      lProxyActive := tiStrToBool(Params.Values[cHTTPProxyServeractive]);
      lProxyServer := Params.Values[cHTTPProxyServerName];
      lProxyPort  := StrToIntDef(Params.Values[cHTTPProxyPort], 0);
      if (lProxyActive) and
         (lProxyServer <> '') and
         (lProxyPort <> 0) then
      begin
        FHTTP.ProxyServer := lProxyServer;
        FHTTP.ProxyPort  := lProxyPort;
      end;
    end;
  end;

end;

{ TtiQueryRemoteXML }

constructor TtiQueryRemoteXML.Create;
begin
  inherited;
  FQueryResponseXML   := TtiQueryXMLLight.Create;
  FParams := TtiQueryTransParams.Create;
  FSQL   := TStringList.Create;
  FActive := false;
end;


destructor TtiQueryRemoteXML.Destroy;
begin
  FSQL.Free;
  FParams.Free;
  FQueryResponseXML.Free;
  inherited;
end;

function TtiQueryRemoteXML.ExecSQL: integer;
  procedure _SaveParams(const ADatabase : TtiDatabase);
  var
    i : integer;
    lParams : TtiQueryTransParams;
    lQuery : TtiQuery;
  begin
    lQuery := TtiQueryXMLLight.Create;
    try
      lQuery.AttachDatabase(ADatabase);
      lParams := TtiQueryTransParams.Create;
      try
        for i := 0 to FParams.Count - 1 do
        begin
          lParams.SetValueAsString(uXMLTags.FieldNameParamName, FParams.Items[i].Name);
          lParams.SetValueAsString(uXMLTags.FieldNameParamKind, FParams.Items[i].KindAsStr);
          lParams.SetValueAsString(uXMLTags.FieldNameParamValue, FParams.Items[i].GetValueAsString);
          lQuery.InsertRow(uXMLTags.TableNameQueryParam, lParams);
        end;
      finally
        lParams.Free;
      end;
    finally
      lQuery.Free;
    end;
  end;

begin

  DBRemoteXML.ClearRequest;

  DBRemoteXML.DBRequestXML.StartTransaction;
  DBRemoteXML.InsertRemoteRequest(rctExecSQL, SQLText);
  _SaveParams(DBRemoteXML.DBRequestXML);
  DBRemoteXML.DBRequestXML.Commit;
  DBRemoteXML.ExecuteRemoteCall;

  if DBRemoteXML.HasResultSet then
    FQueryResponseXML.SelectRow(uXMLTags.TableNameResultSet);
  Result := -1;
  { TODO :
When implementing RowsAffected,
please return correct result
and put FSupportsRowsAffected := True; in TtiQueryXXX.Create;}
end;

function TtiQueryRemoteXML.FieldCount: integer;
begin
  result := FQueryResponseXML.FieldCount;
end;

function TtiQueryRemoteXML.FieldIndex(const AName: string): integer;
begin
  result := FQueryResponseXML.FieldIndex(AName);
end;

function TtiQueryRemoteXML.FieldKind(AIndex: integer): TtiQueryFieldKind;
begin
  result := FQueryResponseXML.FieldKind(AIndex);
end;

function TtiQueryRemoteXML.FieldName(AIndex: integer): string;
begin
  result := FQueryResponseXML.FieldName(AIndex);
end;

function TtiQueryRemoteXML.FieldSize(AIndex: integer): integer;
begin
  result := FQueryResponseXML.FieldSize(AIndex);
end;

function TtiQueryRemoteXML.GetActive: boolean;
begin
  result := FActive;
end;

function TtiQueryRemoteXML.GetEOF: boolean;
begin
  result :=
    not DBRemoteXML.HasResultSet or
    FQueryResponseXML.EOF;
end;

function TtiQueryRemoteXML.GetFieldAsBoolean(const AName: string): boolean;
begin
  result := FQueryResponseXML.FieldAsBoolean[AName];
end;

function TtiQueryRemoteXML.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQueryResponseXML.FieldAsDateTime[AName];
end;

function TtiQueryRemoteXML.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQueryResponseXML.FieldAsFloat[AName];
end;

function TtiQueryRemoteXML.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FQueryResponseXML.FieldAsInteger[AName];
end;

function TtiQueryRemoteXML.GetFieldAsString(const AName: string): string;
begin
  result := FQueryResponseXML.FieldAsString[AName];
end;

function TtiQueryRemoteXML.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FQueryResponseXML.FieldIsNull[AName];
end;

function TtiQueryRemoteXML.GetParamAsBoolean(const AName: string): boolean;
begin
  result := FParams.GetValueAsBoolean(AName);
end;

function TtiQueryRemoteXML.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FParams.GetValueAsDateTime(AName);
end;

function TtiQueryRemoteXML.GetParamAsFloat(const AName: string): Extended;
begin
  result := FParams.GetValueAsFloat(AName);
end;

function TtiQueryRemoteXML.GetParamAsInteger(const AName: string): Int64;
begin
  result := FParams.GetValueAsInteger(AName);
end;

function TtiQueryRemoteXML.GetParamAsString(const AName: string): string;
begin
  result := FParams.GetValueAsString(AName);
end;

function TtiQueryRemoteXML.GetParamIsNull(const AName: String): Boolean;
begin
  result := FParams.ParamIsNull[AName];
end;

function TtiQueryRemoteXML.GetSQL: TStrings;
begin
  result := FSQL;
end;

function TtiQueryRemoteXML.HasNativeLogicalType: boolean;
begin
  result := FQueryResponseXML.HasNativeLogicalType;
end;

function TtiQueryRemoteXML.ParamCount: integer;
begin
  result := FParams.Count;
end;

function TtiQueryRemoteXML.ParamName(AIndex: integer): string;
begin
  result := FParams.ParamName(AIndex);
end;

procedure TtiQueryRemoteXML.Reset;
begin
  // Not sure what to put in here
end;

procedure TtiQueryRemoteXML.SetActive(const AValue: boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    if FActive then
      ExecSQL;
  end;
end;

procedure TtiQueryRemoteXML.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  FParams.SetValueAsBoolean(AName, AValue);
end;

procedure TtiQueryRemoteXML.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
  FParams.SetValueAsDateTime(AName, AValue);
end;

procedure TtiQueryRemoteXML.SetParamAsFloat(const AName: string; const AValue: Extended);
begin
  FParams.SetValueAsFloat(AName, AValue);
end;

procedure TtiQueryRemoteXML.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  FParams.SetValueAsInteger(AName, AValue);
end;

procedure TtiQueryRemoteXML.SetParamAsMacro(const AName, AValue: string);
begin
  SQLText :=
    tiCIStrTran(SQLText,
                 cgtiQueryMacroChr + AName,
                 AValue);
end;

procedure TtiQueryRemoteXML.SetParamAsString(const AName, AValue: string);
begin
  FParams.SetValueAsString(AName, AValue);
end;

procedure TtiQueryRemoteXML.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  FParams.ParamIsNull[ AName ]:= AValue;
end;

procedure TtiQueryRemoteXML.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
  FParams.Clear;
end;

procedure TtiQueryRemoteXML.AssignFieldAsStream(const AName: string;const AValue: TStream);
var
  ls : string;
  lStream : TStringStream;
begin
  ls := GetFieldAsString(AName);
  lStream := TStringStream.Create(ls);
  try
    AValue.Size := 0;
    MimeDecodeStream(lStream, AValue);
    AValue.Position := 0;
  finally
    lStream.Free;
  end;
end;

procedure TtiQueryRemoteXML.Next;
begin
  FQueryResponseXML.Next;
end;

procedure TtiQueryRemoteXML.AssignParamFromStream(const AName: string;const AValue: TStream);
begin
  FParams.SetValueAsStream(AName, AValue);
end;

procedure TtiQueryRemoteXML.AssignParamToStream(const AName: string; const AValue: TStream);
begin
  FParams.AssignValueToStream(AName, AValue);
end;

procedure TtiQueryRemoteXML.Close;
begin
  Active := false;
end;

procedure TtiQueryRemoteXML.Open;
begin
  Active := true;
end;

function TtiQueryRemoteXML.DBRemoteXML: TtiDatabaseRemoteXML;
begin
  result := (Database as TtiDatabaseRemoteXML);
end;

//var
//  lRegPerLayer : TtiRegPerLayer;

procedure TtiQueryRemoteXML.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  if ADatabase <> nil then
    FQueryResponseXML.AttachDatabase((ADatabase as TtiDatabaseRemoteXML).DBResponseXML);
end;

procedure TtiQueryRemoteXML.DetachDatabase;
begin
  inherited;
  FQueryResponseXML.DetachDatabase;
end;

procedure TtiQueryRemoteXML.SetSQLText(const AValue: string);
begin
  FSQL.Text := AValue;
  FParams.Clear;
end;

procedure TtiQueryRemoteXML.AssignFieldAsStreamByIndex(AIndex: Integer; const AValue: TStream);
var
  ls : string;
  lStream : TStringStream;
begin
  ls := GetFieldAsStringByIndex(AIndex);
  lStream := TStringStream.Create(ls);
  try
    AValue.Size := 0;
    MimeDecodeStream(lStream, AValue);
    AValue.Position := 0;
  finally
    lStream.Free;
  end;
end;

function TtiQueryRemoteXML.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
begin
  result := FQueryResponseXML.FieldAsBooleanByIndex[AIndex];
end;

function TtiQueryRemoteXML.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FQueryResponseXML.FieldAsDateTimeByIndex[AIndex];
end;

function TtiQueryRemoteXML.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FQueryResponseXML.FieldAsFloatByIndex[AIndex];
end;

function TtiQueryRemoteXML.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FQueryResponseXML.FieldAsIntegerByIndex[AIndex];
end;

function TtiQueryRemoteXML.GetFieldAsStringByIndex(AIndex: Integer): string;
begin
  result := FQueryResponseXML.FieldAsStringByIndex[AIndex];
end;

function TtiQueryRemoteXML.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FQueryResponseXML.FieldIsNullByIndex[AIndex];
end;

{ TtiPersistenceLayerRemoteXML }

procedure TtiPersistenceLayerRemoteXML.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistRemote;
  APersistenceLayerDefaults.DatabaseName:= cLocalHost;
  APersistenceLayerDefaults.Username:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerRemoteXML.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseRemoteXML;
end;

function TtiPersistenceLayerRemoteXML.GetPersistenceLayerName: string;
begin
  result:= cTIPersistRemote;
end;

function TtiPersistenceLayerRemoteXML.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryRemoteXML;
end;

Initialization
begin
  uXMLTags := TtiXMLTags.Create;
  uXMLTags.OptXMLDBSize := optDBSizeOn;

  RegisterMappings;

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerRemoteXML);

  // Change the default package from XML to Remote
  if (GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cTIPersistXMLLight) <> nil) and
     (GTIOPFManager.PersistenceLayers.DefaultPersistenceLayerName = cTIPersistXMLLight) then
    GTIOPFManager.PersistenceLayers.DefaultPersistenceLayerName := cTIPersistRemote;

end;

finalization
  uXMLTags.Free;
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistRemote);
    
end.


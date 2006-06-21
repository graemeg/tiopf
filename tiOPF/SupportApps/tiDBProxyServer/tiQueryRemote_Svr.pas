unit tiQueryRemote_Svr;

{$I tiDefines.inc}

interface
uses
  tiObjAbs
  ,tiQueryRemote
  ,tiQuery
  ,tiDBConnectionPool
  ,Contnrs
  ,tiQueryXMLLight
  ,Classes
  ,Windows
  ,tiXMLToTIDataSet
  ,tiXMLToTIDataSet1
  ;

const
  cErrorTransactionTimedOut = 'Transaction ID#%s timed out';

type

  TThrdStatefulDBConnectionPoolMonitor = class;
  TSavedDBConnectionHolder = class;
  TtiStatefulDBConnectionPool = class;

  TThrdStatefulDBConnectionPoolMonitor = class( TThread )
  private
    FPool : TtiStatefulDBConnectionPool ;
  public
    constructor CreateExt( pPool : TtiStatefulDBConnectionPool ) ;
    procedure   Execute ; override ;
  end ;

  TSavedDBConnectionHolder = class( TtiObjAbs )
  private
    FTransactionID: string;
    FDBConnection: TPooledDB;
    FLastUsed: TDateTime;
    FDBConnectionName: string;
    FInUse: boolean;
    FOwner: TtiStatefulDBConnectionPool;
    FUserName: string;
    FComputerName: string;
    function GetSecToTimeOut: integer;
    function GetSecInUse: integer;
    procedure SetInUse(const Value: boolean);
  public
    constructor Create ;
    property Owner : TtiStatefulDBConnectionPool read FOwner write FOwner ;
    property TransactionID : string read FTransactionID write FTransactionID ;
    property DBConnection : TPooledDB read FDBConnection write FDBConnection ;
    property DBConnectionName : string read FDBConnectionName write FDBConnectionName ;
    property LastUsed : TDateTime read FLastUsed write FLastUsed ;
    property InUse : boolean read FInUse write SetInUse ;
    property SecToTimeOut : integer read GetSecToTimeOut ;
    property SecInUse : integer read GetSecInUse;
    property ComputerName : string read FComputerName write FComputerName ;
    property UserName : string read FUserName write FUserName ;
  end ;

  TSavedDBConnectionHolderEvent = procedure( const pItem : TSavedDBConnectionHolder ) of object ;

  TtiStatefulDBConnectionPool = class( TtiObjAbs )
  private
    FSavedDBConnections : TObjectList ;
    FNextTransID : Integer ;
    FSemaphore : THandle ;
    FThrdStatefulDBConnectionPoolMonitor : TThrdStatefulDBConnectionPoolMonitor ;
    FTimeOut : real ;
    function    GetNextTransID: string;
    procedure   WaitForSemaphore ;
    procedure   ReleaseSemaphore ;
    property    NextTransID : string read GetNextTransID ;
    procedure   DoCommit( pDBConnection : TPooledDB ) ;
    procedure   DoRollBack( pDBConnection : TPooledDB ) ;
    function    GetCount: integer;
    procedure   UnLock(           const pTransactionID : string ; pMethod : TPooledDBEvent ) ;
    function    Lock(             const pDBConnectionName : string ) : TSavedDBConnectionHolder ;
    //procedure   Remove( const pSavedDBConnectionHolder : TSavedDBConnectionHolder );

  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Count : integer read GetCount ;
    procedure   SweepForTimeOuts ;
    property    TimeOut : real read FTimeOut write FTimeOut ;

    function    FindSavedDBConnectionHolder( const pTransactionID : string ) : TSavedDBConnectionHolder ;
    function    FindSavedDBConnection( const pTransactionID : string ) : TtiDatabase ;
    function    StartTransaction( const pDBConnectionName, pComputerName, pUserName : string) : string ;
    procedure   Commit(           const pTransactionID : string ) ;
    procedure   RollBack(         const pTransactionID : string ) ;

    procedure   GetSummaryStats( var pTotalStatefulDBConnections : integer ;
                                 var pInUseStatefulDBConnections : integer ;
                                 var pWaitingStatefulDBConnections : integer );
    procedure   ForEach(const pMethod : TSavedDBConnectionHolderEvent);

  end ;

  TtiQueryRemoteExec = class( TtiObjAbs )
  private
    FDBRequest          : TtiDatabaseXMLLight;
    //FDBResponse         : TtiDatabaseXMLLight;
    FErrorMessage       : string ;

    FRemoteCommandType  : TtiRemoteCommandType ;
    FRemoteCommandText  : string ;
    FTransactionID      : string ;
    FRemoteComputerName : string ;
    FRemoteUserName     : string ;
    FRowCount           : integer ;
    FQueryParams        : TtiQueryTransParams ;

    FXMLWriterData      : TtiDataSetToXMLWriter ;
    FXMLWriterMessage   : TtiDataSetToXMLWriter ;

    procedure ParseRequest ;
    procedure ExecuteRequest ;

    procedure DBRequestToQuery ;
    procedure DBRequestToQueryParams ;
    procedure GetRemoteCommandType ;

    procedure DoStartTransaction ;
    procedure DoCommit ;
    procedure DoRollBack ;
    procedure DoExecSQL ;

    procedure DoCreateTable ;
    procedure DoDropTable ;
    procedure DoReadMetaDataTables ;
    procedure DoReadMetaDataFields ;

    procedure CreateResponseMetaDataTable;
    procedure InsertResponseMessageData;
    //procedure CreateResponseDataTable(const pQuery: TtiQuery);
    //procedure InsertResponseData(       const pQuery : TtiQuery ) ;

    procedure DoLog(const pMessage : string ; pError : boolean);

  public
    constructor create ;
    destructor  destroy ; override ;
    function    ExecuteRemoteXML( const pInput : string ) : string ;
  protected
  end ;

function ExecuteRemoteXML( const pInput : string ) : string ;
function gStatefulDBConnectionPool : TtiStatefulDBConnectionPool ;

implementation
uses
  tiRJMime
  ,SysUtils
  ,tiUtils
  ,tiPtnVisPerObj_Cli
  ,tiPersist
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiLog
  ,cTIPersist
  ,tiDialogs
  ,tiXML
  ;

var
  uStatefulDBConnectionPool : TtiStatefulDBConnectionPool ;
  uXMLTags : TtiXMLTags ;

function ExecuteRemoteXML( const pInput : string ) : string ;
var
  lQueryRemoteExec : TtiQueryRemoteExec ;
begin
  try
    lQueryRemoteExec := TtiQueryRemoteExec.Create ;
    try
      result := lQueryRemoteExec.ExecuteRemoteXML(pInput);
    finally
      lQueryRemoteExec.Free ;
    end ;
  except
    on e:exception do
      tiFmtException(e, '', 'ExecuteRemoteXML' ) ;
  end;
end ;

function gStatefulDBConnectionPool : TtiStatefulDBConnectionPool ;
begin
  if uStatefulDBConnectionPool = nil then
    uStatefulDBConnectionPool := TtiStatefulDBConnectionPool.Create ;
  result := uStatefulDBConnectionPool ;
end;

{ TtiQueryRemoteExec }

{
procedure TtiQueryRemoteExec.InsertResponseData(const pQuery: TtiQuery);
var
  i : integer ;
  lName : string ;
  lKind : TtiQueryFieldKind ;
  lStream : TMemoryStream ;
begin
  Assert( FXMLWriterData.TestValid(TtiXMLWriter), cTIInvalidObjectError );
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  try
    while not pQuery.EOF do
    begin
      FXMLWriterData.AddRow;
      for i := 0 to pQuery.FieldCount - 1 do
      begin
        lName := pQuery.FieldName(i) ;
        // ToDo: Checking of FieldIsNull will be slow
        if not pQuery.FieldIsNull[lName] then
        begin
          // ToDo: Only read field kind once, then cache the values
          lKind := pQuery.FieldKind(i);
          case lKind of
          qfkString,
          qfkLongString : FXMLWriterData.AddCellAsString(lName,pQuery.FieldAsString[  lName]);
          qfkInteger    : FXMLWriterData.AddCellAsInteger( lName,pQuery.FieldAsInteger[ lName]);
          qfkFloat      : FXMLWriterData.AddCellAsFloat(   lName,pQuery.FieldAsFloat[   lName]);
          qfkDateTime   : FXMLWriterData.AddCellAsDateTime(lName,pQuery.FieldAsDateTime[lName]);
          qfkLogical    : FXMLWriterData.AddCellAsBoolean( lName,pQuery.FieldAsBoolean[ lName]);
          qfkBinary     : begin
                            lStream := TMemoryStream.Create;
                            try
                              pQuery.AssignFieldAsStream(lName,lStream);
                              FXMLWriterData.AddCellAsStream(lName,lStream);
                            finally
                              lStream.Free;
                            end;
                          end ;
          else
            tiFmtException( 'Invalid QueryFieldKind', ClassName, 'InsertResponseData' ) ;
          end ;
        end else
        begin
          FXMLWriterData.AddCellAsString(lName,'') ;
        end ;
      end ;
      Inc(FRowCount);
      pQuery.Next ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'CopyResultData' ) ;
  end ;
end;
}

constructor TtiQueryRemoteExec.create;
begin
  inherited;
  FDBRequest        := TtiDatabaseXMLLight.Create;
  FDBRequest.PersistToFile := false ;
  FDBRequest.OptXMLDBSize := optDBSizeOn ;
  FDBRequest.XMLFieldNameStyle := xfnsInteger ;

  FRemoteCommandType := rctUnknown ;
  FTransactionID     := cNullTransactionID;
  FRowCount          := 0 ;
  FXMLWriterMessage  := TtiDataSetToXMLWriter.Create ;
  FXMLWriterMessage.OptXMLDBSize := optDBSizeOn ;
  FXMLWriterMessage.XMLFieldNameStyle := xfnsInteger ;

  FXMLWriterData     := TtiDataSetToXMLWriter.Create ;
  FXMLWriterData.OptXMLDBSize := optDBSizeOn ;
  FXMLWriterData.XMLFieldNameStyle := xfnsInteger ;

end;

{
procedure TtiQueryRemoteExec.CreateResponseDataTable(const pQuery: TtiQuery);
var
  i : integer ;
  lFieldName : string ;
  lFieldKind : TtiQueryFieldKind ;
  lFieldSize : integer ;
begin
  Assert( FXMLWriterData.TestValid(TtiXMLWriter), cTIInvalidObjectError );
  try
    FXMLWriterData.AddTable(cTableNameResultSet);
    for i := 0 to pQuery.FieldCount - 1 do
    begin
      lFieldName := pQuery.FieldName(i) ;
      lFieldKind := pQuery.FieldKind(i) ;
      lFieldSize := pQuery.FieldSize(i);
      FXMLWriterData.AddField(lFieldName, lFieldKind, lFieldSize);
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'CreateResultTable' ) ;
  end ;
end;
}

destructor TtiQueryRemoteExec.destroy;
begin
  FDBRequest.Free;
  FQueryParams.Free ;
  FXMLWriterMessage.Free;
  FXMLWriterData.Free;
  inherited;
end;

function TtiQueryRemoteExec.ExecuteRemoteXML(const pInput: string): string;
var
  lRequest  : string ;
  lResponse : string ;
  lIsError  : boolean ;
begin
  lIsError := false ;
  try
    try
      lRequest := tiQueryRemoteDecode(pInput, cgsCompressZLib);
      DoLog(lRequest, false);
      FDBRequest.AsString := lRequest ;
      ParseRequest ;
      ExecuteRequest ;
    except
      on e:exception do
      begin
        FErrorMessage := e.Message ;
        DoLog( 'Error:'    + Cr + e.Message + Cr(2) +
               'Request:'  + Cr + lRequest  + Cr(2) +
               'Response:' + Cr + FXMLWriterData.AsString, true) ;
        lIsError := true ;
      end;
    end ;
    InsertResponseMessageData;
    lResponse :=
      uXMLTags.DatabaseHeader +
      FXMLWriterMessage.TableData +
      FXMLWriterData.TableData +
      uXMLTags.DatabaseFooter;
    if not lIsError then
      DoLog(lResponse, false);
    result := tiQueryRemoteEncode(lResponse, cgsCompressZLib) ;
    Assert( Trim( result ) <> '', 'Error in result string.' ) ;
  except
    on e:exception do
      DoLog( 'Error:'    + Cr + e.Message + Cr(2) +
             'Request:'  + Cr + lRequest  + Cr(2) +
             'Response:' + Cr + lResponse, true ) ;
  end;
end;

procedure TtiQueryRemoteExec.ParseRequest ;
begin
  GetRemoteCommandType ;
  if FRemoteCommandType = rctExecSQL then
  begin
    FQueryParams      := TtiQueryTransParams.Create ;
    DBRequestToQuery ;
    DBRequestToQueryParams ;
  end ;
  if FDBRequest.InTransaction then
    FDBRequest.RollBack ;
end ;

procedure TtiQueryRemoteExec.ExecuteRequest ;
begin
  GetRemoteCommandType ;
  case FRemoteCommandType of
  rctStartTransaction   : DoStartTransaction ;
  rctCommit             : DoCommit ;
  rctRollBack           : DoRollBack ;
  rctExecSQL            : DoExecSQL ;
  rctCreateTable        : DoCreateTable ;
  rctDropTable          : DoDropTable ;
  rctReadMetaDataTables : DoReadMetaDataTables ;
  rctReadMetaDataFields : DoReadMetaDataFields ;
  else
    tiFmtException( 'Invalid TtiRemoteCommandType', ClassName, 'ExecuteRequest' ) ;
  end ;
end ;

procedure TtiQueryRemoteExec.DBRequestToQuery ;
var
  lQuery : TtiQueryXMLLight ;
begin
  Assert( FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError );
  try
    lQuery := TtiQueryXMLLight.Create ;
    try
      lQuery.AttachDatabase(FDBRequest);
      lQuery.SelectRow(uXMLTags.TableNameQuery);
      if lQuery.EOF then
        tiFmtException('Invalid data query request', ClassName, '_LoadQuery' ) ;
      FQueryParams.SQL := FRemoteCommandText;
    finally
      lQuery.Free ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'LoadQuery' ) ;
  end;
end ;

procedure TtiQueryRemoteExec.DBRequestToQueryParams ;
var
  lQuery : TtiQueryXMLLight ;
begin
  Assert( FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError );
  Assert( FQueryParams.TestValid(TtiQueryTransParams), cTIInvalidObjectError );
  try
    lQuery := TtiQueryXMLLight.Create ;
    try
      lQuery.AttachDatabase(FDBRequest);
      lQuery.SelectRow(uXMLTags.TableNameQueryParam);
      while not lQuery.EOF do
      begin
        FQueryParams.AddInstance(
          lQuery.FieldAsString[uXMLTags.FieldNameParamName],
          lQuery.FieldAsString[uXMLTags.FieldNameParamKind],
          lQuery.FieldAsString[uXMLTags.FieldNameParamValue]);
        lQuery.Next ;
      end ;
    finally
      lQuery.Free ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, '_LoadQueryParams' ) ;
  end;
end ;

procedure TtiQueryRemoteExec.GetRemoteCommandType;
var
  lQuery : TtiQueryXMLLight ;
  lCommandType : string ;
begin
  Assert( FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError );
  try
    lQuery := TtiQueryXMLLight.Create ;
    try
      lQuery.AttachDatabase(FDBRequest);
      lQuery.SelectRow(uXMLTags.TableNameQuery);
      if lQuery.EOF then
        tiFmtException('Invalid data query request', ClassName, '_LoadQuery' ) ;
      lCommandType        := lQuery.FieldAsString[uXMLTags.FieldNameCommandType];
      FRemoteCommandType  := StrToRemoteCommandType(lCommandType);
      FTransactionID      := lQuery.FieldAsString[uXMLTags.FieldNameTransactionID];
      FRemoteCommandText  := lQuery.FieldAsString[uXMLTags.FieldNameQuerySQL];
      FRemoteComputerName := lQuery.FieldAsString[uXMLTags.FieldNameComputerName];
      FRemoteUserName     := lQuery.FieldAsString[uXMLTags.FieldNameUserName];
    finally
      lQuery.Free ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'LoadQuery' ) ;
  end;
end;

procedure TtiQueryRemoteExec.DoCommit;
begin
  Assert( gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError );
  gStatefulDBConnectionPool.Commit(FTransactionID);
  FTransactionID := cNullTransactionID ;
end;

procedure TtiQueryRemoteExec.DoRollBack;
begin
  Assert( gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError );
  gStatefulDBConnectionPool.RollBack(FTransactionID);
  FTransactionID := cNullTransactionID ;
end;

procedure TtiQueryRemoteExec.DoExecSQL;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
  lDatabase : TtiDatabase ;
  lQuery : TtiQuery ;
  lStart : DWord ;
  lSQL : string ;
begin
  try
    Assert( gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError );
    Assert(FTransactionID <> '', 'TransactionID not assigned');
    lQuery := gTIPerMgr.DefaultPerLayer.tiQueryClass.Create ;
    try
      lSavedDBConnectionHolder := gStatefulDBConnectionPool.FindSavedDBConnectionHolder(FTransactionID);
      lSavedDBConnectionHolder.InUse := true ;
      try
        Assert( lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
        Assert( lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
        Assert( lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
        lDatabase := lSavedDBConnectionHolder.DBConnection.Database;
        lQuery.AttachDatabase(lDatabase);
        FQueryParams.AssignToQuery(lQuery);
        lSQL := Copy( lQuery.SQLText, 1, 50 ) ;
        lStart := GetTickCount ;
        LogArray([  'About to run SQL    ', lSQL, GetTickCount-lStart], lsQueryTiming);
        lQuery.Active := true ;
//        if not lQuery.EOF then
//        begin
          lStart := GetTickCount ;
          LogArray(['  About to build XML', lSQL, GetTickCount-lStart], lsQueryTiming);
          //CreateResponseDataTable(lQuery);
          //InsertResponseData(lQuery);
          FRowCount := FXMLWriterData.AssignFromTIQuery(uXMLTags.TableNameResultSet, lQuery);
          LogArray(['    Done            ', lSQL, GetTickCount-lStart], lsQueryTiming);
//        end ;
      finally
        lSavedDBConnectionHolder.InUse := false ;
      end ;
    finally
      lQuery.Free ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DoExecSQL' ) ;
  end;
end;

procedure TtiQueryRemoteExec.DoStartTransaction;
begin
  Assert( gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError );
  FTransactionID :=
    gStatefulDBConnectionPool.StartTransaction(
      gTIPerMgr.DefaultDBConnectionName,
      FRemoteComputerName, FRemoteUserName);
end;

procedure TtiQueryRemoteExec.InsertResponseMessageData;
begin
  Assert( FXMLWriterMessage.TestValid(TtiDataSetToXMLWriter), cTIInvalidObjectError );
  try
    FXMLWriterMessage.AddTable(uXMLTags.TableNameResultMessage);
    FXMLWriterMessage.AddField(uXMLTags.FieldNameResultError, qfkString, 9999);
    FXMLWriterMessage.AddField(uXMLTags.FieldNameTransactionID, qfkString, 9999);
    FXMLWriterMessage.AddField(uXMLTags.FieldNameResultRowCount, qfkInteger);
    FXMLWriterMessage.AddRow;
    FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameResultError, FErrorMessage) ;
    FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameTransactionID, FTransactionID) ;
    FXMLWriterMessage.AddCellAsInteger(uXMLTags.FieldNameResultRowCount, FRowCount) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'CopyResultData' ) ;
  end ;
end;

procedure TtiQueryRemoteExec.DoCreateTable;
var
  lQuery : TtiQueryXMLLight ;
  lMD : TtiDBMetaDataTable ;
begin
  Assert( FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError );
  try
    lMD := TtiDBMetaDataTable.Create ;
    try
      lQuery := TtiQueryXMLLight.Create ;
      try
        lQuery.AttachDatabase(FDBRequest);
        lQuery.SelectRow(uXMLTags.TableNameMetaData);
        if lQuery.EOF then
          tiFmtException('Unable to find table metadata', ClassName, 'DoCreateTable' ) ;
        lMD.Name := lQuery.FieldAsString[uXMLTags.FieldNameMetaDataTableName];
        while not lQuery.EOF do
        begin
          lMD.AddInstance(
            lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldName],
            StrToQueryFieldKind(lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldKind]),
            lQuery.FieldAsInteger[uXMLTags.FieldNameMetaDataFieldWIdth]);
          lQuery.Next ;
        end;
      finally
        lQuery.Free ;
      end ;
      gTIPerMgr.CreateTable(lMD);
    finally
      lMD.Free;
    end;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'LoadQuery' ) ;
  end;
end;

procedure TtiQueryRemoteExec.DoDropTable;
var
  lMD : TtiDBMetaDataTable ;
begin
  try
    lMD := TtiDBMetaDataTable.Create ;
    try
      gTIPerMgr.DropTable(FRemoteCommandText);
    finally
      lMD.Free;
    end;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'LoadQuery' ) ;
  end;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataFields;
var
  lPooledDB : TPooledDB ;
  lDatabase : TtiDatabase ;
  lMD       : TtiDBMetaDataTable ;
  i         : integer ;
begin
  Assert( FXMLWriterData.TestValid(TtiDataSetToXMLWriter), cTIInvalidObjectError );
  CreateResponseMetaDataTable;

  lPooledDB := gTIPerMgr.DefaultPerLayer.DefaultDBConnectionPool.Lock ;
  lDatabase := lPooledDB.Database ;
  try
    lMD := TtiDBMetaDataTable.Create ;
    try
      lMD.Name := FRemoteCommandText ;
      lDatabase.ReadMetaDataFields(lMD);
      for i := 0 to lMD.Count - 1 do
      begin
        FXMLWriterData.AddRow;
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataTableName, lMD.Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldName, lMD.Items[i].Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldKind, lMD.Items[i].KindAsStr);
        FXMLWriterData.AddCellAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, lMD.Items[i].Width);
      end ;
    finally
      lMD.Free;
    end;
  finally
    gTIPerMgr.DefaultPerLayer.DefaultDBConnectionPool.UnLock(lPooledDB);
  end ;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataTables;
var
  lPooledDB : TPooledDB ;
  lDatabase : TtiDatabase ;
  lMD       : TtiDBMetaData ;
  i         : integer ;
begin
  Assert( FXMLWriterMessage.TestValid(TtiDataSetToXMLWriter), cTIInvalidObjectError );
  CreateResponseMetaDataTable;
  lPooledDB := gTIPerMgr.DefaultPerLayer.DefaultDBConnectionPool.Lock ;
  lDatabase := lPooledDB.Database ;
  try
    lMD := TtiDBMetaData.Create ;
    try
      lDatabase.ReadMetaDataTables(lMD);
      for i := 0 to lMD.Count - 1 do
      begin
        FXMLWriterData.AddRow;
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataTableName,   lMD.Items[i].Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldName,   '');
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldKind,   cgaQueryFieldKind[qfkString]);
        FXMLWriterData.AddCellAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, 0);
      end ;
    finally
      lMD.Free;
    end;
  finally
    gTIPerMgr.DefaultPerLayer.DefaultDBConnectionPool.UnLock(lPooledDB);
  end ;
end;

procedure TtiQueryRemoteExec.DoLog(const pMessage : string ; pError : boolean);
begin
//  if not pError then
//    Log(pMessage)
//  else
//    LogError(pMessage);
end;

procedure TtiQueryRemoteExec.CreateResponseMetaDataTable;
begin
  FXMLWriterData.AddTable(uXMLTags.TableNameMetaData);
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataTableName,  qfkString, 255 ) ;
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldName,  qfkString, 255 ) ;
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldKind,  qfkString, 20 ) ;
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldWidth, qfkInteger ) ;
end;

{ TtiStatefulDBConnectionPool }

constructor TtiStatefulDBConnectionPool.Create;
var
  lSemaphoreName : string ;
begin
  inherited ;
  FSavedDBConnections := TObjectList.Create ;
  FNextTransID := 1 ;
  lSemaphoreName := ClassName ;
  FSemaphore := CreateSemaphore( nil, 1, 1,
                                 PChar( lSemaphoreName )) ;
  FTimeOut := cDBProxyServerTimeOut ;
  FThrdStatefulDBConnectionPoolMonitor := TThrdStatefulDBConnectionPoolMonitor.CreateExt(Self) ;
end;

destructor TtiStatefulDBConnectionPool.Destroy;
begin
  FThrdStatefulDBConnectionPoolMonitor.Terminate;
  FThrdStatefulDBConnectionPoolMonitor.WaitFor;
  FThrdStatefulDBConnectionPoolMonitor.Free;
  FSavedDBConnections.Free ;
  CloseHandle( FSemaphore ) ;
  inherited;
end;

function TtiStatefulDBConnectionPool.Lock( const pDBConnectionName : string ) : TSavedDBConnectionHolder ;
begin
  result := nil ;
  try
    WaitForSemaphore ;
    try
      result := TSavedDBConnectionHolder.Create ;
      result.DBConnectionName := pDBConnectionName ;
      result.TransactionID    := NextTransID ;
      result.DBConnection := gTIPerMgr.DefaultPerLayer.DBConnectionPools.Lock( pDBConnectionName ) ;
      result.Owner := Self ;
      Assert( result.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
      FSavedDBConnections.Add( result ) ;
      result.LastUsed       := now ;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'Lock' ) ;
  end;
end;

function TtiStatefulDBConnectionPool.FindSavedDBConnectionHolder(
            const pTransactionID: string ): TSavedDBConnectionHolder;
var
  i : integer ;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
begin
  result := nil ;
  try
    for i := 0 to FSavedDBConnections.Count - 1 do
    begin
      lSavedDBConnectionHolder := TSavedDBConnectionHolder( FSavedDBConnections.Items[i] ) ;
      Assert( lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
      if ( lSavedDBConnectionHolder.TransactionID = pTransactionID ) then
      begin
        lSavedDBConnectionHolder.LastUsed := now ;
        result := lSavedDBConnectionHolder ;
        Break ; //==>
      end ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'FindSavedDBConnectionHolder' ) ;
  end;
  if result = nil then
    raise exception.CreateFmt( cErrorTransactionTimedOut, [pTransactionID] ) ;
end ;

function TtiStatefulDBConnectionPool.GetNextTransID: string;
begin
  result := IntToStr(FNextTransID) ;
  Inc( FNextTransID ) ;
end;

procedure TtiStatefulDBConnectionPool.WaitForSemaphore;
begin
  if WaitForSingleObject( FSemaphore, 60000 ) = WAIT_TIMEOUT then
    tiFmtException( 'Timed out waiting for a semaphore',
                    ClassName,
                    'WaitForSemaphore' ) ;
end;

procedure TtiStatefulDBConnectionPool.ReleaseSemaphore;
begin
  Windows.ReleaseSemaphore( FSemaphore, 1, nil ) ;
end;

procedure TtiStatefulDBConnectionPool.DoCommit( pDBConnection: TPooledDB);
begin
  Assert( pDBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
  Assert( pDBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
  pDBConnection.Database.Commit ;
end;

procedure TtiStatefulDBConnectionPool.DoRollBack(pDBConnection: TPooledDB);
begin
  Assert( pDBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
  Assert( pDBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
  pDBConnection.Database.RollBack ;
end;

procedure TtiStatefulDBConnectionPool.UnLock(const pTransactionID : string ;
                                             pMethod: TPooledDBEvent );
var
  lSavedDBConnection : TSavedDBConnectionHolder ;
  i : integer ;
begin
  try
    WaitForSemaphore ;
    try
      try
        lSavedDBConnection := FindSavedDBConnectionHolder( pTransactionID ) ;
        Assert( lSavedDBConnection.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
        Assert( lSavedDBConnection.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
        pMethod( lSavedDBConnection.DBConnection ) ;
        gTIPerMgr.DefaultPerLayer.DBConnectionPools.UnLock( lSavedDBConnection.DBConnectionName,
                                     lSavedDBConnection.DBConnection ) ;
        i := FSavedDBConnections.IndexOf( lSavedDBConnection ) ;
        FSavedDBConnections.Delete( i ) ;
      except
        on e:exception do
          tiFmtException( e, ClassName, 'CommitAndUnLock' ) ;
      end ;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'UnLock' ) ;
  end;
end;

function TtiStatefulDBConnectionPool.GetCount: integer;
begin
  result := FSavedDBConnections.Count ;
end;

function TtiStatefulDBConnectionPool.StartTransaction(const pDBConnectionName, pComputerName, pUserName : string): string;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
begin
  try
    lSavedDBConnectionHolder := Lock(pDBConnectionName);
    Assert( lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
    Assert( lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
    Assert( lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
    lSavedDBConnectionHolder.ComputerName := pComputerName ;
    lSavedDBConnectionHolder.UserName := pUserName ;
    result := lSavedDBConnectionHolder.TransactionID ;
    lSavedDBConnectionHolder.DBConnection.Database.StartTransaction ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'StartTransaction' ) ;
  end;
end;

procedure TtiStatefulDBConnectionPool.Commit(const pTransactionID: string);
begin
  UnLock( pTransactionID, DoCommit ) ;
end;

procedure TtiStatefulDBConnectionPool.RollBack(const pTransactionID: string);
begin
  UnLock( pTransactionID, DoRollBack ) ;
end;

function TtiStatefulDBConnectionPool.FindSavedDBConnection(const pTransactionID: string): TtiDatabase;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
begin
  result := nil ;
  try
    lSavedDBConnectionHolder := FindSavedDBConnectionHolder(pTransactionID);
    if lSavedDBConnectionHolder <> nil then
    begin
      Assert( lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
      Assert( lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
      Assert( lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
      result := lSavedDBConnectionHolder.DBConnection.Database;
    end else
      result := nil ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'FindSavedDBConnection' ) ;
  end;
end;

constructor TThrdStatefulDBConnectionPoolMonitor.CreateExt(pPool: TtiStatefulDBConnectionPool);
begin
  Create( true ) ;
  FreeOnTerminate := false ;
  FPool := pPool ;
 resume ;
end ;

procedure TThrdStatefulDBConnectionPoolMonitor.Execute;
var
  i : integer ;
begin
  while not terminated do
  begin
    // Sleep for 10 seconds, but keep checking terminated
    i := 0 ;
    while ( i < 10 ) and
          ( not terminated ) do
    begin
      Inc( i ) ;
      sleep( 1000 ) ;
    end ;
    if not Terminated then
      FPool.SweepForTimeOuts ;
  end ;

end;

procedure TtiStatefulDBConnectionPool.SweepForTimeOuts;
var
  i : integer ;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
begin
  try
    WaitForSemaphore ;
    try
      if Count = 0 then
        Exit ; //==>
      // TimeOut is in minutes, so convert to TDateTime
      for i :=  Count - 1 downto 0 do
      begin
        lSavedDBConnectionHolder := FSavedDBConnections.Items[i] as TSavedDBConnectionHolder ;
        Assert( lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError );
        Assert( lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError );
        Assert( lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError );
        if ( not lSavedDBConnectionHolder.InUse ) and
           ( lSavedDBConnectionHolder.SecToTimeOut <= 0 ) then
        begin
          if lSavedDBConnectionHolder.DBConnection.Database.InTransaction then
              lSavedDBConnectionHolder.DBConnection.Database.Rollback ;
          gTIPerMgr.DefaultPerLayer.DBConnectionPools.UnLock( lSavedDBConnectionHolder.DBConnectionName,
                                       lSavedDBConnectionHolder.DBConnection ) ;
          FSavedDBConnections.Delete( i ) ;
        end ;
      end ;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SweepForTimeOuts' ) ;
  end;
end;

procedure TtiStatefulDBConnectionPool.GetSummaryStats(
  var pTotalStatefulDBConnections, pInUseStatefulDBConnections,
  pWaitingStatefulDBConnections: integer);
var
  i : integer ;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder ;
begin
  pTotalStatefulDBConnections   := 0 ;
  pInUseStatefulDBConnections   := 0 ;
  pWaitingStatefulDBConnections := 0 ;
  try
    WaitForSemaphore ;
    try
      for i := 0 to FSavedDBConnections.Count - 1 do
      begin
        lSavedDBConnectionHolder := TSavedDBConnectionHolder( FSavedDBConnections.Items[i] ) ;
        if lSavedDBConnectionHolder.InUse then
          Inc(pInUseStatefulDBConnections)
        else
          Inc(pWaitingStatefulDBConnections);
        Inc(pTotalStatefulDBConnections);
      end ;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'GetSummaryStats' ) ;
  end;
end;

procedure TtiStatefulDBConnectionPool.ForEach(const pMethod: TSavedDBConnectionHolderEvent);
var
  lSavedDBConnection : TSavedDBConnectionHolder ;
  i : integer ;
begin
  try
    WaitForSemaphore ;
    try
      for i := 0 to FSavedDBConnections.Count - 1 do
      begin
        lSavedDBConnection := TSavedDBConnectionHolder(FSavedDBConnections.Items[i]) ;
        pMethod(lSavedDBConnection);
      end ;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'ForEach' ) ;
  end;
end;
{
procedure TtiStatefulDBConnectionPool.Remove(
  const pSavedDBConnectionHolder: TSavedDBConnectionHolder);
begin
  try
    WaitForSemaphore ;
    try
      FSavedDBConnections.Extract(pSavedDBConnectionHolder);
      pSavedDBConnectionHolder.Free;
    finally
      ReleaseSemaphore ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'Remove' ) ;
  end;
end;
}
{ TSavedDBConnectionHolder }

constructor TSavedDBConnectionHolder.Create;
begin
  inherited ;
  FInUse := false ;
end;

function TSavedDBConnectionHolder.GetSecInUse: integer;
begin
  result := Trunc(( Now - LastUsed ) * 24 * 60 * 60  ) ;
end;

function TSavedDBConnectionHolder.GetSecToTimeOut: integer;
begin
  if FInUse then
    result := cSecToTimeOutLocked
  else
    result := Trunc( Owner.TimeOut * 60 ) - SecInUse ;
end;

procedure TSavedDBConnectionHolder.SetInUse(const Value: boolean);
begin
  FInUse := Value;
  LastUsed := Now ;
end;

initialization
  uXMLTags := TtiXMLTags.Create;
  uXMLTags.OptXMLDBSize := optDBSizeOn ;

finalization
  uStatefulDBConnectionPool.Free ;
  uXMLTags.Free;

end.

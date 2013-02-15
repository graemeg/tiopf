unit tiQueryRemote_Svr;

{$I tiDefines.inc}

interface
uses
  tiBaseObject,
  tiQueryRemote,
  tiDBConnectionPool,
  tiQueryXMLLight,
  tiXMLToTIDataSet,
  tiConstants,
  tiStreams,
  tiQuery,
  tiThread,
  Contnrs,
  Classes,
  Windows,
  SyncObjs;

const
  CErrorTransactionTimedOut = 'Transaction ID#%s timed out';
  CErrorDatabaseConnectionBeingRolledBack = 'Attept to execute a database command against a connection that is being rolled back';

type

  TThrdStatefulDBConnectionPoolMonitor = class;
  TSavedDBConnectionHolder = class;
  TtiStatefulDBConnectionPool = class;

  TThrdStatefulDBConnectionPoolMonitor = class(TtiSleepThread)
  private
    FPool : TtiStatefulDBConnectionPool;
    FSweepInterval: integer;
  public
    constructor CreateExt(
      const APool : TtiStatefulDBConnectionPool;
      const ASweepInterval: integer);
    procedure   Execute; override;
  end;

  TSavedDBConnectionHolder = class(TtiBaseObject)
  private
    FTransactionID: string;
    FDBConnection: TtiDatabase;
    FLastUsed: TDateTime;
    FDBConnectionName: string;
    FQueryIsExecuting: boolean;
    FOwner: TtiStatefulDBConnectionPool;
    FUserName: string;
    FComputerName: string;
    FRollBackAtNextOpportunity: boolean;
    function GetSecToTimeOut: integer;
    function GetSecInUse: integer;
    procedure SetQueryIsExecuting(const AValue: boolean);
  public
    constructor Create;
    property Owner : TtiStatefulDBConnectionPool read FOwner write FOwner;
    property TransactionID : string read FTransactionID write FTransactionID;
    property DBConnection : TtiDatabase read FDBConnection write FDBConnection;
    property DBConnectionName : string read FDBConnectionName write FDBConnectionName;
    property ComputerName : string read FComputerName write FComputerName;
    property UserName : string read FUserName write FUserName;

    property LastUsed : TDateTime read FLastUsed;
    property QueryIsExecuting : boolean read FQueryIsExecuting write SetQueryIsExecuting;
    property SecToTimeOut : integer read GetSecToTimeOut;
    property SecInUse : integer read GetSecInUse;
    function MustRemoveItemFromPool: boolean;
    property RollBackAtNextOpportunity: boolean read FRollBackAtNextOpportunity write FRollBackAtNextOpportunity;

  end;

  TSavedDBConnectionHolderEvent = procedure(const AItem : TSavedDBConnectionHolder) of object;

  TPooledDBEvent = procedure (pPooledDB : TtiDatabase) of object;

  TtiStatefulDBConnectionPool = class(TtiBaseObject)
  private
    FSavedDBConnections : TObjectList;
    FNextTransID : Integer;
    FCritSect: TCriticalSection;
    FThrdStatefulDBConnectionPoolMonitor : TThrdStatefulDBConnectionPoolMonitor;
    FTimeOut : Extended;
    FSweepInterval: integer;
    function    GetNextTransID: string;
    procedure DoUnLockSavedDBConnection(const ASavedDBConnection: TSavedDBConnectionHolder);
    property    NextTransID : string read GetNextTransID;
    function    GetCount: integer;
    function    Lock(            const ADBConnectionName : string): TSavedDBConnectionHolder;

  public
    constructor Create(const ATimeOut: Extended);
    destructor  Destroy; override;
    property    Count : integer read GetCount;
    procedure   SweepForTimeOuts;
    property    TimeOut : Extended read FTimeOut;
    property    SweepInterval: integer read FSweepInterval;

    function    FindSavedDBConnectionHolder(const pTransactionID : string): TSavedDBConnectionHolder;
    function    FindSavedDBConnection(const pTransactionID : string): TtiDatabase;
    function    StartTransaction(const ADBConnectionName, pComputerName, AUserName : string): string;
    procedure   Commit(          const ATransID : string);
    procedure   RollBack(        const ATransID : string);

    procedure   GetSummaryStats(var pTotalStatefulDBConnections : integer;
                                 var pInUseStatefulDBConnections : integer;
                                 var pWaitingStatefulDBConnections : integer);
    procedure   ForEach(const AMethod : TSavedDBConnectionHolderEvent);

  end;

  TtiQueryRemoteExec = class(TtiBaseObject)
  private
    FDBRequest         : TtiDatabaseXMLLight;
    FErrorMessage      : string;

    FRemoteCommandType : TtiRemoteCommandType;
    FRemoteCommandText : string;
    FTransactionID     : string;
    FRemoteComputerName : string;
    FRemoteUserName    : string;
    FRowCount          : integer;
    FQueryParams       : TtiQueryTransParams;

    FXMLWriterData     : TtiDataBufferToXMLWriter;
    FXMLWriterMessage  : TtiDataBufferToXMLWriter;

    procedure ParseRequest;
    procedure ExecuteRequest;

    procedure DBRequestToQuery;
    procedure DBRequestToQueryParams;
    procedure GetRemoteCommandType;

    procedure DoStartTransaction;
    procedure DoCommit;
    procedure DoRollBack;
    procedure DoExecSQL;

    procedure DoCreateTable;
    procedure DoDropTable;
    procedure DoReadMetaDataTables;
    procedure DoReadMetaDataFields;

    procedure CreateResponseMetaDataTable;
    procedure InsertResponseMessageData;
    procedure LogTransaction(const AMessage: string; const AParams: array of const);

  public
    constructor create;
    destructor  Destroy; override;
    function    ExecuteRemoteXML(const pInput : string): string;
  protected
  end;

function ExecuteRemoteXML(const pInput : string): string;
function GStatefulDBConnectionPool(
  const ATimeOut: Extended = CDefaultStatefulDBConnectionPoolTimeOut) : TtiStatefulDBConnectionPool;
procedure FreeAndNilStatefulDBConnectionPool;

implementation
uses
  tiLog,
  tiXML,
  tiUtils,
  tiExcept,
  tiMadExcept,
  tiOPFManager,
  Variants,
  SysUtils;

var
  UStatefulDBConnectionPool : TtiStatefulDBConnectionPool;
  uXMLTags : TtiXMLTags;

function ExecuteRemoteXML(const pInput : string): string;
var
  lQueryRemoteExec : TtiQueryRemoteExec;
begin
  lQueryRemoteExec := TtiQueryRemoteExec.Create;
  try
    result := lQueryRemoteExec.ExecuteRemoteXML(pInput);
  finally
    lQueryRemoteExec.Free;
  end;
end;

function GStatefulDBConnectionPool(
  const ATimeOut: Extended = CDefaultStatefulDBConnectionPoolTimeOut) : TtiStatefulDBConnectionPool;
begin
  if UStatefulDBConnectionPool = nil then
    UStatefulDBConnectionPool := TtiStatefulDBConnectionPool.Create(ATimeOut);
  result := uStatefulDBConnectionPool;
end;

procedure FreeAndNilStatefulDBConnectionPool;
begin
  FreeAndNil(UStatefulDBConnectionPool);
end;

const
  CDefaultSweepInterval = 10; {Seconds}

{ TtiQueryRemoteExec }

constructor TtiQueryRemoteExec.create;
begin
  inherited;
  FDBRequest       := TtiDatabaseXMLLight.Create;
  FDBRequest.PersistToFile := false;
  FDBRequest.OptXMLDBSize := optDBSizeOn;
  FDBRequest.XMLFieldNameStyle := xfnsInteger;

  FRemoteCommandType := rctUnknown;
  FTransactionID    := cNullTransactionID;
  FRowCount         := 0;
  FXMLWriterMessage := TtiDataBufferToXMLWriter.Create;
  FXMLWriterMessage.OptXMLDBSize := optDBSizeOn;
  FXMLWriterMessage.XMLFieldNameStyle := xfnsInteger;

  FXMLWriterData    := TtiDataBufferToXMLWriter.Create;
  FXMLWriterData.OptXMLDBSize := optDBSizeOn;
  FXMLWriterData.XMLFieldNameStyle := xfnsInteger;

end;

destructor TtiQueryRemoteExec.destroy;
begin
  FDBRequest.Free;
  FQueryParams.Free;
  FXMLWriterMessage.Free;
  FXMLWriterData.Free;
  inherited;
end;

function TtiQueryRemoteExec.ExecuteRemoteXML(const pInput: string): string;
var
  lRequest : string;
  lResponse : string;
  LStart: DWord;
begin
  try
    try
      lRequest := tiDecompressDecode(pInput, cgsCompressZLib);
      FDBRequest.AsString := lRequest;
      ParseRequest;
      ExecuteRequest;
    except
      on e:exception do
        FErrorMessage := e.Message;
    end;
    LStart:= GetTickCount;
    InsertResponseMessageData;
    lResponse :=
      uXMLTags.DatabaseHeader +
      FXMLWriterMessage.TableData +
      FXMLWriterData.TableData +
      uXMLTags.DatabaseFooter;
    LogTransaction('Time to write XML: %dms', [GetTickCount-lStart]);
    LStart:= GetTickCount;
    lResponse := tiCompressEncode(lResponse, cgsCompressZLib);
    LogTransaction('Time to compress and encode result set: %dms', [GetTickCount-lStart]);
    result:= LResponse;
    Assert(Trim(result) <> '', 'Error in result string.');
  except
    on e:exception do
      Log('Error:'    + Cr + e.Message + Cr(2) +
           'Request:'  + Cr + lRequest  + Cr(2) +
           'Response:' + Cr + lResponse, lsError);
  end;
end;

procedure TtiQueryRemoteExec.ParseRequest;
begin
  GetRemoteCommandType;
  if FRemoteCommandType = rctExecSQL then
  begin
    FQueryParams     := TtiQueryTransParams.Create;
    DBRequestToQuery;
    DBRequestToQueryParams;
  end;
  if FDBRequest.InTransaction then
    FDBRequest.RollBack;
end;

procedure TtiQueryRemoteExec.ExecuteRequest;
begin
  GetRemoteCommandType;
  case FRemoteCommandType of
  rctStartTransaction  : DoStartTransaction;
  rctCommit            : DoCommit;
  rctRollBack          : DoRollBack;
  rctExecSQL           : DoExecSQL;
  rctCreateTable       : DoCreateTable;
  rctDropTable         : DoDropTable;
  rctReadMetaDataTables : DoReadMetaDataTables;
  rctReadMetaDataFields : DoReadMetaDataFields;
  else
    raise EtiOPFProgrammerException.Create('Invalid TtiRemoteCommandType');
  end;
end;

procedure TtiQueryRemoteExec.DBRequestToQuery;
var
  lQuery : TtiQueryXMLLight;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), CTIErrorInvalidObject);
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(FDBRequest);
    lQuery.SelectRow(uXMLTags.TableNameQuery);
    if lQuery.EOF then
      raise EtiOPFProgrammerException.Create('Invalid data query request');
    FQueryParams.SQL := FRemoteCommandText;
  finally
    lQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.DBRequestToQueryParams;
var
  LQuery: TtiQueryXMLLight;
  LParamIsNull: Boolean;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), CTIErrorInvalidObject);
  Assert(FQueryParams.TestValid(TtiQueryTransParams), CTIErrorInvalidObject);
  LQuery := TtiQueryXMLLight.Create;
  try
    LQuery.AttachDatabase(FDBRequest);
    LQuery.SelectRow(uXMLTags.TableNameQueryParam);
    while not LQuery.EOF do
    begin
      LParamIsNull :=
          (LQuery.FieldIndex(uXMLTags.FieldNameParamIsNull) <> -1) and
          tiStrToBool(LQuery.FieldAsString[uXMLTags.FieldNameParamIsNull]);
      FQueryParams.AddInstance(
        LQuery.FieldAsString[uXMLTags.FieldNameParamName],
        LQuery.FieldAsString[uXMLTags.FieldNameParamKind],
        LQuery.FieldAsString[uXMLTags.FieldNameParamValue],
        LParamIsNull);
      LQuery.Next;
    end;
  finally
    LQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.GetRemoteCommandType;
var
  lQuery : TtiQueryXMLLight;
  lCommandType : string;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), CTIErrorInvalidObject);
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(FDBRequest);
    lQuery.SelectRow(uXMLTags.TableNameQuery);
    if lQuery.EOF then
      raise EtiOPFProgrammerException.Create('Invalid data query request');
    lCommandType       := lQuery.FieldAsString[uXMLTags.FieldNameCommandType];
    FRemoteCommandType := StrToRemoteCommandType(lCommandType);
    FTransactionID     := lQuery.FieldAsString[uXMLTags.FieldNameTransactionID];
    FRemoteCommandText := lQuery.FieldAsString[uXMLTags.FieldNameQuerySQL];
    FRemoteComputerName := lQuery.FieldAsString[uXMLTags.FieldNameComputerName];
    FRemoteUserName    := lQuery.FieldAsString[uXMLTags.FieldNameUserName];
  finally
    lQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoCommit;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), CTIErrorInvalidObject);
  Log('Commit: TransactionID = %s', [FTransactionID], lsDebug);
  gStatefulDBConnectionPool.Commit(FTransactionID);
  FTransactionID := cNullTransactionID;
end;

procedure TtiQueryRemoteExec.DoRollBack;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), CTIErrorInvalidObject);
  Log('RollBack: TransactionID = %s', [FTransactionID], lsDebug);
  gStatefulDBConnectionPool.RollBack(FTransactionID);
  FTransactionID := cNullTransactionID;
end;

procedure TtiQueryRemoteExec.DoExecSQL;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
  lDatabase : TtiDatabase;
  lQuery : TtiQuery;
  lStart : DWord;
  lSQL : string;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), CTIErrorInvalidObject);
  Assert(FTransactionID <> '', 'TransactionID not assigned');
  Log('ExecSQL: TransactionID = %s', [FTransactionID], lsDebug);
  lQuery := GTIOPFManager.DefaultPerLayer.QueryClass.Create;
  try
    Log('ExecSQL: Getting DB connection', lsDebug);
    lSavedDBConnectionHolder := gStatefulDBConnectionPool.FindSavedDBConnectionHolder(FTransactionID);
    if LSavedDBConnectionHolder.RollBackAtNextOpportunity then
      raise Exception.Create(CErrorDatabaseConnectionBeingRolledBack);
    lSavedDBConnectionHolder.QueryIsExecuting := true;
    try
      Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), CTIErrorInvalidObject);
      Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TtiDatabase), CTIErrorInvalidObject);
      Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TtiDatabase), CTIErrorInvalidObject);
      lDatabase := lSavedDBConnectionHolder.DBConnection;
      lQuery.AttachDatabase(lDatabase);
      Log('ExecSQL: Assigning query params', lsDebug);
      FQueryParams.AssignToQuery(lQuery);
      lSQL:= lQuery.SQLText;
      lSQL:= tiStrTran(lSQL, '  ', ' ');
      lSQL:= tiStrTran(lSQL, #10, '');
      lSQL:= tiStrTran(lSQL, #13, ' ');
      lSQL:= tiAddEllipsis(lSQL, 975);
      LogTransaction('About to run SQL: %s', [lSQL]);
      lStart:= GetTickCount;
      lQuery.Active := true;
      LogTransaction('Time to execute query: %dms', [GetTickCount-lStart]);
      lStart:= GetTickCount;
      FRowCount := FXMLWriterData.AssignFromTIQuery(uXMLTags.TableNameResultSet, lQuery);
      LogTransaction('Time to fetch result set (%d rows): %dms', [FRowCount, GetTickCount-lStart]);
    finally
      lQuery.Active := false;
      lSavedDBConnectionHolder.QueryIsExecuting := false;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoStartTransaction;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), CTIErrorInvalidObject);
  Log('StartTransaction', lsDebug);
  FTransactionID :=
    gStatefulDBConnectionPool.StartTransaction(
      GTIOPFManager.DefaultDBConnectionName,
      FRemoteComputerName, FRemoteUserName);
  Log('StartTransaction: TransactionID = %s', [FTransactionID], lsDebug);
end;

procedure TtiQueryRemoteExec.InsertResponseMessageData;
begin
  Assert(FXMLWriterMessage.TestValid(TtiDataBufferToXMLWriter), CTIErrorInvalidObject);
  FXMLWriterMessage.AddTable(uXMLTags.TableNameResultMessage);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameResultError, qfkString, 9999);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameTransactionID, qfkString, 9999);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameResultRowCount, qfkInteger);
  FXMLWriterMessage.AddRow;
  FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameResultError, FErrorMessage);
  FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameTransactionID, FTransactionID);
  FXMLWriterMessage.AddCellAsInteger(uXMLTags.FieldNameResultRowCount, FRowCount);
end;

procedure TtiQueryRemoteExec.LogTransaction(const AMessage: string;
  const AParams: array of const);
begin
  if FTransactionID <> '' then
    Log('Trans ID: ' + FTransactionID + '. ' +
        AMessage, AParams, lsQueryTiming);
end;

procedure TtiQueryRemoteExec.DoCreateTable;
var
  lQuery : TtiQueryXMLLight;
  lMD : TtiDBMetaDataTable;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), CTIErrorInvalidObject);
  Log('CreateTable', lsDebug);
  lMD := TtiDBMetaDataTable.Create;
  try
    lQuery := TtiQueryXMLLight.Create;
    try
      lQuery.AttachDatabase(FDBRequest);
      lQuery.SelectRow(uXMLTags.TableNameMetaData);
      if lQuery.EOF then
        raise EtiOPFProgrammerException.Create('Unable to find table metadata');
      lMD.Name := lQuery.FieldAsString[uXMLTags.FieldNameMetaDataTableName];
      while not lQuery.EOF do
      begin
        lMD.AddInstance(
          lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldName],
          StrToQueryFieldKind(lQuery.FieldAsString[uXMLTags.FieldNameMetaDataFieldKind]),
          lQuery.FieldAsInteger[uXMLTags.FieldNameMetaDataFieldWIdth]);
        lQuery.Next;
      end;
    finally
      lQuery.Free;
    end;
    GTIOPFManager.CreateTable(lMD);
  finally
    lMD.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoDropTable;
var
  lMD : TtiDBMetaDataTable;
begin
  Log('DropTable', lsDebug);
  lMD := TtiDBMetaDataTable.Create;
  try
    GTIOPFManager.DropTable(FRemoteCommandText);
  finally
    lMD.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataFields;
var
  LDatabase : TtiDatabase;
  lMD      : TtiDBMetaDataTable;
  i        : integer;
begin
  Assert(FXMLWriterData.TestValid(TtiDataBufferToXMLWriter), CTIErrorInvalidObject);
  Log('ReadMetaDataFields', lsDebug);
  CreateResponseMetaDataTable;

  LDatabase := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock;
  try
    lMD := TtiDBMetaDataTable.Create;
    try
      lMD.Name := FRemoteCommandText;
      LDatabase.ReadMetaDataFields(lMD);
      for i := 0 to lMD.Count - 1 do
      begin
        FXMLWriterData.AddRow;
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataTableName, lMD.Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldName, lMD.Items[i].Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldKind, lMD.Items[i].KindAsStr);
        FXMLWriterData.AddCellAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, lMD.Items[i].Width);
      end;
    finally
      lMD.Free;
    end;
  finally
    GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataTables;
var
  LDatabase : TtiDatabase;
  lMD      : TtiDBMetaData;
  i        : integer;
begin
  Assert(FXMLWriterMessage.TestValid(TtiDataBufferToXMLWriter), CTIErrorInvalidObject);
  Log('ReadMetaDataTables', lsDebug);
  CreateResponseMetaDataTable;
  LDatabase := GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock;
  try
    lMD := TtiDBMetaData.Create;
    try
      LDatabase.ReadMetaDataTables(lMD);
      for i := 0 to lMD.Count - 1 do
      begin
        FXMLWriterData.AddRow;
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataTableName,   lMD.Items[i].Name);
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldName,   '');
        FXMLWriterData.AddCellAsString(uXMLTags.FieldNameMetaDataFieldKind,   cgaQueryFieldKind[qfkString]);
        FXMLWriterData.AddCellAsInteger(uXMLTags.FieldNameMetaDataFieldWidth, 0);
      end;
    finally
      lMD.Free;
    end;
  finally
    GTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TtiQueryRemoteExec.CreateResponseMetaDataTable;
begin
  FXMLWriterData.AddTable(uXMLTags.TableNameMetaData);
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataTableName,  qfkString, 255);
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldName,  qfkString, 255);
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldKind,  qfkString, 20);
  FXMLWriterData.AddField(uXMLTags.FieldNameMetaDataFieldWidth, qfkInteger);
end;

{ TtiStatefulDBConnectionPool }

constructor TtiStatefulDBConnectionPool.Create(
  const ATimeOut: Extended);
begin
  inherited Create;
  FSavedDBConnections := TObjectList.Create;
  FNextTransID := 1;
  FCritSect:= TCriticalSection.Create;
  FTimeOut := ATimeOut;
  FSweepInterval:= CDefaultSweepInterval;
  FThrdStatefulDBConnectionPoolMonitor := TThrdStatefulDBConnectionPoolMonitor.CreateExt(Self, FSweepInterval);
end;

destructor TtiStatefulDBConnectionPool.Destroy;
begin
  FThrdStatefulDBConnectionPoolMonitor.Terminate;
  FThrdStatefulDBConnectionPoolMonitor.WaitFor;
  FThrdStatefulDBConnectionPoolMonitor.Free;
  FSavedDBConnections.Free;
  FCritSect.Free;
  inherited;
end;

function TtiStatefulDBConnectionPool.Lock(const ADBConnectionName : string): TSavedDBConnectionHolder;
begin
  FCritSect.Enter;
  try
    result := TSavedDBConnectionHolder.Create;
    result.DBConnectionName := ADBConnectionName;
    result.TransactionID   := NextTransID;
    result.DBConnection := GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(ADBConnectionName);
    result.Owner := Self;
    Assert(result.DBConnection.TestValid(TtiDatabase), CTIErrorInvalidObject);
    FSavedDBConnections.Add(result);
  finally
    FCritSect.Leave;
  end;
end;

function TtiStatefulDBConnectionPool.FindSavedDBConnectionHolder(
            const pTransactionID: string): TSavedDBConnectionHolder;
var
  i : integer;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  result := nil;
  for i := 0 to FSavedDBConnections.Count - 1 do
  begin
    lSavedDBConnectionHolder := TSavedDBConnectionHolder(FSavedDBConnections.Items[i]);
    Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), CTIErrorInvalidObject);
    if (lSavedDBConnectionHolder.TransactionID = pTransactionID) then
    begin
      result := lSavedDBConnectionHolder;
      Break; //==>
    end;
  end;
  if result = nil then
    raise exception.CreateFmt(CErrorTransactionTimedOut, [pTransactionID]);
end;

function TtiStatefulDBConnectionPool.GetNextTransID: string;
begin
  result := IntToStr(FNextTransID);
  Inc(FNextTransID);
end;

procedure TtiStatefulDBConnectionPool.DoUnLockSavedDBConnection(
  const ASavedDBConnection: TSavedDBConnectionHolder);
begin
  GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(
  ASavedDBConnection.DBConnectionName,
  ASavedDBConnection.DBConnection);
end;

function TtiStatefulDBConnectionPool.GetCount: integer;
begin
  result := FSavedDBConnections.Count;
end;

function TtiStatefulDBConnectionPool.StartTransaction(const ADBConnectionName, pComputerName, AUserName : string): string;
var
  LSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  LSavedDBConnectionHolder := Lock(ADBConnectionName);
  Assert(LSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), CTIErrorInvalidObject);
  Assert(LSavedDBConnectionHolder.DBConnection.TestValid(TtiDatabase), CTIErrorInvalidObject);
  if LSavedDBConnectionHolder.RollBackAtNextOpportunity then
    raise Exception.Create(CErrorDatabaseConnectionBeingRolledBack);
  LSavedDBConnectionHolder.ComputerName := pComputerName;
  LSavedDBConnectionHolder.UserName := AUserName;
  result := LSavedDBConnectionHolder.TransactionID;
  LSavedDBConnectionHolder.DBConnection.StartTransaction;
end;

procedure TtiStatefulDBConnectionPool.Commit(const ATransID: string);
var
  LSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  FCritSect.Enter;
  try
    LSavedDBConnectionHolder := FindSavedDBConnectionHolder(ATransID);
    if LSavedDBConnectionHolder.RollBackAtNextOpportunity then
      raise Exception.Create(CErrorDatabaseConnectionBeingRolledBack);
    FSavedDBConnections.Extract(LSavedDBConnectionHolder);
  finally
    FCritSect.Leave;
  end;
  try
    LSavedDBConnectionHolder.DBConnection.Commit;
  finally
    DoUnLockSavedDBConnection(LSavedDBConnectionHolder);
    LSavedDBConnectionHolder.Free;
  end;
end;

procedure TtiStatefulDBConnectionPool.RollBack(const ATransID: string);
var
  LSavedDBConnection : TSavedDBConnectionHolder;
  LRollBack: boolean;
begin
  FCritSect.Enter;
  try
    LSavedDBConnection := FindSavedDBConnectionHolder(ATransID);
    LRollBack:= not LSavedDBConnection.QueryIsExecuting;
    if LRollBack then
      FSavedDBConnections.Extract(LSavedDBConnection)
    else
      LSavedDBConnection.RollBackAtNextOpportunity:= true;
  finally
    FCritSect.Leave;
  end;
  if LRollBack then
  begin
    try
      LSavedDBConnection.DBConnection.RollBack;
    finally
      DoUnLockSavedDBConnection(LSavedDBConnection);
      LSavedDBConnection.Free;
    end;
  end;
end;

function TtiStatefulDBConnectionPool.FindSavedDBConnection(const pTransactionID: string): TtiDatabase;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  lSavedDBConnectionHolder := FindSavedDBConnectionHolder(pTransactionID);
  if lSavedDBConnectionHolder <> nil then
  begin
    Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), CTIErrorInvalidObject);
    Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TtiDatabase), CTIErrorInvalidObject);
    result := lSavedDBConnectionHolder.DBConnection;
  end else
    result := nil;
end;

constructor TThrdStatefulDBConnectionPoolMonitor.CreateExt(
  const APool : TtiStatefulDBConnectionPool;
  const ASweepInterval: integer);
begin
  Create(false);
  Priority := tpLowest;
  FreeOnTerminate := false;
  FPool := APool;
  FSweepInterval:= ASweepInterval;
end;

procedure TThrdStatefulDBConnectionPoolMonitor.Execute;
begin
  while SleepAndCheckTerminated(FSweepInterval * 1000) do
    FPool.SweepForTimeOuts;
end;

procedure TtiStatefulDBConnectionPool.SweepForTimeOuts;
  procedure _ScanForTimeOuts(const ATargetList: TObjectList);
  var
    i: integer;
    LItem: TSavedDBConnectionHolder;
  begin
    FCritSect.Enter;
    try
      for i :=  Count - 1 downto 0 do
      begin
        LItem := FSavedDBConnections.Items[i] as TSavedDBConnectionHolder;
        if LItem.MustRemoveItemFromPool then
        begin
          FSavedDBConnections.Extract(LItem);
          ATargetList.Add(LItem);
          Log('TransID: ' + LItem.TransactionID + '. Added to sweep list');
        end;
      end;
    finally
      FCritSect.Leave;
    end;
  end;

  procedure _SweepList(const AList: TObjectList);
  var
    i: integer;
    LItem: TSavedDBConnectionHolder;
  begin
    for i := 0 to AList.Count-1 do
    begin
      LItem:= AList.Items[i] as TSavedDBConnectionHolder;
      DoUnLockSavedDBConnection(LItem);
    end;
  end;
var
  FSweepList: TObjectList;
begin
  if Count = 0 then
    Exit; //==>
  FSweepList:= TObjectList.Create;
  try
    _ScanForTimeOuts(FSweepList);
    _SweepList(FSweepList);
  finally
    FSweepList.Free;
  end;
end;

procedure TtiStatefulDBConnectionPool.GetSummaryStats(
  var pTotalStatefulDBConnections, pInUseStatefulDBConnections,
  pWaitingStatefulDBConnections: integer);
var
  i : integer;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  pTotalStatefulDBConnections  := 0;
  pInUseStatefulDBConnections  := 0;
  pWaitingStatefulDBConnections := 0;
  FCritSect.Enter;
  try
    for i := 0 to FSavedDBConnections.Count - 1 do
    begin
      lSavedDBConnectionHolder := TSavedDBConnectionHolder(FSavedDBConnections.Items[i]);
      if lSavedDBConnectionHolder.QueryIsExecuting then
        Inc(pInUseStatefulDBConnections)
      else
        Inc(pWaitingStatefulDBConnections);
      Inc(pTotalStatefulDBConnections);
    end;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiStatefulDBConnectionPool.ForEach(const AMethod: TSavedDBConnectionHolderEvent);
var
  lSavedDBConnection : TSavedDBConnectionHolder;
  i : integer;
begin
  FCritSect.Enter;
  try
    for i := 0 to FSavedDBConnections.Count - 1 do
    begin
      lSavedDBConnection := TSavedDBConnectionHolder(FSavedDBConnections.Items[i]);
      AMethod(lSavedDBConnection);
    end;
  finally
    FCritSect.Leave;
  end;
end;

{ TSavedDBConnectionHolder }

constructor TSavedDBConnectionHolder.Create;
begin
  inherited;
  FQueryIsExecuting := false;
  FLastUsed      := now;
  FRollBackAtNextOpportunity:= false;
end;

function TSavedDBConnectionHolder.GetSecInUse: integer;
begin
  result := Trunc((Now - LastUsed) * 24 * 60 * 60 );
end;

function TSavedDBConnectionHolder.GetSecToTimeOut: integer;
begin
  if FQueryIsExecuting then
    result := cSecToTimeOutLocked
  else
    result := Trunc(Owner.TimeOut * 60) - SecInUse;
end;

function TSavedDBConnectionHolder.MustRemoveItemFromPool: boolean;
var
  LNotInUse:   Boolean;
  LTimeOut:     Boolean;
begin
  LNotInUse  := not QueryIsExecuting;
  LTimeOut    := (SecToTimeOut <= 0);
  result      := LNotInUse and (LTimeOut or RollBackAtNextOpportunity);
end;

procedure TSavedDBConnectionHolder.SetQueryIsExecuting(const AValue: boolean);
begin
  FQueryIsExecuting := AValue;
  FLastUsed := Now;
end;

initialization
  uXMLTags := TtiXMLTags.Create;
  uXMLTags.OptXMLDBSize := optDBSizeOn;

finalization
  FreeAndNilStatefulDBConnectionPool;
  uXMLTags.Free;

end.

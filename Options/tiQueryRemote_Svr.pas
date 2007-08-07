unit tiQueryRemote_Svr;

{$I tiDefines.inc}

interface
uses
  tiBaseObject
  ,tiQueryRemote
  ,tiQuery
  ,tiDBConnectionPool
  ,Contnrs
  ,tiQueryXMLLight
  ,Classes
  ,Windows
  ,tiXMLToTIDataSet
  ,tiStreams
  ,SyncObjs
 ;

const
  cErrorTransactionTimedOut = 'Transaction ID#%s timed out';

type

  TThrdStatefulDBConnectionPoolMonitor = class;
  TSavedDBConnectionHolder = class;
  TtiStatefulDBConnectionPool = class;

  TThrdStatefulDBConnectionPoolMonitor = class(TThread)
  private
    FPool : TtiStatefulDBConnectionPool;
  public
    constructor CreateExt(APool : TtiStatefulDBConnectionPool);
    procedure   Execute; override;
  end;

  TSavedDBConnectionHolder = class(TtiBaseObject)
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
    procedure SetInUse(const AValue: boolean);
  public
    constructor Create;
    property Owner : TtiStatefulDBConnectionPool read FOwner write FOwner;
    property TransactionID : string read FTransactionID write FTransactionID;
    property DBConnection : TPooledDB read FDBConnection write FDBConnection;
    property DBConnectionName : string read FDBConnectionName write FDBConnectionName;
    property LastUsed : TDateTime read FLastUsed write FLastUsed;
    property InUse : boolean read FInUse write SetInUse;
    property SecToTimeOut : integer read GetSecToTimeOut;
    property SecInUse : integer read GetSecInUse;
    property ComputerName : string read FComputerName write FComputerName;
    property UserName : string read FUserName write FUserName;
  end;

  TSavedDBConnectionHolderEvent = procedure(const AItem : TSavedDBConnectionHolder) of object;

  TtiStatefulDBConnectionPool = class(TtiBaseObject)
  private
    FSavedDBConnections : TObjectList;
    FNextTransID : Integer;
    FCritSect: TCriticalSection;
    FThrdStatefulDBConnectionPoolMonitor : TThrdStatefulDBConnectionPoolMonitor;
    FTimeOut : Extended;
    function    GetNextTransID: string;
    property    NextTransID : string read GetNextTransID;
    procedure   DoCommit(pDBConnection : TPooledDB);
    procedure   DoRollBack(pDBConnection : TPooledDB);
    function    GetCount: integer;
    procedure   UnLock(          const pTransactionID : string; AMethod : TPooledDBEvent);
    function    Lock(            const ADBConnectionName : string): TSavedDBConnectionHolder;

  public
    constructor Create;
    destructor  Destroy; override;
    property    Count : integer read GetCount;
    procedure   SweepForTimeOuts;
    property    TimeOut : Extended read FTimeOut write FTimeOut;

    function    FindSavedDBConnectionHolder(const pTransactionID : string): TSavedDBConnectionHolder;
    function    FindSavedDBConnection(const pTransactionID : string): TtiDatabase;
    function    StartTransaction(const ADBConnectionName, pComputerName, AUserName : string): string;
    procedure   Commit(          const pTransactionID : string);
    procedure   RollBack(        const pTransactionID : string);

    procedure   GetSummaryStats(var pTotalStatefulDBConnections : integer;
                                 var pInUseStatefulDBConnections : integer;
                                 var pWaitingStatefulDBConnections : integer);
    procedure   ForEach(const AMethod : TSavedDBConnectionHolderEvent);

  end;

  TtiQueryRemoteExec = class(TtiBaseObject)
  private
    FDBRequest         : TtiDatabaseXMLLight;
    //FDBResponse        : TtiDatabaseXMLLight;
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

  public
    constructor create;
    destructor  Destroy; override;
    function    ExecuteRemoteXML(const pInput : string): string;
  protected
  end;

function ExecuteRemoteXML(const pInput : string): string;
function gStatefulDBConnectionPool : TtiStatefulDBConnectionPool;

implementation
uses
   tiUtils
  ,tiGUIUtils
  ,tiOPFManager
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiLog
  ,tiConstants
  ,tiDialogs
  ,tiXML
  ,tiExcept
  ,SysUtils
 ;

var
  uStatefulDBConnectionPool : TtiStatefulDBConnectionPool;
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

function gStatefulDBConnectionPool : TtiStatefulDBConnectionPool;
begin
  if uStatefulDBConnectionPool = nil then
    uStatefulDBConnectionPool := TtiStatefulDBConnectionPool.Create;
  result := uStatefulDBConnectionPool;
end;

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
    InsertResponseMessageData;
    lResponse :=
      uXMLTags.DatabaseHeader +
      FXMLWriterMessage.TableData +
      FXMLWriterData.TableData +
      uXMLTags.DatabaseFooter;
    lResponse := tiCompressEncode(lResponse, cgsCompressZLib);
    result := lResponse;
    Assert(Trim(result) <> '', 'Error in result string.');
  except
    on e:exception do
      Log('Error:'    + Cr + e.Message + Cr(2) +
           'Request:'  + Cr + lRequest  + Cr(2) +
           'Response:' + Cr + lResponse);
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
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError);
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
  lQuery : TtiQueryXMLLight;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError);
  Assert(FQueryParams.TestValid(TtiQueryTransParams), cTIInvalidObjectError);
  lQuery := TtiQueryXMLLight.Create;
  try
    lQuery.AttachDatabase(FDBRequest);
    lQuery.SelectRow(uXMLTags.TableNameQueryParam);
    while not lQuery.EOF do
    begin
      FQueryParams.AddInstance(
        lQuery.FieldAsString[uXMLTags.FieldNameParamName],
        lQuery.FieldAsString[uXMLTags.FieldNameParamKind],
        lQuery.FieldAsString[uXMLTags.FieldNameParamValue]);
      lQuery.Next;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.GetRemoteCommandType;
var
  lQuery : TtiQueryXMLLight;
  lCommandType : string;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError);
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
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError);
  gStatefulDBConnectionPool.Commit(FTransactionID);
  FTransactionID := cNullTransactionID;
end;

procedure TtiQueryRemoteExec.DoRollBack;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError);
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
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError);
  Assert(FTransactionID <> '', 'TransactionID not assigned');
  lQuery := gTIOPFManager.DefaultPerLayer.tiQueryClass.Create;
  try
    lSavedDBConnectionHolder := gStatefulDBConnectionPool.FindSavedDBConnectionHolder(FTransactionID);
    lSavedDBConnectionHolder.InUse := true;
    try
      Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
      Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
      Assert(lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
      lDatabase := lSavedDBConnectionHolder.DBConnection.Database;
      lQuery.AttachDatabase(lDatabase);
      FQueryParams.AssignToQuery(lQuery);
      lSQL:= lQuery.SQLText;
      lSQL:= tiStrTran(lSQL, '  ', ' ');
      lSQL:= tiStrTran(lSQL, #10, '');
      lSQL:= tiStrTran(lSQL, #13, ' ');
      lSQL:= tiAddEllipsis(lSQL, 975);
      Log('About to run SQL: ' + lSQL);
      lStart:= GetTickCount;
      lQuery.Active := true;
      FRowCount := FXMLWriterData.AssignFromTIQuery(uXMLTags.TableNameResultSet, lQuery);
      Log('  Rows returned: ' + IntToStr(FRowCount) + ' taking ' + IntToStr(GetTickCount-lStart) + 'ms');
    finally
      lSavedDBConnectionHolder.InUse := false;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoStartTransaction;
begin
  Assert(gStatefulDBConnectionPool.TestValid(TtiStatefulDBConnectionPool), cTIInvalidObjectError);
  FTransactionID :=
    gStatefulDBConnectionPool.StartTransaction(
      gTIOPFManager.DefaultDBConnectionName,
      FRemoteComputerName, FRemoteUserName);
end;

procedure TtiQueryRemoteExec.InsertResponseMessageData;
begin
  Assert(FXMLWriterMessage.TestValid(TtiDataBufferToXMLWriter), cTIInvalidObjectError);
  FXMLWriterMessage.AddTable(uXMLTags.TableNameResultMessage);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameResultError, qfkString, 9999);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameTransactionID, qfkString, 9999);
  FXMLWriterMessage.AddField(uXMLTags.FieldNameResultRowCount, qfkInteger);
  FXMLWriterMessage.AddRow;
  FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameResultError, FErrorMessage);
  FXMLWriterMessage.AddCellAsString(uXMLTags.FieldNameTransactionID, FTransactionID);
  FXMLWriterMessage.AddCellAsInteger(uXMLTags.FieldNameResultRowCount, FRowCount);
end;

procedure TtiQueryRemoteExec.DoCreateTable;
var
  lQuery : TtiQueryXMLLight;
  lMD : TtiDBMetaDataTable;
begin
  Assert(FDBRequest.TestValid(TtiDatabaseXMLLight), cTIInvalidObjectError);
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
    gTIOPFManager.CreateTable(lMD);
  finally
    lMD.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoDropTable;
var
  lMD : TtiDBMetaDataTable;
begin
  lMD := TtiDBMetaDataTable.Create;
  try
    gTIOPFManager.DropTable(FRemoteCommandText);
  finally
    lMD.Free;
  end;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataFields;
var
  lPooledDB : TPooledDB;
  lDatabase : TtiDatabase;
  lMD      : TtiDBMetaDataTable;
  i        : integer;
begin
  Assert(FXMLWriterData.TestValid(TtiDataBufferToXMLWriter), cTIInvalidObjectError);
  CreateResponseMetaDataTable;

  lPooledDB := gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock;
  lDatabase := lPooledDB.Database;
  try
    lMD := TtiDBMetaDataTable.Create;
    try
      lMD.Name := FRemoteCommandText;
      lDatabase.ReadMetaDataFields(lMD);
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
    gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(lPooledDB);
  end;
end;

procedure TtiQueryRemoteExec.DoReadMetaDataTables;
var
  lPooledDB : TPooledDB;
  lDatabase : TtiDatabase;
  lMD      : TtiDBMetaData;
  i        : integer;
begin
  Assert(FXMLWriterMessage.TestValid(TtiDataBufferToXMLWriter), cTIInvalidObjectError);
  CreateResponseMetaDataTable;
  lPooledDB := gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.Lock;
  lDatabase := lPooledDB.Database;
  try
    lMD := TtiDBMetaData.Create;
    try
      lDatabase.ReadMetaDataTables(lMD);
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
    gTIOPFManager.DefaultPerLayer.DefaultDBConnectionPool.UnLock(lPooledDB);
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

constructor TtiStatefulDBConnectionPool.Create;
begin
  inherited;
  FSavedDBConnections := TObjectList.Create;
  FNextTransID := 1;
  FCritSect:= TCriticalSection.Create;
  FTimeOut := cDBProxyServerTimeOut;
  FThrdStatefulDBConnectionPoolMonitor := TThrdStatefulDBConnectionPoolMonitor.CreateExt(Self);
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
    result.DBConnection := gTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(ADBConnectionName);
    result.Owner := Self;
    Assert(result.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
    FSavedDBConnections.Add(result);
    result.LastUsed      := now;
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
    Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
    if (lSavedDBConnectionHolder.TransactionID = pTransactionID) then
    begin
      lSavedDBConnectionHolder.LastUsed := now;
      result := lSavedDBConnectionHolder;
      Break; //==>
    end;
  end;
  if result = nil then
    raise exception.CreateFmt(cErrorTransactionTimedOut, [pTransactionID]);
end;

function TtiStatefulDBConnectionPool.GetNextTransID: string;
begin
  result := IntToStr(FNextTransID);
  Inc(FNextTransID);
end;

procedure TtiStatefulDBConnectionPool.DoCommit(pDBConnection: TPooledDB);
begin
  Assert(pDBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
  Assert(pDBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  pDBConnection.Database.Commit;
end;

procedure TtiStatefulDBConnectionPool.DoRollBack(pDBConnection: TPooledDB);
begin
  Assert(pDBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
  Assert(pDBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  pDBConnection.Database.RollBack;
end;

procedure TtiStatefulDBConnectionPool.UnLock(const pTransactionID : string;
                                             AMethod: TPooledDBEvent);
var
  lSavedDBConnection : TSavedDBConnectionHolder;
  i : integer;
begin
  FCritSect.Enter;
  try
    lSavedDBConnection := FindSavedDBConnectionHolder(pTransactionID);
    Assert(lSavedDBConnection.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
    Assert(lSavedDBConnection.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
    AMethod(lSavedDBConnection.DBConnection);
    gTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lSavedDBConnection.DBConnectionName,
                                 lSavedDBConnection.DBConnection);
    i := FSavedDBConnections.IndexOf(lSavedDBConnection);
    FSavedDBConnections.Delete(i);
  finally
    FCritSect.Leave;
  end;
end;

function TtiStatefulDBConnectionPool.GetCount: integer;
begin
  result := FSavedDBConnections.Count;
end;

function TtiStatefulDBConnectionPool.StartTransaction(const ADBConnectionName, pComputerName, AUserName : string): string;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  lSavedDBConnectionHolder := Lock(ADBConnectionName);
  Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
  Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
  Assert(lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  lSavedDBConnectionHolder.ComputerName := pComputerName;
  lSavedDBConnectionHolder.UserName := AUserName;
  result := lSavedDBConnectionHolder.TransactionID;
  lSavedDBConnectionHolder.DBConnection.Database.StartTransaction;
end;

procedure TtiStatefulDBConnectionPool.Commit(const pTransactionID: string);
begin
  UnLock(pTransactionID, DoCommit);
end;

procedure TtiStatefulDBConnectionPool.RollBack(const pTransactionID: string);
begin
  UnLock(pTransactionID, DoRollBack);
end;

function TtiStatefulDBConnectionPool.FindSavedDBConnection(const pTransactionID: string): TtiDatabase;
var
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  lSavedDBConnectionHolder := FindSavedDBConnectionHolder(pTransactionID);
  if lSavedDBConnectionHolder <> nil then
  begin
    Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
    Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
    Assert(lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
    result := lSavedDBConnectionHolder.DBConnection.Database;
  end else
    result := nil;
end;

constructor TThrdStatefulDBConnectionPoolMonitor.CreateExt(APool: TtiStatefulDBConnectionPool);
begin
  Create(true);
  FreeOnTerminate := false;
  FPool := APool;
  Resume;
end;

procedure TThrdStatefulDBConnectionPoolMonitor.Execute;
var
  i : integer;
begin
  while not terminated do
  begin
    // Sleep for 10 seconds, but keep checking terminated
    i := 0;
    while (i < 10) and
          (not terminated) do
    begin
      Inc(i);
      sleep(1000);
    end;
    if not Terminated then
      FPool.SweepForTimeOuts;
  end;

end;

procedure TtiStatefulDBConnectionPool.SweepForTimeOuts;
var
  i : integer;
  lSavedDBConnectionHolder : TSavedDBConnectionHolder;
begin
  FCritSect.Enter;
  try
    if Count = 0 then
      Exit; //==>
    // TimeOut is in minutes, so convert to TDateTime
    for i :=  Count - 1 downto 0 do
    begin
      lSavedDBConnectionHolder := FSavedDBConnections.Items[i] as TSavedDBConnectionHolder;
      Assert(lSavedDBConnectionHolder.TestValid(TSavedDBConnectionHolder), cTIInvalidObjectError);
      Assert(lSavedDBConnectionHolder.DBConnection.TestValid(TPooledDB), cTIInvalidObjectError);
      Assert(lSavedDBConnectionHolder.DBConnection.Database.TestValid(TtiDatabase), cTIInvalidObjectError);
      if (not lSavedDBConnectionHolder.InUse) and
         (lSavedDBConnectionHolder.SecToTimeOut <= 0) then
      begin
        if lSavedDBConnectionHolder.DBConnection.Database.InTransaction then
            lSavedDBConnectionHolder.DBConnection.Database.Rollback;
        gTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(lSavedDBConnectionHolder.DBConnectionName,
                                     lSavedDBConnectionHolder.DBConnection);
        FSavedDBConnections.Delete(i);
      end;
    end;
  finally
    FCritSect.Leave;
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
      if lSavedDBConnectionHolder.InUse then
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
  FInUse := false;
end;

function TSavedDBConnectionHolder.GetSecInUse: integer;
begin
  result := Trunc((Now - LastUsed) * 24 * 60 * 60 );
end;

function TSavedDBConnectionHolder.GetSecToTimeOut: integer;
begin
  if FInUse then
    result := cSecToTimeOutLocked
  else
    result := Trunc(Owner.TimeOut * 60) - SecInUse;
end;

procedure TSavedDBConnectionHolder.SetInUse(const AValue: boolean);
begin
  FInUse := AValue;
  LastUsed := Now;
end;

initialization
  uXMLTags := TtiXMLTags.Create;
  uXMLTags.OptXMLDBSize := optDBSizeOn;

finalization
  uStatefulDBConnectionPool.Free;
  uXMLTags.Free;

end.

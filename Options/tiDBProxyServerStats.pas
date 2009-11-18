unit tiDBProxyServerStats;

interface
uses
  tiBaseObject
  ,tiQueryXMLLight
  ,tiPool
  ,tiQueryRemote_Svr
  ,tiQuery
 ;

type

  TtiDBProxyServerStats = class(TtiBaseObject)
  private
    FDatabase : TtiDatabaseXMLLight;
    FTestRefreshRate: integer;
    procedure InsertSummary;
    procedure InsertDBConnectionPool;
    procedure InsertStatefulDBConPool;
    procedure ForEachDBConnectionPoolItem(const APooledItem: TtiPooledItem);
    procedure ForEachStatefulDBConnectionItem(const AItem: TSavedDBConnectionHolder);
    procedure SetTestRefreshRate(const AValue: integer);
    function  GetStatsPageTitle : string;
    function  GetApplicationUpTimeAsString: string;
  public
    constructor create;
    destructor  Destroy; override;
    procedure   Execute;
    function    AsXML : string;
    function    AsHTML : string;
    property    TestRefreshRate : integer read FTestRefreshRate write SetTestRefreshRate;
  end;

implementation
uses
   SysUtils
  ,tiDataBuffer_Cli
  ,tiOPFManager
  ,tiUtils
  ,tiDBConnectionPool
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ,tiConstants
 ;


const
  cTableNameSummary                 = 'summary';
  cFieldSummaryDatabaseName         = 'databasename';
  cFieldSummaryTimeOnServer         = 'timeonserver';
  cFieldSummaryApplicationStartTime = 'applicationstarttime';
  cFieldSummaryApplicationUpTime    = 'applicationuptime';
  cFieldSummaryPersistenceLayerName = 'persistencelayername';
  cFieldSummaryUserName             = 'username';
  cFieldSummaryPassword             = 'password';

  cFieldSummaryTotalDBConnections   = 'totaldbconnections';
  cFieldSummaryLockedDBConnections  = 'lockeddbconnections';
  cFieldSummaryAvailableDBConnections = 'availabledbconnections';
  cFieldSummaryDBConnectionTimeOut    = 'dbconnectiontimeout';

  cFieldSummaryTotalStatefulDBConnections   = 'totalstatefuldbconnections';
  cFieldSummaryInUseStatefulDBConnections   = 'inusestatefuldbConnections';
  cFieldSummaryWaitingStatefulDBConnections = 'waitingstatefuldbconnections';
  cFieldSummaryStatefulDBConnectionTimeOut  = 'statefuldbconnectionTimeOut';

  cTableNameDBConnectionPool       = 'dbconnectionpool';
  cFieldDBConPoolID                = 'id';
  cFieldDBConPoolLocked            = 'locked';
  cFieldDBConPoolError             = 'error';
  cFieldDBConPoolSecInUse          = 'secinuse';
  cFieldDBConPoolSecToTimeOut      = 'sectotimeout';

  cTableNameStateFulDBConPool         = 'statefuldbconpool';
  cFieldStateFulDBConPoolComputerName = 'computername';
  cFieldStateFulDBConPoolUserName     = 'username';
  cFieldStateFulDBConPoolTransID      = 'transactionid';
  cFieldStateFulDBConPoollInUse       = 'inuse';
  cFieldStateFulDBConPoolSecInUse     = 'secinuse';
  cFieldStateFulDBConPoolSecToTimeOut = 'sectotimeout';

  cScriptAutoRefresh =
    '<script>' + #13 +
    '<!--' + #13 +
    // enter refresh time in "minutes:seconds" Minutes should range from 0 to inifinity. Seconds should range from 0 to 59
    'var limit="0:%s"' + #13 +
    '' + #13 +
    'if (document.images){' + #13 +
    'var parselimit=limit.split(":")' + #13 +
    'parselimit=parselimit[0]*60+parselimit[1]*1' + #13 +
    '}' + #13 +
    'function beginrefresh(){' + #13 +
    'if (!document.images)' + #13 +
    'return' + #13 +
    'if (parselimit==1)' + #13 +
    'window.location.reload()' + #13 +
    'else{' + #13 +
    'parselimit-=1' + #13 +
    'curmin=Math.floor(parselimit/60)' + #13 +
    'cursec=parselimit%%60' + #13 +
    'if (curmin!=0)' + #13 +
    'curtime=curmin+" minutes and "+cursec+" seconds left until page refresh!"' + #13 +
    'else' + #13 +
    'curtime=cursec+" seconds left until page refresh!"' + #13 +
    'window.status=curtime' + #13 +
    'setTimeout("beginrefresh()",1000)' + #13 +
    '}' + #13 +
    '}' + #13 +
    '' + #13 +
    'window.onload=beginrefresh' + #13 +
    '//-->' + #13 +
    '</script>';

{ TtiDBProxyServerStats }

function TtiDBProxyServerStats.AsXML: string;
begin
  result := FDatabase.AsString;
end;

constructor TtiDBProxyServerStats.create;
var
  lMDT : TtiDBMetaDataTable;
begin
  inherited;
  FTestRefreshRate := 10;

  FDatabase := TtiDatabaseXMLLight.Create;
  FDatabase.PersistToFile := false;

  lMDT := TtiDBMetaDataTable.Create;
  try
    lMDT.Name := cTableNameSummary;
    lMDT.AddInstance(cFieldSummaryTimeOnServer,qfkDateTIme,0);
    lMDT.AddInstance(cFieldSummaryApplicationStartTime,qfkDateTIme,0);
    lMDT.AddInstance(cFieldSummaryApplicationUpTime,qfkString,0);
    lMDT.AddInstance(cFieldSummaryPersistenceLayerName,qfkString,0);
    lMDT.AddInstance(cFieldSummaryDatabaseName,qfkString,0);
    lMDT.AddInstance(cFieldSummaryUserName,qfkString,0);
    lMDT.AddInstance(cFieldSummaryPassword,qfkString,0);
    lMDT.AddInstance(cFieldSummaryTotalDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryLockedDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryAvailableDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryDBConnectionTimeOut,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryTotalStatefulDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryInUseStatefulDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryWaitingStatefulDBConnections,qfkInteger,0);
    lMDT.AddInstance(cFieldSummaryStatefulDBConnectionTimeOut,qfkInteger,0);
    FDatabase.CreateTable(lMDT);
  finally
    lMDT.Free;
  end;

  lMDT := TtiDBMetaDataTable.Create;
  try
    lMDT.Name := cTableNameDBConnectionPool;
    lMDT.AddInstance(cFieldDBConPoolID,qfkInteger,0);
    lMDT.AddInstance(cFieldDBConPoolLocked,qfkLogical,0);
    lMDT.AddInstance(cFieldDBConPoolError,qfkLogical,0);
    lMDT.AddInstance(cFieldDBConPoolSecInUse,qfkInteger,0);
    lMDT.AddInstance(cFieldDBConPoolSecToTimeOut,qfkInteger,0);
    FDatabase.CreateTable(lMDT);
  finally
    lMDT.Free;
  end;

  lMDT := TtiDBMetaDataTable.Create;
  try
    lMDT.Name := cTableNameStateFulDBConPool;
    lMDT.AddInstance(cFieldStateFulDBConPoolComputerName, qfkString,0);
    lMDT.AddInstance(cFieldStateFulDBConPoolUserName,     qfkString,0);
    lMDT.AddInstance(cFieldStateFulDBConPoolTransID,      qfkString,0);
    lMDT.AddInstance(cFieldStateFulDBConPoollInUse,       qfkLogical,0);
    lMDT.AddInstance(cFieldStateFulDBConPoolSecInUse,     qfkInteger,0);
    lMDT.AddInstance(cFieldStateFulDBConPoolSecToTimeOut, qfkInteger,0);
    FDatabase.CreateTable(lMDT);
  finally
    lMDT.Free;
  end;

end;

destructor TtiDBProxyServerStats.destroy;
begin
  FDatabase.Free;
  inherited;
end;

procedure TtiDBProxyServerStats.Execute;
begin
  FDatabase.DeleteRow(cTableNameSummary, nil);
  InsertSummary;
  FDatabase.DeleteRow(cTableNameDBConnectionPool, nil);
  InsertDBConnectionPool;
  FDatabase.DeleteRow(cTableNameStateFulDBConPool, nil);
  InsertStatefulDBConPool;
end;

procedure TtiDBProxyServerStats.InsertSummary;
var
  lTotalDBConnections            : integer;
  lLockedDBConnections           : integer;
  lAvailableDBConnections        : integer;
  lTotalStatefulDBConnections    : integer;
  lInUseStatefulDBConnections   : integer;
  lWaitingStatefulDBConnections : integer;
  FParams : TtiQueryParams;
begin

  lTotalDBConnections    := GTIOPFManager.DefaultDBConnectionPool.Count;
  lLockedDBConnections   := GTIOPFManager.DefaultDBConnectionPool.CountLocked;
  lAvailableDBConnections := lTotalDBConnections - lLockedDBConnections;

  gStatefulDBConnectionPool.GetSummaryStats(
    lTotalStatefulDBConnections,
    lInUseStatefulDBConnections,
    lWaitingStatefulDBConnections);

  FParams := TtiQueryParams.Create;
  try
    FParams.SetValueAsDateTime(cFieldSummaryTimeOnServer, Now);
    FParams.SetValueAsDateTime(cFieldSummaryApplicationStartTime, GTIOPFManager.ApplicationStartTime);
    FParams.SetValueAsString( cFieldSummaryApplicationUpTime, GetApplicationUpTimeAsString);
    FParams.SetValueAsString( cFieldSummaryPersistenceLayerName, GTIOPFManager.DefaultPersistenceLayerName);
    FParams.SetValueAsString( cFieldSummaryDatabaseName, GTIOPFManager.DefaultDBConnectionName);
    FParams.SetValueAsString( cFieldSummaryUserName, GTIOPFManager.DefaultDBConnectionPool.DBConnectParams.UserName);
    FParams.SetValueAsString( cFieldSummaryPassword,  tiReplicate('*', Length(GTIOPFManager.DefaultDBConnectionPool.DBConnectParams.Password)));
    FParams.SetValueAsInteger(cFieldSummaryTotalDBConnections, lTotalDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryLockedDBConnections, lLockedDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryAvailableDBConnections, lAvailableDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryDBConnectionTimeOut, GTIOPFManager.DefaultDBConnectionPool.WaitTime);

    FParams.SetValueAsInteger(cFieldSummaryTotalStatefulDBConnections,   lTotalStatefulDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryInUseStatefulDBConnections,   lInUseStatefulDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryWaitingStatefulDBConnections, lWaitingStatefulDBConnections);
    FParams.SetValueAsInteger(cFieldSummaryStatefulDBConnectionTimeOut,  (Trunc(gStatefulDBConnectionPool.TimeOut * 60)));

    FDatabase.InsertRow(cTableNameSummary, FParams);

  finally
    FParams.Free;
  end;

end;

procedure TtiDBProxyServerStats.ForEachDBConnectionPoolItem(const APooledItem : TtiPooledItem);
var
  FParams : TtiQueryParams;
  lListCount: Integer;
begin
  Assert(APooledItem.TestValid(TtiPooledItem), CTIErrorInvalidObject);
  lListCount:= APooledItem.Owner.Count;
  FParams := TtiQueryParams.Create;
  try
    FParams.SetValueAsInteger(  cFieldDBConPoolSecInUse,      APooledItem.SecInUse);
    FParams.SetValueAsBoolean(  cFieldDBConPoolLocked,        APooledItem.Locked);
    FParams.SetValueAsBoolean(  cFieldDBConPoolError,         APooledItem.MustRemoveItemFromPool(lListCount));
    FParams.SetValueAsInteger(  cFieldDBConPoolID,            APooledItem.Index);
    FParams.SetValueAsInteger(  cFieldDBConPoolSecToTimeOut,  APooledItem.SecToTimeOut);
    FDatabase.InsertRow(        cTableNameDBConnectionPool, FParams);
  finally
    FParams.Free;
  end;
end;

procedure TtiDBProxyServerStats.InsertDBConnectionPool;
var
  lPool : TtiDBConnectionPool;
begin
  lPool := GTIOPFManager.DefaultDBConnectionPool;
  lPool.ForEachPooledItem(ForEachDBConnectionPoolItem);
end;

procedure TtiDBProxyServerStats.ForEachStatefulDBConnectionItem(const AItem : TSavedDBConnectionHolder );
var
  FParams : TtiQueryParams;
begin
  FParams := TtiQueryParams.Create;
  try
    FParams.SetValueAsString(   cFieldStateFulDBConPoolUserName,  AItem.UserName);
    FParams.SetValueAsString(   cFieldStateFulDBConPoolComputerName,  AItem.ComputerName);
    FParams.SetValueAsInteger(  cFieldStateFulDBConPoolSecInUse, AItem.SecInUse);
    FParams.SetValueAsBoolean(  cFieldStateFulDBConPoollInUse,   AItem.InUse);
    FParams.SetValueAsString(   cFieldStateFulDBConPoolTransID,  AItem.TransactionID);
    FParams.SetValueAsInteger(  cFieldStateFulDBConPoolSecToTimeOut,  AItem.SecToTimeOut);
    FDatabase.InsertRow(        cTableNameStateFulDBConPool, FParams);
  finally
    FParams.Free;
  end;
end;

procedure TtiDBProxyServerStats.InsertStatefulDBConPool;
begin
  gStatefulDBConnectionPool.ForEach(ForEachStatefulDBConnectionItem);
end;

function TtiDBProxyServerStats.AsHTML: string;
var
  lDataSet : TtiDataBuffer;
begin
  result :=
    '<html>' +
    '<head>' +
    '<title>' + GetStatsPageTitle + '</title>' +
    Format(cScriptAutoRefresh, [tiPad0(IntToStr(FTestRefreshRate), 2)]) +
    '</head>' +
    '<body>' +
    '<h2>' + GetStatsPageTitle +'</h2>';

  lDataSet := FDatabase.FindDataSetByName(cTableNameStateFulDBConPool);
  result := result + tiDataSetToHTML(lDataSet);

  result := result + '<p>';
  lDataSet := FDatabase.FindDataSetByName(cTableNameDBConnectionPool);
  result := result + tiDataSetToHTML(lDataSet);

  result := result + '<p>';
  lDataSet := FDatabase.FindDataSetByName(cTableNameSummary);
  result := result + tiDataSetToHTMLV(lDataSet);

  result := result + '</body>' + '</html>';
end;

procedure TtiDBProxyServerStats.SetTestRefreshRate(const AValue: integer);
begin
  if AValue <= 1 then
    FTestRefreshRate := 2
  else
    FTestRefreshRate := AValue;
end;

function TtiDBProxyServerStats.GetStatsPageTitle: string;
begin
  result := GTIOPFManager.DefaultDBConnectionName + ' Status';
end;

function TtiDBProxyServerStats.GetApplicationUpTimeAsString: string;
var
  lTime : TDateTime;
  lH, lM, lS, lMS : Word;
begin
  lTime := Now-GTIOPFManager.ApplicationStartTime;
  DecodeTime(lTime, lH, lM, lS, lMS);
  result := IntToStr(Trunc(lTime)) + ' days ' +
    IntToStr(lH) + ':' +
    IntToStr(lM) + ':' +
    IntToStr(lS) + ' hours';
end;

end.

unit tiDBProxyServerStats;

interface
uses
  tiBaseObject,
  tiQueryXMLLight,
  tiPool,
  tiQueryRemote_Svr,
  tiQuery,
  tiDataBuffer_bom;

type

  TtiDBProxyServerStats = class(TtiBaseObject)
  private
    FDBList : TtiDataBufferList;
    FDBSummary: TtiDataBuffer;
    FDBConPool: TtiDataBuffer;
    FDBStatefulConPool: TtiDataBuffer;
    FTestRefreshRate: integer;
    procedure InsertSummary;
    procedure InsertDBConnectionPool;
    procedure InsertStatefulDBConPool;
    procedure ForEachDBConnectionPoolItem(const AItem: TtiPooledItem);
    procedure ForEachStatefulDBConnectionItem(const AItem: TSavedDBConnectionHolder);
    procedure SetTestRefreshRate(const AValue: integer);
    function  GetStatsPageTitle : string;
    function  GetApplicationUpTimeAsString: string;
    function  GetTimeToLiveText(
      const AStaticConnection, ALocked, APendingRemoval: boolean;
      const ATimeToLive: integer): string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute;
    function    AsHTML : string;
    function    AsXML : string;
    property    TestRefreshRate : integer read FTestRefreshRate write SetTestRefreshRate;
  end;

implementation
uses
  tiUtils,
  tiOPFManager,
  tiDataBuffer_Cli,
  tiXMLtoTIDataSet,
  tiDBConnectionPool,
  tiQueryTXTAbs,
  tiConstants,
  SysUtils;

const
  CTableNameSummary                 = 'System Summary';
  CFieldSummaryTimeOnServer         = 'Time on Server';
  CFieldSummaryApplicationStartTime = 'Application Start Time';
  CFieldSummaryApplicationUpTime    = 'Application Up Time';

  CFieldSummaryDBConPoolMinPoolSize           = 'DB Connection Pool - Min pool size';
  CFieldSummaryDBConPoolMaxPoolSize           = 'DB Connection Pool - Max pool size';
  CFieldSummaryDBConPoolWaitTime              = 'DB Connection Pool - Wait for lock time (sec)';
  CFieldSummaryDBConPoolTimeOut               = 'DB Connection Pool - Time out (sec)';
  CFieldSummaryDBConPoolSweepInterval             = 'DB Connection Pool - Sweep interval (sec)';
  CFieldSummaryStatefulDBConPoolTimeOut       = 'Stateful DB Connection Pool - Time out (sec)';
  CFieldSummaryStatefulDBConPoolSweepInterval = 'Stateful DB Connection Pool - Sweep interval (sec)';

  CTableNameDBConnectionPool       = 'Database Connection Pool';
  CFieldDBConPoolID                = 'Connection ID';
  CFieldDBConPoolLocked            = 'Is Locked?';
  CFieldDBConPoolSecInUse          = 'Age (sec)';
  CFieldDBConPoolSecToTimeOut      = 'Time to live (sec)';

  CTableNameStateFulDBConPool         = 'Stateful Database Connection Pool';
  CFieldStateFulDBConPoolComputerName = 'Computer Name';
  CFieldStateFulDBConPoolUserName     = 'User Name';
  CFieldStateFulDBConPoolTransID      = 'Transaction ID';
  CFieldStateFulDBConPoolIsQueryExecuting       = 'Is Query Executing?';
  CFieldStateFulDBConPoolSecInUse     = 'Time in Use (sec)';
  CFieldStateFulDBConPoolSecToTimeOut = 'Time to Live (sec)';

  CDefaultRefreshRate = 2;

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
  result:= tiTIDataBufferListToXMLString(FDBList);
end;

constructor TtiDBProxyServerStats.create;
begin
  inherited;
  FTestRefreshRate := CDefaultRefreshRate;

  FDBList:= TtiDataBufferList.Create;

  FDBSummary:= FDBList.AddInstance(CTableNameSummary);
  FDBSummary.Fields.AddInstance(CFieldSummaryTimeOnServer,                   qfkDateTIme, 0);
  FDBSummary.Fields.AddInstance(CFieldSummaryApplicationStartTime,           qfkDateTIme, 0);
  FDBSummary.Fields.AddInstance(CFieldSummaryApplicationUpTime,              qfkString,   0);
  FDBSummary.Fields.AddInstance(CFieldSummaryDBConPoolMinPoolSize,           qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryDBConPoolMaxPoolSize,           qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryDBConPoolWaitTime,              qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryDBConPoolTimeOut,               qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryDBConPoolSweepInterval,             qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryStatefulDBConPoolTimeOut,       qfkInteger,  0);
  FDBSummary.Fields.AddInstance(CFieldSummaryStatefulDBConPoolSweepInterval, qfkInteger,  0);

  FDBConPool:= FDBList.AddInstance(CTableNameDBConnectionPool);
  FDBConPool.Fields.AddInstance(CFieldDBConPoolID,           qfkInteger, 0);
  FDBConPool.Fields.AddInstance(CFieldDBConPoolLocked,       qfkLogical, 0);
  FDBConPool.Fields.AddInstance(CFieldDBConPoolSecInUse,     qfkInteger, 0);
  FDBConPool.Fields.AddInstance(CFieldDBConPoolSecToTimeOut, qfkString,  0);

  FDBStatefulConPool:= FDBList.AddInstance(CTableNameStateFulDBConPool);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolComputerName, qfkString,  0);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolUserName,     qfkString,  0);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolTransID,      qfkString,  0);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolIsQueryExecuting,       qfkLogical, 0);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolSecInUse,     qfkInteger, 0);
  FDBStatefulConPool.Fields.AddInstance(CFieldStateFulDBConPoolSecToTimeOut, qfkString, 0);

end;

destructor TtiDBProxyServerStats.destroy;
begin
  FDBList.Free;
  inherited;
end;

procedure TtiDBProxyServerStats.Execute;
begin
  InsertSummary;
  InsertDBConnectionPool;
  InsertStatefulDBConPool;
end;

procedure TtiDBProxyServerStats.InsertSummary;
var
  LRow: TtiDataBufferRow;
begin
  FDBSummary.ClearRows;
  LRow:= FDBSummary.AddInstance;
  LRow.FieldAsDateTime[CFieldSummaryTimeOnServer]:= Now;
  LRow.FieldAsDateTime[CFieldSummaryApplicationStartTime]:=  GTIOPFManager.ApplicationStartTime;
  LRow.FieldAsString[ CFieldSummaryApplicationUpTime]:=  GetApplicationUpTimeAsString;
  LRow.FieldAsInteger[CFieldSummaryDBConPoolMinPoolSize]:= GTIOPFManager.DefaultDBConnectionPool.MinPoolSize;
  LRow.FieldAsInteger[CFieldSummaryDBConPoolMaxPoolSize]:= GTIOPFManager.DefaultDBConnectionPool.MaxPoolSize;
  LRow.FieldAsInteger[CFieldSummaryDBConPoolWaitTime]   := GTIOPFManager.DefaultDBConnectionPool.WaitTime;
  LRow.FieldAsInteger[CFieldSummaryDBConPoolTimeOut]    := Trunc(GTIOPFManager.DefaultDBConnectionPool.TimeOut * 60);
  LRow.FieldAsInteger[CFieldSummaryDBConPoolSweepInterval]  := GTIOPFManager.DefaultDBConnectionPool.SweepTime;
  LRow.FieldAsInteger[CFieldSummaryStatefulDBConPoolTimeOut]:= Trunc(gStatefulDBConnectionPool.TimeOut * 60);
  LRow.FieldAsInteger[CFieldSummaryStatefulDBConPoolSweepInterval]:= gStatefulDBConnectionPool.SweepInterval;
end;

procedure TtiDBProxyServerStats.ForEachDBConnectionPoolItem(const AItem : TtiPooledItem);
var
  LRow: TtiDataBufferRow;
begin
  LRow:= FDBConPool.AddInstance;
  LRow.FieldAsInteger[CFieldDBConPoolSecInUse]    := AItem.SecInUse;
  LRow.FieldAsBoolean[CFieldDBConPoolLocked]      := AItem.Locked;
  LRow.FieldAsInteger[CFieldDBConPoolID]          := AItem.Index;
  LRow.FieldAsString[CFieldDBConPoolSecToTimeOut]:=
    GetTimeToLiveText(
      AItem.IsStaticConnection,
      AItem.Locked,
      AItem.MustRemoveItemFromPool,
      AItem.SecToTimeOut);
end;

procedure TtiDBProxyServerStats.InsertDBConnectionPool;
var
  LPool : TtiDBConnectionPool;
begin
  FDBConPool.ClearRows;
  LPool := GTIOPFManager.DefaultDBConnectionPool;
  LPool.ForEachPooledItem(ForEachDBConnectionPoolItem);
end;

procedure TtiDBProxyServerStats.ForEachStatefulDBConnectionItem(const AItem : TSavedDBConnectionHolder );
var
  LRow: TtiDataBufferRow;
begin
  LRow:= FDBStatefulConPool.AddInstance;
  LRow.FieldAsString[ CFieldStateFulDBConPoolUserName        ]:= AItem.UserName;
  LRow.FieldAsString[ CFieldStateFulDBConPoolComputerName    ]:= AItem.ComputerName;
  LRow.FieldAsInteger[CFieldStateFulDBConPoolSecInUse        ]:= AItem.SecInUse;
  LRow.FieldAsBoolean[CFieldStateFulDBConPoolIsQueryExecuting]:= AItem.QueryIsExecuting;
  LRow.FieldAsString[ CFieldStateFulDBConPoolTransID         ]:= AItem.TransactionID;
  LRow.FieldAsString[CFieldStateFulDBConPoolSecToTimeOut     ]:=
    GetTimeToLiveText(
      False,
      AItem.QueryIsExecuting,
      AItem.MustRemoveItemFromPool,
      AItem.SecToTimeOut);
end;

procedure TtiDBProxyServerStats.InsertStatefulDBConPool;
begin
  gStatefulDBConnectionPool.ForEach(ForEachStatefulDBConnectionItem);
end;

function TtiDBProxyServerStats.AsHTML: string;
begin
  result :=
    '<html>' +
    '<head>' +
    '<title>' + GetStatsPageTitle + '</title>' +
    Format(cScriptAutoRefresh, [tiPad0(IntToStr(FTestRefreshRate), 2)]) +
    '</head>' +
    '<body>' +
    '<h2>' + GetStatsPageTitle +'</h2>';

  result := result + '<p>';
  result := result + tiDataSetToHTMLV(FDBSummary);

  result := result + '<p>';
  result := result + tiDataSetToHTML(FDBConPool);

  result := result + '<p>';
  result := result + tiDataSetToHTML(FDBStatefulConPool);

  result := result + '</body>' + '</html>';
end;

procedure TtiDBProxyServerStats.SetTestRefreshRate(const AValue: integer);
begin
  if AValue <= 1 then
    FTestRefreshRate := CDefaultRefreshRate
  else
    FTestRefreshRate := AValue;
end;

function TtiDBProxyServerStats.GetStatsPageTitle: string;
begin
  result := tiApplicationName + ' Status';
end;

function TtiDBProxyServerStats.GetTimeToLiveText(const AStaticConnection,
  ALocked, APendingRemoval: boolean; const ATimeToLive: integer): string;
begin
  if AStaticConnection then
    result:= 'Forever'
  else if ALocked then
    result:= 'In Use'
  else if APendingRemoval then
    result:= 'Pending Removal'
  else
    result:= IntToStr(ATimeToLive);
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

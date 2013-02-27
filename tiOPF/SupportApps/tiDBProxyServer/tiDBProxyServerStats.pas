unit tiDBProxyServerStats;

interface
uses
  tiObjAbs
  ,tiQueryXMLLight
  ,tiPoolAbs
  ,tiQueryRemote_Svr
  ;

type

  TtiDBProxyServerStats = class( TtiObjAbs )
  private
    FDatabase : TtiDatabaseXMLLight ;
    FTestRefreshRate: integer;
    procedure InsertSummary;
    procedure InsertDBConnectionPool;
    procedure InsertStatefulDBConPool;
    procedure ForEachDBConnectionPoolItem(const pPooledItem: TPooledItem);
    procedure ForEachStatefulDBConnectionItem(const pItem: TSavedDBConnectionHolder);
    procedure SetTestRefreshRate(const Value: integer);
    function  GetStatsPageTitle : string ;
    function  GetApplicationUpTimeAsString: string;
  public
    constructor create ;
    destructor  destroy ; override ;
    procedure   Execute ;
    function    AsXML : string ;
    function    AsHTML : string ;
    property    TestRefreshRate : integer read FTestRefreshRate write SetTestRefreshRate ;
  end ;

implementation
uses
   SysUtils
  ,tiDataSet_Cli
  ,tiQuery
  ,tiPersist
  ,tiUtils
  ,tiDBConnectionPool
  ,tiDataSet_BOM
  ,tiQueryTXTAbs
  ,cTIPersist
  ;


const
  cTableNameSummary                 = 'summary' ;
  cFieldSummaryDatabaseName         = 'databasename' ;
  cFieldSummaryTimeOnServer         = 'timeonserver' ;
  cFieldSummaryApplicationStartTime = 'applicationstarttime' ;
  cFieldSummaryApplicationUpTime    = 'applicationuptime' ;
  cFieldSummaryPersistenceLayerName = 'persistencelayername' ;
  cFieldSummaryUserName             = 'username' ;
  cFieldSummaryPassword             = 'password' ;

  cFieldSummaryTotalDBConnections   = 'totaldbconnections' ;
  cFieldSummaryLockedDBConnections  = 'lockeddbconnections' ;
  cFieldSummaryAvailableDBConnections = 'availabledbconnections' ;
  cFieldSummaryDBConnectionTimeOut    = 'dbconnectiontimeout' ;

  cFieldSummaryTotalStatefulDBConnections   = 'totalstatefuldbconnections' ;
  cFieldSummaryInUseStatefulDBConnections   = 'inusestatefuldbConnections' ;
  cFieldSummaryWaitingStatefulDBConnections = 'waitingstatefuldbconnections' ;
  cFieldSummaryStatefulDBConnectionTimeOut  = 'statefuldbconnectionTimeOut' ;

  cTableNameDBConnectionPool       = 'dbconnectionpool' ;
  cFieldDBConPoolID                = 'id' ;
  cFieldDBConPoolLocked            = 'locked' ;
  cFieldDBConPoolError             = 'error' ;
  cFieldDBConPoolSecInUse          = 'secinuse' ;
  cFieldDBConPoolSecToTimeOut      = 'sectotimeout' ;

  cTableNameStateFulDBConPool         = 'statefuldbconpool' ;
  cFieldStateFulDBConPoolComputerName = 'computername' ;
  cFieldStateFulDBConPoolUserName     = 'username' ;
  cFieldStateFulDBConPoolTransID      = 'transactionid' ;
  cFieldStateFulDBConPoollInUse       = 'inuse' ;
  cFieldStateFulDBConPoolSecInUse     = 'secinuse' ;
  cFieldStateFulDBConPoolSecToTimeOut = 'sectotimeout' ;

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
  lMDT : TtiDBMetaDataTable ;
begin
  inherited ;
  FTestRefreshRate := 10 ;

  FDatabase := TtiDatabaseXMLLight.Create ;
  FDatabase.PersistToFile := false;

  lMDT := TtiDBMetaDataTable.Create ;
  try
    lMDT.Name := cTableNameSummary ;
    lMDT.AddField(cFieldSummaryTimeOnServer,qfkDateTIme,0);
    lMDT.AddField(cFieldSummaryApplicationStartTime,qfkDateTIme,0);
    lMDT.AddField(cFieldSummaryApplicationUpTime,qfkString,0);
    lMDT.AddField(cFieldSummaryPersistenceLayerName,qfkString,0);
    lMDT.AddField(cFieldSummaryDatabaseName,qfkString,0);
    lMDT.AddField(cFieldSummaryUserName,qfkString,0);
    lMDT.AddField(cFieldSummaryPassword,qfkString,0);
    lMDT.AddField(cFieldSummaryTotalDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryLockedDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryAvailableDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryDBConnectionTimeOut,qfkInteger,0);
    lMDT.AddField(cFieldSummaryTotalStatefulDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryInUseStatefulDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryWaitingStatefulDBConnections,qfkInteger,0);
    lMDT.AddField(cFieldSummaryStatefulDBConnectionTimeOut,qfkInteger,0);
    FDatabase.CreateTable(lMDT);
  finally
    lMDT.Free;
  end;

  lMDT := TtiDBMetaDataTable.Create ;
  try
    lMDT.Name := cTableNameDBConnectionPool ;
    lMDT.AddField(cFieldDBConPoolID,qfkInteger,0);
    lMDT.AddField(cFieldDBConPoolLocked,qfkLogical,0);
    lMDT.AddField(cFieldDBConPoolError,qfkLogical,0);
    lMDT.AddField(cFieldDBConPoolSecInUse,qfkInteger,0);
    lMDT.AddField(cFieldDBConPoolSecToTimeOut,qfkInteger,0);
    FDatabase.CreateTable(lMDT);
  finally
    lMDT.Free;
  end;

  lMDT := TtiDBMetaDataTable.Create ;
  try
    lMDT.Name := cTableNameStateFulDBConPool ;
    lMDT.AddField( cFieldStateFulDBConPoolComputerName, qfkString,0);
    lMDT.AddField( cFieldStateFulDBConPoolUserName,     qfkString,0);
    lMDT.AddField( cFieldStateFulDBConPoolTransID,      qfkString,0);
    lMDT.AddField( cFieldStateFulDBConPoollInUse,       qfkLogical,0);
    lMDT.AddField( cFieldStateFulDBConPoolSecInUse,     qfkInteger,0);
    lMDT.AddField( cFieldStateFulDBConPoolSecToTimeOut, qfkInteger,0);
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
end ;

procedure TtiDBProxyServerStats.InsertSummary;
var
  lTotalDBConnections             : integer ;
  lLockedDBConnections            : integer ;
  lAvailableDBConnections         : integer ;
  lTotalStatefulDBConnections     : integer ;
  lInUseStatefulDBConnections    : integer ;
  lWaitingStatefulDBConnections : integer ;
  FParams : TtiQueryParams ;
begin

  lTotalDBConnections     := gTIPerMgr.DefaultDBConnectionPool.Count ;
  lLockedDBConnections    := gTIPerMgr.DefaultDBConnectionPool.CountLocked;
  lAvailableDBConnections := lTotalDBConnections - lLockedDBConnections;

  gStatefulDBConnectionPool.GetSummaryStats(
    lTotalStatefulDBConnections,
    lInUseStatefulDBConnections,
    lWaitingStatefulDBConnections);

  FParams := TtiQueryParams.Create ;
  try
    FParams.SetValueAsDateTime( cFieldSummaryTimeOnServer, Now ) ;
    FParams.SetValueAsDateTime( cFieldSummaryApplicationStartTime, gTIPerMgr.ApplicationStartTime ) ;
    FParams.SetValueAsString(  cFieldSummaryApplicationUpTime, GetApplicationUpTimeAsString ) ;
    FParams.SetValueAsString(  cFieldSummaryPersistenceLayerName, gTIPerMgr.DefaultPerLayerName );
    FParams.SetValueAsString(  cFieldSummaryDatabaseName, gTIPerMgr.DefaultDBConnectionName );
    FParams.SetValueAsString(  cFieldSummaryUserName, gTIPerMgr.DefaultDBConnectionPool.DBConnectParams.UserName );
    FParams.SetValueAsString(  cFieldSummaryPassword,  tiReplicate( '*', Length( gTIPerMgr.DefaultDBConnectionPool.DBConnectParams.UserPassword)));
    FParams.SetValueAsInteger( cFieldSummaryTotalDBConnections, lTotalDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryLockedDBConnections, lLockedDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryAvailableDBConnections, lAvailableDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryDBConnectionTimeOut, gTIPerMgr.DefaultDBConnectionPool.WaitTime);

    FParams.SetValueAsInteger( cFieldSummaryTotalStatefulDBConnections,   lTotalStatefulDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryInUseStatefulDBConnections,   lInUseStatefulDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryWaitingStatefulDBConnections, lWaitingStatefulDBConnections);
    FParams.SetValueAsInteger( cFieldSummaryStatefulDBConnectionTimeOut,  (Trunc(gStatefulDBConnectionPool.TimeOut * 60 )));

    FDatabase.InsertRow( cTableNameSummary, FParams ) ;

  finally
    FParams.Free;
  end;

end;

procedure TtiDBProxyServerStats.ForEachDBConnectionPoolItem( const pPooledItem : TPooledItem );
var
  FParams : TtiQueryParams ;
  lListCount: Integer;
begin
  Assert( pPooledItem.TestValid(TPooledItem), cTIInvalidObjectError );
  lListCount:= pPooledItem.Owner.Count;
  FParams := TtiQueryParams.Create ;
  try
    FParams.SetValueAsInteger(   cFieldDBConPoolSecInUse,      pPooledItem.SecInUse ) ;
    FParams.SetValueAsBoolean(   cFieldDBConPoolLocked,        pPooledItem.Locked );
    FParams.SetValueAsBoolean(   cFieldDBConPoolError,         pPooledItem.MustRemoveItemFromPool(lListCount));
    FParams.SetValueAsInteger(   cFieldDBConPoolID,            pPooledItem.Index );
    FParams.SetValueAsInteger(   cFieldDBConPoolSecToTimeOut,  pPooledItem.SecToTimeOut );
    FDatabase.InsertRow(         cTableNameDBConnectionPool, FParams ) ;
  finally
    FParams.Free;
  end;
end ;

procedure TtiDBProxyServerStats.InsertDBConnectionPool;
var
  lPool : TDBConnectionPool ;
begin
  lPool := gTIPerMgr.DefaultDBConnectionPool ;
  lPool.ForEachPooledItem(ForEachDBConnectionPoolItem);
end ;

procedure TtiDBProxyServerStats.ForEachStatefulDBConnectionItem( const pItem : TSavedDBConnectionHolder  );
var
  FParams : TtiQueryParams ;
begin
  FParams := TtiQueryParams.Create ;
  try
    FParams.SetValueAsString(    cFieldStateFulDBConPoolUserName,  pItem.UserName );
    FParams.SetValueAsString(    cFieldStateFulDBConPoolComputerName,  pItem.ComputerName );
    FParams.SetValueAsInteger(   cFieldStateFulDBConPoolSecInUse, pItem.SecInUse ) ;
    FParams.SetValueAsBoolean(   cFieldStateFulDBConPoollInUse,   pItem.InUse );
    FParams.SetValueAsString(    cFieldStateFulDBConPoolTransID,  pItem.TransactionID );
    FParams.SetValueAsInteger(   cFieldStateFulDBConPoolSecToTimeOut,  pItem.SecToTimeOut );
    FDatabase.InsertRow(         cTableNameStateFulDBConPool, FParams ) ;
  finally
    FParams.Free;
  end;
end ;

procedure TtiDBProxyServerStats.InsertStatefulDBConPool;
begin
  gStatefulDBConnectionPool.ForEach(ForEachStatefulDBConnectionItem);
end;

function TtiDBProxyServerStats.AsHTML: string;
var
  lDataSet : TtiDataSet ;
begin
  result :=
    '<html>' +
    '<head>' +
    '<title>' + GetStatsPageTitle + '</title>' +
    Format( cScriptAutoRefresh, [tiPad0(IntToStr(FTestRefreshRate), 2)] ) +
    '</head>' +
    '<body>' +
    '<h2>' + GetStatsPageTitle +'</h2>' ;

  lDataSet := FDatabase.FindDataSetByName(cTableNameStateFulDBConPool);
  result := result + tiDataSetToHTML(lDataSet);

  result := result + '<p>' ;
  lDataSet := FDatabase.FindDataSetByName(cTableNameDBConnectionPool);
  result := result + tiDataSetToHTML(lDataSet);

  result := result + '<p>' ;
  lDataSet := FDatabase.FindDataSetByName(cTableNameSummary);
  result := result + tiDataSetToHTMLV(lDataSet);

  result := result + '</body>' + '</html>';
end;

procedure TtiDBProxyServerStats.SetTestRefreshRate(const Value: integer);
begin
  if Value <= 1 then
    FTestRefreshRate := 2
  else
    FTestRefreshRate := Value;
end;

function TtiDBProxyServerStats.GetStatsPageTitle: string;
begin
  result := gTIPerMgr.DefaultDBConnectionName + ' Status' ;
end;

function TtiDBProxyServerStats.GetApplicationUpTimeAsString: string;
var
  lTime : TDateTime ;
  lH, lM, lS, lMS : Word ;
begin
  lTime := Now-gTIPerMgr.ApplicationStartTime;
  DecodeTime(lTime, lH, lM, lS, lMS);
  result := IntToStr( Trunc( lTime )) + ' days ' +
    IntToStr( lH ) + ':' +
    IntToStr( lM ) + ':' +
    IntToStr( lS ) + ' hours' ;
end;

end.

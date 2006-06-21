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

  Purpose:
    Wrapper the TQuery object as a visitor

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{

  // Templates
  // Code template key combination: VR
  //----------------------------------------------------------------------------
  TVisXXX_Read = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end ;

  // Code template key combination: VC
  //---------------------------------------------------------------------------
  TVisXXX_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  // Code template key combination: VU
  //----------------------------------------------------------------------------
  TVisXXX_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  // Code template key combination: VD
  //----------------------------------------------------------------------------
  TVisXXX_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
  end ;

}


{$I tiDefines.inc}

unit tiPtnVisSQL;

interface
uses
   tiDBConnectionPool
  ,tiPtnVis
  ,SysUtils
  ,tiQuery
  ,Classes
  ,tiPtnVisPerObj
  ;

type

  // A visitor manager for TVisDBAbs visitors
  TtiPerObjVisitorCtrlr = class( TVisitorCtrlr )
  private
    FPooledDB : TPooledDB ;
    FtiQueryClass : TtiQueryClass ;
  protected
    property  PooledDB : TPooledDB read FPooledDB write FPooledDB ;
    procedure SetPerLayerName(const Value: string); override ;
  public
    procedure BeforeExecuteAll( pVisitors : TList )      ; override ;
    procedure BeforeExecuteOne( pVisitor : TVisitorAbs ) ; override ;
    procedure AfterExecuteOne( pVisitor : TVisitorAbs  ) ; override ;
    procedure AfterExecuteAll( pVisitors : TList )       ; override ;
    procedure AfterExecuteError( pVisitors : TList )     ; override ;
  end ;

  { TODO : Add a reference to the TUserName object so the _Svr units do not have to ref the Security_Cli unit. }

  // Adds an owned query object
  // Note: It is not necessary to manually lock and unlock DBConnections
  // from this level and below - TVisitorMgrDB does this for you.
  // Adds a pooled database connection
  //----------------------------------------------------------------------------
  TtiPerObjVisitor = class( TVisitorAbs )
  private
    FVisitedList  : TList ;
    FDatabase     : TtiDatabase ;
    FQuery        : TtiQuery ;
    function    GetQuery: TtiQuery;
    procedure   SetQuery(const Value: TtiQuery);
  protected
    function    GetVisited: TPerObjAbs; reintroduce ;
    procedure   SetVisited(const Value: TPerObjAbs); reintroduce ; virtual ;
    procedure   LogQueryTiming( const pQueryName : string ;
                                pQueryTime : integer ;
                                pScanTime : integer ) ;
    procedure   SetupParams     ; virtual ;
    procedure   Final ; virtual ;
    property    VisitedList : TList read FVisitedList ;
    property    Database : TtiDatabase read FDatabase write FDatabase ;
    property    Query : TtiQuery read GetQuery write SetQuery ;
  public
    Constructor create ; override ;
    destructor  destroy ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    function    VisitorControllerClass : TVisitorControllerClass ; override ;
    property    Visited : TPerObjAbs read GetVisited write SetVisited ;
  end;

  //----------------------------------------------------------------------------
  TVisOwnedQrySelectAbs = class( TtiPerObjVisitor )
  protected
    procedure   Init            ; virtual ;
    procedure   MapRowToObject  ; virtual ;
    procedure   OpenQuery    ; virtual ; abstract ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisDBIndependentRead = class( TVisOwnedQrySelectAbs )
  private
    FQueryParams : TtiQueryParams ;
    FTableName : string;
  protected
    procedure OpenQuery ; override ;
    property TableName : string read FTableName write FTableName ;
    property QueryParams : TtiQueryParams read FQueryParams ;
  public
    constructor create ; override ;
    destructor  destroy ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisOwnedQrySelect = class( TVisOwnedQrySelectAbs )
  protected
    procedure OpenQuery ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisOwnedQryUpdate = class( TtiPerObjVisitor )
  protected
    procedure   Init            ; virtual ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

implementation
uses
  tiUtils
  {$IFDEF MSWINDOWS}
  ,Dialogs
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QDialogs
  ,Types
  {$ENDIF LINUX}
  ,tiLog
  ,tiPersist
  ,tiPerObjOIDAbs
  ,tiRegPerLayer
  ,ctiPersist
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiPerObjVisitor
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPerObjVisitor.create;
begin
  inherited;
  FVisitedList     := TList.Create ;
  // Query is created and assigned by TtiPerObjVisitorCtrlr
end;

destructor TtiPerObjVisitor.destroy;
begin
  FVisitedList.Free;
  FQuery.Free ;
  inherited;
end;

procedure TtiPerObjVisitor.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited ) ;
  if AcceptVisitor then
    VisitedList.Add( pVisited ) ;
end;

procedure TtiPerObjVisitor.Final;
begin
  case TPerObjAbs( Visited ).ObjectState of
  posDeleted : ; // Do nothing
  posDelete  : TPerObjAbs( Visited ).ObjectState := posDeleted ;
  else
    TPerObjAbs( Visited ).ObjectState := posClean ;
  end ;
end;

function TtiPerObjVisitor.GetQuery: TtiQuery;
begin
  Assert( FQuery <> nil, 'FQuery not assigned' ) ;
  result := FQuery ;
end;

function TtiPerObjVisitor.GetVisited: TPerObjAbs;
begin
  result := TPerObjAbs( inherited GetVisited ) ;
end;

procedure TtiPerObjVisitor.LogQueryTiming( const pQueryName: string;
                                    pQueryTime : integer ;
                                    pScanTime: integer);
var
  lClassName : string ;
begin

  lClassName := ClassName ;

  // We don't want to log access to the SQLManager queries, and this
  // is one possible way of blocking this.
  if SameText( lClassName, 'TVisReadGroupPK' ) or
     SameText( lClassName, 'TVisReadQueryPK' ) or
     SameText( lClassName, 'TVisReadQueryDetail' ) or
     SameText( lClassName, 'TVisReadParams' ) then
    Exit ; //==>

  Log( {tiPadR( lClassName, cuiQueryTimingSpacing ) +}
       tiPadR( pQueryName, cuiQueryTimingSpacing ) + ' ' +
       tiPadR( IntToStr( pQueryTime ), 7 ) +
       tiPadR( IntToStr( pScanTime ), 7 ),
       lsQueryTiming )

end;

procedure TtiPerObjVisitor.SetQuery(const Value: TtiQuery);
begin
  Assert(FQuery = nil, 'FQuery already assigned' ) ;
  FQuery := Value ;
end;

procedure TtiPerObjVisitor.SetupParams;
begin
// Do nothing
end;

procedure TtiPerObjVisitor.SetVisited(const Value: TPerObjAbs);
begin
  inherited SetVisited( Value ) ;
end;

function TtiPerObjVisitor.VisitorControllerClass: TVisitorControllerClass;
begin
  result := TtiPerObjVisitorCtrlr ;
end;

{ TtiPerObjVisitorCtrlr }

procedure TtiPerObjVisitorCtrlr.AfterExecuteAll( pVisitors : TList );
var
  i, j : integer ;
  lVisitor : TtiPerObjVisitor ;
  lVisited : TPerObjAbs ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>

  FPooledDB.Database.Commit ;

  try
    for i := 0 to pVisitors.Count - 1 do begin
      // This should not be necessary, however there are times when a visitor
      // group will contain several types of visitors. We should add some code so
      // we do not get passed a list of visitors which contains incompatable types
      if not ( TObject( pVisitors.Items[i] ) is TtiPerObjVisitor ) then
        Continue ; //==>
      lVisitor := ( TObject( pVisitors.Items[i] ) as TtiPerObjVisitor ) ;
      for j := 0 to lVisitor.VisitedList.Count - 1 do
      begin
        if gTIPerMgr.Terminated then
          Exit ; //==>
        lVisited := TPerObjAbs( lVisitor.VisitedList.Items[j] ) ;
        lVisitor.Visited := lVisited ;
        lVisitor.Final ;
        lVisitor.Visited := nil ;
      end ;
    end ;
    if gTIPerMgr.Terminated then
      Exit ; //==>
    inherited AfterExecuteAll( pVisitors ) ;
  except
    on e:exception do
      tiFmtException( e,
                      ClassName,
                      'AfterExecuteAll' ) ;
  end ;

  if gTIPerMgr.Terminated then
    Exit ; //==>

  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  Assert( lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>' ) ;
  lRegPerLayer.DBConnectionPools.UnLock( DBConnectionName, FPooledDB ) ;

end;

procedure TtiPerObjVisitorCtrlr.AfterExecuteError;
var
  lRegPerLayer : TtiRegPerLayer ;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  Assert( lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>' ) ;
  FPooledDB.Database.RollBack ;
  lRegPerLayer.DBConnectionPools.UnLock( DBConnectionName, FPooledDB ) ;
end;

procedure TtiPerObjVisitorCtrlr.AfterExecuteOne(pVisitor : TVisitorAbs);
begin
  TtiPerObjVisitor( pVisitor ).Database := nil ;
end;

procedure TtiPerObjVisitorCtrlr.BeforeExecuteAll( pVisitors : TList );
var
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  Assert( lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + PerLayerName +'>' ) ;
  FPooledDB := lRegPerLayer.DBConnectionPools.Lock( DBConnectionName ) ;
  FPooledDB.Database.StartTransaction ;
end;

procedure TtiPerObjVisitorCtrlr.BeforeExecuteOne(pVisitor : TVisitorAbs );
begin
  Assert( pVisitor is TtiPerObjVisitor, pVisitor.ClassName + ' not a TVisDBAbs' ) ;
  Assert( TtiPerObjVisitor( pVisitor ).FQuery = nil, 'TtiPerObjVisitor( pVisitor ).Query already assigned' ) ;
  Assert( FtiQueryClass <> nil, 'FtiQueryClass not assigned' ) ;
  TtiPerObjVisitor( pVisitor ).FQuery := FtiQueryClass.Create ;
  TtiPerObjVisitor( pVisitor ).Database := FPooledDB.Database ;
  TtiPerObjVisitor( pVisitor ).FQuery.AttachDatabase(
    TtiPerObjVisitor( pVisitor ).Database);
end;

{ TVisPerObjAwareAbs }
{
constructor TVisPerObjAwareAbs.Create;
begin
  inherited;
  FVisitedList     := TList.Create ;
end;

destructor TVisPerObjAwareAbs.Destroy;
begin
  FVisitedList.Free ;
  inherited;
end;

procedure TVisPerObjAwareAbs.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited ) ;
  if AcceptVisitor then
    VisitedList.Add( pVisited ) ;
end;

procedure TVisPerObjAwareAbs.Final;
begin
  case TPerObjAbs( Visited ).ObjectState of
  posDeleted : ; // Do nothing
  posDelete  : TPerObjAbs( Visited ).ObjectState := posDeleted ;
  else
    TPerObjAbs( Visited ).ObjectState := posClean ;
  end ;
end;

function TVisPerObjAwareAbs.VisitorControllerClass: TVisitorControllerClass;
begin
  result := TVisitorMgrPerObjAbsAware ;
end;
}
{ TVisitorMgrPerObjAbsAware }
{
procedure TVisitorMgrPerObjAbsAware.AfterExecuteAll(pVisitors: TList);
var
  i, j : integer ;
  lVisitor : TVisPerObjAwareAbs ;
  lVisited : TPerObjAbs ;
begin
  try
    for i := 0 to pVisitors.Count - 1 do begin
      // This should not be necessary, however there are times when a visitor
      // group will contain several types of visitors. We should add some code so
      // we do not get passed a list of visitors which contains incompatable types
      if not ( TObject( pVisitors.Items[i] ) is TVisPerObjAwareAbs ) then
        Continue ; //==>
      lVisitor := ( TObject( pVisitors.Items[i] ) as TVisPerObjAwareAbs ) ;
      for j := 0 to lVisitor.VisitedList.Count - 1 do begin
        lVisited := TPerObjAbs( lVisitor.VisitedList.Items[j] ) ;
        lVisitor.Visited := lVisited ;
        lVisitor.Final ;
        lVisitor.Visited := nil ;
      end ;
    end ;
    inherited AfterExecuteAll( pVisitors ) ;
  except
    on e:exception do
      tiFmtException( e,
                      ClassName,
                      'AfterExecuteAll' ) ;
  end ;
end;
}

procedure TtiPerObjVisitorCtrlr.SetPerLayerName(const Value: string);
var
  lRegPerLayer : TtiRegPerLayer ;
begin
  inherited;
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  Assert( lRegPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );
  FtiQueryClass := lRegPerLayer.tiQueryClass;
end;

{ TVisOwnedQrySelectAbs }

procedure TVisOwnedQrySelectAbs.Execute(const pData: TVisitedAbs);
  procedure _ScanQuery ;
  begin
    Query.ContinueScan := True;
    while ( not Query.EOF ) and
          ( Query.ContinueScan ) and
          ( not gTIPerMgr.Terminated ) do begin
      try
        MapRowToObject ;
      except
        on e:exception do
          tiFmtException( e, ClassName, '_ScanQuery') ;
      end ;
      Query.Next ;
    end ;
    Query.Close ;
  end ;
var
  liStart : DWord ;
  liQueryTime : DWord ;
begin

  if gTIPerMgr.Terminated then
    Exit ; //==>

  try
    Inherited Execute( pData ) ;

    if not DoAcceptVisitor then
      Exit ; //==>

    Assert( Database <> nil, 'DBConnection not set in ' + ClassName ) ;

    if pData <> nil then begin
      Visited := TPerObjAbs( pData ) ;
    end else begin
      Visited := nil ;
    end ;

    Init ;

    SetupParams ;
    liStart := GetTickCount ;
    OpenQuery ;
    try
      liQueryTime := GetTickCount - liStart ;
      liStart := GetTickCount ;
      _ScanQuery ;
      LogQueryTiming( ClassName, liQueryTime, GetTickCount - liStart ) ;
    finally
      Query.Close ;
    end ;
  except
    on e:exception do
// ToDo: OID
      tiFmtException( e,
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString(TPerObjAbs( Visited ).OID) + Cr +
                      '  ObjectState:  ' + TPerObjAbs( Visited ).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName,
                      ClassName, 'Execute' )

  end ;

end;

procedure TVisOwnedQrySelectAbs.Init;
begin
  // Do nothing
end;

procedure TVisOwnedQrySelectAbs.MapRowToObject;
begin
  raise exception.create( 'MapRowToObject has not been ' +
                          'overridden in the concrete: ' + ClassName ) ;
end;

{ TVisOwnedQryUpdate }

procedure TVisOwnedQryUpdate.Execute(const pData: TVisitedAbs);
  procedure _ExecuteQuery ;
  var
    liStart : DWord ;
  begin
    try
      liStart := GetTickCount ;
      Query.ExecSQL ;
      LogQueryTiming( ClassName, GetTickCount - liStart, 0 ) ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_ExecuteQuery') ;
    end ;
  end ;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>

  try
    Inherited Execute( pData ) ;
    if not DoAcceptVisitor then
      exit ; //==>

    Init ;
    SetupParams ;
    _ExecuteQuery ;
  except
    on e:exception do
      tiFmtException( e,
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString(TPerObjAbs( Visited ).OID) + Cr +
                      '  ObjectState:  ' + TPerObjAbs( Visited ).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName,
                      ClassName, 'Execute' )
                      ;

  end ;

end;

procedure TVisOwnedQryUpdate.Init;
begin
  // Do nothing
end;

{ TVisOwnedQrySelect }

procedure TVisOwnedQrySelect.OpenQuery;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>
  try
    Query.Open ;
  except
    on e:exception do
      tiFmtException( e, ClassName, '_OpenQuery' ) ;
  end ;
end;

{ TVisDBIndependentRead }

constructor TVisDBIndependentRead.create;
begin
  inherited;
  FQueryParams := TtiQueryParams.Create ;
end;

destructor TVisDBIndependentRead.destroy;
begin
  FQueryParams.Free ;
  inherited;
end;

procedure TVisDBIndependentRead.OpenQuery;
begin
  Assert( FTableName <> '', 'TableName not assigned' ) ;
  Query.SelectRow( FTableName, FQueryParams ) ;
end;

end.



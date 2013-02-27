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

  Please submit changes to tiOPF@techinsite.com.au

  Revision History:
    July 2003, Peter Hinrichsen, Extracted from tiPntVisSQL

  Purpose:
    Apply SQL read from the database (with the SQLManager classes) to
    visitors

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }


{$I tiDefines.inc}

unit tiPtnVisSQLMgr;

interface
uses
   tiDBConnectionPool
  ,tiPtnVis
  ,SysUtils
  ,tiQuery
  ,Classes
  ,tiPtnVisPerObj
  ,tiPtnVisSQL
  ;

type

  TSQLMgrDatabaseMappings = class ;
  TSQLMgrDatabaseMapping  = class ;

  TSQLMgrDatabaseMappings = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TSQLMgrDatabaseMapping ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLMgrDatabaseMapping); reintroduce ;
  public
    property    Items[i:integer] : TSQLMgrDatabaseMapping read GetItems write SetItems ;
    procedure   Add( pObject : TSQLMgrDatabaseMapping   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    procedure   RegisterMapping( const pDatabaseName : string ; pSQLMgrDataSource : string ) ;
    procedure   UnRegisterMapping( const pDatabaseName : string ) ;
    function    FindSQLMgrDataSourceByDatabaseName( const pDatabaseName : string ) : string ;
  published
  end ;

  TSQLMgrDatabaseMapping  = class( TPerObjAbs )
  private
    FDatabaseName: string;
    FSQLMgrDataSource: string;
  protected
    function    GetOwner: TSQLMgrDatabaseMappings; reintroduce ;
    procedure   SetOwner(const Value: TSQLMgrDatabaseMappings); reintroduce ;
  public
    property    Owner       : TSQLMgrDatabaseMappings read GetOwner      write SetOwner ;
  published
    property SQLMgrDataSource : string read FSQLMgrDataSource write FSQLMgrDataSource ;
    property DatabaseName     : string read FDatabaseName     write FDatabaseName ;
  end ;

  // Uses the SQLManager to get the SQL
  //----------------------------------------------------------------------------
  TVisSQLMgrAbs = class( TtiPerObjVisitor )
  private
    FsQueryName      : string ;
    FbInitCalled     : boolean ;
    FbGetQueryCalled : boolean ;

  protected
    procedure   Init            ; virtual ;
    procedure   DoInit ;

    procedure   DoSetupParams ;
    procedure   DoGetQuery ;
    procedure   AssignSQLFromSQLManager( pQuery : TtiQuery ; pSQLMgrDataSource : string ; pQueryName : string ) ;

  public
    Constructor Create ; override ;
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
    property    QueryName : string read FsQueryName write FsQueryName ;

  end ;

  // Perhaps this could be replaced with a property on the TPerObjAbs which stops
  // the object state being set to diryt in the first place - or even better, just another
  // object state would do.
  TVisQryObjectStateDeleted = class( TVisSQLMgrAbs )
  protected
    function  AcceptVisitor  : boolean ; override ;
    procedure Init              ; override ;
    procedure SetupParams       ; override ;
    procedure Final             ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisQrySelect = class( TVisSQLMgrAbs )
  protected
    procedure MapRowToObject ; virtual ;
  public
    constructor Create ; override ;
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisQryUpdate = class( TVisSQLMgrAbs )
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisQryDelete = class( TVisQryUpdate )
  protected
    // Override this and check the visitor's class type
    function  AcceptVisitor : boolean ; override ;
    // Override this to set the query name
    // procedure Init              ; override ;
    // (We could set the table name using a macro param, but this will reduce
    //  the value of any compile time checking against the db structure, so it
    //  is not done.)
    procedure SetupParams       ; override ;
  end ;

function gSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings ;

implementation
uses
  tiUtils
  ,Dialogs
  ,tiLog
  ,Windows
  ,tiPersist
  ,tiSQLMgr_BOM
  ,tiPerObjOIDAbs
  ;

var
  uSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings ;

function gSQLMgrDatabaseMappings : TSQLMgrDatabaseMappings ;
begin
  if uSQLMgrDatabaseMappings = nil then
    uSQLMgrDatabaseMappings := TSQLMgrDatabaseMappings.Create ;
  result := uSQLMgrDatabaseMappings ;
end;
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisSQLMgrAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisQrySelect
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisQrySelect.Create;
begin
  inherited;
end;

procedure TVisQrySelect.Execute( const pData : TVisitedAbs ) ;
  procedure _OpenQuery ;
  begin
    if gTIPerMgr.Terminated then
      Exit ; //==>
    try
      Query.Open ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_OpenQuery' ) ;
    end ;
  end ;

  procedure _ScanQuery ;
  begin
    Query.ContinueScan := True;
    while ( not Query.EOF ) and
          ( Query.ContinueScan ) and
          ( not gTIPerMgr.Terminated ) do
    begin
      try
        MapRowToObject ;
      except
        on e:exception do
          tiFmtException( e, 'Error in MapRowToObject', ClassName, '_ScanQuery') ;
      end ;
      Query.Next ;
    end ;
    Query.Close ;
  end ;
var
  liStart : DWord ;
  liQueryTime : DWord ;
begin

  if gTIPerMgr.Terminated then Exit ; //==>
  try
    Inherited Execute( pData ) ;

    if not DoAcceptVisitor then
      Exit ; //==>

    Assert( Database <> nil, 'Database not set in ' + ClassName ) ;

    if pData <> nil then begin
      Visited := TPerObjAbs( pData ) ;
    end else begin
      Visited := nil ;
    end ;

    DoInit ;
    DoGetQuery ;
    DoSetupParams ;
    liStart := GetTickCount ;
    _OpenQuery ;
    try
      liQueryTime := GetTickCount - liStart ;
      liStart := GetTickCount ;
      _ScanQuery ;
      LogQueryTiming( QueryName, liQueryTime, GetTickCount-liStart ) ;
    finally
      Query.Close ;
    end ;
  except
    on e:exception do
      tiFmtException( e,
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString( TPerObjAbs( Visited ).OID) + Cr +
                      '  ObjectState:  ' + TPerObjAbs( Visited ).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName,
                      ClassName, 'Execute' )

  end ;

end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisQryUpdate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TVisQryUpdate.Execute(const  pData: TVisitedAbs);
  procedure _ExecuteQuery ;
  var
    liStart : DWord ;
  begin
    try
      liStart := GetTickCount ;
      Query.ExecSQL ;
      LogQueryTiming( QueryName, GetTickCount - liStart, 0 ) ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_ExecuteQuery') ;
    end ;
  end ;
begin
  try
    Inherited Execute( pData ) ;
    if not DoAcceptVisitor then
      exit ; //==>

    Init ;
    DoGetQuery ;
// Database Already Connected
//    Query.AttachDatabase( DBConnection.Database ) ;
//    try
      DoSetupParams ;
      _ExecuteQuery ;
//    finally
//      Query.DetachDatabase ;
//    end ;
  except
    on e:exception do
// ToDo: OID
      tiFmtException( e,
                      'Visited:        ' + Visited.ClassName + Cr +
                      '  OID:          ' + OIDToString(TPerObjAbs( Visited ).OID) + Cr +
                      '  ObjectState:  ' + TPerObjAbs( Visited ).ObjectStateAsString + Cr +
                      '  DatabaseName: ' + Database.DatabaseName,
                      ClassName, 'Execute' ) ;

  end ;

end;


procedure TVisSQLMgrAbs.Init;
begin
  raise exception.create( 'Init has not been ' +
                          'overridden in the concrete: ' + ClassName ) ;
end;

procedure TVisQrySelect.MapRowToObject;
begin
  raise exception.create( 'MapRowToObject has not been ' +
                          'overridden in the concrete: ' + ClassName ) ;
end;

constructor TVisSQLMgrAbs.Create;
begin
  inherited;
  FbInitCalled     := false ;
  FbGetQueryCalled := false ;
end;

procedure TVisSQLMgrAbs.Execute(const pData: TVisitedAbs);
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>

  inherited Execute( pData ) ;

  if gTIPerMgr.Terminated then
    Exit ; //==>

  Assert( pData <> nil,
          'Visited is nil' ) ;
  Assert( ( pData is TPerObjAbs ),
          'Visited is not a TPerObjAbs' + #13 +
          'Visited: ' + pData.ClassName + #13 +
         'Visitor: ' + ClassName + #13 ) ;
end;

procedure TVisSQLMgrAbs.DoInit;
begin
  if FbInitCalled then
    Exit ; //==>

  if gTIPerMgr.Terminated then
    Exit ; //==>
    
  try
    Init ;
    FbInitCalled := true ;
    Assert( QueryName <> '', 'QueryName unassigned.' ) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'Init' ) ;
  end ;
end;

procedure TVisSQLMgrAbs.DoSetupParams;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>

  try
    SetupParams ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DoSetupParams') ;
  end ;
end;

procedure TVisSQLMgrAbs.DoGetQuery;
var
  lSQLMgrDataSource : string ;
begin
  if FbGetQueryCalled then
    Exit ; //==>

  if gTIPerMgr.Terminated then
    Exit ; //==>

  try

    Assert( QueryName <> '', 'QueryName not assigned' ) ;
    Assert( VisitorController <> nil, 'VisitorMgr not assigned' ) ;
    Assert( VisitorController.SQLMgrDataSource <> '', 'SQLMgrDataSource not assigned' ) ;

    // This will fail in the tiAppLaunchWeb because it is linking in the
    // tiQueryHTTP directly, and there is no mapping between query factory and
    // db setup. Fix.
    
    lSQLMgrDataSource :=
      gSQLMgrDatabaseMappings.FindSQLMgrDataSourceByDatabaseName(
        VisitorController.DBConnectionName ) ;
        
    Assert( lSQLMgrDataSource <> '', 'SQLMgrDataSource not assigned.' ) ;

    AssignSQLFromSQLManager( Query, lSQLMgrDataSource, QueryName ) ;

    FbGetQueryCalled := true ;
    Assert( Query <> nil, 'Query not assigned' ) ;

  except
    on e:exception do
      tiFmtException( e, ClassName, 'DoGetQuery' ) ;
  end ;
  Assert( Database <> nil, 'DBConnection not set in ' + ClassName ) ;

end;

{ TVisQryObjectStateDeleted }

function TVisQryObjectStateDeleted.AcceptVisitor: boolean;
begin
  result := ( Visited is TPerObjAbs ) and
            ( TPerObjAbs( Visited ).ObjectState = posDelete ) ;
end;

procedure TVisQryObjectStateDeleted.Final;
begin
  if gTIPerMgr.Terminated then
    Exit ; //==>
  TPerObjAbs( Visited ).ObjectState := posDeleted ;
end;

procedure TVisQryObjectStateDeleted.Init;
begin
  // Do nothing
end;

procedure TVisQryObjectStateDeleted.SetupParams;
begin
  // Do nothing
end;

{ TVisQryDelete }

function TVisQryDelete.AcceptVisitor: boolean;
begin
  result := ( Visited.ObjectState = posDelete ) ;
end;

procedure TVisQryDelete.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := Visited.OID ;
  {$ELSE}
    Visited.OID.AssignToTIQuery( 'OID', Query ) ;
  {$ENDIF}
end;

{ TSQLMgrDatabaseMappings }

procedure TSQLMgrDatabaseMappings.Add(pObject: TSQLMgrDatabaseMapping; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TSQLMgrDatabaseMappings.FindSQLMgrDataSourceByDatabaseName(
  const pDatabaseName: string): string;
var
  lData : TSQLMgrDatabaseMapping ;
begin
  lData := TSQLMgrDatabaseMapping( FindByProps( ['DatabaseName'], [LowerCase(pDatabaseName)] )) ;
  Assert( lData <> nil,
          'Unable to find SQLMgrDatabaseMapping for <' +
          pDatabaseName + '>' ) ;
  result := lData.SQLMgrDataSource ;
end;

function TSQLMgrDatabaseMappings.GetItems( i: integer): TSQLMgrDatabaseMapping;
begin
  result := TSQLMgrDatabaseMapping( inherited GetItems( i ));
end;

procedure TSQLMgrDatabaseMappings.RegisterMapping(
  const pDatabaseName: string; pSQLMgrDataSource: string);
var
  lData : TSQLMgrDatabaseMapping ;
begin
  lData := TSQLMgrDatabaseMapping.Create ;
  lData.DatabaseName := LowerCase(pDatabaseName) ;
  lData.SQLMgrDataSource := LowerCase(pSQLMgrDataSource) ;
  Add( lData ) ;
end;

procedure TSQLMgrDatabaseMappings.SetItems(i: integer; const Value: TSQLMgrDatabaseMapping);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TSQLMgrDatabaseMappings.UnRegisterMapping( const pDatabaseName: string);
var
  lData : TSQLMgrDatabaseMapping ;
begin
  lData := TSQLMgrDatabaseMapping( FindByProps( ['DatabaseName'], [LowerCase(pDatabaseName)] )) ;
  if lData <> nil then
    Remove( lData ) ;
end;

{ TSQLMgrDatabaseMapping }

function TSQLMgrDatabaseMapping.GetOwner: TSQLMgrDatabaseMappings;
begin
  result := TSQLMgrDatabaseMappings( inherited GetOwner ) ;
end;

procedure TSQLMgrDatabaseMapping.SetOwner( const Value: TSQLMgrDatabaseMappings);
begin
  inherited SetOwner( Value ) ;
end;

procedure TVisSQLMgrAbs.AssignSQLFromSQLManager(pQuery: TtiQuery; pSQLMgrDataSource, pQueryName: string);
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrQuery   : TSQLMgrQuery ;
  lsl : TStringList ;
begin

  if gTIPerMgr.Terminated then
    Exit ; //==>

  lSQLMgr := gSQLMgrs.FindByDatabaseName( pSQLMgrDataSource ) ;
  if lSQLMgr = nil then
    tiFmtException( 'Unable to find SQLMgr for database <' +
                    pSQLMgrDataSource + '>',
                    ClassName, 'GetQuery' ) ;

  lSQLMgrQuery := lSQLMgr.FindCreateQueryByName( pQueryName ) ;

  if lSQLMgrQuery = nil then
    tiFmtException( 'Query <' +
                    pQueryName +
                   '> not found in query factory.',
                   ClassName,
                   'GetQuery' ) ;

  // This was added in an attempt to force SetSQL to be called.
  // (SetSQL might contain some custom code that will not execute
  //  when SQL.Text := bla is called.
  lsl := TStringList.Create ;
  try
    lsl.Text :=
    '/*' + lSQLMgrQuery.QueryName + '*/' + CrLf +
    lSQLMgrQuery.SQL ;
    pQuery.SQL := lsl ;
  finally
    lsl.Free ;
  end;

end;

initialization

finalization
  uSQLMgrDatabaseMappings.Free;

end.



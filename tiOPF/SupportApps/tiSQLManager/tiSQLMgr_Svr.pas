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

  Revision history:
    November 2000, Peter Hinrichsen, Made open source
    October 2001,  Ian Krigsman, UpperCase( ) added around:
                    FieldByName, Parms.ByName, SQL strings
                    due to some quirk in IBSQL that makes field access case sensitive
                    Also changed cusReadGroupPK string "name" to "group_name" due to
                    a reserved name error
    October 2001,  Ian Krigsman, UpperCase( ) added around:
                     Read SQL

    November 2001, Peter Hinrichsen, Refactored to use newly created
                   TVisOwnedQrySelect family of classes
    12 Nov 2001,   Peter Hinrichsen, Added conditional define 'LEGACY_SQLMGR' to
                   allow for change in structure of SQLMan_SQL table. Column
                   called SQL was renamed to Query_SQL to achieve compatability
                   with ADO access to a MDB database.
  Purpose:
    SQLMgr server side visitors.

  Classes:


  ToDo:

    1. Just like Delphi has design time packages (that contain property editors)
      and runtime packages (that don't have the property editors), we could divide
      these visitors up into design time (linked into the SQLManager) and runtime
      (linked into tiPersist). This would give a tiny performance gain and make
      the exe a byte or two smaller.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiSQLMgr_Svr;

interface
uses
   classes
  ,tiSQLMgr_BOM
  ,tiPtnVis
  ,tiPtnVisPerObj
  ,ComCtrls
  ,tiPtnVisSQL
  ,tiQuery
  ,tiPtnVisSQLMgr
  ;

type

  // Read Group primary key info
  //----------------------------------------------------------------------------
  TVisReadGroupPK = class( TVisDBIndependentRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read Query primary key info
  //----------------------------------------------------------------------------
  TVisReadQueryPK = class( TVisDBIndependentRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read a queries SQL by the Querie's name
  //----------------------------------------------------------------------------
  TVisReadQueryByName = class( TVisDBIndependentRead )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
    procedure  Final          ; override ;
  end ;

{
  // Read the details of a query
  //----------------------------------------------------------------------------
  TVisReadQueryDetail = class( TVisOwnedQrySelect )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Read the parameters for a given query
  //----------------------------------------------------------------------------
  TVisReadParams = class( TVisOwnedQrySelect )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  Init           ; override ;
    procedure  SetupParams    ; override ;
    procedure  MapRowToObject ; override ;
  end ;

  // Create (insert) a query
  //----------------------------------------------------------------------------
  TVisCreateQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a query
  //----------------------------------------------------------------------------
  TVisUpdateQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a query
  //----------------------------------------------------------------------------
  TVisDeleteQuery = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Create a group to hold a family of related queries
  //----------------------------------------------------------------------------
  TVisCreateGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a group (change its name or position)
  //----------------------------------------------------------------------------
  TVisUpdateGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a group
  //----------------------------------------------------------------------------
  TVisDeleteGroup = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Update a parameter
  //----------------------------------------------------------------------------
  TVisUpdateParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Create (insert) a parameter
  //----------------------------------------------------------------------------
  TVisCreateParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Delete a parameter
  //----------------------------------------------------------------------------
  TVisDeleteParam = class( TVisOwnedQryUpdate )
  protected
    function    AcceptVisitor : boolean ; override ;
    procedure   Init                    ; override ;
    procedure   SetupParams             ; override ;
  end ;

  // Mark the container of the parameters as deleted
  //----------------------------------------------------------------------------
  TVisDeleteParamContainer = class( TVisQryObjectStateDeleted )
  protected
    function AcceptVisitor : boolean ; override ;
  end ;
}

implementation
uses
  Dialogs   // ShowMessage, for debugging
  ,SysUtils // IntToStr
  ,Windows
  ,tiLog
  ,tiPersist
  ,tiUtils
  ,ctiPersist
  ;

// The SQL strings used to read and write for the query factory are as constants
// because they can't be read from the database until they have been read from
// the database (chickens and eggs).
// In the case of the read SQL, the order by clause is generally the display text.
// This was not the intention, but the GUI is not ready for order by sort_order to
// work.

{
const

  cusSQLMgrGroupReadPK =
//    '/* SQLMgrGroupReadPK */' +
    'select                            ' +
    '  disp_order        as disp_order ' +
    '  ,oid              as oid        ' +
    '  ,group_name       as group_name ' +   //ipk 4/10/01 Was "name", which is a reserved word
    'from                              ' +
    '  sqlman_group                    ' +
    'order by                          ' +
    '   disp_order                     '
    ;

  cusSQLMgrQueryReadPK =
//    '/* SQLMgrQueryReadPK */' +
    'select                              ' +
    '  disp_order        as disp_order   ' +
    '  ,group_oid        as group_oid    ' +
    '  ,oid              as oid          ' +
    '  ,query_name       as query_name   ' +
    '  ,test_include     as test_include ' +
    '  ,query_locked     as query_locked ' +
    'from                                ' +
    '   sqlman_sql   s                   ' +
    'order by                            ' +
    '   disp_order                     '
    ;

  cusSQLMgrQueryReadDetail =
//    '/* SQLMgrQueryReadDetail */' +
    'select                    ' +
    '  query_description       ' +
    '  ,&SQL_Col_Name as Query_SQL ' +
    'from                      ' +
    '   sqlman_sql             ' +
    'where                     ' +
    '   oid = :oid             '
    ;

  cusSQLMgrQueryReadByQueryName =
//    '/* cusSQLMgrQueryReadByQueryName */' +
    'select                    ' +
    '   group_oid              '+
    '  ,&SQL_Col_Name as Query_SQL ' +
    'from                      ' +
    '   sqlman_sql             ' +
    'where                     ' +
    '  query_name = :query_name'
    ;

  cusSQLMgrQueryCreate =
//    '/* SQLMgrQueryCreate */' +
    ' insert into sqlman_sql                 ' +
    ' ( oid, group_oid, disp_order, query_name, query_description, &SQL_Col_Name, query_locked, test_include )    ' +
    ' values                                 ' +
    ' ( :OID, :Group_OID, :disp_order, :Query_Name, :query_description, :Query_SQL, :query_locked, :test_include )'  ;

  cusSQLMgrQueryUpdate =
//    '/* SQLMgrQueryUpdate */' +
    'update sqlman_sql          ' +
    ' set                       ' +
    ' query_name = :query_name, ' +
    ' query_description = :query_description, ' +
    ' disp_order = :disp_order, ' +
    ' &SQL_Col_Name        = :Query_sql,        ' +
    ' group_oid  = :group_oid,  ' +
    ' query_locked  = :query_locked,   ' +
    ' test_include  = :test_include    ' +
    'where oid   = :oid         '  ;

  cusSQLMgrParamRead =
//    '/* SQLMgrParamRead */' +
    'select          ' +
    '   oid          ' +
    '  ,sql_oid      ' +
    '  ,disp_order   ' +
    '  ,param_name   ' +
    '  ,param_type   ' +
    '  ,param_value  ' +
    '  ,param_isnull ' +
    'from            ' +
    '   sqlman_param ' +
    'where           ' +
    '   sql_oid = :sql_oid ' +
    'order by        ' +
    '  disp_order   ' ;

  cusSQLMgrQueryDelete =
//    '/* SQLMgrQueryDelete */' +
    'delete from sqlman_sql     ' +
    'where oid   = :oid         ' ;

  cusSQLMgrGroupCreate =
//    '/* SQLMgrGroupCreate */' +
    'insert into sqlman_group        ' +
    '( oid, group_name, disp_order ) ' +
    'values                          ' +
    '( :oid, :group_name, :disp_order ) '  ;

  cusSQLMgrGroupUpdate =
//    '/* SQLMgrGroupUpdate */' +
    'update sqlman_group        ' +
    ' set                       ' +
    ' group_name = :group_name, ' +
    ' disp_order = :disp_order  ' +
    'where oid   = :oid         ' ;

  cusSQLMgrGroupDelete =
//    '/* SQLMgrGroupDelete */' +
    'delete from sqlman_group   ' +
    'where oid   = :oid         ' ;

  cusSQLMgrParamUpdate =
//    '/* SQLMgrParamUpdate */' +
    'update sqlman_param          ' +
    ' set                         ' +
    ' param_name = :param_name,   ' +
    ' disp_order = :disp_order,   ' +
    ' param_type = :param_type,   ' +
    ' param_value= :param_value,  ' +
    ' param_isnull= :param_isnull ' +
    'where oid   = :oid           ' ;

  cusSQLMgrParamCreate =
//    '/* SQLMgrParamCreate */' +
    'insert into sqlman_param ' +
    ' ( oid,          ' +
    '   sql_oid,      ' +
    '   param_name,   ' +
    '   disp_order,   ' +
    '   param_type,   ' +
    '   param_value,  ' +
    '   param_isnull )' +
    ' values          ' +
    ' ( :oid,          ' +
    '   :sql_oid,      ' +
    '   :param_name,   ' +
    '   :disp_order,   ' +
    '   :param_type,   ' +
    '   :param_value,  ' +
    '   :param_isnull )' ;

  cusSQLMgrParamDelete =
//    '/* SQLMgrParamDelete */' +
    'delete from sqlman_param ' +
    'where oid = :oid ' ;
}

(*
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisUpdateQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
function TVisUpdateQuery.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            ( Visited.ObjectState = posUpdate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisUpdateGroup
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
function TVisUpdateGroup.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrGroup ) and
            (( Visited as TPerObjAbs ).ObjectState = posUpdate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCreateGroup
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCreateGroup.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrGroup ) and
            ( Visited.ObjectState = posCreate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisDeleteGroup
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisDeleteGroup.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrGroup ) and
            ( Visited.ObjectState = posDelete ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisDeleteQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
function TVisDeleteQuery.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            ( Visited.ObjectState = posDelete ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCreateQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
function TVisCreateQuery.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            ( Visited.ObjectState = posCreate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisUpdateParam
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisUpdateParam.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrParam ) and
            ( Visited.ObjectState = posUpdate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCreateParam
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCreateParam.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrParam ) and
            ( Visited.ObjectState = posCreate ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisDeleteParam
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisDeleteParam.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrParam ) and
            (( Visited as TPerObjAbs ).ObjectState = posDelete ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisReadQueryDetail
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisReadQueryDetail.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            (( Visited as TPerObjAbs ).ObjectState = posPK ) ;
end;

procedure TVisReadQueryDetail.Init;
begin
  Query.SQLText :=
    UpperCase(
      tiStrTran( cusSQLMgrQueryReadDetail,
                 '&SQL_Col_Name',
                 cuSQLColName )) ;
end;

procedure TVisReadQueryDetail.MapRowToObject ;
var
  lData : TSQLMgrQuery ;
begin
  lData           := TSQLMgrQuery( Visited ) ;
  lData.QueryDesc := Query.FieldAsString[ 'query_description' ] ;
  lData.SQL       := Query.FieldAsString[ 'Query_SQL' ] ;
  lData.ObjectState := posClean ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisReadParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisReadParams.AcceptVisitor : boolean;
begin
  result := (  Visited  is TSQLMgrParams ) and
            (( Visited as TSQLMgrParams ).ObjectState = posEmpty ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisReadParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TVisReadParams.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamRead );
end;

procedure TVisReadParams.MapRowToObject ;
var
  lData : TSQLMgrParam ;
begin
  lData             := TSQLMgrParam.Create ;
  lData.ObjectState := posClean ;
  {$IFDEF OID_AS_INT64}
    lData.OID := Query.FieldAsInteger['OID'];
  {$ELSE}
    lData.OID.AssignFromTIQuery( 'OID', Query ) ;
  {$ENDIF}
  lData.DispOrder    := Query.FieldAsInteger[ 'Disp_Order' ]  ;
  lData.ParamName    := Query.FieldAsString[  'Param_Name' ]  ;
  lData.ParamTypeStr := Query.FieldAsString[  'Param_Type' ]  ;
  lData.ParamValue   := Query.FieldAsString[  'Param_Value' ] ;
  lData.IsNull       := Query.FieldAsBoolean[  'Param_IsNull' ] ;
  ( Visited as TSQLMgrParams ).Add( lData ) ;
end;
*)

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisReadGroupPK
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisReadGroupPK.AcceptVisitor : boolean;
begin
  result := ( Visited is TSQLMgr ) and
            ( Visited.ObjectState = posEmpty ) ;
end;

// -----------------------------------------------------------------------------
procedure TVisReadGroupPK.Init;
begin
  TableName := cTableNameSQLManGroup ;
end;

procedure TVisReadGroupPK.MapRowToObject ;
var
  lData : TSQLMgrGroup ;
begin

  // ToDo: Change all references to SQLManager field names to constancts

  lData             := TSQLMgrGroup.Create ;
  lData.ObjectState := posClean ;
  {$IFDEF OID_AS_INT64}
    lData.OID := Query.FieldAsInteger['OID'];
  {$ELSE}
    lData.OID.AssignFromTIQuery( Query ) ;
  {$ENDIF}
  lData.DispOrder   := Query.FieldAsInteger[ 'Disp_order' ] ;
  lData.GroupName   := Query.FieldAsString[  'group_name' ] ;
  ( Visited as TSQLMgr ).Add( lData ) ;
end;

function TVisReadQueryPK.AcceptVisitor: boolean;
begin
  result := (  Visited is TSQLMgr ) and
            (( Visited as TPerObjAbs ).ObjectState = posEmpty ) ;
end;

// -----------------------------------------------------------------------------
procedure TVisReadQueryPK.Init;
begin
  TableName := cTableNameSQLManSQL ;
end;

procedure TVisReadQueryPK.MapRowToObject;
var
  lSQLMgr : TSQLMgr ;
  lSQLMgrGroup : TSQLMgrGroup ;
  lData : TSQLMgrQuery ;
  i : integer ;
begin

  // ToDo: Change all references to SQLManager field names to constancts

  Assert( Visited.TestValid(TSQLMgr), cTIInvalidObjectError);
  lSQLMgr := ( Visited as TSQLMgr ) ;

  lData             := TSQLMgrQuery.Create ;
  lData.ObjectState := posPK ;
  {$IFDEF OID_AS_INT64}
    lData.OID := Query.FieldAsInteger['OID'];
  {$ELSE}
    lData.OID.AssignFromTIQuery( Query ) ;
  {$ENDIF}
  lData.DispOrder   := Query.FieldAsInteger[ 'Disp_order' ] ;
  lData.QueryName   := Query.FieldAsString[  'Query_name' ] ;
  lData.QueryLocked := Query.FieldAsBoolean[ 'Query_Locked' ];
  lData.TestInclude := Query.FieldAsBoolean[ 'Test_Include' ];

  // Now scan for the correct group to add the query to
  for i := 0 to lSQLMgr.Count - 1 do
  begin
    Assert( lSQLMgr.Items[i].TestValid(TSQLMgrGroup), cTIInvalidObjectError);
    lSQLMgrGroup := (lSQLMgr.Items[i] as TSQLMgrGroup );
  {$IFDEF OID_AS_INT64}
    if lSQLMgrGroup.OID = Query.FieldAsInteger['Group_OID'] then
    begin
      lSQLMgrGroup.Add( lData, false ) ;
      Break ; //==>
    end ;
  {$ELSE}
    if lSQLMgrGroup.OID.EqualsQueryField( 'Group_OID', Query ) then
    begin
      lSQLMgrGroup.Add( lData, false ) ;
      Break ; //==>
    end ;
  {$ENDIF}
  end ;
  Assert( lData.Owner <> nil, 'SQLMgrQuery.Owner = nil');
end;

(*
procedure TVisReadQueryDetail.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := ( Visited as TPerObjAbs ).OID ;
  {$ELSE}
    ( Visited as TPerObjAbs ).OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisReadParams.SetupParams;
begin
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['SQL_OID'] := ( Visited as TSQLMgrParams ).Owner.OID ;
  {$ELSE}
    ( Visited as TSQLMgrParams ).Owner.OID.AssignToTIQuery( 'SQL_OID', Query ) ;
  {$ENDIF}
end;

{ TVisDeleteParamContainer }

function TVisDeleteParamContainer.AcceptVisitor: boolean;
begin
  result := ( inherited AcceptVisitor ) and
            ( Visited is TSQLMgrParams ) ;
end;

procedure TVisCreateQuery.Init;
begin
  Query.SQLText :=
    UpperCase(
      tiStrTran( cusSQLMgrQueryCreate,
                 '&SQL_Col_Name',
                 cuSQLColName )) ;
end;

procedure TVisCreateQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  Query.ParamAsString[  'query_name'        ] := lData.QueryName ;
  Query.ParamAsString[  'Query_description' ] := lData.QueryDesc ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'               ] := lData.OID ;
    Query.ParamAsInteger[ 'Group_oid'         ] := lData.Owner.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
    lData.Owner.OID.AssignToTIQuery( 'Group_oid', Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ 'Disp_Order'        ] := lData.DispOrder ;
  Query.ParamAsBoolean[ 'Query_Locked'      ] := lData.QueryLocked;
  Query.ParamAsBoolean[ 'Test_Include'      ] := lData.TestInclude;
  //Query.ParamAsTextBLOB[ 'Query_SQL'              ] := lData.SQL ;
  Query.ParamAsString[ 'Query_SQL'              ] := lData.SQL ;
end;

procedure TVisUpdateQuery.Init;
begin
  Query.SQLText :=
    UpperCase(
      tiStrTran( cusSQLMgrQueryUpdate,
                 '&SQL_Col_Name',
                 cuSQLColName )) ;
end;

procedure TVisUpdateQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  Query.ParamAsString[   'query_name'        ] := lData.QueryName ;
  Query.ParamAsString[   'query_description' ] := lData.QueryDesc ;
  Query.ParamAsInteger[  'disp_order' ]        := lData.DispOrder ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[  'group_oid'  ]        := lData.Owner.OID ;
    Query.ParamAsInteger[  'oid' ]               := lData.OID ;
  {$ELSE}
    lData.Owner.OID.AssignToTIQuery( 'group_oid', Query ) ;
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}

  Query.ParamAsBoolean[  'query_locked'  ]     := lData.QueryLocked ;
  Query.ParamAsBoolean[  'Test_Include'  ]     := lData.TestInclude ;
  //Query.ParamAsTextBlob[ 'Query_SQL' ]         := lData.SQL ;
  Query.ParamAsString[ 'Query_SQL' ]         := lData.SQL ;
end;

procedure TVisDeleteQuery.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrQueryDelete ) ;
end;

procedure TVisDeleteQuery.SetupParams;
var
  lData : TSQLMgrQuery ;
begin
  lData := TSQLMgrQuery( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisCreateGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupCreate ) ;
end;

procedure TVisCreateGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsString[   cFieldNameGroupName ] := lData.GroupName ;
  Query.ParamAsInteger[  cFieldNameGroupDispOrder  ] := lData.DispOrder ;
end;

procedure TVisUpdateGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupUpdate ) ;
end;

procedure TVisUpdateGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  Query.ParamAsString[  'group_Name' ] := lData.GroupName ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'        ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ 'disp_order' ] := lData.DispOrder ;
end;

procedure TVisDeleteGroup.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrGroupDelete ) ;
end;

procedure TVisDeleteGroup.SetupParams;
var
  lData : TSQLMgrGroup ;
begin
  lData := TSQLMgrGroup( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[  'oid' ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

procedure TVisUpdateParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamUpdate ) ;
end;

procedure TVisUpdateParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'oid'     ] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
  Query.ParamAsInteger[ cFieldNameParamDispOrder ] := lData.DispOrder ;
  Query.ParamAsString[ 'param_name'   ] := lData.ParamName ;
  Query.ParamAsString[ 'param_type'   ] := lData.ParamTypeStr;
  Query.ParamAsString[ 'param_value'  ] := lData.ParamValue ;
  Query.ParamAsBoolean[ 'param_isnull'] := lData.IsNull;
end;

procedure TVisCreateParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamCreate ) ;
end;

procedure TVisCreateParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger[ 'sql_oid' ]   := lData.Owner.Owner.OID  ;
    Query.ParamAsInteger[ 'oid'     ]   := lData.OID  ;
  {$ELSE}
    lData.Owner.Owner.OID.AssignToTIQuery( 'SQL_OID', Query ) ;
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}

  Query.ParamAsInteger[ cFieldNameParamDispOrder ] := lData.DispOrder ;
  Query.ParamAsString[  'param_name'  ]   := lData.ParamName  ;
  Query.ParamAsString[  'param_type'  ]   := lData.ParamTypeStr  ;
  Query.ParamAsString[  'param_value' ]   := lData.ParamValue ;
  Query.ParamAsBoolean[  'param_isnull' ] := lData.IsNull ;
end;

procedure TVisDeleteParam.Init;
begin
  Query.SQLText := UpperCase( cusSQLMgrParamDelete ) ;
end;

procedure TVisDeleteParam.SetupParams;
var
  lData : TSQLMgrParam ;
begin
  lData := TSQLMgrParam( Visited ) ;
  {$IFDEF OID_AS_INT64}
    Query.ParamAsInteger['OID'] := lData.OID ;
  {$ELSE}
    lData.OID.AssignToTIQuery( Query ) ;
  {$ENDIF}
end;

{ TVisReadQueryByName }
*)

function TVisReadQueryByName.AcceptVisitor: boolean;
begin
  result := ( Visited is TSQLMgrQuery ) and
            (( Visited as TPerObjAbs ).ObjectState = posEmpty ) ;
end;

procedure TVisReadQueryByName.Final;
begin
  // Do nothing. ObjectState is set in MapRowToObject;
end;

procedure TVisReadQueryByName.Init;
begin
  TableName := cTableNameSQLManSQL ;
end;

procedure TVisReadQueryByName.MapRowToObject;
var
  lData : TSQLMgrQuery ;
begin
  lData           := TSQLMgrQuery( Visited ) ;
  lData.SQL       := Query.FieldAsString[ cFieldNameSQLSQL ] ;
  lData.ObjectState := posClean ;
end;

procedure TVisReadQueryByName.SetupParams;
begin
  QueryParams.SetValueAsString(cFieldNameSQLName,
                               ( Visited as TSQLMgrQuery ).QueryName );
end;


initialization


{
  // Register visitors with the VisitorCache
  gTIPerMgr.RegReadPKVisitor( TVisReadGroupPK ) ;
  gTIPerMgr.RegReadPKVisitor( TVisReadQueryPK ) ;

  gTIPerMgr.RegReadVisitor( TVisReadQueryDetail ) ;
  gTIPerMgr.RegReadVisitor( TVisReadParams ) ;

  // The order of these is important:
  // Child nodes must be deleted before parents,
  // Parent nodes must be added before children.
  gTIPerMgr.RegSaveVisitor( TVisDeleteParam ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteParamContainer ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisDeleteGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateParam ) ;
  gTIPerMgr.RegSaveVisitor( TVisUpdateGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateGroup ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateQuery ) ;
  gTIPerMgr.RegSaveVisitor( TVisCreateParam ) ;

}

  gTIPerMgr.VisMgr.RegisterVisitor(cVisSQLMgrReadByQueryName, TVisReadQueryByName);
  gTIPerMgr.VisMgr.RegisterVisitor(cVisSQLMgrReadPK, TVisReadGroupPK);
  gTIPerMgr.VisMgr.RegisterVisitor(cVisSQLMgrReadPK, TVisReadQueryPK);




end.



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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{

  Should OID be a published property?
    It would make it a lot easier here if it was because where ever OID is
    referenced, there must be an if then block to extract it in code.

    An alternative would be to have a helper function on the TPerObjAbs which
    could be used to get or set any value. This could use RTTI if the property
    is published, or hard code if not.

    This might be the tidiest solution as it would reduce our dependency on
    RTTI and published properties.

  ToDo:
    1. Give control over sort order after a read (perhaps by the PK?)
    2. Set the objectState of the top object after a save.

}


{$I tiDefines.inc}

unit tiClassToDBMap_Srv;

interface
uses
  tiPtnVisPerObj
  ,tiPtnVisSQL
  ,tiPtnVis
  ,Classes
  ,tiQuery
  ,tiClassToDBMap_BOM
  ;

type

  TVisAutoAbs = class( TtiPerObjVisitor )
  protected
    FWhereAttrColMaps : TtiAttrColMaps ;
    FAttrColMaps : TtiAttrColMaps ;
    FWhere  : TtiQueryParams ;
    FParams : TtiQueryParams ;
    FVisitedClassType : TPerObjAbsClass ;
    procedure AddToParams( const pParams : TtiQueryParams ;
                           const pAttrColMaps : TtiAttrColMaps ;
                           const pData : TPerObjAbs ) ; virtual ;
    procedure QueryResultToObject( const pTarget : TPerObjAbs ; const pAttrColMaps : TtiAttrColMaps ) ;
  protected
    procedure GetWhereAttrColMaps ; virtual ; abstract ;
    procedure GetAttrColMaps ; virtual ; abstract ;
    procedure SetupParams; override ;
    function  ParamsToString( const pParams : TtiQueryParams ) : string ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
    constructor Create ; override ;
    destructor  Destroy ; override ;
  end ;

  TVisAutoReadThis = class( TVisAutoAbs )
  protected
    FSetObjectState : boolean ;
    procedure   GetWhereAttrColMaps ; override ;
    procedure   GetAttrColMaps ; override ;
    function    AcceptVisitor : boolean ; override ;
    procedure   MapRowToObject ;
    procedure   DoExecute ;
    procedure   Final ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisAutoCollectionRead = class( TVisAutoAbs )
  private
    FClassDBCollection : TtiClassDBCollection ;
    FClassToCreate : TPerObjAbsClass ;
    FHasParent: Boolean ;
    FClassesWithParent : TList ;
//    procedure PopulateIfChildClasses;
    procedure ReadDataForParentClass(pCollection: TtiClassDBCollection);
    procedure ReadDataForChildClasses(pCollection: TtiClassDBCollection);

  protected
    FSetObjectState : boolean ;
    procedure   GetWhereAttrColMaps ; override ;
    procedure   GetAttrColMaps ; override ;
    function    AcceptVisitor : boolean ; override ;
    procedure   MapRowToObject( pCheckForDuplicates : boolean ) ;
    procedure   SetContinueVisiting ; virtual ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisAutoCollectionPKRead = class( TVisAutoCollectionRead )
  protected
    procedure   GetAttrColMaps ; override ;
    procedure   SetContinueVisiting ; override ;
  end ;

  TVisAutoUpdateAbs = class( TVisAutoAbs )
  protected
    procedure  GetWhereAttrColMaps ; override ;
    procedure  GetAttrColMaps ; override ;
    procedure  DoExecuteQuery ; virtual ; abstract ;
  public
    procedure  Execute( const pData : TVisitedAbs ) ; override ;
  end ;

  TVisAutoDelete = class( TVisAutoUpdateAbs )
  protected
    procedure  GetAttrColMaps ; override ;
    function   AcceptVisitor : boolean ; override ;
    procedure  DoExecuteQuery ; override ;
  public
    constructor Create ; override ;
  end ;

  TVisAutoUpdate = class( TVisAutoUpdateAbs )
  protected
    function   AcceptVisitor : boolean ; override ;
    procedure  DoExecuteQuery ; override ;
  end ;

  TVisAutoCreate = class( TVisAutoUpdateAbs )
  protected
    procedure GetWhereAttrColMaps ; override ;
    procedure GetAttrColMaps ; override ;
    function  AcceptVisitor : boolean ; override ;
    procedure DoExecuteQuery ; override ;
  end ;

implementation
uses
  SysUtils
  ,tiPersist
  ,tiLog
  ,tiUtils
  ,TypInfo
  ,tiDialogs
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDAbs
  ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisAutoAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TVisAutoAbs.AddToParams( const pParams : TtiQueryParams ; const pAttrColMaps : TtiAttrColMaps ; const pData : TPerObjAbs ) ;
  procedure _SetOIDParam( const pParams : TtiQueryParams ; const pData : TPerObjAbs ; const pColName : string ; const pPropName : string ) ;
  begin
    {$IFDEF OID_AS_INT64}
      if Pos( 'OWNER', UpperCase( pPropName )) <> 0 then
      begin
        // If we are calling create (or update?) then we will want to set Owner.OID
        if Self is TVisAutoUpdateAbs then
        begin
          Assert( pData.Owner <> nil, 'Attempting to read a collection but the collections''s Visited.Owner is not assigned' ) ;
          pParams.SetValueAsInteger(pColName, pData.Owner.OID)
        end else
          pParams.SetValueAsInteger(pColName, pData.OID)
      end else
        pParams.SetValueAsInteger(pColName, pData.OID);
    {$ELSE}
      if Pos( 'OWNER', UpperCase( pPropName )) <> 0 then
      begin
        // If we are calling create (or update?) then we will want to set Owner.OID
        if Self is TVisAutoUpdateAbs then
        begin
          Assert( pData.Owner <> nil, 'Attempting to read a collection but the collections''s Visited.Owner is not assigned' ) ;
          pData.Owner.OID.AssignToTIQueryParam(pColName, pParams);
        end else
          pData.OID.AssignToTIQueryParam(pColName, pParams);
      end else
        pData.OID.AssignToTIQueryParam(pColName, pParams);
    {$ENDIF}
  end;
var
  lAttrColMap : TtiAttrColMap ;
  i : integer ;
  lColName : string ;
  lPropName : string ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  pParams.Clear ;
  for i := 0 to pAttrColMaps.Count - 1 do
  begin
    lAttrColMap := pAttrColMaps.Items[i] ;
    lColName  := lAttrColMap.DBColMap.ColName ;
    lPropName := lAttrColMap.AttrMap.AttrName ;
    if  ( Pos( 'OID', UpperCase(lPropName)) <> 0 )
    and ( Pos( '_OID', UpperCase(lPropName)) = 0 ) then
      _SetOIDParam( pParams, pData, lColName, lPropName )
    else
      pParams.SetValueFromProp( pData, lPropName, lColName ) ;
  end ;
end;

constructor TVisAutoAbs.Create;
begin
  inherited;
  FWhereAttrColMaps := TtiAttrColMaps.Create ;
  FWhereAttrColMaps.OwnsObjects := false ;
  FWhereAttrColMaps.AutoSetItemOwner := false ;
  FWhere  := TtiQueryParams.Create ;

  FAttrColMaps := TtiAttrColMaps.Create ;
  FAttrColMaps.OwnsObjects := false ;
  FAttrColMaps.AutoSetItemOwner := false ;
  FParams := TtiQueryParams.Create ;

end;

destructor TVisAutoAbs.Destroy;
begin
  FWhereAttrColMaps.Free ;
  FWhere.Free;
  FParams.Free;
  FAttrColMaps.Free;
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *// *
// * TVisAutoCollectionRead
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAutoCollectionRead.AcceptVisitor: boolean;
begin
  result :=
    ((Visited.ObjectState = posEmpty) or
     (Visited.ObjectState = posPK)) and
    (gTIPerMgr.ClassDBMappingMgr.Collections.IsCollection(
     TPerObjAbsClass(Visited.ClassType))) ;
end;

procedure TVisAutoCollectionRead.Execute(const pData: TVisitedAbs);
var
  lCollections : TList ;
  i : integer ;
begin
  inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  FSetObjectState := false ;
  FClassesWithParent.Clear ;

  try
// Attached by the VisitorCtrlr
//    Query.AttachDatabase( DBConnection.Database ) ;
//    try
      // It might be a collection of several different classes, so
      // we have to read once for each possible class type
      lCollections := TList.Create ;
      try
        lCollections.Clear ;
        gTIPerMgr.ClassDBMappingMgr.Collections.FindByCollection(
          FVisitedClassType,
          lCollections ) ;

        ReadDataForParentClass( lCollections.Items[0] );
        for i := 1 to lCollections.Count - 1 do
          ReadDataForChildClasses( lCollections.Items[i] ) ;

      finally
        lCollections.Free ;
      end ;
//    finally
//      Query.DetachDatabase ;
//    end ;

//    PopulateIfChildClasses ;

  except
    on e:exception do
      tiFmtException( e, ClassName, 'Execute' ) ;
  end ;
  SetContinueVisiting ;
end;

procedure TVisAutoCollectionRead.ReadDataForParentClass( pCollection : TtiClassDBCollection ) ;
var
  lTableName : string ;
  lParams : string ;
begin
  FClassDBCollection := pCollection ;
  SetupParams;
  lTableName := FWhereAttrColMaps.TableName ;
  lParams := ParamsToString( FWhere ) ;
  LogArray([ClassName + '.ReadDataForParentClass', lTableName, lParams ]);
  Query.SelectRow( FAttrColMaps.TableName, FWhere ) ;
  while not Query.EOF do
  begin
    MapRowToObject( false ) ;
    Query.Next ;
  end ;
  Query.Close ;
end ;

procedure TVisAutoCollectionRead.ReadDataForChildClasses( pCollection : TtiClassDBCollection ) ;

  procedure _GetWhereAttrColMaps( const pData : TPerObjAbs ) ;
  var
    i : integer ;
  begin
    Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;

    gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
      TPerObjAbsClass( FClassDBCollection.PerObjAbsClass ), FWhereAttrColMaps ) ;

    // Remove any mappings that are not foreign key mappings
    for i := FWhereAttrColMaps.Count - 1 downto 0 do
      if ( not ( pktFK in FWhereAttrColMaps.Items[i].DBColMap.PKInfo )) then
        FWhereAttrColMaps.Delete(i);

    AddToParams( FWhere, FWhereAttrColMaps, pData ) ;

  end ;

var
  i : integer ;
  lCount : integer ;
  lTableName : string ;
begin
  FClassDBCollection := pCollection ;
  GetAttrColMaps ;
  for i := 0 to TPerObjList(Visited).Count - 1 do
  begin
    _GetWhereAttrColMaps(TPerObjList(Visited).Items[i]) ;
    lTableName := FWhereAttrColMaps.TableName ;
    Assert(lTableName <> '', 'Unable to find table name. FWhereAttrColMaps.Count = '
           + IntToStr(FWhereAttrColMaps.Count) + '. Suspect a missing [pktFK] value ' 
           + 'in the child classes RegisterMapping calls.' );
    LogArray([ClassName + '.ReadDataForChildClasses', lTableName, ParamsToString( FWhere ) ]);
    Query.SelectRow( lTableName, FWhere ) ;
    lCount := 0 ;
    while not Query.EOF do
    begin
      MapRowToObject( true ) ;
      Query.Next ;
      Inc( lCount ) ;
    end ;
    Query.Close ;
    Assert( lCount <= 1, 'Query returned rowcount > 1 it was ' + IntToStr( lCount )) ;
  end ;
end ;


{
procedure TVisAutoCollectionRead.PopulateIfChildClasses ;
var
  i : integer ;
  lVisAutoReadThis : TVisAutoReadThis ;
begin
  // If a class has a registered parent, then only its PK info will
  // have been read. This will read remaining data on these classes.
  // It was tempting to just call ReadThis on each object in the list, but
  // that caused another database connection to be created which failed if
  // the dbConnectionPool.MaxCount value was 1
  // (as it is for the XML persistence layer. )
  if FClassesWithParent.Count <> 0 then
  begin
    lVisAutoReadThis := TVisAutoReadThis.Create ;
    try
      lVisAutoReadThis.DBConnection := Self.DBConnection ;
      for i := 0 to FClassesWithParent.Count - 1 do
        TPerObjAbs( FClassesWithParent.Items[i] ).Iterate( lVisAutoReadThis ) ;

      // Force Final to be called (calling Final is usually handled by the visitor manager)
      for i := 0 to FClassesWithParent.Count - 1 do
      begin
        lVisAutoReadThis.Visited := TPerObjAbs( FClassesWithParent.Items[i] ) ;
        lVisAutoReadThis.Final ;
      end ;

    finally
      lVisAutoReadThis.Free ;
    end ;
  end ;
end ;
}

procedure TVisAutoCollectionRead.MapRowToObject( pCheckForDuplicates : boolean ) ;
  function _DoesOwnObjects( pData : TPerObjAbs ) : Boolean ;
  var
    lList : TList ;
    i : integer ;
  begin
    result := ( pData.PropCount( [tkClass] ) > 0 ) ;
    if not result then
      Exit ; //==>
    result := false ;
    lList := TList.Create ;
    try
      pData.FindAllByClassType( TPerObjAbs, lList ) ;
      for i := 0 to lList.Count - 1 do
        if TPerObjAbs( lList.Items[i] ).Owner = pData then
        begin
          result := true ;
          Break ; //==>
        end ;
    finally
      lList.Free ;
    end ;
  end ;

  function _DuplicateObject( var pIndex : integer ) : boolean ;
  var
    lData : TPerObjAbs ;
    lOID : TOID ;
    lPKColName : string ;
    i : integer ;
  begin

    lPKColName := '' ;
    for i := 0 to FAttrColMaps.Count - 1 do
      if ( pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo ) then
      begin
        lPKColName := FAttrColMaps.Items[i].DBColMap.ColName;
        Break ; //==>
      end ;

    Assert( lPKColName <> '', 'Can not determine primary key column. FAttrColMaps.Count <> 1' ) ;
    {$IFDEF OID_AS_INT64}
      lOID := Query.FieldAsInteger[lPKColName];
      lData := TPerObjList( Visited ).Find(lOID) ;
      result := ( lData <> nil ) ;
      if result then
        pIndex := TPerObjList( Visited ).IndexOf(lData)
      else
        pIndex := -1 ;
    {$ELSE}
      lOID := gTIPerMgr.OIDFactory.CreateOID ;
      try
        lOID.AssignFromTIQuery(lPKColName, Query);
        lData := TPerObjList( Visited ).Find(lOID) ;
        result := ( lData <> nil ) ;
        if result then
          pIndex := TPerObjList( Visited ).IndexOf(lData)
        else
          pIndex := -1 ;
      finally
        lOID.Free ;
      end;
    {$ENDIF}
  end ;

var
  lDataOld : TPerObjAbs ;
  lDataNew : TPerObjAbs ;
  lIndex   : integer ;
  lList    : TPerObjList ;
begin
  if FAttrColMaps.Count = 0 then
    Exit ; //==>

  try

    // If we are working with a collection of objects of different types, there
    // is a chance that the object will be read more than once from more than one
    // table. For example, if we have a parent and child object, which are
    // persisted accross two tables, and the parent object is a valid object.
    // Say there are some entries in the parent table only, and some in both
    // the parent and child tables. The parent table will be read first so
    // an instance of the abstract class will be created. Next the child table
    // will be read and if there is a record there, then the first object will
    // be of the wrong type. To fix this, the original instance is removed and
    // a new copy created.
    lList := TPerObjList( Visited ) ;
    if pCheckForDuplicates and _DuplicateObject(lIndex) then
    begin
      lDataOld := lList.Items[lIndex] ;
      lDataNew := FClassToCreate.Create ;
      lDataNew.Assign(lDataOld);
      lIndex   := lList.IndexOf(lDataOld) ;
      lList.Insert(lIndex, lDataNew );
      lList.Remove(lDataOld);
    end else
    begin
      lDataNew  := FClassToCreate.Create ;
      lList.Add( lDataNew ) ;
    end ;

    QueryResultToObject( lDataNew, FAttrColMaps ) ;

    // Can do better than this rather messy call
    // (If its a collection, or it owns objects - not totally reliable
    if ((gTIPerMgr.ClassDBMappingMgr.Collections.IsCollection(FClassToCreate)) or
        ( _DoesOwnObjects( lDataNew ))) {or
        ( FHasParent )} then
      lDataNew.ObjectState := posPK
    else
      lDataNew.ObjectState := posClean ;

    if FHasParent then
      FClassesWithParent.Add( lDataNew ) ;

  except
    on e:exception do
      tiFmtException( e, ClassName, 'MapRowToObject' ) ;
  end ;

end;

{ TVisAutoUpdateAbs }

procedure TVisAutoUpdateAbs.Execute(const pData: TVisitedAbs);
var
  lClassMaps : TtiClassMaps ;
  i : integer ;
begin
  inherited Execute( pData ) ;
  if not AcceptVisitor then
    Exit ; //==>

// Attached by the VisitorCtrlr
//  Query.AttachDatabase( DBConnection.Database ) ;
//  try
    lClassMaps := TtiClassMaps.Create ;
    try
      lClassMaps.OwnsObjects := false ;
      gTIPerMgr.ClassDBMappingMgr.ClassMaps.FindAllParents(
        FVisitedClassType, lClassMaps ) ;
      // For Create and Update
      if IterateDirection = vidTopDown then
      begin
        for i := 0 to lClassMaps.Count - 1 do
        begin
          FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass ;
          SetupParams ;
          DoExecuteQuery ;
        end ;
      end
      else
      // For Delete
      begin
        for i := lClassMaps.Count - 1 downto 0 do
        begin
          FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass ;
          SetupParams ;
          DoExecuteQuery ;
        end ;
      end ;
    finally
      lClassMaps.Free;
    end ;
//  finally
//    Query.DetachDatabase ;
//  end ;
end;

procedure TVisAutoUpdateAbs.GetAttrColMaps;
var
  i : integer ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps ) ;
    
  // Remove any mappings that are primary key mappings
  for i := FAttrColMaps.Count - 1 downto 0 do
    if (( pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FAttrColMaps.Delete(i);

  // Remove any mappings that are foreign key mappings
  for i := FAttrColMaps.Count - 1 downto 0 do
    if (( pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FAttrColMaps.Delete(i);

  AddToParams( FParams, FAttrColMaps, Visited ) ;
end;

procedure TVisAutoUpdateAbs.GetWhereAttrColMaps;
var
  i : integer ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FWhereAttrColMaps ) ;
  // Remove any mappings that are not primary key mappings
  for i := FWhereAttrColMaps.Count - 1 downto 0 do
    if ( not ( pktDB in FWhereAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FWhereAttrColMaps.Delete(i);
  AddToParams( FWhere,  FWhereAttrColMaps, Visited ) ;
end;

{ TVisAutoDelete }

function TVisAutoDelete.AcceptVisitor: boolean;
begin
  result := ( Visited.ObjectState = posDelete ) and
            ( gTIPerMgr.ClassDBMappingMgr.ClassMaps.IsClassReg(
              TPerObjAbsClass(Visited.ClassType))) ;
end;

constructor TVisAutoDelete.Create;
begin
  inherited;
  IterateDirection := vidBottomUp ;
end;

procedure TVisAutoDelete.DoExecuteQuery;
begin
  Query.DeleteRow( FWhereAttrColMaps.TableName, FWhere ) ;
end;

procedure TVisAutoDelete.GetAttrColMaps;
begin
  // For delete, we hav no AttrColMaps
end;

{ TVisAutoUpdate }

function TVisAutoUpdate.AcceptVisitor: boolean;
begin
  result := ( Visited.ObjectState = posUpdate ) and
            ( gTIPerMgr.ClassDBMappingMgr.ClassMaps.IsClassReg(
              TPerObjAbsClass(Visited.ClassType))) ;
end;

{
procedure TVisAutoUpdate.DoSetupQuery;
begin
  Query.SetupForUpdate( FAttrColMaps, nil ) ;
end;
}

procedure TVisAutoUpdate.DoExecuteQuery;
begin
  Query.UpdateRow( FWhereAttrColMaps.TableName, FParams, FWhere ) ;
end;

{ TVisAutoCreate }

function TVisAutoCreate.AcceptVisitor: boolean;
begin
  result := ( Visited.ObjectState = posCreate ) and
            ( gTIPerMgr.ClassDBMappingMgr.ClassMaps.IsClassReg(
              TPerObjAbsClass(Visited.ClassType))) ;
end;

procedure TVisAutoCreate.DoExecuteQuery;
begin
  try
    Query.InsertRow( FAttrColMaps.TableName, FParams ) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DoExecuteQuery' ) ;
  end ;
end;

procedure TVisAutoCreate.GetAttrColMaps;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps ) ;
 AddToParams( FParams, FAttrColMaps, Visited ) ;
end;

procedure TVisAutoCreate.GetWhereAttrColMaps;
begin
  // Do nothing
end;

{ TVisAutoThisRead }

function TVisAutoReadThis.AcceptVisitor: boolean;
begin
  result :=
    ((Visited.ObjectState = posEmpty) or
     (Visited.ObjectState = posPK)) and
    ( gTIPerMgr.ClassDBMappingMgr.ClassMaps.IsClassReg(TPerObjAbsClass(Visited.ClassType))) ;
end;

procedure TVisAutoReadThis.DoExecute;
var
  lCount : integer ;
  lTableName : string ;
begin
  SetupParams;
  lTableName := FWhereAttrColMaps.TableName ;
  LogArray([ClassName + '.DoExecute', lTableName, ParamsToString( FWhere )]);
  Query.SelectRow( lTableName, FWhere ) ;
  try
    lCount := 0 ;
    while not Query.EOF do
    begin
      if lCount>0 then
        raise exception.create( 'Query returned more than one row');
      MapRowToObject ;
      Query.Next ;
      Inc(lCount);
    end ;
  finally
    Query.Close ;
  end ;
end;

procedure TVisAutoReadThis.Execute(const pData: TVisitedAbs);
var
  lClassMaps : TtiClassMaps ;
  i : integer ;
begin
  inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  FSetObjectState := false ;

  try
// Attached by the VisitorCtrlr
//    Query.AttachDatabase( DBConnection.Database ) ;
//    try
      lClassMaps := TtiClassMaps.Create ;
      try
        lClassMaps.OwnsObjects := false ;
        gTIPerMgr.ClassDBMappingMgr.ClassMaps.FindAllParents(
        FVisitedClassType, lClassMaps ) ;
        for i := 0 to lClassMaps.Count - 1 do
        begin
          FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass ;
          DoExecute ;
        end ;
      finally
        lClassMaps.Free ;
      end ;
//    finally
//      Query.DetachDatabase ;
//    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'Execute' ) ;
  end ;
  // If we are going to check object state in AcceptVisitor, then we will have to set
  // it in a reliable way after visiting. At the moment, it is set to
  // posClean or posDeleted in TVisPerObjAwareAbs. It will have to set it to
  // posPK if the object being added is itself a collection.
  // The check in AcceptVisitor for posEmpty will have to be extended to allow
  // for posPK too.
end;

procedure TVisAutoReadThis.Final;
begin
  if FSetObjectState then
  begin
    // Just a double check, the same as AcceptVisitor
    Assert((Visited.ObjectState = posEmpty) or
           (Visited.ObjectState = posPK),
           'Object state on ' + Visited.ClassName +
           ' not posEmpty or posPK it''s ' +
          Visited.ObjectStateAsString ) ;
    if (gTIPerMgr.ClassDBMappingMgr.Collections.IsCollection(
       TPerObjAbsClass(Visited.ClassType))) then
      Visited.ObjectState := posPK
    else
      Visited.ObjectState := posClean ;
  end ;
end;

procedure TVisAutoReadThis.GetAttrColMaps;
var
  i : integer ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps ) ;
  for i := FAttrColMaps.Count - 1 downto 0 do
    if (( pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FAttrColMaps.Delete(i);
end;

procedure TVisAutoReadThis.GetWhereAttrColMaps;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllPKMappingsByMapToClass(
    FVisitedClassType, FWhereAttrColMaps ) ;
  AddToParams( FWhere, FWhereAttrColMaps, Visited ) ;
end;

procedure TVisAutoReadThis.MapRowToObject;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;
  QueryResultToObject( Visited, FAttrColMaps ) ;
  FSetObjectState := true ;
end ;

procedure TVisAutoAbs.QueryResultToObject( const pTarget : TPerObjAbs ; const pAttrColMaps: TtiAttrColMaps);
  procedure _SetPropValue( const pTarget : TPerObjAbs ; const pAttrColMap : TtiAttrColMap ) ;
  var
    lPropName  : string ;
    lColName   : string ;
    lFieldKind : TtiQueryFieldKind ;
    lPropType  : TTypeKind ;
    lInt       : Int64 ;
    lStream    : TStream ;
    lString    : string ;
  begin
    lColName  := pAttrColMap.DBColMap.ColName ;
    lPropName := pAttrColMap.AttrMap.AttrName ;

    // Some hacking around OIDs. Tidy this up
    if SameText( lPropName, 'OID' ) then
    begin
      {$IFDEF OID_AS_INT64}
         pTarget.OID := Query.FieldAsInteger[lColName];
       {$ELSE}
         pTarget.OID.AssignFromTIQuery(lColName, Query);
      {$ENDIF}
      Exit ; //==>
    end ;

    if SameText( lPropName, 'DispOrder' ) then
    begin
      lInt := Query.FieldAsInteger[ lColName ];
      pTarget.DispOrder := lInt;
      Exit ; //==>
    end ;

    // ToDo: When setting a property in the auto map visitors, might be better to determine
    //       which set method to use based on the property type, rather than the db-field type.
    lFieldKind := Query.FieldKind( Query.FieldIndex( lColName )) ;
    if ( pTarget.IsReadWriteProp( lPropName )) or
       ( lFieldKind = qfkBinary ) then
    begin
      case lFieldKind of                          
      qfkString,
      qfkLongString : begin
                      lString := Query.FieldAsString[  lColName] ;
                      {$IFDEF BOOLEAN_CHAR_1}
                        if ((UpperCase( lString ) = 'T' ) or
                            ( UpperCase( lString ) = 'F' )) and
                           ( tiGetSimplePropType(pTarget, lPropName) = tiTKBoolean ) then
                         TypInfo.SetOrdProp(pTarget, lPropName, Ord( UpperCase(lString) = 'T' )) ;
                      {$ELSE}
                        if ((UpperCase( lString ) = 'TRUE' ) or
                            ( UpperCase( lString ) = 'FALSE' )) and
                           ( tiGetSimplePropType(pTarget, lPropName) = tiTKBoolean ) then
                         TypInfo.SetOrdProp(pTarget, lPropName, Ord( UpperCase(lString) = 'TRUE' )) ;
                      {$ENDIF}
                      TypInfo.SetStrProp(pTarget,   lPropName, lString);
                      end ;
      qfkInteger    : begin
                        lPropType := PropType( pTarget, lPropName ) ;
                        lInt := Query.FieldAsInteger[lColName];
                        if ( lPropType = tkInt64 ) then
                           // and (( lInt < Low(LongInt)) or ( lInt > High(LongInt ))) then
                        begin
                          //tiFmtException('The auto mapping framework does not support Int64', ClassName, '_SetPropValue' ) ;
                          //lInt64 := Query.FieldAsInteger[lColName];
                          //TypInfo.SetOrdProp(pTarget, lPropName, lInt64);
                          TypInfo.SetInt64Prop(pTarget, lPropName, lInt);
                        end else
                          TypInfo.SetOrdProp(pTarget, lPropName, lInt);
                      end ;
      qfkFloat      : TypInfo.SetFloatProp(pTarget, lPropName, Query.FieldAsFloat[   lColName]);
      qfkDateTime   : TypInfo.SetFloatProp(pTarget, lPropName, Query.FieldAsDateTime[lColName]);
      qfkLogical    : TypInfo.SetOrdProp(pTarget, lPropName, Ord( Query.FieldAsBoolean[ lColName]));
      qfkBinary     : begin
                        lStream := TypInfo.GetObjectProp(pTarget, lPropName) as TStream ;
                        Query.AssignFieldAsStream(lColName, lStream);
                      end ;
      else
        tiFmtException('Invalid fieldkind', ClassName, '_SetPropValue' ) ;
      end ;
    end ;
  end ;

var
  i : integer ;
  lAttrColMap : TtiAttrColMap ;
begin
  try
    for i := 0 to FAttrColMaps.Count - 1 do
    begin
      lAttrColMap := FAttrColMaps.Items[i] ;
      _SetPropValue( pTarget, lAttrColMap ) ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'MapRowToObject' ) ;
  end ;
end ;

procedure TVisAutoCollectionRead.GetAttrColMaps;
var
  i : integer ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;

  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    TPerObjAbsClass( FClassDBCollection.PerObjAbsClass ), FAttrColMaps ) ;

  FClassToCreate := FAttrColMaps.Items[0].AttrMap.Owner.PerObjAbsClass ;
  FHasParent := gTIPerMgr.ClassDBMappingMgr.ClassMaps.HasParent( FClassToCreate );

  // If the class we are reading is a concrete class and it
  // has parents registered, the we should only read its
  // OID and read its data in a subsequent call.
  if (FHasParent) then
  begin
{
    // Remove any mappings that are not PK mappings
    for i := FAttrColMaps.Count - 1 downto 0 do
      if not( pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo ) then
        FAttrColMaps.Delete(i);
}        
  end
  else
  begin
    // Remove any foreign key mappings
    for i := FAttrColMaps.Count - 1 downto 0 do
      if pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo then
        FAttrColMaps.Delete(i);
  end ;

end;

procedure TVisAutoCollectionRead.GetWhereAttrColMaps;
var
  i : integer ;
begin
  Assert( FVisitedClassType <> nil, 'FVisitedClassType = nil' ) ;

  gTIPerMgr.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    TPerObjAbsClass( FClassDBCollection.PerObjAbsClass ), FWhereAttrColMaps ) ;

  // Remove any mappings that are not foreign key mappings
  for i := FWhereAttrColMaps.Count - 1 downto 0 do
    if ( not ( pktFK in FWhereAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FWhereAttrColMaps.Delete(i);

  AddToParams( FWhere, FWhereAttrColMaps, Visited ) ;

end;

{ TVisAutoCollectionPKRead }

//procedure TVisAutoCollectionPKRead.Final;
//begin
//  Visited.ObjectState := posPK ;
//end;

procedure TVisAutoCollectionPKRead.GetAttrColMaps;
var
  i : integer ;
begin
  inherited;
  // Remove any mappings that are not primary key, or primary key -readable
  for i := FAttrColMaps.Count - 1 downto 0 do
    if ( not ( pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo )) and
       ( not ( pktReadable in FAttrColMaps.Items[i].DBColMap.PKInfo )) then
      FAttrColMaps.Delete(i);
end;


procedure TVisAutoAbs.SetupParams;
begin
  GetWhereAttrColMaps;
  GetAttrColMaps;
end;

procedure TVisAutoAbs.Execute(const pData: TVisitedAbs);
begin
  inherited Execute( pData ) ;
  FVisitedClassType := TPerObjAbsClass(pData.ClassType);
end;

//procedure TVisAutoCollectionRead.Final;
//begin
//  if FSetObjectState then
//    if (gTIPerMgr.ClassDBMappingMgr.Collections.IsCollection(
//       TPerObjAbsClass(Visited.ClassType))) then
//      Visited.ObjectState := posPK
//    else
//      Visited.ObjectState := posClean ;
//end;

procedure TVisAutoCollectionRead.SetContinueVisiting;
begin
  //  Do nothing, this method is used in the child class
end;

//function TVisAutoCollectionPKRead.GetObjectState: TPerObjectState;
//begin
//  result := posPK ;
//end;

procedure TVisAutoCollectionPKRead.SetContinueVisiting;
begin
  ContinueVisiting := false ;
end;

//function TVisAutoCollectionRead.GetObjectState: TPerObjectState;
//begin
//  result := posClean ;
//end;

constructor TVisAutoCollectionRead.Create;
begin
  inherited;
  FClassesWithParent := TList.Create ;
end;

destructor TVisAutoCollectionRead.Destroy;
begin
  FClassesWithParent.Free ;
  inherited;
end;

function TVisAutoAbs.ParamsToString(const pParams: TtiQueryParams): string;
var
  i : integer ;
  lName : string ;
  lValue : string ;
begin
  result := '' ;
  for i := 0 to pParams.Count - 1 do
  begin
    lName  := pParams.ParamName(i) ;
    lValue := pParams.Items[i].GetValueAsString ;
    result := result + lName + ' = ' + lValue + ', ' ;
  end ;
end;

end.

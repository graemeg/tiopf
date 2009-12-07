unit AssociationObjectVisitorsUnit;

interface

uses
  tiVisitorDB;

type
  TvisAssociation_Read = class (TVisOwnedQrySelect)
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end;

  TVisAssociation_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisAssociation_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisAssociation_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
    procedure SetupParams    ; override ;
  end ;

procedure RegisterAssociationVisitors;

implementation

uses
  tiObject,
  tiOPFManager,
  RelationshipManagerUnit,
  BOMUnit;

procedure RegisterAssociationVisitors;
begin
  gTIOPFManager.RegReadVisitor(TvisAssociation_Read);
  gTIOPFManager.RegSaveVisitor(TVisAssociation_Create);
  gTIOPFManager.RegSaveVisitor(TVisAssociation_Update);
  gTIOPFManager.RegSaveVisitor(TVisAssociation_Delete);
end;

{ TvisAssociation_Read }

function TvisAssociation_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TAssociationObjectList ) and
            ( Visited.ObjectState = posEmpty );
end;

procedure TvisAssociation_Read.Init;
begin
  Query.SQLText := 'select OID, FROM_OID, TO_OID, REL_TYPE from RELATIONSHIPS';
end;

procedure TvisAssociation_Read.MapRowToObject;
var
  temp : TAssociationObject;

begin
  temp := TAssociationObject.Create;
  temp.OID.AssignFromTIQuery('OID',Query);
  //we must read RelType before we can get Source and Target lists
  temp.RelType := Query.FieldAsInteger['REL_TYPE'];
  temp.SourceObject := TBusinessObject(
   GetRelationshipManager.GetSourceList(temp.RelType).Find(
   Query.FieldAsString['FROM_OID']));
  temp.TargetObject := TBusinessObject(
   GetRelationshipManager.GetTargetList(temp.RelType).Find(
   Query.FieldAsString['TO_OID']));
  temp.ObjectState := posClean;
  TAssociationObjectList(Visited).Add(Temp);
end;

procedure TvisAssociation_Read.SetupParams;
begin
  // Do Nothing because there are no parameters to initialize in the query
end;

{ TVisAssociation_Create }

function TVisAssociation_Create.AcceptVisitor: boolean;
begin
  result := ( Visited is TAssociationObject ) and
            ( Visited.ObjectState = posCreate );
end;

procedure TVisAssociation_Create.Init;
begin
  Query.SQLText :=
    'insert into RELATIONSHIPS values(:OID, :FROM_OID, :TO_OID, :REL_TYPE)';
end;

procedure TVisAssociation_Create.SetupParams;
var
  temp: TAssociationObject;
begin
  temp := TAssociationObject(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['FROM_OID'] := temp.SourceObject.OID.AsString;
  Query.ParamAsString['TO_OID'] := temp.TargetObject.OID.AsString;;
  Query.ParamAsInteger['REL_TYPE'] := temp.RelType;
end;

{ TVisAssociation_Update }

function TVisAssociation_Update.AcceptVisitor: boolean;
begin
  result := ( Visited is TAssociationObject) and
            ( Visited.ObjectState = posUpdate );
end;

procedure TVisAssociation_Update.Init;
begin
  Query.SQLText :=
    'update RELATIONSHIPS set FROM_OID = :FROM_OID, TO_OID = :TO_OID, REL_TYPE = :REL_TYPE '+
    'where OID = :OID';
end;

procedure TVisAssociation_Update.SetupParams;
var
  temp: TAssociationObject;
begin
  temp := TAssociationObject(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['FROM_OID'] := temp.SourceObject.OID.AsString;
  Query.ParamAsString['TO_OID'] := temp.TargetObject.OID.AsString;;
  Query.ParamAsInteger['REL_TYPE'] := temp.RelType;
end;

{ TVisAssociation_Delete }

function TVisAssociation_Delete.AcceptVisitor: boolean;
begin
  result := ( Visited is TAssociationObject) and
            ( Visited.ObjectState = posDelete );
end;

procedure TVisAssociation_Delete.Init;
begin
  Query.SQLText := 'delete from RELATIONSHIPS where OID = :OID';
end;

procedure TVisAssociation_Delete.SetupParams;
var
  temp: TAssociationObject;
begin
  temp := TAssociationObject(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
end;

end.

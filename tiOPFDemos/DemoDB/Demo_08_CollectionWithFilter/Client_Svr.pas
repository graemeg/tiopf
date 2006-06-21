unit Client_Svr;

interface
uses
  tiPtnVisSQL
  ;

type

  TVisClient_Read = class( TVisOwnedQrySelect )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end ;

  TVisClient_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

implementation
uses
  Client_BOM
  ,tiPersist
  ,tiPtnVisPerObj
  ,tiLog
  ,SysUtils
  ,Dialogs
  ;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TClientsByName ) ;
end;

procedure TVisClient_Read.Init;
begin
  TClientsByName(Visited).Clear ;
  Query.SQL.Text :=
    'select OID, Client_Name, Client_ID from Client ' +
    'where Client_Name like ''' +
    UpperCase(TClientsByName(Visited).ClientName) + '%''' ;
end;

procedure TVisClient_Read.MapRowToObject;
var
  lClient : TClient ;
begin
  lClient := TClient.Create ;
  lClient.OID.AssignFromTIQuery('OID',Query);
  lClient.ClientName := Query.FieldAsString['Client_Name'];
  lClient.ClientID := Query.FieldAsString['Client_ID'];
  lClient.ObjectState := posClean ;
  TClients(Visited).Add(lClient);
end;

procedure TVisClient_Read.SetupParams;
begin
//
end;

{ TVisClient_Create }

function TVisClient_Create.AcceptVisitor: boolean;
begin
  result := ( Visited is TClient ) and
            ( Visited.ObjectState = posCreate ) ;
  LogArray([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Create.Init;
begin
  Query.SQL.Text :=
    'Insert into Client ( OID, Client_Name, Client_ID ) ' +
    'Values ' +
    '( :OID, :Client_Name, :Client_ID )' ;
end;

procedure TVisClient_Create.SetupParams;
var
  lData : TClient ;
begin
  lData := Visited as TClient ;
  lData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['Client_Name'] := lData.ClientName ;
  Query.ParamAsString['Client_ID'] := lData.ClientID ;
end;


initialization
  gTIPerMgr.RegReadVisitor(TVisClient_Read);
  gTIPerMgr.RegSaveVisitor(TVisClient_Create);
end.


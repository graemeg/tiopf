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

  TVisClient_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisClient_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
    procedure SetupParams    ; override ;
  end ;

implementation
uses
  Client_BOM
  ,tiPersist
  ,tiPtnVisPerObj
  ,tiLog
  ;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TClients ) and
            ( Visited.ObjectState = posEmpty ) ;
  LogArray([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Read.Init;
begin
  Query.SQL.Text :=
    'select OID, Client_Name, Client_ID from Client' ;
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
  // Do nothing
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

{ TVisClient_Update }

function TVisClient_Update.AcceptVisitor: boolean;
begin
  result := ( Visited is TClient ) and
            ( Visited.ObjectState = posUpdate ) ;
  LogArray([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Update.Init;
begin
  Query.SQL.Text :=
    'Update Client Set ' +
    '  Client_Name = :Client_Name ' +
    ' ,Client_ID = :Client_ID ' +
    'where ' +
    ' OID = :OID' ;
end;

procedure TVisClient_Update.SetupParams;
var
  lData : TClient ;
begin
  lData := Visited as TClient ;
  lData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['Client_Name'] := lData.ClientName ;
  Query.ParamAsString['Client_ID'] := lData.ClientID ;
end;

{ TVisClient_Delete }

function TVisClient_Delete.AcceptVisitor: boolean;
begin
  result := ( Visited is TClient ) and
            ( Visited.ObjectState = posDelete ) ;
  LogArray([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result ]);
end;

procedure TVisClient_Delete.Init;
begin
  Query.SQL.Text :=
    'delete from client where oid = :oid' ;
end;

procedure TVisClient_Delete.SetupParams;
var
  lData : TClient ;
begin
  lData := Visited as TClient ;
  lData.OID.AssignToTIQuery('OID', Query);
end;

initialization
  gTIPerMgr.RegReadVisitor(TVisClient_Read);
  gTIPerMgr.RegSaveVisitor(TVisClient_Create);
  gTIPerMgr.RegSaveVisitor(TVisClient_Update);
  gTIPerMgr.RegSaveVisitor(TVisClient_Delete);
end.


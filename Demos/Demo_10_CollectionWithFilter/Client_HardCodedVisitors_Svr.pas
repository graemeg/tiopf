unit Client_HardCodedVisitors_Svr;

interface
uses
  tiVisitorDB
 ;

type

  TVisClient_Read = class(TVisOwnedQrySelect)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  TVisClient_Create = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

procedure RegisterVisitors;

implementation
uses
   tiOPFManager
  ,tiObject
  ,SysUtils
  ,Client_BOM
 ;

procedure RegisterVisitors;
begin
  GTIOPFManager.RegReadVisitor(TVisClient_Read);
  GTIOPFManager.RegSaveVisitor(TVisClient_Create);
end;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientsLike);
end;

procedure TVisClient_Read.Init;
begin
  // Do nothing
end;

procedure TVisClient_Read.MapRowToObject;
var
  lClient: TClient;
begin
  lClient:= TClient.Create;
  lClient.OID.AssignFromTIQuery('OID',Query);
  lClient.ClientName:= Query.FieldAsString['Client_Name'];
  lClient.ClientID:= Query.FieldAsString['Client_ID'];
  lClient.ObjectState:= posClean;
  TClientsLike(Visited).Clients.Add(lClient);
end;

procedure TVisClient_Read.SetupParams;
begin
  Query.SQLText:=
    'select ' +
    '   OID ' +
    '  ,Client_Name ' +
    '  ,Client_ID ' +
    'from  ' +
    '  Client  ' +
    'where  ' +
    '  Client_Name like ''' +
      UpperCase((Visited as TClientsLike).ClientNameLike) + '%''';
end;

{ TVisClient_Create }

function TVisClient_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClient_Create.Init;
begin
  Query.SQLText:=
    'Insert into Client (OID, Client_Name, Client_ID) ' +
    'Values ' +
    '(:OID,:Client_Name,:Client_ID)';
end;

procedure TVisClient_Create.SetupParams;
var
  lData: TClient;
begin
  lData:= Visited as TClient;
  lData.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['Client_Name']:= lData.ClientName;
  Query.ParamAsString['Client_ID']:= lData.ClientID;
end;

end.


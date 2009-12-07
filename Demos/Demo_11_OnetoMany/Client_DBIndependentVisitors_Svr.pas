unit Client_DBIndependentVisitors_Svr;

interface
uses
   tiVisitorDBAutoGen
  ,tiObject
  ,Client_BOM
 ;

type

  TVisClient_Read = class(TVisDBAutoGenRead)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
    procedure Final(const AVisited: TtiObject); override;
  end;

  TVisClient_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClient_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClient_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;


  TVisPhoneNumber_Read = class(TVisDBAutoGenRead)
  private
    FClient: TClient;
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  TVisPhoneNumber_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisPhoneNumber_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisPhoneNumber_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

procedure RegisterVisitors;

implementation
uses
   tiOPFManager
  ,tiQuery
  ,SysUtils
 ;

procedure RegisterVisitors;
begin
  // The registration order is important
  GTIOPFManager.RegReadVisitor(TVisClient_Read);
  GTIOPFManager.RegReadVisitor(TVisPhoneNumber_Read);

  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Update);
  GTIOPFManager.RegSaveVisitor(TVisClient_Update);

  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Delete);
  GTIOPFManager.RegSaveVisitor(TVisClient_Delete);

  GTIOPFManager.RegSaveVisitor(TVisClient_Create);
  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Create);

end;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisClient_Read.Final(const AVisited: TtiObject);
begin
  // Do nothing as TVisPhoneNumber_Read will set ObjectState to posClean
end;

procedure TVisClient_Read.Init;
begin
  TableName:= 'Client';
end;

procedure TVisClient_Read.MapRowToObject;
var
  LClient: TClient;
begin
  LClient:= TClient.Create;
  LClient.OID.AssignFromTIQuery('OID',Query);
  LClient.ClientName:= Query.FieldAsString['Client_Name'];
  LClient.ClientID:= Query.FieldAsString['Client_ID'];
  LClient.ObjectState:= posClean;
  TClients(Visited).Add(LClient);
end;

procedure TVisClient_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisClient_Create }

function TVisClient_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClient_Create.SetupParams;
var
  LData: TClient;
begin
  LData:= Visited as TClient;
  TableName:= 'Client';
  QueryType:= qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_Name', LData.ClientName);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClient_Update }

function TVisClient_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClient_Update.SetupParams;
var
  LData: TClient;
begin
  LData:= Visited as TClient;
  TableName:= 'Client';
  QueryType:= qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_Name', LData.ClientName);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClient_Delete }

function TVisClient_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClient_Delete.SetupParams;
begin
  inherited;
  TableName:= 'Client';
end;

{ TVisPhoneNumber_Read }

function TVisPhoneNumber_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisPhoneNumber_Read.Init;
begin
  TableName:= 'Phone_Number';
end;

procedure TVisPhoneNumber_Read.MapRowToObject;
var
  LData: TPhoneNumber;
  LClientOID: string;
begin
  LClientOID:= Query.FieldAsString['Client_OID'];
  if (FClient = nil) or
     (FClient.OID.AsString <> LClientOID) then
    FClient:= (Visited as TClients).Find(LClientOID) as TClient;

  if FClient=nil then
    raise Exception.CreateFmt('Can not find client OID=%s', [LClientOID]);

  LData:= TPhoneNumber.Create;
  FClient.PhoneNumbers.Add(LData);
  LData.OID.AssignFromTIQuery(Query);
  LData.NumberType:= Query.FieldAsString['Number_Type'];
  LData.NumberText:= Query.FieldAsString['Number_Text'];
  LData.ObjectState:= posClean;

end;

procedure TVisPhoneNumber_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisPhoneNumber_Create }

function TVisPhoneNumber_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TPhoneNumber) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisPhoneNumber_Create.SetupParams;
var
  LData: TPhoneNumber;
begin
  LData:= Visited as TPhoneNumber;
  TableName:= 'Phone_Number';
  QueryType:= qtInsert;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_OID', LData.Owner.OID.AsString);
  QueryParams.SetValueAsString('Number_Type', LData.NumberType);
  QueryParams.SetValueAsString('Number_Text', LData.NumberText);
end;

{ TVisPhoneNumber_Update }

function TVisPhoneNumber_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TPhoneNumber) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisPhoneNumber_Update.SetupParams;
var
  LData: TPhoneNumber;
begin
  LData:= Visited as TPhoneNumber;
  TableName:= 'Phone_Number';
  QueryType:= qtUpdate;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Number_Type', LData.NumberType);
  QueryParams.SetValueAsString('Number_Text', LData.NumberText);
end;

{ TVisPhoneNumber_Delete }

function TVisPhoneNumber_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TPhoneNumber) and
            (inherited AcceptVisitor);
end;

procedure TVisPhoneNumber_Delete.SetupParams;
begin
  inherited;
  TableName:= 'Phone_Number';
end;

end.


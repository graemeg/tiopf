unit Client_HardCodedVisitors_Svr;

{$I tiDefines.inc}

interface

uses
   tiVisitorDB
  ,tiObject
  ,Client_BOM
 ;

type

  TVisClient_Read = class(TVisOwnedQrySelect)
  private
    FClient: TClient;
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

  TVisClient_Update = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

  TVisClient_Delete = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
    procedure Final(const AVisited: TtiObject); override;
  end;

  TVisPhoneNumber_Create = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

  TVisPhoneNumber_Update = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init          ; override;
    procedure SetupParams   ; override;
  end;

  TVisPhoneNumber_Delete = class(TVisOwnedQryUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams   ; override;
  end;

procedure RegisterVisitors;

implementation
uses
   tiOPFManager
 ;

procedure RegisterVisitors;
begin
  // Note: Registration order is important
  GTIOPFManager.RegReadVisitor(TVisClient_Read);

  GTIOPFManager.RegSaveVisitor(TVisClient_Create);
  GTIOPFManager.RegSaveVisitor(TVisClient_Update);
  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Create);
  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Update);

  GTIOPFManager.RegSaveVisitor(TVisPhoneNumber_Delete);
  GTIOPFManager.RegSaveVisitor(TVisClient_Delete);

end;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisClient_Read.Init;
begin
  Query.SQL.Text:=
    'select ' +
    '   c.oid          as client_oid ' +
    '  ,c.client_name  as client_name ' +
    '  ,c.Client_ID    as Client_id ' +
    '  ,ph.oid         as phone_number_oid ' +
    '  ,ph.number_type as phone_number_type ' +
    '  ,ph.number_text as phone_number_text ' +
    'from ' +
    '          client        c ' +
    'left join phone_number ph on ph.client_oid = c.oid ' +
    'order by ' +
    'client_name ';
end;

procedure TVisClient_Read.MapRowToObject;
var
  lPhoneNumber: TPhoneNumber;
begin
	if (FClient = nil) or
		 (FClient.OID.AsString <> Query.FieldAsString['Client_oid']) then
	begin
		FClient:= TClient.Create;
		FClient.OID.AssignFromTIQuery('Client_OID',Query);
		FClient.ClientName := Query.FieldAsString['Client_Name'];
		FClient.ClientID   := Query.FieldAsString['Client_ID'];
		FClient.ObjectState:= posClean;
		FClient.PhoneNumbers.ObjectState:= posClean;
		TClients(Visited).Add(FClient);
	end;

	if Query.FieldAsString['Phone_Number_OID'] <> '' then
	begin
		lPhoneNumber:= TPhoneNumber.Create;
		lPhoneNumber.OID.AssignFromTIQuery('Phone_Number_OID', Query);
		lPhoneNumber.NumberType:= Query.FieldAsString['Phone_Number_Type'];
    lPhoneNumber.NumberText:= Query.FieldAsString['Phone_Number_Text'];
    lPhoneNumber.ObjectState:= posClean;
    FClient.PhoneNumbers.Add(lPhoneNumber);
  end;

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

procedure TVisClient_Create.Init;
begin
  Query.SQL.Text:=
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

{ TVisClient_Update }

function TVisClient_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClient_Update.Init;
begin
  Query.SQL.Text:=
    'Update Client Set ' +
    '  Client_Name =:Client_Name ' +
    ' ,Client_ID =:Client_ID ' +
		'where ' +
		' OID =:OID';
end;

procedure TVisClient_Update.SetupParams;
var
	lData: TClient;
begin
	lData:= Visited as TClient;
	lData.OID.AssignToTIQuery('OID', Query);
	Query.ParamAsString['Client_Name']:= lData.ClientName;
	Query.ParamAsString['Client_ID']:= lData.ClientID;
end;

{ TVisClient_Delete }

function TVisClient_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClient) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClient_Delete.Final(const AVisited: TtiObject);
var
  lData: TClient;
begin
  inherited;
  lData:= AVisited as TClient;
  lData.PhoneNumbers.ObjectState:= posClean;
end;

procedure TVisClient_Delete.Init;
begin
  Query.SQL.Text:=
    'delete from client where oid =:oid';
end;

procedure TVisClient_Delete.SetupParams;
var
  lData: TClient;
begin
  lData:= Visited as TClient;
	lData.OID.AssignToTIQuery('OID', Query);
end;

{ TVisPhoneNumber_Create }

function TVisPhoneNumber_Create.AcceptVisitor: boolean;
begin
	result:= (Visited is TPhoneNumber) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisPhoneNumber_Create.Init;
begin
  Query.SQL.Text:=
    'Insert into Phone_Number (OID, Client_OID, Number_Type, Number_Text) ' +
    'Values ' +
    '(:OID,:Client_OID,:Number_Type,:Number_Text)';
end;

procedure TVisPhoneNumber_Create.SetupParams;
var
  lData: TPhoneNumber;
begin
  lData:= Visited as TPhoneNumber;
	lData.OID.AssignToTIQuery('OID', Query);
	lData.Owner.OID.AssignToTIQuery('Client_OID', Query);
	Query.ParamAsString['Number_Type']:= lData.NumberType;
  Query.ParamAsString['Number_Text']:= lData.NumberText;
end;

{ TVisPhoneNumber_Update }

function TVisPhoneNumber_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TPhoneNumber) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisPhoneNumber_Update.Init;
begin
  Query.SQL.Text:=
    'Update Phone_Number Set ' +
    '  Number_Type =:Number_Type ' +
    ' ,Number_Text =:Number_Text ' +
    'where ' +
    ' OID =:OID';
end;

procedure TVisPhoneNumber_Update.SetupParams;
var
  lData: TPhoneNumber;
begin
  lData:= Visited as TPhoneNumber;
	lData.OID.AssignToTIQuery('OID', Query);
	Query.ParamAsString['OID']:= lData.OID.AsString;
	Query.ParamAsString['Number_Type']:= lData.NumberType;
  Query.ParamAsString['Number_Text']:= lData.NumberText;
end;

{ TVisPhoneNumber_Delete }

function TVisPhoneNumber_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TPhoneNumber) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisPhoneNumber_Delete.Init;
begin
  Query.SQL.Text:=
    'delete from phone_number where oid =:oid';
end;

procedure TVisPhoneNumber_Delete.SetupParams;
var
  lData: TPhoneNumber;
begin
	lData:= Visited as TPhoneNumber;
	lData.OID.AssignToTIQuery('OID', Query);
end;

end.


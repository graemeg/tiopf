unit Client_DBIndependentVisitors_Svr;

{$I tiDefines.inc}

interface

uses
  tiVisitorDB
  ,tiVisitorDBAutoGen
 ;

type

  // Read visitors
  TVisClientCompany_Read = class(TVisDBAutoGenRead)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  TVisClientPerson_Read = class(TVisDBAutoGenRead)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  TVisClientAbs_Read = class(TVisDBAutoGenRead)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
    procedure MapRowToObject; override;
  end;

  // Update visitors
  TVisClientAbs_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClientCompany_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClientPerson_Update = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  // Delete visitors
  TVisClientCompany_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams  ; override;
  end;

  TVisClientPerson_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams  ; override;
  end;

  TVisClientAbs_Delete = class(TVisDBAutoGenDelete)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  // Create visitors
  TVisClientAbs_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  TVisClientCompany_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

  // TClientPerson
  TVisClientPerson_Create = class(TVisDBAutoGenUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure SetupParams   ; override;
  end;

procedure RegisterVisitors;

implementation
uses
   tiOPFManager
  ,tiObject
  ,tiQuery
  ,SysUtils
  ,Client_BOM
 ;

procedure RegisterVisitors;
begin

  // Note: Registration order is important
  gTIOPFManager.RegReadVisitor(TVisClientCompany_Read);
  gTIOPFManager.RegReadVisitor(TVisClientPerson_Read);
  gTIOPFManager.RegReadVisitor(TVisClientAbs_Read);

  gTIOPFManager.RegSaveVisitor(TVisClientAbs_Update);
  gTIOPFManager.RegSaveVisitor(TVisClientPerson_Update);
  gTIOPFManager.RegSaveVisitor(TVisClientCompany_Update);

  gTIOPFManager.RegSaveVisitor(TVisClientPerson_Delete);
  gTIOPFManager.RegSaveVisitor(TVisClientCompany_Delete);
  gTIOPFManager.RegSaveVisitor(TVisClientAbs_Delete);

  gTIOPFManager.RegSaveVisitor(TVisClientAbs_Create);
  gTIOPFManager.RegSaveVisitor(TVisClientPerson_Create);
  gTIOPFManager.RegSaveVisitor(TVisClientCompany_Create);

end;

{ TVisClientPerson_Read }

function TVisClientPerson_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisClientPerson_Read.MapRowToObject;
var
  lClient: TClientPerson;
begin
  lClient:= TClientPerson.Create;
  lClient.OID.AssignFromTIQuery('OID', Query);
  lClient.GivenName:= Query.FieldAsString['Given_Name'];
  lClient.FamilyName:= Query.FieldAsString['Family_Name'];
  TClients(Visited).Add(lClient);
end;

procedure TVisClientPerson_Read.SetupParams;
begin
  TableName:= 'client_person';
end;

{ TVisClientPerson_Create }

function TVisClientPerson_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientPerson) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClientPerson_Create.SetupParams;
var
  LData: TClientPerson;
begin
  QueryType:= qtInsert;
  TableName:= 'client_person';
  LData:= Visited as TClientPerson;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Given_Name', LData.GivenName);
  QueryParams.SetValueAsString('Family_Name', LData.FamilyName);
end;

{ TVisClientPerson_Update }

function TVisClientPerson_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientPerson) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClientPerson_Update.SetupParams;
var
  lData: TClientPerson;
begin
  TableName:= 'client_person';
  QueryType:= qtUpdate;
  lData:= Visited as TClientPerson;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Given_Name', LData.GivenName);
  QueryParams.SetValueAsString('Family_Name', LData.FamilyName);
end;

{ TVisClientPerson_Delete }

function TVisClientPerson_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientPerson) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClientPerson_Delete.SetupParams;
begin
  inherited;
  TableName:= 'client_person';
end;

{ TVisClientAbs_Create }

function TVisClientAbs_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientAbs) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClientAbs_Create.SetupParams;
var
  LData: TClientAbs;
begin
  QueryType:= qtInsert;
  TableName:= 'client_abs';
  LData:= Visited as TClientAbs;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClientCompany_Read }

function TVisClientCompany_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisClientCompany_Read.MapRowToObject;
var
  lClient: TClientCompany;
begin
  lClient:= TClientCompany.Create;
  lClient.OID.AssignFromTIQuery('OID', Query);
  lClient.CompanyName:= Query.FieldAsString['Company_Name'];
  TClients(Visited).Add(lClient);
end;

procedure TVisClientCompany_Read.SetupParams;
begin
  TableName:= 'client_company';
end;

{ TVisClientAbs_Update }

function TVisClientAbs_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientAbs) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClientAbs_Update.SetupParams;
var
  lData: TClientAbs;
begin
  TableName:= 'client_abs';
  QueryType:= qtUpdate;
  lData:= Visited as TClientAbs;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Client_ID', LData.ClientID);
end;

{ TVisClientAbs_Delete }

function TVisClientAbs_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientAbs) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClientAbs_Delete.SetupParams;
begin
  inherited;
  TableName:= 'client_abs';
end;

{ TVisClientCompany_Create }

function TVisClientCompany_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientCompany) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClientCompany_Create.SetupParams;
var
  LData: TClientCompany;
begin
  QueryType:= qtInsert;
  TableName:= 'client_company';
  LData:= Visited as TClientCompany;
  QueryParams.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Company_Name', LData.CompanyName);
end;

{ TVisClientCompany_Update }

function TVisClientCompany_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientCompany) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClientCompany_Update.SetupParams;
var
  lData: TClientCompany;
begin
  TableName:= 'client_company';
  QueryType:= qtUpdate;
  lData:= Visited as TClientCompany;
  QueryWhere.SetValueAsString('OID', LData.OID.AsString);
  QueryParams.SetValueAsString('Company_Name', LData.CompanyName);
end;

{ TVisClientCompany_Delete }

function TVisClientCompany_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TClientCompany) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClientCompany_Delete.SetupParams;
begin
  inherited;
  TableName:= 'client_company';
end;

{ TVisClientAbs_Read }

function TVisClientAbs_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TClients) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisClientAbs_Read.MapRowToObject;
var
  lClient: TClientAbs;
  LOID: string;
begin
  LOID:= Query.FieldAsString['OID'];
  LClient:= (Visited as TClients).Find(LOID) as TClientAbs;
  if LClient = nil then
    raise Exception.CreateFmt('Can not find client OID="%s"', [LOID]);
  lClient.ClientID:= Query.FieldAsString['client_id'];
  LClient.ObjectState:= posClean;
end;

procedure TVisClientAbs_Read.SetupParams;
begin
  TableName:= 'client_abs';
end;

end.


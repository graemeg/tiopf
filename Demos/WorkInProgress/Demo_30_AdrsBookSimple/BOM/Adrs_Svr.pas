unit Adrs_Svr;

{$I tiDefines.inc}

interface
uses
   tiVisitorDB
  ,Adrs_BOM
;

type

  TVisPersonList_Read = class(TtiVisitorSelect)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
    procedure MapRowToObject; override;
  end;

  TVisPerson_Create = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
  end;

  TVisPerson_Update = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
  end;

  TVisPerson_Delete = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
  end;

  TVisEAdrsList_Read = class(TtiVisitorSelect)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
    procedure MapRowToObject; override;
  end;

  TVisEAdrs_Create = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
  end;

  TVisEAdrs_Update = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override;
  end;

  TVisEAdrs_Delete = class(TtiVisitorUpdate)
  protected
    function  AcceptVisitor: boolean; override;
    procedure Init         ; override;
    procedure SetupParams  ; override; 
  end;
  
implementation
uses
   tiOPFManager
  ,tiObject
  ,tiConstants
  ,tiQuery
  ,Adrs_SQL
  ,Adrs_Constants
 ;

{ TVisAdrs_Read }

function TVisPersonList_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TPersonList);
end;

procedure TVisPersonList_Read.Init;
begin
  Query.SQLText:= cQryPeople_Select;
end;

procedure TVisPersonList_Read.MapRowToObject;
var
  LData: TPerson;
begin
  LData:= TPerson.Create;
  (Visited as TPersonList).Add(LData);
  LData.OID.AsString:= Query.FieldAsString['oid'];
  LData.FirstName:=    Query.FieldAsString['first_name'];
  LData.LastName:=     Query.FieldAsString['last_name'];
  LData.Title:=        Query.FieldAsString['title'];
  LData.Initials:=     Query.FieldAsString['initials'];
  LData.Notes:=        Query.FieldAsString['notes'];
  LData.ObjectState:= posPK;
end;

procedure TVisPersonList_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisAdrs_Create }

function TVisPerson_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
           (Visited.ObjectState = posCreate);
end;

procedure TVisPerson_Create.Init;
begin
  Query.SQLText:= cQryPeople_Create;
end;

procedure TVisPerson_Create.SetupParams;
var
  LData: TPerson;
begin
  Assert(Visited.TestValid(TPerson), CTIErrorInvalidObject);
  LData:= (Visited as TPerson);
  Query.ParamAsString['oid']       := LData.OID.AsString;
  Query.ParamAsString['first_name']:= LData.FirstName;
  Query.ParamAsString['last_name'] := LData.LastName;
  Query.ParamAsString['title']     := LData.Title;
  Query.ParamAsString['initials']  := LData.Initials;
  Query.ParamAsString['notes']     := LData.Notes;
end;

{ TVisAdrs_Update }

function TVisPerson_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
           (Visited.ObjectState = posUpdate);
end;

procedure TVisPerson_Update.Init;
begin
  Query.SQLText:= cQryPeople_Update;
end;

procedure TVisPerson_Update.SetupParams;
var
  LData: TPerson;
begin
  Assert(Visited.TestValid(TPerson), CTIErrorInvalidObject);
  LData:= (Visited as TPerson);
  Query.ParamAsString['oid']       := LData.OID.AsString;
  Query.ParamAsString['first_name']:= LData.FirstName;
  Query.ParamAsString['last_name'] := LData.LastName;
  Query.ParamAsString['title']     := LData.Title;
  Query.ParamAsString['initials']  := LData.Initials;
  Query.ParamAsString['notes']     := LData.Notes;
end;

{ TVisAdrs_Delete }

function TVisPerson_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
           (Visited.ObjectState = posDelete);
end;

procedure TVisPerson_Delete.Init;
begin
  Query.SQLText:= cQryPeople_Delete;
end;

procedure TVisPerson_Delete.SetupParams;
begin
  Query.ParamAsString['oid']:= Visited.OID.AsString;
end;

{ TVisEAdrsList_Read }

function TVisEAdrsList_Read.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
           (Visited.ObjectState = posPK);
end;

procedure TVisEAdrsList_Read.Init;
begin
  Query.SQLText:= cQryEAdrs_Read;
end;

procedure TVisEAdrsList_Read.MapRowToObject;
var
  LData: TEAdrs;
begin
  Assert(Visited.TestValid(TPerson), CTIErrorInvalidObject);
  LData:= TEAdrs.Create;
  (Visited as TPerson).EAdrsList.Add(LData);
  LData.OID.AsString:= Query.FieldAsString['oid'];
  LData.AdrsType:= Query.FieldAsString['eadrs_type'];
  LData.AdrsText:= Query.FieldAsString['eadrs_text'];
  LData.ObjectState:= posClean;
end;

procedure TVisEAdrsList_Read.SetupParams;
var
  LData: TPerson;
begin
  Assert(Visited.TestValid(TPerson), CTIErrorInvalidObject);
  LData:= (Visited as TPerson);
  Query.ParamAsString['oid_person']       := LData.OID.AsString;
end;

{ TVisEAdrs_Create }

function TVisEAdrs_Create.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
           (Visited.ObjectState = posCreate);
end;

procedure TVisEAdrs_Create.Init;
begin
  Query.SQLText:= cQryEAdrs_Create;
end;

procedure TVisEAdrs_Create.SetupParams;
var
  LData: TEAdrs;
begin
  Assert(Visited.TestValid(TEAdrs), CTIErrorInvalidObject);
  LData:= Visited as TEAdrs;
  Query.ParamAsString['oid']:= LData.OID.AsString;
  Query.ParamAsString['oid_person']:= LData.Owner.OID.AsString;
  Query.ParamAsString['eadrs_type']:= LData.AdrsType;
  Query.ParamAsString['eadrs_text']:= LData.AdrsText;
end;

{ TVisEAdrs_Update }

function TVisEAdrs_Update.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
           (Visited.ObjectState = posUpdate);
end;

procedure TVisEAdrs_Update.Init;
begin
  Query.SQLText:= cQryEAdrs_Update;
end;

procedure TVisEAdrs_Update.SetupParams;
var
  LData: TEAdrs;
begin
  Assert(Visited.TestValid(TEAdrs), CTIErrorInvalidObject);
  LData:= Visited as TEAdrs;
  Query.ParamAsString['oid']:= LData.OID.AsString;
  Query.ParamAsString['eadrs_type']:= LData.AdrsType;
  Query.ParamAsString['eadrs_text']:= LData.AdrsText;
end;

{ TVisEAdrs_Delete }

function TVisEAdrs_Delete.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
           (Visited.ObjectState = posDelete);
end;

procedure TVisEAdrs_Delete.Init;
begin
  Query.SQLText:= cQryEAdrs_Delete;
end;

procedure TVisEAdrs_Delete.SetupParams;
begin
  Query.ParamAsString['oid']:= Visited.OID.AsString;
end;

initialization

  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsReadPK, TVisPersonList_Read);
  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsRead,   TVisEAdrsList_Read);

  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisPerson_Delete);
  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisPerson_Update);
  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisPerson_Create);

  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisEAdrs_Create);
  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisEAdrs_Update);
  GTIOPFManager.VisitorManager.RegisterVisitor(cVisAdrsSave,   TVisEAdrs_Delete);

end.

unit Adrs_Cli;

{$I tiDefines.inc}

interface
uses
   tiVisitor
  ,tiVisitorDB
  ,tiObject
 ;

type

  TVisAdrsBookSetAllToCreate = class(TtiVisitor)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

procedure SetAllToCreate(const pData: TtiObject);


implementation
uses
  Adrs_BOM
 ;

procedure SetAllToCreate(const pData: TtiObject);
var
  lVis: TVisAdrsBookSetAllToCreate;
begin
  lVis:= TVisAdrsBookSetAllToCreate.Create;
  try
    pData.Iterate(lVis);
  finally
    lVis.Free;
  end;
end;


{ TVisAdrsBookSetAllToCreate }

function TVisAdrsBookSetAllToCreate.AcceptVisitor: boolean;
begin
  result:=
    ((Visited is TPerson) or
     (Visited is TCompany) or
     (Visited is TAdrsAbs)) and
     (TtiObject(Visited).ObjectState in [ posCreate, posUpdate, posClean ]);

end;

procedure TVisAdrsBookSetAllToCreate.Execute(const AVisited: TtiVisited);
begin
  Inherited Execute(AVisited);

  if not AcceptVisitor then
    exit; //==>

  TtiObject(Visited).ObjectState:= posCreate;

end;

end.

unit tiPerObjList;

//DEFINEMACRO:TMyClasses=The collection class
//DEFINEMACRO:TMyClass=The collectable class owned by TMyClasses
//DEFINEMACRO:TOwner=The Top hierarquie

interface

type
  TCodeTemplate = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): <!TMyClass!> ; reintroduce ;
    procedure   SetItems(i: integer; const Value: <!TMyClass!>); reintroduce ;
    function    GetOwner: <!TOwner!>; reintroduce ;
    procedure   SetOwner(const Value: <!TOwner!>); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    property    Items[i:integer] : <!TMyClass!> read GetItems write SetItems ;
    procedure   Add( pObject : <!TMyClass!>   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : <!TOwner!> read GetOwner      write SetOwner ;
  published
  end ;

implementation

function TCodeTemplate.GetOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := inherited GetOID ;
end;

procedure TCodeTemplate.Add(pObject: <!TMyClass!>; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TCodeTemplate.GetItems(i: integer): <!TMyClass!>;
begin
  result := <!TMyClass!>( inherited GetItems( i )) ;
end;

function TCodeTemplate.GetOwner: <!TOwner!>;
begin
  result := <!TOwner!>( inherited GetOwner );
end;

procedure TCodeTemplate.SetItems(i: integer; const Value: <!TMyClass!>);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TCodeTemplate.SetOwner(const Value: <!TOwner!>);
begin
  inherited SetOwner( Value );
end;

end.

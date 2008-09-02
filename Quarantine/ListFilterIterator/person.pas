unit person;
{<:
   Sample BOM of TPerson.
}

interface
uses
  SysUtils
  ,Classes
  ,tiObject
  ,tiObjectListFilterIterator
  ;

type

  // -----------------------------------------------------------------
  //  Enumeration
  // -----------------------------------------------------------------

  {: Person gender. }
  TPersonGender = (pgFemale, pgMale);

  // -----------------------------------------------------------------
  //  Class Objects
  // -----------------------------------------------------------------

  {: A person object. }
  TPerson = class(TtiObject)
  private
    FAge: Integer;
    FLastName: string;
    FAlive: Boolean;
    FFirstName: string;
    FDateOfHire: TDateTime;
    FGender: TPersonGender;
    procedure SetAge(const Value: Integer);
    procedure SetAlive(const Value: Boolean);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetDateOfHire(const Value: TDateTime);
    procedure SetGender(const Value: TPersonGender);
  published
    property    FirstName: string read FFirstName write SetFirstName;
    property    LastName: string read FLastName write SetLastName;
    property    Age: Integer read FAge write SetAge;
    property    DateOfHire: TDateTime read FDateOfHire write SetDateOfHire;
    property    Alive: Boolean read FAlive write SetAlive;
    property    Gender: TPersonGender read FGender write SetGender;
  end;

  {: List of @link(TPerson) objects. }
  TPersonList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TPerson; reintroduce;
    procedure   SetItems(i: integer; const AValue: TPerson); reintroduce;
  public
    property    Items[i:integer] : TPerson read GetItems write SetItems;
    procedure   Add(AObject : TPerson); reintroduce;
  end;

  {: Iterator to register. }
  TPersonListIterator = class(TtiListFilterIterator)
  public
    function    Current: TPerson; reintroduce;
  end;

implementation

{ TPerson }

procedure TPerson.SetAge(const Value: Integer);
begin
  FAge := Value;
end;

procedure TPerson.SetAlive(const Value: Boolean);
begin
  FAlive := Value;
end;

procedure TPerson.SetDateOfHire(const Value: TDateTime);
begin
  FDateOfHire := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetGender(const Value: TPersonGender);
begin
  FGender := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TPersonList }

procedure TPersonList.Add(AObject: TPerson);
begin
  inherited Add(AObject);
end;

function TPersonList.GetItems(i: integer): TPerson;
begin
  result := TPerson(inherited GetItems(i));
end;

procedure TPersonList.SetItems(i: integer; const AValue: TPerson);
begin
  inherited SetItems(i, AValue);
end;

{ TPersonListIterator }

function TPersonListIterator.Current: TPerson;
begin
  result := TPerson(inherited Current);
end;

initialization;
  gListIteratorMgr.RegisterIterator(TPersonList, TPersonListIterator);

end.

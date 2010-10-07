unit ContactDisplay;

interface

Uses model, displayhelpers, tiObject;

Type
  TContactDisplay = class(TBaseDisplayObject)
  private
    FContact : TContact;
    Procedure SetContact(const AValue : TContact);
    function GetDisplay(AIndex : Integer) : String;
    function CheckContact : Boolean;
  public
    constructor CreateCustom(const AContact : TContact);
    destructor Destroy; override;
    property Contact : TContact read FContact write SetContact;
  published
    property FirstName : String index 0 read GetDisplay;
    property Mobile : String index 2 read GetDisplay;
    property LastName : String index 1 read GetDisplay;
    property Email : String index 3 read GetDisplay;
    property Comments : String index 4 read GetDisplay;
    property DateOfBirth : String index 5 read GetDisplay;
    property HomeAddress: string index 6 read GetDisplay;
  End;

  TContactDisplayList = class(TBaseDisplayList)
  protected
    function CreateDisplayInstance(AItem: TtiObject): TBaseDisplayObject; override;
    function FindDisplayObject(AObject: TtiObject): TBaseDisplayObject; override;
  end;

implementation

uses SysUtils;

{ TContactDisplay }

function TContactDisplay.CheckContact: Boolean;
begin
  Result := Assigned(FContact);
end;

constructor TContactDisplay.CreateCustom(const AContact: TContact);
begin
  Inherited Create;
  Contact := AContact;
end;

destructor TContactDisplay.Destroy;
begin
  Contact := nil;
  inherited;
end;

function TContactDisplay.GetDisplay(AIndex: Integer): String;
begin
  if CheckContact then
  begin
    case AIndex of
      0 : Result := Contact.FirstName;
      1 : Result := Contact.LastName;
      2 : Result := Contact.Mobile;
      3 : Result := Contact.EMail;
      4 : Result := Contact.Comments;
      5 : Result := DateToStr(Contact.DateOfBirth);
      6 : Result := Contact.HomeAddress;
    end; { Case }
  end;
end;

procedure TContactDisplay.SetContact(const AValue: TContact);
begin
  if Contact = AValue then Exit;
  if CheckContact then
    FContact.DetachObserver(Self);
  FContact := AValue;
  if CheckContact then
    FContact.AttachObserver(Self);
end;

{ TContactDisplayList }

function TContactDisplayList.CreateDisplayInstance(AItem: TtiObject): TBaseDisplayObject;
begin
  Result := TContactDisplay.CreateCustom(TContact(AItem));
end;

function TContactDisplayList.FindDisplayObject(AObject: TtiObject): TBaseDisplayObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if (TContactDisplay(Items[i]).Contact = AObject) then
    begin
      Result := TBaseDisplayObject(Items[i]);
      break;
    end;
  end;
end;

end.

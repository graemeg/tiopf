unit tiFilteredObjectList;

interface

uses
  tiObject
  ,tiCriteria  , classes
  ;

type

  TtiFilteredObjectList = class(TtiObjectList)
  private
    FCriteria: TPerCriteria;
  protected
    function    GetCriteria: TPerCriteria;
  public
    constructor Create; override;
    destructor  Destroy; override;

    {: Returns true if the ObjectList has selection critera }
    function HasCriteria: boolean;
    {: Property based selection critera used when reading the list.  This is declared as TtiObject to get around circular references but is of type TPerCriteria}
    property Criteria: TPerCriteria read GetCriteria;

  end;

implementation

{ TtiFilteredObjectList }

constructor TtiFilteredObjectList.Create;
begin
  inherited;
  FCriteria:= nil;
end;

destructor TtiFilteredObjectList.Destroy;
begin
  FCriteria.free;
  inherited;
end;

function TtiFilteredObjectList.GetCriteria: TPerCriteria;
begin
  if not assigned(FCriteria) then
    FCriteria:= TPerCriteria.Create(ClassName);

  result:= FCriteria;
end;

function TtiFilteredObjectList.HasCriteria: boolean;
begin
  result:= Assigned(FCriteria) and FCriteria.HasCriteria;
end;

end.

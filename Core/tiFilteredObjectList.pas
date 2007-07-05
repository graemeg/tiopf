unit tiFilteredObjectList;

interface

uses
  tiObject
  ,tiCriteria  , classes
  ;

type

  TtiFilteredObjectList = class(TtiObjectList)
  private
  protected
    function    GetCriteria: TPerCriteria; reintroduce;
  public
{: Property based selection critera used when reading the list}
    property    Criteria: TPerCriteria read GetCriteria;

  end;

implementation

{ TtiFilteredObjectList }

function TtiFilteredObjectList.GetCriteria: TPerCriteria;
begin
  result:= inherited GetCriteria as TPerCriteria;
end;

end.

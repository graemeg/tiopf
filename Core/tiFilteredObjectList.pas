unit tiFilteredObjectList;

{$I tiDefines.inc}

interface

uses
  tiObject,
  tiCriteria,
  Classes;

type

  TtiFilteredObjectList = class(TtiObjectList, ItiFiltered)
  private
    FCriteria: TtiCriteria;
  protected
    function GetCriteria: TtiCriteria;
  public
    constructor Create; override;
    destructor Destroy; override;
    {: Returns true if the ObjectList has selection critera }
    function HasCriteria: boolean;
    function HasOrderBy: boolean;
    {: Property based selection critera used when reading the list.  This
       is declared as TtiObject to get around circular references but is
       of type TtiCriteria }
    property Criteria: TtiCriteria read GetCriteria;
  end;


  TtiFilteredObjThreadList = class(TPerObjThreadList, ItiFiltered)
  private
    FCriteria: TtiCriteria;
  protected
    function GetCriteria: TtiCriteria;
  public
    constructor Create; override;
    destructor Destroy; override;
    {: Returns true if the ObjectList has selection critera }
    function HasCriteria: boolean;
    function HasOrderBy: boolean;
    {: Property based selection critera used when reading the list.  This
       is declared as TtiObject to get around circular references but is
       of type TtiCriteria }
    property Criteria: TtiCriteria read GetCriteria;
  end;


implementation

{ TtiFilteredObjectList }

constructor TtiFilteredObjectList.Create;
begin
  inherited;
  FCriteria := nil;
end;

destructor TtiFilteredObjectList.Destroy;
begin
  FCriteria.Free;
  inherited;
end;

function TtiFilteredObjectList.GetCriteria: TtiCriteria;
begin
  if not assigned(FCriteria) then
    FCriteria := TtiCriteria.Create(ClassName);

  Result := FCriteria;
end;

function TtiFilteredObjectList.HasCriteria: boolean;
begin
  Result := Assigned(FCriteria) and FCriteria.HasCriteria;
end;

function TtiFilteredObjectList.HasOrderBy: boolean;
begin
  Result := Assigned(FCriteria) and FCriteria.HasOrderBy;
end;

{ TtiFilteredObjThreadList }

constructor TtiFilteredObjThreadList.Create;
begin
  inherited;
  FCriteria := TtiCriteria.Create(ClassName);
end;

destructor TtiFilteredObjThreadList.Destroy;
begin
  FCriteria.Free;
  inherited;
end;

function TtiFilteredObjThreadList.GetCriteria: TtiCriteria;
begin
  Result := FCriteria;
end;

function TtiFilteredObjThreadList.HasCriteria: boolean;
begin
  Result := FCriteria.HasCriteria;
end;

function TtiFilteredObjThreadList.HasOrderBy: boolean;
begin
  Result := FCriteria.HasOrderBy;
end;

end.



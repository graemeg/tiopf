unit tiFilteredObjectList;

{$I tiDefines.inc}

interface

uses
  Classes,
  SysUtils,
  tiObject,
{$IFDEF DELPHI2010ORABOVE}
  tiGenericList,
  tiAutoMapSelect,
{$ENDIF}
  tiCriteria;

type

  TtiFilteredObjectList = class(TtiObjectList, ItiFiltered)
  private
    FCriteria: TtiCriteria;
  protected
    function GetCriteria: TtiCriteria;
  public
    destructor Destroy; override;
    {: Returns true if the ObjectList has selection critera }
    function HasCriteria: boolean;
    function HasOrderBy: boolean;
    {: Property based selection critera used when reading the list.  This
       is declared as TtiObject to get around circular references but is
       of type TtiCriteria }
    property Criteria: TtiCriteria read GetCriteria;
  end;

  ItiFilteredSelectable = interface
  ['{2588C36B-3596-4863-87A4-46394FD22073}']
    procedure Read;
    procedure Save;
    {: Returns the class type of the items in the list }
    function ItemClass: TtiClass;
  end;

{$IFDEF DELPHI2010ORABOVE}
  TtiFilteredObjectList<T: TtiObject> = class(TtiObjectList<T>, ItiFiltered,
      ItiFilteredSelectable)
  private
    FCriteria: TtiCriteria;
  protected
    function GetCriteria: TtiCriteria;
    function ItemClass: TtiClass; virtual;
  public
    destructor Destroy; override;
    //TODO: Is this a hack? Read must be surfaced to use Select
    procedure Read; override;
    procedure Save; override;
    function Select: ItiAutoMapSelect<T>;
    {: Returns true if the ObjectList has selection critera }
    function HasCriteria: boolean;
    function HasOrderBy: boolean;
    {: Property based selection critera used when reading the list.  This
       is declared as TtiObject to get around circular references but is
       of type TtiCriteria }
    property Criteria: TtiCriteria read GetCriteria;
  end;
{$ENDIF}


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

{ TtiFilteredObjectList<T> }

{$IFDEF DELPHI2010ORABOVE}
destructor TtiFilteredObjectList<T>.Destroy;
begin
  FCriteria.Free;
  inherited;
end;

function TtiFilteredObjectList<T>.GetCriteria: TtiCriteria;
begin
  if not assigned(FCriteria) then
    FCriteria := TtiCriteria.Create(ClassName);
  Result := FCriteria;
end;

function TtiFilteredObjectList<T>.HasCriteria: boolean;
begin
  Result := Assigned(FCriteria) and FCriteria.HasCriteria;
end;

function TtiFilteredObjectList<T>.HasOrderBy: boolean;
begin
  Result := Assigned(FCriteria) and FCriteria.HasOrderBy;
end;

function TtiFilteredObjectList<T>.ItemClass: TtiClass;
begin
  Result := T;
end;

procedure TtiFilteredObjectList<T>.Read;
begin
  inherited;
end;

procedure TtiFilteredObjectList<T>.Save;
begin
  inherited;
end;

function TtiFilteredObjectList<T>.Select: ItiAutoMapSelect<T>;
begin
  Result := TtiAutoMapSelectInt<T>.Create(Self);
end;
{$ENDIF}

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


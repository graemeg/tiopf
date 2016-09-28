{
  Instead of adding GUI formatting and sorting to our BOM classes, we have
  helper classes defined here that does the fancy display formatting for
  us. These display objects can then be used as the Subject in mediators like
  the TtiModelMediator.
}

unit tiDisplayHelpers;

{$I tiDefines.inc}

interface

uses
  Classes, SysUtils, tiObject;

type

  TBaseDisplayObject = class(TtiObject)
  public
    procedure Update(ASubject: TtiObject); override;
  end;


  TBaseDisplayList = class(TtiObjectList)
  private
    FSubject: TtiObjectList;
    procedure   ItemAdded(AObject: TtiObject);
    procedure   ItemDeleted(AObject: TtiObject);
    procedure   RebuildList;
  protected
    function    CreateDisplayInstance(AItem: TtiObject): TBaseDisplayObject; virtual; abstract;
    function    FindDisplayObject(AObject: TtiObject): TBaseDisplayObject; virtual; abstract;
  public
    constructor CreateCustom(ASubject: TtiObjectList);
    destructor  Destroy; override;
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject = nil); override;
    property    Subject: TtiObjectList read FSubject;
  end;


implementation

{ TBaseDisplayObject }

procedure TBaseDisplayObject.Update(ASubject: TtiObject);
begin
  inherited Update(ASubject);
  NotifyObservers;
end;


{ TBaseDisplayList }

procedure TBaseDisplayList.ItemAdded(AObject: TtiObject);
begin
  Add(CreateDisplayInstance(AObject));
end;

procedure TBaseDisplayList.ItemDeleted(AObject: TtiObject);
var
  o: TBaseDisplayObject;
begin
  { find display instance and remove that!! NOT the AObject }
  o := FindDisplayObject(AObject);
  if Assigned(o) then
    Remove(o);
end;

procedure TBaseDisplayList.RebuildList;
var
  i: integer;
begin
  Empty;  // empty ourselves
  for i := 0 to FSubject.Count-1 do
  begin
    ItemAdded(FSubject.Items[i]);
  end;
end;

constructor TBaseDisplayList.CreateCustom(ASubject: TtiObjectList);
begin
  Create;
  OwnsObjects := True;
  FSubject := ASubject;
  FSubject.AttachObserver(self);
end;

destructor TBaseDisplayList.Destroy;
begin
  FSubject.DetachObserver(self);
  inherited Destroy;
end;

procedure TBaseDisplayList.Update(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject);
begin
  if (AOperation=noAddItem) then
    ItemAdded(AData)
  else if (AOperation=noDeleteItem) then
    ItemDeleted(AData)
  else if (AOperation=noChanged) then
  begin
     { Safety measure: The displaylist could have been created after the
       FSubject was populated. So would wouldn't have received noAddItem changes. }
    if (FSubject.Count<>Count) or (FSubject.Count=0) then
      RebuildList;
  end
  else if (AOperation=noReSort) then
    RebuildList
  else
    inherited Update(ASubject, AOperation, AData);
end;

end.

unit tiDataFormData;

{$I tiDefines.inc}

interface

uses
  tiBaseObject,
  tiObject;
  
type

  TtiDataFormData = class(TtiObject)
  private
    FData: TtiObject;
    FOwnsData: boolean;
    FSavesData: boolean;
    FOnEditsSave: TtiObjectEvent;
    FOnEditsCancel: TtiObjectEvent;
    function GetHasSave: boolean;
  protected
    function GetData: TtiObject; virtual;
    procedure SetData(AData: TtiObject); virtual;
    function GetIsDirty: boolean; virtual;
    procedure DoPrepareSave; virtual;
    procedure DoSave; virtual;
    procedure DoCancel; virtual;
  public
    constructor Create(AOwnsData: boolean; ASavesData: boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure PrepareSave;
    procedure Save; override;
    procedure Cancel;
    function IsValid(var AErrorMessage: string): boolean; overload;
    function IsValid: boolean; overload;
    property Data: TtiObject read GetData write SetData;
    property IsDirty: boolean read GetIsDirty;
    property OwnsData: boolean read FOwnsData write FOwnsData;
    property SavesData: boolean read FSavesData write FSavesData;
    property OnEditsSave: TtiObjectEvent read FOnEditsSave write FOnEditsSave;
    property OnEditsCancel: TtiObjectEvent read FOnEditsCancel write FOnEditsCancel;
    property HasSave: boolean read GetHasSave;
  end;

  TtiDataFormClonedData = class(TtiDataFormData)
  private
    FEditedData: TtiObject;
  protected
    function GetData: TtiObject; override;
    procedure SetData(AData: TtiObject); override;
    function GetIsDirty: boolean; override;
    procedure DoPrepareSave; override;
    procedure DoUndo; virtual;
  public
    destructor Destroy; override;
    procedure Undo;
    function OriginalData: TtiObject; virtual;
    function EditedData: TtiObject; virtual;
  end;


implementation
uses
  SysUtils,
  tiConstants;

{ TtiDataFormData }

constructor TtiDataFormData.Create(AOwnsData: boolean; ASavesData: boolean);
begin
  FOwnsData := AOwnsData;
  FSavesData := ASavesData;
end;

destructor TtiDataFormData.Destroy;
begin
  if OwnsData then
    FData.Free;
  inherited;
end;

procedure TtiDataFormData.PrepareSave;
begin
  DoPrepareSave;
end;

procedure TtiDataFormData.Save;
begin
  DoSave;
end;

procedure TtiDataFormData.Cancel;
begin
  DoCancel;
end;

procedure TtiDataFormData.DoPrepareSave;
begin
  // We should have ultimate source of data.
  Assert(FData.TestValid(TtiObject), CTIErrorInvalidObject);
  // Set the virtual data (might be different from original) as dirty.
  Data.Dirty := true;
end;

procedure TtiDataFormData.DoSave;
begin
  if Assigned(FOnEditsSave) then
    FOnEditsSave(FData);
end;

procedure TtiDataFormData.DoCancel;
begin
  if Assigned(FOnEditsCancel) then
    FOnEditsCancel(FData);
end;

function TtiDataFormData.GetData: TtiObject;
begin
  Result := FData;
end;

procedure TtiDataFormData.SetData(AData: TtiObject);
begin
  if OwnsData then
    FData.Free;
  FData := AData;
end;

function TtiDataFormData.GetHasSave: boolean;
begin
  Result := SavesData or Assigned(FOnEditsSave);
end;

function TtiDataFormData.GetIsDirty: boolean;
begin
  Result := True;
end;

function TtiDataFormData.IsValid(var AErrorMessage: string): boolean;
begin
  // Note: virtual Data
  AErrorMessage := '';
  Result := (Data <> nil) and Data.IsValid(AErrorMessage);
end;

function TtiDataFormData.IsValid: boolean;
begin
  // Note: virtual Data
  Result := (Data <> nil) and Data.IsValid;
end;

{ TtiDataFormClonedData }

destructor TtiDataFormClonedData.Destroy;
begin
  FEditedData.Free;
  inherited;
end;

procedure TtiDataFormClonedData.Undo;
begin
  DoUndo;
end;

procedure TtiDataFormClonedData.DoPrepareSave;
begin
  Assert(FEditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  inherited;
  OriginalData.Assign(EditedData);
  EditedData.ObjectState := OriginalData.ObjectState;
end;

procedure TtiDataFormClonedData.DoUndo;
begin
  SetData(OriginalData);
end;

function TtiDataFormClonedData.GetIsDirty: boolean;
begin
  Assert(FEditedData.TestValid(TtiBaseObject, true), CTIErrorInvalidObject);
  if (OriginalData <> nil) and (EditedData <> nil) then
  begin
    if EditedData.ObjectState = posCreate then
      Result := True
    else
      Result := not OriginalData.Equals(EditedData)
  end else
    Result := False;
end;

function TtiDataFormClonedData.GetData: TtiObject;
begin
  // The data that the form is mainly interested in is the cloned data for editing.
  Result := FEditedData;
end;

procedure TtiDataFormClonedData.SetData(AData: TtiObject);
begin
  inherited;
  FreeAndNil(FEditedData);
  if OriginalData <> nil then
    FEditedData := OriginalData.Clone;
end;

function TtiDataFormClonedData.OriginalData: TtiObject;
begin
  Result := inherited GetData;
end;

function TtiDataFormClonedData.EditedData: TtiObject;
begin
  Result := FEditedData;
end;

end.

unit FtiEmbeddedDataForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList,

  tiObject, StdCtrls, ExtCtrls;

type
  TFormTIEmbeddedDataForm = class(TForm)
    AL: TActionList;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    procedure ALUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FFormErrorMessage: string;
    FFormInfoMessage: string;
    function GetFormCaption: string;
    procedure SetFormCaption(const AValue: string);
    procedure SetFormErrorMessage(const AValue: string);
    procedure SetFormInfoMessage(const AValue: string);
  protected
    FOriginalData: TtiObject;
    FEditedData: TtiObject;
    function FormIsValid: Boolean; virtual;
    function FormIsDirty: Boolean; virtual;
    procedure SetData(const AValue: TtiObject); virtual;
    procedure SetDataAsNew(const AData: TtiObject);
    // Link persistent aware controls to FEditedData.
    procedure LinkControls; virtual;
    // Refresh data displayed in non persistent aware controls.
    procedure RefreshControls; virtual;
    // Update state of controls.
    procedure UpdateControlState; virtual;
  public
    property Data: TtiObject read FOriginalData write SetData;
    property FormCaption: string read GetFormCaption write SetFormCaption;
    property FormErrorMessage: string read FFormErrorMessage write SetFormErrorMessage;
    property FormInfoMessage: string read FFormInfoMessage write SetFormInfoMessage;
  end;

implementation

uses
  tiConstants
  ,tiApplicationMenuSystem
  ;

{$R *.dfm}

procedure TFormTIEmbeddedDataForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEditedData);
  inherited;
end;

procedure TFormTIEmbeddedDataForm.SetData(const AValue: TtiObject);
begin
  FOriginalData := AValue;
  FreeAndNil(FEditedData);
  if Assigned(FOriginalData) then
    FEditedData := FOriginalData.Clone;
  LinkControls;
  RefreshControls;
end;

procedure TFormTIEmbeddedDataForm.SetDataAsNew(const AData: TtiObject);
begin
  FOriginalData := nil;
  FreeAndNil(FEditedData);
  FEditedData := AData;
  LinkControls;
  RefreshControls;
end;

procedure TFormTIEmbeddedDataForm.ALUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  LErrorMessage: string;
begin
  UpdateControlState;

  LErrorMessage := '';
  if Assigned(FEditedData) then
    FEditedData.IsValid(LErrorMessage);
  gAMS.FormErrorMessage := LErrorMessage;
end;

function TFormTIEmbeddedDataForm.FormIsDirty: Boolean;
begin
  Assert(FOriginalData.TestValid(TtiObject, True), CTIErrorInvalidObject);
  Assert(FEditedData.TestValid(TtiObject, True), CTIErrorInvalidObject);
  Result := Assigned(FEditedData) and
      ((not Assigned(FOriginalData)) or // New
       (not FEditedData.Equals(FOriginalData))); // Modified
//      (Assigned(FEditedData) and (not Assigned(FOriginalData))) or // New
//      (Assigned(FEditedData) and Assigned(FOriginalData) and
//       (not FOriginalData.Equals(FEditedData))); // Modified
end;

function TFormTIEmbeddedDataForm.FormIsValid: Boolean;
var
  LErrorMessage: string;
begin
  Assert(FEditedData.TestValid(TtiObject, True), CTIErrorInvalidObject);
  if Assigned(FEditedData) then
  begin
    Result := FEditedData.IsValid(LErrorMessage);
    if LErrorMessage <> FFormErrorMessage then
      FormErrorMessage := LErrorMessage;
  end else
    Result := True;
end;

function TFormTIEmbeddedDataForm.GetFormCaption: string;
begin
  Result := lblCaption.Caption;
end;

procedure TFormTIEmbeddedDataForm.SetFormCaption(const AValue: string);
begin
  lblCaption.Caption := AValue;
  Self.Caption := AValue;
end;

procedure TFormTIEmbeddedDataForm.SetFormErrorMessage(const AValue: string);
begin
  FFormErrorMessage := AValue;
  gAMS.FormErrorMessage := AValue;
end;

procedure TFormTIEmbeddedDataForm.SetFormInfoMessage(const AValue: string);
begin
  FFormInfoMessage := AValue;
  gAMS.FormInfoMessage := AValue;
end;

procedure TFormTIEmbeddedDataForm.LinkControls;
begin
  // Override in descendant.
end;

procedure TFormTIEmbeddedDataForm.RefreshControls;
begin
  // Override in descendant if required (non per-aware controls etc).
end;

procedure TFormTIEmbeddedDataForm.UpdateControlState;
begin
  // Override in descendant if required.
end;

end.

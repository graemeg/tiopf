unit FtiFormMgrDataForm;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiObject, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly,
  ExtCtrls, ComCtrls, ToolWin, tiBaseObject, Contnrs, FtiFormMgrForm;

const
  cCaptionUndo        = 'Undo  [Ctrl+Z]';
  cCaptionCancelClose = 'Cancel and close  [Esc]';
  cCaptionSaveClose   = 'Save and close  [Ctrl+S]';

type
  TtiDataFormData = class;

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

  TtiFormMgrDataForm = class(TtiFormMgrForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFormData: TtiDataFormData;
    FaUndo: TtiAMSAction;
    FaSaveClose: TtiAMSAction;
    FaCancelClose: TtiAMSAction;

    procedure SaveCloseHandler;
    procedure CancelCloseHandler;
    function GetOnEditsSave: TtiObjectEvent;
    procedure SetOnEditsSave(AOnEditsSave: TtiObjectEvent);
    function GetOnEditsCancel: TtiObjectEvent;
    procedure SetOnEditsCancel(AOnEditsCancel: TtiObjectEvent);
  protected
    function  CreateFormData: TtiDataFormData; virtual; abstract;

    procedure aCloseExecute(Sender: TObject); override;
    procedure aUndoExecute(Sender: TObject);virtual;
    procedure aSaveCloseExecute(Sender: TObject);virtual;
    procedure aCancelCloseExecute(Sender: TObject);virtual;

    procedure DoCancelClose;

    procedure Save;
    procedure SetupButtons; override;
    procedure SetButtonsVisible(const AValue: TtiButtonsVisible); override;

    function  FormIsValid: boolean; override;
    procedure DoALUpdate(Action: TBasicAction; var Handled: Boolean); override;
    function  FormIsDirty: boolean; virtual;

    procedure SetData(const AValue: TtiObject); virtual;
    function  GetData: TtiObject; virtual;
    function  OriginalData: TtiObject; virtual;
    function  EditedData: TtiObject; virtual;

    // Implement these in the concrete...
    procedure DoSave; virtual;
    procedure DoBeforeSave; virtual;
    procedure DoAfterSave; virtual;
    procedure DoAfterDiscard; virtual;
    procedure DoAfterUndo; virtual;
  public
    property  OnEditsSave: TtiObjectEvent read GetOnEditsSave write SetOnEditsSave;
    property  OnEditsCancel: TtiObjectEvent read GetOnEditsCancel write SetOnEditsCancel;
    property  Data: TtiObject read GetData write SetData;
    property  FormData: TtiDataFormData read FFormData;
  end;

implementation
uses
  tiUtils
  ,tiGUIINI
  ,tiConstants
  ,Menus
  ,tiGUIUtils
  ,tiImageMgr
  ,tiResources
  ,tiExcept
 ;

{$R *.DFM}

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
    Result := not OriginalData.Equals(EditedData)
  else
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

{ TtiFormMgrDataForm }

procedure TtiFormMgrDataForm.FormCreate(Sender: TObject);
begin
  inherited;

  FFormData := CreateFormData;
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  
  FaCancelClose := AddAction(cCaptionCancelClose, 'Cancel your edits and close' + ClassName , aCancelCloseExecute, VK_ESCAPE, []);
  FaCancelClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CloseWindow);

  FaUndo := AddAction(cCaptionUndo, 'Un-do changes' + ClassName , aUndoExecute, Ord('Z'), [ssCtrl]);
  FaUndo.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_UnDo);

  FaSaveClose := AddAction(cCaptionSaveClose, 'Save changes' + ClassName , aSaveCloseExecute, Ord('S'), [ssCtrl]);
  FaSaveClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Save);
end;

procedure TtiFormMgrDataForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFormData);
  inherited;
end;

function TtiFormMgrDataForm.GetData: TtiObject;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  Result := FFormData.Data;
end;

procedure TtiFormMgrDataForm.SetData(const AValue: TtiObject);
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  BeginUpdate;
  try
    FFormData.Data := AValue;
  finally
    EndUpdate;
  end;
end;

function TtiFormMgrDataForm.OriginalData: TtiObject;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  if FFormData is TtiDataFormClonedData then
    Result := (FFormData as TtiDataFormClonedData).OriginalData
  else
    Result := FFormData.Data;
end;

function TtiFormMgrDataForm.EditedData: TtiObject;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  // Note: Data is virtual
  Result := FFormData.Data;
end;

function TtiFormMgrDataForm.GetOnEditsSave: TtiObjectEvent;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  Result := FFormData.OnEditsSave;
end;

procedure TtiFormMgrDataForm.SetOnEditsSave(AOnEditsSave: TtiObjectEvent);
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  FFormData.OnEditsSave := AOnEditsSave;
end;

function TtiFormMgrDataForm.GetOnEditsCancel: TtiObjectEvent;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  Result := FFormData.OnEditsCancel;
end;

procedure TtiFormMgrDataForm.SetOnEditsCancel(AOnEditsCancel: TtiObjectEvent);
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  FFormData.OnEditsCancel := AOnEditsCancel;
end;

function TtiFormMgrDataForm.FormIsValid: boolean;
var
  ls: string;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  Result := inherited FormIsValid;
  if Result then
  begin
    Result := FFormData.IsValid(ls);
    if ls <> FormErrorMessage then
      FormErrorMessage := ls;
  end;
end;

function TtiFormMgrDataForm.FormIsDirty: boolean;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  Result := FFormData.IsDirty;
end;

procedure TtiFormMgrDataForm.SetupButtons;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  if not FUpdateButtons then
    Exit; //==>
  // No data
  if FormData.Data = nil then
    ButtonsVisible := btnVisReadOnly
  else
  begin
    // Only show the Save and Cancel buttons if saving is required.
    if FormData.HasSave and (ButtonsVisible <> btnVisReadOnly) then
      ButtonsVisible := btnVisReadWrite
    else
      ButtonsVisible := btnVisReadOnly;
  end;
end;

procedure TtiFormMgrDataForm.DoALUpdate(Action: TBasicAction; var Handled: Boolean);
var
  LFormIsDirty: Boolean;
  LFormIsValid: Boolean;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  inherited;
  // We may be using FormIsValid, even if therre are no buttons visible
  LFormIsValid := FormIsValid;
  if FUpdateButtons then
  begin
    if ButtonsVisible = btnVisReadWrite then
    begin
      LFormIsDirty := FormIsDirty;
      FaUndo.Enabled := (FFormData is TtiDataFormClonedData) and LFormIsDirty;
      FaSaveClose.Enabled := LFormIsValid and LFormIsDirty;
      if LFormIsDirty then
        FaCancelClose.Caption := cCaptionCancelClose
      else
        FaCancelClose.Caption := cCaptionClose;
    end;
  end;
end;

procedure TtiFormMgrDataForm.Save;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  DoBeforeSave;
  FFormData.PrepareSave;
  DoSave;
  FFormData.Save;
  DoAfterSave;
end;

procedure TtiFormMgrDataForm.DoCancelClose;
begin
  DoCloseForm(CancelCloseHandler);
end;

procedure TtiFormMgrDataForm.aCloseExecute(Sender: TObject);
begin
  DoCancelClose;
end;

procedure TtiFormMgrDataForm.aSaveCloseExecute(Sender: TObject);
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  DoCloseForm(SaveCloseHandler);
end;

procedure TtiFormMgrDataForm.aCancelCloseExecute(Sender: TObject);
begin
  DoCancelClose;
end;

procedure TtiFormMgrDataForm.SaveCloseHandler;
begin
  Save;
end;

procedure TtiFormMgrDataForm.CancelCloseHandler;
begin
  Assert(FFormData.TestValid(TtiDataFormData), CTIErrorInvalidObject);
  FFormData.Cancel;
  DoAfterDiscard;
end;

procedure TtiFormMgrDataForm.aUndoExecute(Sender: TObject);
var
  lFocusControl: TWinControl;
begin
  Assert(FFormData.TestValid(TtiDataFormClonedData), CTIErrorInvalidObject);
  BeginUpdate;
  try
    lFocusControl := Screen.ActiveControl;
    // Revert to original data and let UI update.
    SetData((FFormData as TtiDataFormClonedData).OriginalData);
    DoAfterUndo;
    if (lFocusControl <> nil) and
       (lFocusControl.CanFocus) then
    begin
      Self.SetFocus;
      lFocusControl.SetFocus;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TtiFormMgrDataForm.SetButtonsVisible(const AValue: TtiButtonsVisible);
begin
  inherited;
  FaUndo.Visible := (ButtonsVisible = btnVisReadWrite) and (FFormData is TtiDataFormClonedData);
  FaSaveClose.Visible := ButtonsVisible = btnVisReadWrite;
  FaCancelClose.Visible := ButtonsVisible = btnVisReadWrite;
end;

procedure TtiFormMgrDataForm.DoAfterDiscard;
begin
  // Implement in concrete
end;

procedure TtiFormMgrDataForm.DoBeforeSave;
begin
  // Implement in concrete
end;

procedure TtiFormMgrDataForm.DoSave;
begin
  // Implement in concrete
end;

procedure TtiFormMgrDataForm.DoAfterSave;
begin
  // Implement in concrete
end;

procedure TtiFormMgrDataForm.DoAfterUndo;
begin
  // Implement in concrete
end;

end.

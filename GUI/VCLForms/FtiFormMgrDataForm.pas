unit FtiFormMgrDataForm;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, ToolWin, Contnrs

  // tiOPF
  ,tiBaseObject
  ,tiObject
  ,tiReadOnly
  ,FtiFormMgrForm
  ,tiModelMediator
  ,tiDataFormData
  ;

const
  cCaptionUndo        = 'Undo  [Ctrl+Z]';
  cCaptionCancelClose = 'Cancel and close  [Esc]';
  cCaptionSaveClose   = 'Save and close  [Ctrl+S]';

type

  TtiFormMgrDataForm = class(TtiFormMgrForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFormData: TtiDataFormData;
    FaUndo: TtiAMSAction;
    FaSaveClose: TtiAMSAction;
    FaCancelClose: TtiAMSAction;
    FFormSettings: TtiObject;
    FModelMediators: TtiModelMediatorList;

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
    procedure SetFormSettings(const AFormSettings: TtiObject); virtual;
    function  GetData: TtiObject; virtual;
    function  OriginalData: TtiObject; virtual;
    function  EditedData: TtiObject; virtual;

    // Implement these in the concrete...
    procedure DoClearControlDataBindings; virtual;
    procedure DoSetControlDataBindings; virtual;
    procedure DoSave; virtual;
    procedure DoBeforeSave; virtual;
    procedure DoAfterSave; virtual;
    procedure DoAfterDiscard; virtual;
    procedure DoAfterUndo; virtual;

    // If using model-GUI-mediator return a model mediator name for the form data 
    function  ModelMediatorName: string; virtual;
    property  ModelMediators: TtiModelMediatorList read FModelMediators;
  public
    property  OnEditsSave: TtiObjectEvent read GetOnEditsSave write SetOnEditsSave;
    property  OnEditsCancel: TtiObjectEvent read GetOnEditsCancel write SetOnEditsCancel;
    property  Data: TtiObject read GetData write SetData;
    property  FormSettings: TtiObject read FFormSettings write SetFormSettings;
    property  FormData: TtiDataFormData read FFormData;
  end;

implementation
uses
  // Delphi
  Menus
  // tiOPF
  ,tiUtils
  ,tiGUIINI
  ,tiConstants
  ,tiGUIUtils
  ,tiImageMgr
  ,tiResources
  ,tiExcept
  ,tiMediators
  ,tiListMediators
 ;

{$R *.DFM}

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

  FModelMediators := TtiModelMediatorList.Create(Self);
  FModelMediators.Name := Self.ClassName + 'ModelMediators';
  if ModelMediatorName <> '' then
    FModelMediators.Add(ModelMediatorName);
  tiMediators.RegisterFallBackMediators;
  tiListMediators.RegisterFallBackListMediators;
end;

procedure TtiFormMgrDataForm.FormDestroy(Sender: TObject);
begin
  FModelMediators.Free;
  FreeAndNil(FFormData);
  inherited;
end;

function TtiFormMgrDataForm.ModelMediatorName: string;
begin
  result := '';
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
    if ModelMediatorName <> '' then
      FModelMediators.SubjectByName[ModelMediatorName] := nil;

    DoClearControlDataBindings;
    FFormData.Data := AValue;
    if Assigned(Data) then
      DoSetControlDataBindings;

    // This must be done after DoSetControlDataBindings which could change the
    // contents of controls and therefore affect item selection by mediators.
    if ModelMediatorName <> '' then
    begin
      FModelMediators.SubjectByName[ModelMediatorName] := Data;
      FModelMediators.ActiveByName[ModelMediatorName] := true;
    end;

    SetupButtons;
  finally
    EndUpdate;
  end;
end;

procedure TtiFormMgrDataForm.SetFormSettings(const AFormSettings: TtiObject);
begin
  FFormSettings := AFormSettings;
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

procedure TtiFormMgrDataForm.DoClearControlDataBindings;
begin
  // Implement in concrete
end;

procedure TtiFormMgrDataForm.DoSetControlDataBindings;
begin
  // Implement in concrete
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
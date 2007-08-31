{$I tiDefines.inc}

unit FtiFormMgrForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiObject, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly,
  ExtCtrls, ComCtrls, ToolWin, tiBaseObject, Contnrs;

const
  TI_CLOSEACTIVEFORM = WM_USER + 1000;
  cCaptionClose       = 'Close  [Esc]';
  cCaptionUndo        = 'Undo  [Ctrl+Z]';
  cCaptionCancelClose = 'Cancel and close  [Esc]';
  cCaptionSaveClose   = 'Save and close  [Ctrl+S]';

type

  TtiUserFeedbackMessageType = (tiufmtInfo, tiufmtError);
  TFormTIFormMgrForm = class;
  TtiOnShowFormEvent = procedure(const pForm : TFormTIFormMgrForm) of object;
  TtiOnFormMessageEvent = procedure(const AMessage: string; pMessageType: TtiUserFeedbackMessageType) of object;
  TLogEvent = procedure(const AMessage : string) of object;
  TtiFormMgr = class;

  TtiButtonsVisible = (btnVisNone, btnVisReadOnly, btnVisReadWrite);

// Paste this into the concrete class
//  protected
//    procedure SetData(const AValue: TtiObject); override;
//    function  FormIsValid : boolean; override;

  // Custom ApplicationMenuSystem action
  TtiAMSAction = class(TAction)
  private
    FShowInMenuSystem: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property ShowInMenuSystem: boolean read FShowInMenuSystem write FShowInMenuSystem;
  end;


  // ToDo: Remove pnlCaption - but this will require some effort as it may be referenced by child forms
  TFormTIFormMgrForm = class(TForm)
    pnlCaption: TPanel;
    lblCaption: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FAL        : TActionList;
    FaClose     : TtiAMSAction;
    FaUndo      : TtiAMSAction;
    FaSaveClose : TtiAMSAction;
    FaCancelClose: TtiAMSAction;
    FaDummy     : TtiAMSAction;

    FButtonsVisible: TtiButtonsVisible;
    FForceCanLeave: boolean;
    FIsModal : boolean;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FFormErrorMessage: string;
    FFormInfoMessage: string;
    FOnEditsSave: TtiObjectEvent;
    FOnEditsCancel: TtiObjectEvent;
    FFormMgr: TtiFormMgr;
//    FButtonsSetup: Boolean;
    FLastActiveControl: TWinControl;

    function  GetFormCaption: string;
    procedure SetFormCaption(const AValue: string);
    procedure SetButtonsVisible(const AValue: TtiButtonsVisible);
    function  GetBorderColor: TColor;
    procedure SetBorderColor(const AValue: TColor);
    procedure SetFormErrorMessage(const AValue: string);
    procedure SetFormInfoMessage(const AValue: string);
    // These are set by the FormMgr so can be hidden from outside view
    property  OnEditsSave: TtiObjectEvent read FOnEditsSave Write FOnEditsSave;
    property  OnEditsCancel: TtiObjectEvent read FOnEditsCancel Write FOnEditsCancel;
    procedure SetIsModal(const AValue: Boolean);

  protected
    FData       : TtiObject;
    FDataBuffer : TtiObject;
    FUpdateButtons : boolean;

    procedure alUpdate(Action: TBasicAction; var Handled: Boolean);virtual;
    function  AddAction(const pCaption: string;
                        const pOnExecute: TNotifyEvent;
                        const pHint: string = '';
                              pImageIndex: Integer = -1;
                              pHelpContext: Integer = -1): TtiAMSAction; overload;
    function  AddAction(const pCaption: string; const pHint: string;
                        const pOnExecute: TNotifyEvent;
                        pShortCutKey: Word; pShortCutShiftState: TShiftState): TtiAMSAction; overload;

    procedure aUndoExecute(Sender: TObject);virtual;
    procedure aSaveCloseExecute(Sender: TObject);virtual;
    procedure aCancelCloseExecute(Sender: TObject);virtual;
    procedure aCloseExecute(Sender: TObject);virtual;
    procedure aDummyExecute(Sender: TObject);

    function  FormIsValid : boolean; virtual;
    function  FormIsDirty : boolean; virtual;
    procedure SetData(const AValue: TtiObject); virtual;
    function  Databuffer : TtiObject; virtual;
    function  GetData: TtiObject; virtual;

    // Implement these in the concrete...
    property  ButtonsVisible : TtiButtonsVisible read FButtonsVisible write SetButtonsVisible;
    property  ForceCanLeave : boolean read FForceCanLeave write FForceCanLeave;
    procedure DoBeforeSave; virtual;
    procedure DoAfterSave; virtual;
    procedure DoAfterDiscard; virtual;
    procedure DoAfterUndo; virtual;
    procedure SetupButtons; virtual;
    procedure SelectFirstControl; virtual;

    property  UpdateButtons : boolean read FUpdateButtons write FUpdateButtons;
    property  LastActiveControl: TWinControl read FLastActiveControl Write FLastActiveControl;

  public
    procedure AssignActions(const AList: TList);
    property  Data : TtiObject read GetData write SetData;
    property  FormCaption : string read GetFormCaption write SetFormCaption;
    property  IsModal : Boolean read FIsModal write SetIsModal;
    property  BorderColor : TColor read GetBorderColor write SetBorderColor;
    property  OnFormMessage : TtiOnFormMessageEvent read FOnFormMessageEvent Write FOnFormMessageEvent;
    property  FormErrorMessage: string read FFormErrorMessage Write SetFormErrorMessage;
    property  FormInfoMessage:  string read FFormInfoMessage Write SetFormInfoMessage;
    property  FormMgr: TtiFormMgr read FFormMgr Write FFormMgr;
  end;

  TFormTIFormMgrFormClass = class of TFormTIFormMgrForm;

  TtiFormMgr = class(TtiBaseObject)
  private
    FForms : TObjectList;
    FModalForms : TObjectList;
    FActiveForm : TFormTIFormMgrForm;
    FParentPnl: TPanel;
    FBorderColor: TColor;
    FOnShowForm: TtiOnShowFormEvent;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    function    GetNextForm: TFormTIFormMgrForm;
    function    GetPrevForm: TFormTIFormMgrForm;
    function    GetForms(i: integer): TFormTIFormMgrForm;
    procedure   SetForms(i: integer; const AValue: TFormTIFormMgrForm);
    function    GetActiveForm: TFormTIFormMgrForm;
    procedure   SetActiveForm(const AValue: TFormTIFormMgrForm);
    procedure   Add(const pForm : TFormTIFormMgrForm);
    procedure   DoCloseForm(const pForm : TFormTIFormMgrForm);
    function    CloseCurrentFormActivatePreviousForm(pClose : boolean): boolean;
    procedure   SetBorderColor(const AValue: TColor);
    procedure   DoOnShowForm(const pForm : TFormTIFormMgrForm);
    procedure   SetParentPnl(const AValue: TPanel);
    procedure   DoBeginUpdate;
    procedure   DoEndUpdate;
    procedure   HideActiveForm;
    function    GetFormCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure   ShowForm(     const pFormClass     : TFormTIFormMgrFormClass;
                               const AData          : TtiObject = nil;
                                     pModal         : Boolean = False;
                               const pOnEditsSave   : TtiObjectEvent = nil;
                               const pOnEditsCancel : TtiObjectEvent = nil;
                                     pReadOnly      : boolean = False); overload;
    procedure   ShowForm(     const pForm          : TFormTIFormMgrForm     ;
                               const AData          : TtiObject = nil;
                                     pModal         : Boolean = False;
                               const pOnEditsSave   : TtiObjectEvent = nil;
                               const pOnEditsCancel : TtiObjectEvent = nil;
                                     pReadOnly      : boolean = False); overload;
    procedure   ShowFormModal(const pFormClass     : TFormTIFormMgrFormClass;
                               const AData          : TtiObject;
                               const pOnEditsSave   : TtiObjectEvent = nil;
                               const pOnEditsCancel : TtiObjectEvent = nil);

    procedure   BringToFront(const pForm : TFormTIFormMgrForm; pFocusFirstControl : Boolean);

    function    FindForm(const pFormClass : TFormTIFormMgrFormClass; const AData : TtiObject): TFormTIFormMgrForm;
    function    IndexOf(const pForm : TFormTIFormMgrForm): integer;

    procedure   CloseForm(const pForm : TFormTIFormMgrForm);
    procedure   CloseAllForms;
    procedure   RemoveForm(const pForm : TFormTIFormMgrForm);

    property    ActiveForm : TFormTIFormMgrForm read GetActiveForm write SetActiveForm;
    property    Forms[i:integer]: TFormTIFormMgrForm read GetForms write SetForms;
    property    FormCount: Integer read GetFormCount;
    property    ParentPnl : TPanel read FParentPnl write SetParentPnl;
    procedure   ShowPrevForm;
    procedure   ShowNextForm;
    property    NextForm : TFormTIFormMgrForm read GetNextForm;
    property    PrevForm : TFOrmTIFormMgrForm read GetPrevForm;
    property    BorderColor : TColor read FBorderColor write SetBorderColor;
    property    OnShowForm : TtiOnShowFormEvent read FOnShowForm write FOnShowForm;
    property    OnFormMessage: TtiOnFormMessageEvent read FOnFormMessageEvent write FOnFormMessageEvent;
    property    OnBeginUpdate: TNotifyEvent read FOnBeginUpdate Write FOnBeginUpdate;
    property    OnEndUpdate:   TNotifyEvent read FOnEndUpdate Write FOnEndUpdate;
    procedure   AssignFormList(const AStrings : TStrings);
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

procedure TFormTIFormMgrForm.FormCreate(Sender: TObject);
begin
  inherited;

  KeyPreview := true;

  {$IFDEF DEBUG}
  Assert(HelpContext = 0, ClassName + ' help context has been set in the IDE for form "' + ClassName +'"');
  {$ENDIF}

  FAL := TActionList.Create(Self);
  FAL.OnUpdate := ALUpdate;

  FaClose := AddAction(cCaptionClose, 'Close this page' + ClassName , aCloseExecute, VK_ESCAPE, []);
  FaClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CloseWindow);

  FaCancelClose := AddAction(cCaptionCancelClose, 'Cancel your edits and close' + ClassName , aCancelCloseExecute, VK_ESCAPE, []);
  FaCancelClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CloseWindow);

  FaUndo := AddAction(cCaptionUndo, 'Un-do changes' + ClassName , aUndoExecute, Ord('Z'), [ssCtrl]);
  FaUndo.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_UnDo);

  FaSaveClose := AddAction(cCaptionSaveClose, 'Save changes' + ClassName , aSaveCloseExecute, Ord('S'), [ssCtrl]);
  FaSaveClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_Save);

  FaDummy := AddAction('DymmyAction', aDummyExecute);
  lblCaption.Action := FaDummy;

  FForceCanLeave := false;
  FIsModal := false;
  FUpdateButtons := true;

  pnlCaption.TabOrder := ControlCount - 1;

  // A hack to remove the pnlCaption, without going through all the pain
  // of removing references in descendants of TFormTIFormMgrForm
  // This pnl contains a TLable, which is hooked up to a TAction - necessary to get the form's
  // ActionList.OnUpdate event to fire. (This is where form validation takes place)
  pnlCaption.Visible := True;
  pnlCaption.Height := 0;

end;

procedure TFormTIFormMgrForm.FormDestroy(Sender: TObject);
begin
  // The currently active form will have it's parent set. If the application is
  // shutting down, then the parent will be destroyed when the main form
  // is destroyed. This will cause the active form to be destroyed twice,
  // with the associated AV. The If statement below stops this.
  if FormMgr.IndexOf(Self) <> -1 then
    FormMgr.RemoveForm(Self);
  FreeAndNil(FDataBuffer);
  inherited;
end;

procedure TFormTIFormMgrForm.SetData(const AValue: TtiObject);
begin
  FAL.OnUpdate := nil;
  try
    FData := AValue;
    FreeAndNil(FDataBuffer);
    if FData <> nil then
      FDataBuffer := FData.Clone;
  finally
    FAL.OnUpdate := ALUpdate;
  end;
end;

procedure TFormTIFormMgrForm.alUpdate(Action: TBasicAction; var Handled: Boolean);
var
  lFormIsDirty : Boolean;
  lFormIsValid : Boolean;
begin
  Handled := true;
  Assert(FData.TestValid(TtiObject, true), cTIInvalidObjectError + ' <FData>');
  Assert(FDataBuffer.TestValid(TtiObject, true), cTIInvalidObjectError + '<FDataBuffer>');
  lFormIsDirty := FormIsDirty;
  lFormIsValid := FormIsValid;
  if FUpdateButtons then
  begin
    case FButtonsVisible of
    btnVisNone     :; // Do nothing
    btnVisReadOnly :; // Do nothing
    btnVisReadWrite : begin
                        FaUndo.Enabled := (FData <> nil) and lFormIsDirty;
                        FaSaveClose.Enabled := (FData <> nil) and lFormIsValid and lFormIsDirty;
                        if lFormIsDirty then
                          FaCancelClose.Caption := cCaptionCancelClose
                        else
                          FaCancelClose.Caption := cCaptionClose;
                      end;
    else
      raise EtiOPFProgrammerException.Create('Invalid TtiButtonsVisible');
    end;
  end;
end;

function TFormTIFormMgrForm.FormIsValid: boolean;
var
  ls: string;
begin
  Assert(DataBuffer.TestValid(TtiObject, true), cTIInvalidObjectError);
  if DataBuffer <> nil then
  begin
    Result := DataBuffer.IsValid(ls);
    if ls <> FFormErrorMessage then
      FormErrorMessage := ls;
  end else
    result := true;
end;


procedure TFormTIFormMgrForm.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons;
end;

procedure TFormTIFormMgrForm.SetupButtons;
begin
  if not FUpdateButtons then
   Exit; //==>
  // No data
  if (FData = nil) or
     (FDataBuffer = nil) then
    ButtonsVisible := btnVisReadOnly
  else
  begin
    // Only show the Save and Cancel buttons if the form is 'Modal'
    if FIsModal and (ButtonsVisible <> btnVisReadOnly) then
      ButtonsVisible := btnVisReadWrite
    else
      ButtonsVisible := btnVisReadOnly;
  end;
end;

procedure TFormTIFormMgrForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0; // Eat the Beep
    SelectNext(ActiveControl AS TWinControl, True, True) // Forward
  end
  else
  if Key = #2 then
    SelectNext(ActiveControl AS TWinControl, False, True) // Backward
end;

function TFormTIFormMgrForm.GetFormCaption: string;
begin
  result := lblCaption.Caption;
end;

procedure TFormTIFormMgrForm.SetFormCaption(const AValue: string);
begin
  lblCaption.Caption := AValue;
end;

procedure TFormTIFormMgrForm.aSaveCloseExecute(Sender: TObject);
begin
  Assert(FData <> nil, 'FData not assigned');
  Assert(FDataBuffer <> nil, 'FDataBuffer not assigned');
  try
    FAL.OnUpdate := nil;
    Visible := False;
    DoBeforeSave;
    FDataBuffer.Dirty := true;
    FData.Assign(FDataBuffer);
    FDataBuffer.ObjectState := Data.ObjectState;
    DoAfterSave;
  finally
    FormMgr.CloseForm(Self);
  end;
end;

procedure TFormTIFormMgrForm.aCancelCloseExecute(Sender: TObject);
begin
  try
    FAL.OnUpdate := nil;
    Visible := False;
    DoAfterDiscard;
  finally
    FormMgr.CloseForm(Self);
  end;
end;

procedure TFormTIFormMgrForm.aUndoExecute(Sender: TObject);
var
  lFocusControl: TWinControl;
begin
  FAL.OnUpdate := nil;
  try
    lFocusControl := Screen.ActiveControl;
    SetData(Data);
    DoAfterUndo;
    if (lFocusControl <> nil) and
       (lFocusControl.CanFocus) then
    begin
      Self.SetFocus;
      lFocusControl.SetFocus;
    end;
  finally
    FAL.OnUpdate := ALUpdate;
  end;
end;

procedure TFormTIFormMgrForm.aCloseExecute(Sender: TObject);
begin
  aCancelCloseExecute(nil);
end;

procedure TFormTIFormMgrForm.SetButtonsVisible(const AValue: TtiButtonsVisible);
begin
//  FButtonsSetup := True;
  FButtonsVisible := AValue;
  case FButtonsVisible of
  btnVisNone     : begin
                      FaClose.Visible      := False;
                      FaUndo.Visible       := False;
                      FaSaveClose.Visible  := False;
                      FaCancelClose.Visible := False;
                    end;
  btnVisReadOnly : begin
                      FaClose.Visible      := True;
                      FaUndo.Visible       := False;
                      FaSaveClose.Visible  := False;
                      FaCancelClose.Visible := False;
                    end;
  btnVisReadWrite : begin
                      FaClose.Visible      := False;
                      FaUndo.Visible       := True;
                      FaSaveClose.Visible  := True;
                      FaCancelClose.Visible := True;
                    end;
  else
    raise EtiOPFProgrammerException.Create('Invalid TtiButtonsVisible');
  end;
end;

procedure TFormTIFormMgrForm.DoAfterDiscard;
begin
  if Assigned(FOnEditsCancel) then
    FOnEditsCancel(Data);
end;

procedure TFormTIFormMgrForm.DoBeforeSave;
begin
  // Implement in concrete
end;

procedure TFormTIFormMgrForm.DoAfterUndo;
begin
  // Implement in concrete
end;

function TFormTIFormMgrForm.FormIsDirty: boolean;
begin
  Assert(Data.TestValid(TtiBaseObject, true), cTIInvalidObjectError);
  Assert(DataBuffer.TestValid(TtiBaseObject, true), cTIInvalidObjectError);
  if (Data <> nil) and (DataBuffer <> nil) then
    Result := (not Data.Equals(DataBuffer))
  else
    Result := False;
end;

function TFormTIFormMgrForm.GetBorderColor: TColor;
begin
  result := pnlCaption.Color;
end;

procedure TFormTIFormMgrForm.SetBorderColor(const AValue: TColor);
begin
  pnlCaption.Color := AValue;
end;

function TFormTIFormMgrForm.AddAction(
  const pCaption: string;
  const pOnExecute:   TNotifyEvent;
  const pHint:        string = '';
        pImageIndex:  Integer = -1;
        pHelpContext: Integer = -1): TtiAMSAction;
begin
  Result := TtiAMSAction.Create(FAL);
  Result.ActionList := FAL;
  Result.Caption  := pCaption;
  Result.OnExecute := pOnExecute;
  Result.Hint     := pHint;
  Result.ImageIndex := pImageIndex;
  Result.HelpContext := pHelpContext;
end;

function TFormTIFormMgrForm.AddAction(const pCaption, pHint: string;
  const pOnExecute: TNotifyEvent; pShortCutKey: Word; pShortCutShiftState: TShiftState): TtiAMSAction;
begin
  Result := AddAction(pCaption, pOnExecute, pHint);
  Result.ShortCut := Shortcut(Word(pShortCutKey), pShortCutShiftState);
end;

function TFormTIFormMgrForm.Databuffer: TtiObject;
begin
  Result := FDataBuffer;
end;

procedure TFormTIFormMgrForm.SetFormErrorMessage(const AValue: string);
begin
  FFormErrorMessage := AValue;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormErrorMessage, tiufmtError);
end;

procedure TtiFormMgr.Add(const pForm: TFormTIFormMgrForm);
begin
  pForm.BorderStyle := bsNone;
  pForm.Parent := ParentPnl;
  pForm.Align := alClient;
  FForms.Add(pForm);
end;

procedure TtiFormMgr.BringToFront(const pForm: TFormTIFormMgrForm; pFocusFirstControl : Boolean);
begin
  Assert(pForm<>nil, 'pForm not assigned');
  if pForm = FActiveForm then
    Exit; //==>

  // Move to the end of the list
  FForms.Extract(pForm);
  FForms.Add(pForm);

  pForm.Visible := true;
  pForm.SetFocus;

  if (pForm.LastActiveControl <> nil) then
          pForm.LastActiveControl.SetFocus
  else
    pForm.SelectFirstControl;

  if FActiveForm <> nil then
    FActiveForm.Visible := false;
  FActiveForm := pForm;
  DoOnShowForm(FActiveForm);
end;

procedure TtiFormMgr.CloseAllForms;
var
  i : Integer;
begin
  for i := FForms.Count - 1 downto 0 do
    if (FForms.Items[i] as TFormTIFormMgrForm).ButtonsVisible in [btnVisReadOnly, btnVisReadWrite] then
      DoCloseForm(Forms[i]);
end;

procedure TtiFormMgr.DoCloseForm(const pForm: TFormTIFormMgrForm);
begin
  if ActiveForm = pForm then
  begin
    if GetPrevForm <> nil then
      ActiveForm := GetPrevForm
    else
      ActiveForm := GetNextForm;
  end;
  FForms.Extract(pForm);
  if FModalForms.IndexOf(pForm) <> -1 then
    FModalForms.Extract(pForm);
  if ActiveForm = pForm then
    HideActiveForm;
  DoOnShowForm(FActiveForm);
  pForm.Free;
end;

constructor TtiFormMgr.Create;
begin
  inherited;
  FForms := TObjectList.Create(false);
  FModalForms := TObjectList.Create(false);
end;

destructor TtiFormMgr.Destroy;
var
  i : integer;
  lForm : TFormTIFormMgrForm;
  lName : string;
begin
  for i := FForms.Count - 1 downto 0 do
  begin
    lForm := FForms.Items[i] as TFormTIFormMgrForm;
    lName := lForm.ClassName;
    FForms.Extract(lForm);
    try
      FreeAndNil(lForm);
    except
      on e:exception do
        ShowMessage('Error destroying form <' + lName + '>' + Cr +
                     'Message: ' + e.Message + Cr +
                     'Location: ' + ClassName + '.Destroy');
    end;
  end;
  FreeAndNil(FForms);
  FreeAndNil(FModalForms);
  inherited;
end;

function TtiFormMgr.FindForm(const pFormClass: TFormTIFormMgrFormClass; const AData : TtiObject): TFormTIFormMgrForm;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FForms.Count - 1 do
    if (FForms.Items[i] is pFormClass) and
       (TFormTIFormMgrForm(FForms.Items[i]).Data = AData) then
    begin
      result := FForms.Items[i] as TFormTIFormMgrForm;
      Exit; //==>
    end;
end;

function TtiFormMgr.GetActiveForm: TFormTIFormMgrForm;
begin
  result := FActiveForm;
end;

function TtiFormMgr.GetForms(i: integer): TFormTIFormMgrForm;
begin
  result := FForms[i] as TFormTIFormMgrForm;
end;

function TtiFormMgr.GetNextForm: TFormTIFormMgrForm;
begin
  if FForms.Count > 0 then
    result := Forms[0]
  else
    result := nil;
end;

function TtiFormMgr.GetPrevForm: TFormTIFormMgrForm;
var
  lIndex: Integer;
begin
  lIndex := FForms.IndexOf(ActiveForm);
  if lIndex > 0 then
    result := Forms[lIndex-1]
  else
    result := nil;
end;

function TtiFormMgr.IndexOf(const pForm: TFormTIFormMgrForm): integer;
begin
  result := FForms.IndexOf(pForm);
end;

procedure TtiFormMgr.RemoveForm(const pForm: TFormTIFormMgrForm);
begin
  FForms.Remove(pForm)
end;

procedure TtiFormMgr.SetActiveForm(const AValue: TFormTIFormMgrForm);
begin
  if AValue = nil then
  begin
    FActiveForm := AValue;
    Exit; //==>
  end;
  ShowForm(AValue, AValue.Data);
end;

procedure TtiFormMgr.SetForms(i: integer; const AValue: TFormTIFormMgrForm);
begin
  FForms[i]:= AValue;
end;

procedure  TtiFormMgr.ShowForm(
      const pForm         : TFormTIFormMgrForm;
      const AData         : TtiObject = nil;
            pModal        : Boolean = False;
      const pOnEditsSave  : TtiObjectEvent = nil;
      const pOnEditsCancel : TtiObjectEvent = nil;
            pReadOnly     : boolean = False);
begin
  ShowForm(TFormTIFormMgrFormClass(pForm.ClassType), AData, pModal, pOnEditsSave, pOnEditsCancel, pReadOnly);
end;

procedure TtiFormMgr.ShowForm(
      const pFormClass     : TFormTIFormMgrFormClass;
      const AData : TtiObject = nil;
            pModal: Boolean = False;
      const pOnEditsSave  : TtiObjectEvent = nil;
      const pOnEditsCancel : TtiObjectEvent = nil;
            pReadOnly : boolean = False
     );
var
  LForm : TFormTIFormMgrForm;
  LCursor: TCursor;
begin
  Assert(ParentPnl <> nil, 'ParentPnl not assigned');
  LCursor:= Screen.Cursor; // tiAutoCursor not working here - I don't understand why
  try
    DoBeginUpdate;
    try
      if (FActiveForm <> nil) and
         (not FActiveForm.IsModal) and
         (not CloseCurrentFormActivatePreviousForm(false)) then
        Exit; //==>
      LForm := FindForm(pFormClass, AData);
      if LForm <> nil then
        BringToFront(LForm, false)
      else
      begin
        HideActiveForm;
        LForm := pFormClass.Create(nil);

       {$IFDEF DEBUG}
        Assert(LForm.HelpContext <> 0, LForm.ClassName + ' help context not set <' + LForm.Name +'>');
       {$ENDIF}

        LForm.BorderColor   := BorderColor;
        LForm.OnFormMessage := FOnFormMessageEvent;
        LForm.OnEditsSave   := pOnEditsSave;
        LForm.OnEditsCancel := pOnEditsCancel;
        LForm.IsModal       := pModal;
        LForm.FormMgr       := Self;
        // ToDo: Not sure of the need for FModalForms any more
        if pModal then
          FModalForms.Add(LForm);
        Add(LForm);
        if LForm.Data <> AData then
          LForm.Data := AData;
        LForm.SetupButtons;
        BringToFront(LForm, true);
      end;
    finally
      DoEndUpdate;
    end;
  finally
    Screen.Cursor:= LCursor;
  end;
end;

procedure TtiFormMgr.CloseForm(const pForm: TFormTIFormMgrForm);
begin
  Assert(FActiveForm = pForm, 'Close for can only be called with currently active form');
  CloseCurrentFormActivatePreviousForm(true);
end;

function TtiFormMgr.CloseCurrentFormActivatePreviousForm(pClose : boolean): boolean;
var
  lForm : TFormTIFormMgrForm;
  lActiveForm : TFormTIFormMgrForm;
begin
  DoBeginUpdate;
  try
    DoOnShowForm(nil);
    if FActiveForm.IsModal then
    begin
      if GetPrevForm <> nil then
        lActiveForm := GetPrevForm
      else
        lActiveForm := GetNextForm;
      lForm := FActiveForm;
      HideActiveForm;
      FForms.Extract(lForm);
      FreeAndNil(lForm);
      ActiveForm := lActiveForm;
      result := true;
      DoOnShowForm(ActiveForm);
    end else
    begin
      if pClose then
      begin
        DoCloseForm(FActiveForm);
        result := true;
      end
      else begin
        HideActiveForm;
        result := true;
        DoOnShowForm(Nil);
      end;
    end;
  finally
    DoEndUpdate;
  end;
end;

procedure TtiFormMgr.ShowNextForm;
var
  lForm : TFormTIFormMgrForm;
  lActiveForm: TFormTIFormMgrForm;
begin
  lForm := GetNextForm;
  Assert(LForm<>nil, 'GetNextForm returned nil');
  lActiveForm := ActiveForm;
  FForms.Extract(lActiveForm);
  FForms.Add(lActiveForm);
  ShowForm(lForm, lForm.Data);
end;

procedure TtiFormMgr.ShowPrevForm;
var
  LForm : TFormTIFormMgrForm;
  LActiveForm: TFormTIFormMgrForm;
begin
  LForm := GetPrevForm;
  Assert(LForm<>nil, 'GetNextForm returned nil');
  LActiveForm := ActiveForm;
  FForms.Extract(LActiveForm);
  FForms.Insert(0, LActiveForm);
  ShowForm(LForm, LForm.Data);
end;

procedure TtiFormMgr.SetBorderColor(const AValue: TColor);
var
  i : integer;
begin
  FBorderColor := AValue;
  for i := 0 to FForms.Count - 1 do
    (FForms.Items[i] as TFormTIFormMgrForm).BorderColor := FBorderColor;
end;

procedure TtiFormMgr.AssignFormList(const AStrings: TStrings);
var
  i : integer;
begin
  AStrings.Clear;
  for i := 0 to FForms.Count - 1 do
    if FForms.Items[i] <> ActiveForm then
      AStrings.AddObject((FForms.Items[i] as TFormTIFormMgrForm).FormCaption,
                          FForms.Items[i]);
end;

procedure TtiFormMgr.DoOnShowForm(const pForm : TFormTIFormMgrForm);
begin
  if assigned(FOnShowForm) then
    FOnShowForm(pForm);
  if (pForm <> nil) then
  begin
    if Assigned(FOnFormMessageEvent) then
    begin
      if (pForm.FormErrorMessage <> '') then
        FOnFormMessageEvent(pForm.FormErrorMessage, tiufmtError)
//      else if (pForm.FormInfoMessage <> '') then
//        FOnFormMessageEvent(pForm.FormInfoMessage, tiufmtInfo)
      else
        FOnFormMessageEvent('', tiufmtInfo)
    end;
  end;
end;


procedure TtiFormMgr.SetParentPnl(const AValue: TPanel);
begin
  FParentPnl := AValue;
end;

procedure TtiFormMgr.ShowFormModal(
  const pFormClass : TFormTIFormMgrFormClass;
  const AData : TtiObject;
  const pOnEditsSave  : TtiObjectEvent = nil;
  const pOnEditsCancel : TtiObjectEvent = nil);
begin
  ShowForm(pFormClass, AData, True, pOnEditsSave, pOnEditsCancel, False);
end;

procedure TFormTIFormMgrForm.DoAfterSave;
begin
  if Assigned(FOnEditsSave) then
    FOnEditsSave(Data);
end;

procedure TFormTIFormMgrForm.aDummyExecute(Sender: TObject);
begin
  // Do nothing. Dummy action is just to get the ActionLists OnUpdate method to fire
end;

procedure TFormTIFormMgrForm.AssignActions(const AList: TList);
var
  i : Integer;
begin
  AList.Clear;
  for i := 0 to FAL.ActionCount - 1 do
    if (FAL.Actions[i] <> FaDummy) and
       ((FAL.Actions[i] as TtiAMSAction).Visible) and
       ((FAL.Actions[i] as TtiAMSAction).ShowInMenuSystem) then
      AList.Add(FAL.Actions[i]);
end;


procedure TFormTIFormMgrForm.SetIsModal(const AValue: Boolean);
begin
  FIsModal := AValue;
end;

procedure TtiFormMgr.DoBeginUpdate;
begin
  if Assigned(FOnBeginUpdate) then
    FOnBeginUpdate(Self);
end;

procedure TtiFormMgr.DoEndUpdate;
begin
  if Assigned(FOnEndUpdate) then
    FOnEndUpdate(Self);
end;

procedure TtiFormMgr.HideActiveForm;
begin
  if FActiveForm <> nil then
  begin
    if FActiveForm.ContainsControl(Screen.ActiveControl) then
      FActiveForm.LastActiveControl := Screen.ActiveControl;
    FActiveForm.Visible := False;
    FActiveForm := nil;
  end;
end;

function TtiFormMgr.GetFormCount: Integer;
begin
  Result := FForms.Count;
end;

function TFormTIFormMgrForm.GetData: TtiObject;
begin
  Result := FData;
end;

procedure TFormTIFormMgrForm.SetFormInfoMessage(const AValue: string);
begin
  FFormInfoMessage := AValue;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormInfoMessage, tiufmtInfo);
end;

procedure TFormTIFormMgrForm.SelectFirstControl;
begin
  SelectFirst;
end;

{ TtiAMSAction }

constructor TtiAMSAction.Create(AOwner: TComponent);
begin
  inherited;
  FShowInMenuSystem:= True;
end;

end.




{$I tiDefines.inc}

unit FtiFormMgrForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiObject, ActnList, StdCtrls, Buttons, tiReadOnly,
  ExtCtrls, ComCtrls, ToolWin, tiBaseObject, Contnrs;

const
  TI_CLOSEACTIVEFORM = WM_USER + 1000;
  cCaptionClose       = 'Close  [Esc]';

type

  TtiUserFeedbackMessageType = (tiufmtInfo, tiufmtError);
  TtiButtonsVisible = (btnVisNone, btnVisReadOnly, btnVisReadWrite);

  TtiFormMgrForm = class;
  TtiFormMgr = class;

  TtiOnShowFormEvent = procedure(const AForm: TtiFormMgrForm) of object;
  TtiOnFormMessageEvent = procedure(const AMessage: string; pMessageType: TtiUserFeedbackMessageType) of object;
  TLogEvent = procedure(const AMessage: string) of object;
  TtiFormCloseHandler = procedure of object;

  // Custom ApplicationMenuSystem action
  TtiAMSAction = class(TAction)
  private
    FShowInMenuSystem: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property ShowInMenuSystem: boolean read FShowInMenuSystem write FShowInMenuSystem;
  end;

  TtiFormMgrForm = class(TForm)
    lblDummyAction: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FAL: TActionList;
    FaClose: TtiAMSAction;
    FaDummy: TtiAMSAction;
    FFormCaption: string;
    FButtonsVisible: TtiButtonsVisible;
    FContextActionsEnabled: Boolean;
    FIsModal: boolean;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FFormErrorMessage: string;
    FFormInfoMessage: string;
    FFormMgr: TtiFormMgr;
    FLastActiveControl: TWinControl;
    FEscapeKeyEnabled: Boolean;
    FInitialized: Boolean;

    procedure SetFormErrorMessage(const AValue: string);
    procedure SetFormInfoMessage(const AValue: string);
    procedure SetContextActionsEnabled(const AValue: Boolean);
    procedure SetEscapeKeyEnabled(const AValue: boolean);
  protected
    FUpdateButtons : boolean;

    function  AddAction(const ACaption: string;
                        const AOnExecute: TNotifyEvent;
                        const AHint: string = '';
                        const AImageIndex: Integer = -1;
                        const AHelpContext: Integer = -1): TtiAMSAction; overload;
    function  AddAction(const ACaption: string;
                        const AHint: string;
                        const AOnExecute: TNotifyEvent;
                        const AShortCutKey: Word;
                        const AShortCutShiftState: TShiftState): TtiAMSAction; overload;
    procedure DoCloseForm(const ACloseHandler: TtiFormCloseHandler = nil);

    procedure aCloseExecute(Sender: TObject); virtual;
    procedure aDummyExecute(Sender: TObject);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SetupButtons; virtual;
    procedure SetButtonsVisible(const AValue: TtiButtonsVisible); virtual;

    // Override in the concrete if necessary.
    function  FormIsValid: boolean; virtual;
    procedure DoALUpdate(Action: TBasicAction; var Handled: Boolean); virtual;
    procedure SelectFirstControl; virtual;

    property  ButtonsVisible: TtiButtonsVisible read FButtonsVisible write SetButtonsVisible;
    property  UpdateButtons: boolean read FUpdateButtons write FUpdateButtons;
    property  ContextActionsEnabled: Boolean read FContextActionsEnabled write SetContextActionsEnabled;
    property  LastActiveControl: TWinControl read FLastActiveControl Write FLastActiveControl;
  public
    procedure AssignActions(const AList: TList);
    property  FormCaption: string read FFormCaption write FFormCaption;
    property  IsModal: Boolean read FIsModal write FIsModal;
    property  OnFormMessage: TtiOnFormMessageEvent read FOnFormMessageEvent Write FOnFormMessageEvent;
    property  FormErrorMessage: string read FFormErrorMessage Write SetFormErrorMessage;
    property  FormInfoMessage: string read FFormInfoMessage Write SetFormInfoMessage;
    property  FormMgr: TtiFormMgr read FFormMgr Write FFormMgr;
    property  EscapeKeyEnabled: Boolean read FEscapeKeyEnabled write SetEscapeKeyEnabled;
    property  Initialized: Boolean read FInitialized write FInitialized;

    property AL: TActionList read FAL;
  end;

  TtiFormMgrFormClass = class of TtiFormMgrForm;

  TtiFormMgr = class(TtiBaseObject)
  private
    FForms : TObjectList;
    FActiveForm : TtiFormMgrForm;
    FParentPnl: TPanel;
    FOnShowForm: TtiOnShowFormEvent;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    function    GetNextForm: TtiFormMgrForm;
    function    GetPrevForm: TtiFormMgrForm;
    function    GetForms(i: integer): TtiFormMgrForm;
    procedure   SetForms(i: integer; const AValue: TtiFormMgrForm);
    function    GetActiveForm: TtiFormMgrForm;
    procedure   SetActiveForm(const AValue: TtiFormMgrForm);
    procedure   Add(const pForm : TtiFormMgrForm);
    procedure   InitializeForm(const AForm: TtiFormMgrForm;
        const AData: TtiObject; AModal: Boolean;
        const AOnEditsSave: TtiObjectEvent;
        const AOnEditsCancel: TtiObjectEvent; AReadOnly: boolean;
        const AFormSettings: TtiObject; const AReferenceData: TtiObject);
    procedure   DoCloseForm(const pForm : TtiFormMgrForm);
    function    CloseCurrentFormActivatePreviousForm(pClose : boolean): boolean;
    procedure   DoOnShowForm(const pForm : TtiFormMgrForm);
    procedure   SetParentPnl(const AValue: TPanel);
    procedure   DoBeginUpdate;
    procedure   DoEndUpdate;
    procedure   HideActiveForm;
    function    GetFormCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    function    CreateForm(const AFormClass: TtiFormMgrFormClass): TtiFormMgrForm;
    procedure   ShowForm(     const AForm: TtiFormMgrForm;
                              const AData          : TtiObject = nil;
                                    AModal         : Boolean = False;
                              const AOnEditsSave   : TtiObjectEvent = nil;
                              const AOnEditsCancel : TtiObjectEvent = nil;
                                    AReadOnly      : boolean = False;
                              const AFormSettings  : TtiObject = nil;
                              const AReferenceData : TtiObject = nil); overload;
    function    ShowForm(     const AFormClass     : TtiFormMgrFormClass;
                              const AData          : TtiObject = nil;
                                    AModal         : Boolean = False;
                              const AOnEditsSave   : TtiObjectEvent = nil;
                              const AOnEditsCancel : TtiObjectEvent = nil;
                                    AReadOnly      : boolean = False;
                              const AFormSettings  : TtiObject = nil;
                              const AReferenceData : TtiObject = nil): TtiFormMgrForm; overload;
    function    ShowFormModal(const AFormClass     : TtiFormMgrFormClass;
                              const AData          : TtiObject;
                              const AOnEditsSave   : TtiObjectEvent = nil;
                              const AOnEditsCancel : TtiObjectEvent = nil;
                              const AFormSettings  : TtiObject = nil;
                              const AReferenceData : TtiObject = nil): TtiFormMgrForm;
    procedure   BringToFront(const pForm : TtiFormMgrForm; pFocusFirstControl : Boolean);

    function    FindForm(const AFormClass : TtiFormMgrFormClass): TtiFormMgrForm; overload;
    function    FindForm(const AFormClass : TtiFormMgrFormClass; const AData : TtiObject): TtiFormMgrForm; overload;
    function    IndexOf(const pForm : TtiFormMgrForm): integer;
    procedure   AddForm(AForm: TtiFormMgrForm);

    procedure   CloseForm(const pForm : TtiFormMgrForm);
    procedure   CloseAllForms;
    procedure   RemoveForm(const pForm : TtiFormMgrForm);

    property    ActiveForm : TtiFormMgrForm read GetActiveForm write SetActiveForm;
    property    Forms[i:integer]: TtiFormMgrForm read GetForms write SetForms;
    property    FormCount: Integer read GetFormCount;
    property    ParentPnl : TPanel read FParentPnl write SetParentPnl;
    procedure   ShowPrevForm;
    procedure   ShowNextForm;
    property    NextForm : TtiFormMgrForm read GetNextForm;
    property    PrevForm : TtiFormMgrForm read GetPrevForm;
    property    OnShowForm : TtiOnShowFormEvent read FOnShowForm write FOnShowForm;
    property    OnFormMessage: TtiOnFormMessageEvent read FOnFormMessageEvent write FOnFormMessageEvent;
    property    OnBeginUpdate: TNotifyEvent read FOnBeginUpdate Write FOnBeginUpdate;
    property    OnEndUpdate:   TNotifyEvent read FOnEndUpdate Write FOnEndUpdate;
    procedure   AssignFormList(const AStrings : TStrings);
    procedure   SetEscapeKeyEnabled(const AValue: boolean);
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
  ,FtiFormMgrDataForm
  ,tiOPFManager
 ;

{$R *.DFM}

{ TtiAMSAction }

constructor TtiAMSAction.Create(AOwner: TComponent);
begin
  inherited;
  FShowInMenuSystem:= True;
end;

{ TtiFormMgrForm }

procedure TtiFormMgrForm.FormCreate(Sender: TObject);
begin
  inherited;

  KeyPreview := true;

  {$IFDEF DEBUG}
  Assert(HelpContext = 0, ClassName + ' help context has been set in the IDE for form "' + ClassName +'"');
  {$ENDIF}

  FAL := TActionList.Create(Self);
  FAL.OnUpdate := DoALUpdate;

  FaClose := AddAction(cCaptionClose, 'Close this page' + ClassName , aCloseExecute, VK_ESCAPE, []);
  FaClose.ImageIndex := gTIImageListMgr.ImageIndex16(cResTI_CloseWindow);

  // At least one action must be hooked up to the UI so that the action list
  // OnUpdate is called.
  FaDummy := AddAction('DymmyAction', aDummyExecute);
  // A panel is used as it can be set to zero width and height. A label
  lblDummyAction.Action := FaDummy;

  FIsModal := false;
  FButtonsVisible := btnVisNone;
  FUpdateButtons := true;
  FContextActionsEnabled := true;
  FEscapeKeyEnabled := true;
end;

procedure TtiFormMgrForm.FormDestroy(Sender: TObject);
begin
  // The currently active form will have it's parent set. If the application is
  // shutting down, then the parent will be destroyed when the main form
  // is destroyed. This will cause the active form to be destroyed twice,
  // with the associated AV. The If statement below stops this.
  if Assigned(FFormMgr) and (FFormMgr.IndexOf(Self) <> -1) then
    FFormMgr.RemoveForm(Self);
  inherited;
end;

function TtiFormMgrForm.FormIsValid: boolean;
begin
  Result := true;
  // Override in derived / concrete if necessary.
end;

procedure TtiFormMgrForm.SetupButtons;
begin
  if not FUpdateButtons then
    Exit; //==>
  ButtonsVisible := btnVisReadOnly;
end;

procedure TtiFormMgrForm.BeginUpdate;
begin
  FAL.OnUpdate:= nil;
end;

procedure TtiFormMgrForm.EndUpdate;
begin
  FAL.OnUpdate:= DoALUpdate;
end;

procedure TtiFormMgrForm.DoALUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  FaClose.Enabled := ContextActionsEnabled and EscapeKeyEnabled;
  Handled := True;
end;

procedure TtiFormMgrForm.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons;
end;

procedure TtiFormMgrForm.FormKeyPress(Sender: TObject; var Key: Char);
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

procedure TtiFormMgrForm.DoCloseForm(const ACloseHandler: TtiFormCloseHandler);
begin
  try
    FAL.OnUpdate := nil;
    Visible := False;
    if Assigned(ACloseHandler) then
      ACloseHandler;
  finally
    if Assigned(FFormMgr)  then
      FFormMgr.CloseForm(Self);
  end;
end;

procedure TtiFormMgrForm.aCloseExecute(Sender: TObject);
begin
  DoCloseForm;
end;

procedure TtiFormMgrForm.aDummyExecute(Sender: TObject);
begin
  // Do nothing. Dummy action is just to get the ActionLists OnUpdate method to fire
end;

procedure TtiFormMgrForm.SetButtonsVisible(const AValue: TtiButtonsVisible);
begin
  FButtonsVisible := AValue;
  FaClose.Visible := FButtonsVisible = btnVisReadOnly;
end;

procedure TtiFormMgrForm.SetContextActionsEnabled(const AValue: Boolean);
var
  LHandled: Boolean;
begin
  if AValue <> FContextActionsEnabled then
  begin
    FContextActionsEnabled := AValue;
    // Trigger an OnUpdate
    if Assigned(FAL.OnUpdate) and (FAL.ActionCount > 0) then
      FAL.OnUpdate(FAL.Actions[0], LHandled);
  end;
end;

procedure TtiFormMgrForm.SetEscapeKeyEnabled(const AValue: boolean);
var
  i: integer;
begin
  if FEscapeKeyEnabled <> AValue then
  begin
    FEscapeKeyEnabled := AValue;
    for i := 0 to FAL.ActionCount-1 do
      if (FAL.Actions[i] as TAction).ShortCut = Shortcut(Word(VK_ESCAPE), []) then
        (FAL.Actions[i] as TAction).Enabled:= FEscapeKeyEnabled;
  end;
end;

function TtiFormMgrForm.AddAction(
  const ACaption: string;
  const AOnExecute: TNotifyEvent;
  const AHint: string = '';
  const AImageIndex: Integer = -1;
  const AHelpContext: Integer = -1): TtiAMSAction;
begin
  Result := TtiAMSAction.Create(FAL);
  Result.ActionList := FAL;
  Result.Caption := ACaption;
  Result.OnExecute := AOnExecute;
  Result.Hint := AHint;
  Result.ImageIndex := AImageIndex;
  Result.HelpContext := AHelpContext;
end;

function TtiFormMgrForm.AddAction(
  const ACaption: string;
  const AHint: string;
  const AOnExecute: TNotifyEvent;
  const AShortCutKey: Word;
  const AShortCutShiftState: TShiftState): TtiAMSAction;
begin
  Result := AddAction(ACaption, AOnExecute, AHint);
  Result.ShortCut := Shortcut(Word(AShortCutKey), AShortCutShiftState);
end;

procedure TtiFormMgrForm.AssignActions(const AList: TList);
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

procedure TtiFormMgrForm.SetFormInfoMessage(const AValue: string);
begin
  FFormInfoMessage := AValue;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormInfoMessage, tiufmtInfo);
end;

procedure TtiFormMgrForm.SetFormErrorMessage(const AValue: string);
begin
  FFormErrorMessage := AValue;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormErrorMessage, tiufmtError);
end;

procedure TtiFormMgrForm.SelectFirstControl;
begin
  SelectFirst;
end;

{ TtiFormMgr }

procedure TtiFormMgr.Add(const pForm: TtiFormMgrForm);
begin
  pForm.BorderStyle := bsNone;
  pForm.Parent := ParentPnl;
  pForm.Align := alClient;
  FForms.Add(pForm);
end;

procedure TtiFormMgr.BringToFront(const pForm: TtiFormMgrForm; pFocusFirstControl : Boolean);
begin
  Assert(pForm<>nil, 'pForm not assigned');
  if pForm = FActiveForm then
    Exit; //==>

  // Move to the end of the list
  FForms.Extract(pForm);
  FForms.Add(pForm);

  pForm.Visible := true;
  pForm.SetFocus;

  try
    if (pForm.LastActiveControl <> nil) then
      pForm.LastActiveControl.SetFocus
    else
      pForm.SelectFirstControl;
  except
  end;

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
    if (FForms.Items[i] as TtiFormMgrForm).ButtonsVisible in [btnVisReadOnly, btnVisReadWrite] then
      DoCloseForm(Forms[i]);
end;

procedure TtiFormMgr.DoCloseForm(const pForm: TtiFormMgrForm);
begin
  if ActiveForm = pForm then
  begin
    if GetPrevForm <> nil then
      ActiveForm := GetPrevForm
    else
      ActiveForm := GetNextForm;
  end;
  FForms.Extract(pForm);
  if ActiveForm = pForm then
    HideActiveForm;
  DoOnShowForm(FActiveForm);
  pForm.Free;
end;

constructor TtiFormMgr.Create;
begin
  inherited;
  FForms := TObjectList.Create(false);
end;

destructor TtiFormMgr.Destroy;
var
  i : integer;
  lForm : TtiFormMgrForm;
  lName : string;
begin
  for i := FForms.Count - 1 downto 0 do
  begin
    lForm := FForms.Items[i] as TtiFormMgrForm;
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
  inherited;
end;

function TtiFormMgr.FindForm(
  const AFormClass: TtiFormMgrFormClass): TtiFormMgrForm;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FForms.Count - 1 do
    if FForms.Items[i] is AFormClass then
      Exit(FForms.Items[i] as TtiFormMgrForm); //==>
end;

function TtiFormMgr.FindForm(const AFormClass: TtiFormMgrFormClass; const AData : TtiObject): TtiFormMgrForm;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FForms.Count - 1 do
    if (FForms.Items[i] is AFormClass) and
       ((not (FForms.Items[i] is TtiFormMgrDataForm)) or
        (TtiFormMgrDataForm(FForms.Items[i]).Data = AData)) then
    begin
      result := FForms.Items[i] as TtiFormMgrForm;
      Exit; //==>
    end;
end;

function TtiFormMgr.GetActiveForm: TtiFormMgrForm;
begin
  result := FActiveForm;
end;

function TtiFormMgr.GetForms(i: integer): TtiFormMgrForm;
begin
  result := FForms[i] as TtiFormMgrForm;
end;

function TtiFormMgr.GetNextForm: TtiFormMgrForm;
begin
  if FForms.Count > 0 then
    result := Forms[0]
  else
    result := nil;
end;

function TtiFormMgr.GetPrevForm: TtiFormMgrForm;
var
  lIndex: Integer;
begin
  lIndex := FForms.IndexOf(ActiveForm);
  if lIndex > 0 then
    result := Forms[lIndex-1]
  else
    result := nil;
end;

function TtiFormMgr.IndexOf(const pForm: TtiFormMgrForm): integer;
begin
  result := FForms.IndexOf(pForm);
end;

procedure TtiFormMgr.RemoveForm(const pForm: TtiFormMgrForm);
begin
  FForms.Remove(pForm)
end;

procedure TtiFormMgr.SetActiveForm(const AValue: TtiFormMgrForm);
begin
  if AValue = nil then
  begin
    FActiveForm := AValue;
    Exit; //==>
  end;
  ShowForm(AValue);
end;

procedure TtiFormMgr.SetEscapeKeyEnabled(const AValue: boolean);
begin
  FActiveForm.EscapeKeyEnabled := AValue;
end;

procedure TtiFormMgr.SetForms(i: integer; const AValue: TtiFormMgrForm);
begin
  FForms[i]:= AValue;
end;

procedure TtiFormMgr.AddForm(AForm: TtiFormMgrForm);
begin
  if IndexOf(AForm) = -1 then
    Add(AForm);
end;

function TtiFormMgr.CreateForm(const AFormClass: TtiFormMgrFormClass): TtiFormMgrForm;
begin
  Result := AFormClass.Create(ParentPnl);
  Add(Result);
end;

procedure TtiFormMgr.InitializeForm(
  const AForm: TtiFormMgrForm;
  const AData: TtiObject;
        AModal: Boolean;
  const AOnEditsSave: TtiObjectEvent;
  const AOnEditsCancel: TtiObjectEvent;
        AReadOnly: boolean;
  const AFormSettings: TtiObject;
  const AReferenceData: TtiObject);
begin
  if AForm.Initialized then
    raise EtiOPFProgrammerException.Create('Form already initialized');

  {$IFDEF DEBUG}
  Assert(AForm.HelpContext <> 0, AForm.ClassName + ' help context not set <' + AForm.Name +'>');
  {$ENDIF}

  AForm.OnFormMessage := FOnFormMessageEvent;
  if AForm is TtiFormMgrDataForm then
  begin
    TtiFormMgrDataForm(AForm).OnEditsSave   := AOnEditsSave;
    TtiFormMgrDataForm(AForm).OnEditsCancel := AOnEditsCancel;
  end;
  AForm.IsModal       := AModal;
  AForm.FormMgr       := Self;
  if (AFormSettings <> nil) and
     (AForm is TtiFormMgrDataForm) and
     (TtiFormMgrDataForm(AForm).FormSettings <> AFormSettings) then
    TtiFormMgrDataForm(AForm).FormSettings := AFormSettings;
  if (AReferenceData <> nil) and
     (AForm is TtiFormMgrDataForm) and
     (TtiFormMgrDataForm(AForm).ReferenceData <> AReferenceData) then
    TtiFormMgrDataForm(AForm).ReferenceData := AReferenceData;
  if (AData <> nil) and
     (AForm is TtiFormMgrDataForm) and
     (TtiFormMgrDataForm(AForm).Data <> AData) then
    TtiFormMgrDataForm(AForm).Data := AData;
  AForm.SetupButtons;

  AForm.Initialized := true;
end;

// Show an existing instance
procedure TtiFormMgr.ShowForm(
  const AForm: TtiFormMgrForm;
  const AData: TtiObject = nil;
        AModal: Boolean = False;
  const AOnEditsSave: TtiObjectEvent = nil;
  const AOnEditsCancel: TtiObjectEvent = nil;
        AReadOnly: boolean = False;
  const AFormSettings: TtiObject = nil;
  const AReferenceData: TtiObject = nil);
var
  LCursor: TCursor;
  LActiveForm: TtiFormMgrForm;
begin
  if ShuttingDown then
    Exit; //==>

  if IndexOf(AForm) = -1 then
    Exit; //==>

  LCursor := Screen.Cursor;
  try
    DoBeginUpdate;
    LActiveForm := ActiveForm;
    try
      if (FActiveForm <> nil) and
         (not FActiveForm.IsModal) and
         (not CloseCurrentFormActivatePreviousForm(false)) then
        Exit; //==>

        HideActiveForm;
        try
          if not AForm.Initialized then
          begin
            InitializeForm(AForm, AData, AModal, AOnEditsSave, AOnEditsCancel,
                AReadOnly, AFormSettings, AReferenceData);
            BringToFront(AForm, true);
          end else
            BringToFront(AForm, false);
        except
          on e:exception do
          begin
            if LActiveForm <> nil then
              BringToFront(LActiveForm, false);
            raise;
          end;
        end;
    finally
      DoEndUpdate;
    end;
  finally
    Screen.Cursor:= LCursor;
  end;
end;

// Show new instance or find and show existing form of same class with same data.
function TtiFormMgr.ShowForm(
      const AFormClass: TtiFormMgrFormClass;
      const AData: TtiObject = nil;
            AModal: Boolean = False;
      const AOnEditsSave: TtiObjectEvent = nil;
      const AOnEditsCancel: TtiObjectEvent = nil;
            AReadOnly: boolean = False;
      const AFormSettings: TtiObject = nil;
      const AReferenceData: TtiObject = nil
     ): TtiFormMgrForm;
var
  LForm : TtiFormMgrForm;
  LCursor: TCursor;
  LActiveForm: TtiFormMgrForm;
begin
  if ShuttingDown then
    Exit(nil); //==>

  Assert(ParentPnl <> nil, 'ParentPnl not assigned');
  LCursor:= Screen.Cursor; // tiAutoCursor not working here - I don't understand why
  try
    DoBeginUpdate;
    Result := nil;
    LActiveForm:= ActiveForm;
    try
      if (FActiveForm <> nil) and
         (not FActiveForm.IsModal) and
         (not CloseCurrentFormActivatePreviousForm(false)) then
        Exit; //==>
      LForm := FindForm(AFormClass, AData);
      if LForm <> nil then
        BringToFront(LForm, false)
      else
      begin
        HideActiveForm;
        LForm:= nil;
        try
          LForm := CreateForm(AFormClass);

          InitializeForm(LForm, AData, AModal, AOnEditsSave, AOnEditsCancel,
              AReadOnly, AFormSettings, AReferenceData);

          BringToFront(LForm, true);
        except
          on e:exception do
          begin
            RemoveForm(LForm);
            if LActiveForm <> nil then
              BringToFront(LActiveForm, false);
            Raise;
          end;
        end;
      end;
      Result := LForm;
    finally
      DoEndUpdate;
    end;
  finally
    Screen.Cursor:= LCursor;
  end;
end;

function TtiFormMgr.ShowFormModal(const AFormClass: TtiFormMgrFormClass;
  const AData: TtiObject; const AOnEditsSave,
  AOnEditsCancel: TtiObjectEvent; const AFormSettings, AReferenceData: TtiObject): TtiFormMgrForm;
begin
  Result := ShowForm(AFormClass, AData, True {AModal}, AOnEditsSave, AOnEditsCancel, False {AReadOnly}, AFormSettings, AReferenceData);
end;

procedure TtiFormMgr.CloseForm(const pForm: TtiFormMgrForm);
begin
  Assert(FActiveForm = pForm, 'Close for can only be called with currently active form');
  CloseCurrentFormActivatePreviousForm(true);
end;

function TtiFormMgr.CloseCurrentFormActivatePreviousForm(pClose : boolean): boolean;
var
  lForm : TtiFormMgrForm;
  lActiveForm : TtiFormMgrForm;
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
  lForm : TtiFormMgrForm;
  lActiveForm: TtiFormMgrForm;
begin
  lForm := GetNextForm;
  Assert(LForm<>nil, 'GetNextForm returned nil');
  lActiveForm := ActiveForm;
  FForms.Extract(lActiveForm);
  FForms.Add(lActiveForm);
  ShowForm(lForm);
end;

procedure TtiFormMgr.ShowPrevForm;
var
  LForm : TtiFormMgrForm;
  LActiveForm: TtiFormMgrForm;
begin
  LForm := GetPrevForm;
  Assert(LForm<>nil, 'GetNextForm returned nil');
  LActiveForm := ActiveForm;
  FForms.Extract(LActiveForm);
  FForms.Insert(0, LActiveForm);
  ShowForm(LForm);
end;

procedure TtiFormMgr.AssignFormList(const AStrings: TStrings);
var
  i : integer;
begin
  AStrings.Clear;
  for i := 0 to FForms.Count - 1 do
    if FForms.Items[i] <> ActiveForm then
      AStrings.AddObject((FForms.Items[i] as TtiFormMgrForm).FormCaption,
                          FForms.Items[i]);
end;

procedure TtiFormMgr.DoOnShowForm(const pForm : TtiFormMgrForm);
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

end.




{$I tiDefines.inc}

unit FtiFormMgrForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiObject, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly,
  ExtCtrls, ComCtrls, ToolWin, tiBaseObject, Contnrs ;

const
  TI_CLOSEACTIVEFORM = WM_USER + 1000 ;
  cCaptionClose       = 'Close  [Esc]';
  cCaptionUndo        = 'Undo  [Ctrl+Z]';
  cCaptionCancelClose = 'Cancel and close  [Esc]';
  cCaptionSaveClose   = 'Save and close  [Ctrl+S]';

type

  TtiUserFeedbackMessageType = (tiufmtInfo, tiufmtError);
  TFormTIFormMgrForm = class ;
  TtiOnShowFormEvent = procedure( const pForm : TFormTIFormMgrForm ) of object;
  TtiOnFormMessageEvent = procedure( const pMessage: string; pMessageType: TtiUserFeedbackMessageType ) of object ;
  TLogEvent = procedure( const pMessage : string ) of object ;
  TtiFormMgr = class;

  TtiButtonsVisible = ( btnVisNone, btnVisReadOnly, btnVisReadWrite);

// Paste this into the concrete class
//  protected
//    procedure SetData(const Value: TtiObject); override ;
//    function  FormIsValid : boolean ; override ;


  // ToDo: Remove pnlCaption - but this will require some effort as it may be referenced by child forms
  TFormTIFormMgrForm = class( TForm )
    pnlCaption: TPanel;
    lblCaption: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FAL         : TActionList ;
    FaClose      : TAction ;
    FaUndo       : TAction ;
    FaSaveClose  : TAction ;
    FaCancelClose: TAction ;
    FaDummy      : TAction ;

    FButtonsVisible: TtiButtonsVisible;
    FForceCanLeave: boolean;
    FIsModal : boolean ;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FFormErrorMessage: string;
    FFormInfoMessage: string;
    FOnEditsSave: TtiObjectEvent;
    FOnEditsCancel: TtiObjectEvent;
    FFormMgr: TtiFormMgr;
//    FButtonsSetup: Boolean;
    FLastActiveControl: TWinControl;

    function  GetFormCaption: string;
    procedure SetFormCaption(const Value: string);
    procedure SetButtonsVisible(const Value: TtiButtonsVisible);
    function  GetBorderColor: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetFormErrorMessage(const Value: string);
    procedure SetFormInfoMessage(const Value: string);
    // These are set by the FormMgr so can be hidden from outside view
    property  OnEditsSave: TtiObjectEvent read FOnEditsSave Write FOnEditsSave;
    property  OnEditsCancel: TtiObjectEvent read FOnEditsCancel Write FOnEditsCancel;
    procedure SetIsModal(const Value: Boolean);

  protected
    FData        : TtiObject ;
    FDataBuffer  : TtiObject ;
    FUpdateButtons : boolean ;

    procedure alUpdate(Action: TBasicAction; var Handled: Boolean);virtual;
    function  AddAction(const pCaption: string;
                        const pOnExecute: TNotifyEvent ;
                        const pHint: string = '';
                              pImageIndex: Integer = -1;
                              pHelpContext: Integer = -1 ) : TAction; overload ;
    function  AddAction(const pCaption: string; const pHint: string;
                        const pOnExecute: TNotifyEvent;
                        pShortCutKey: Word; pShortCutShiftState: TShiftState ) : TAction; overload ;

    procedure aUndoExecute(Sender: TObject);virtual;
    procedure aSaveCloseExecute(Sender: TObject);virtual;
    procedure aCancelCloseExecute(Sender: TObject);virtual;
    procedure aCloseExecute(Sender: TObject);virtual;
    procedure aDummyExecute(Sender: TObject);

    function  FormIsValid : boolean ; virtual ;
    function  FormIsDirty : boolean ; virtual ;
    procedure SetData(const Value: TtiObject); virtual ;
    function  Databuffer : TtiObject; virtual ;
    function  GetData: TtiObject; virtual;

    // Implement these in the concrete...
    property  ButtonsVisible : TtiButtonsVisible read FButtonsVisible write SetButtonsVisible ;
    property  ForceCanLeave : boolean read FForceCanLeave write FForceCanLeave ;
    procedure DoBeforeSave ; virtual ;
    procedure DoAfterSave ; virtual ;
    procedure DoAfterDiscard ; virtual ;
    procedure SetupButtons ; virtual ;
    procedure SelectFirstControl; virtual;

    property  UpdateButtons : boolean read FUpdateButtons write FUpdateButtons ;
    property  LastActiveControl: TWinControl read FLastActiveControl Write FLastActiveControl;

  public
    procedure AssignActions(const pList: TList);
    property  Data : TtiObject read GetData write SetData ;
    property  FormCaption : string read GetFormCaption write SetFormCaption ;
    property  IsModal : Boolean read FIsModal write SetIsModal ;
    property  BorderColor : TColor read GetBorderColor write SetBorderColor ;
    property  OnFormMessage : TtiOnFormMessageEvent read FOnFormMessageEvent Write FOnFormMessageEvent ;
    property  FormErrorMessage: string read FFormErrorMessage Write SetFormErrorMessage;
    property  FormInfoMessage:  string read FFormInfoMessage Write SetFormInfoMessage;
    property  FormMgr: TtiFormMgr read FFormMgr Write FFormMgr ;
  end;

  TFormTIFormMgrFormClass = class of TFormTIFormMgrForm ;

  TtiFormMgr = class( TtiBaseObject )
  private
    FForms : TObjectList ;
    FModalForms : TObjectList ;
    FActiveForm : TFormTIFormMgrForm ;
    FParentPnl: TPanel;
    FBorderColor: TColor;
    FOnShowForm: TtiOnShowFormEvent;
    FOnFormMessageEvent: TtiOnFormMessageEvent;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    function    GetNextForm: TFormTIFormMgrForm;
    function    GetPrevForm: TFormTIFormMgrForm;
    function    GetForms(i: integer): TFormTIFormMgrForm;
    procedure   SetForms(i: integer; const Value: TFormTIFormMgrForm);
    function    GetActiveForm: TFormTIFormMgrForm;
    procedure   SetActiveForm(const Value: TFormTIFormMgrForm);
    procedure   Add( const pForm : TFormTIFormMgrForm ) ;
    procedure   DoCloseForm( const pForm : TFormTIFormMgrForm ) ;
    function    CloseCurrentFormActivatePreviousForm( pClose : boolean ) : boolean ;
    procedure   SetBorderColor(const Value: TColor);
    procedure   DoOnShowForm(const pForm : TFormTIFormMgrForm);
    procedure   SetParentPnl(const Value: TPanel);
    procedure   DoBeginUpdate;
    procedure   DoEndUpdate;
    procedure   HideActiveForm;
    function    GetFormCount: Integer;
  public
    constructor Create ;
    destructor  Destroy ; override ;

    procedure   ShowForm(      const pFormClass      : TFormTIFormMgrFormClass ;
                               const pData           : TtiObject = nil ;
                                     pModal          : Boolean = False ;
                               const pOnEditsSave    : TtiObjectEvent = nil;
                               const pOnEditsCancel  : TtiObjectEvent = nil;
                                     pReadOnly       : boolean = False ) ; overload ;
    procedure   ShowForm(      const pForm           : TFormTIFormMgrForm      ;
                               const pData           : TtiObject = nil ;
                                     pModal          : Boolean = False ;
                               const pOnEditsSave    : TtiObjectEvent = nil;
                               const pOnEditsCancel  : TtiObjectEvent = nil;
                                     pReadOnly       : boolean = False ) ; overload ;
    procedure   ShowFormModal( const pFormClass      : TFormTIFormMgrFormClass ;
                               const pData           : TtiObject ;
                               const pOnEditsSave    : TtiObjectEvent = nil;
                               const pOnEditsCancel  : TtiObjectEvent = nil);

    procedure   BringToFront( const pForm : TFormTIFormMgrForm ; pFocusFirstControl : Boolean ) ;

    function    FindForm( const pFormClass : TFormTIFormMgrFormClass ; const pData : TtiObject ) : TFormTIFormMgrForm ;
    function    IndexOf( const pForm : TFormTIFormMgrForm ) : integer ;

    procedure   CloseForm( const pForm : TFormTIFormMgrForm ) ;
    procedure   CloseAllForms ;
    procedure   RemoveForm( const pForm : TFormTIFormMgrForm ) ;

    property    ActiveForm : TFormTIFormMgrForm read GetActiveForm write SetActiveForm ;
    property    Forms[i:integer] : TFormTIFormMgrForm read GetForms write SetForms ;
    property    FormCount: Integer read GetFormCount;
    property    ParentPnl : TPanel read FParentPnl write SetParentPnl ;
    procedure   ShowPrevForm ;
    procedure   ShowNextForm ;
    property    NextForm : TFormTIFormMgrForm read GetNextForm ;
    property    PrevForm : TFOrmTIFormMgrForm read GetPrevForm ;
    property    BorderColor : TColor read FBorderColor write SetBorderColor ;
    property    OnShowForm : TtiOnShowFormEvent read FOnShowForm write FOnShowForm ;
    property    OnFormMessage: TtiOnFormMessageEvent read FOnFormMessageEvent write FOnFormMessageEvent;
    property    OnBeginUpdate: TNotifyEvent read FOnBeginUpdate Write FOnBeginUpdate;
    property    OnEndUpdate:   TNotifyEvent read FOnEndUpdate Write FOnEndUpdate;
    procedure   AssignFormList(const pStrings : TStrings);
  end ;

implementation
uses
  tiUtils
  ,tiRegINI
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
//  FButtonsSetup := False ;
  KeyPreview := true ;

  FAL := TActionList.Create( Self ) ;
  FAL.OnUpdate := ALUpdate ;

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

  FForceCanLeave := false ;
  FIsModal := false ;
  FUpdateButtons := true ;

  pnlCaption.TabOrder := ControlCount - 1 ;

  // A hack to remove the pnlCaption, without going through all the pain
  // of removing references in descendants of TFormTIFormMgrForm
  // This pnl contains a TLable, which is hooked up to a TAction - necessary to get the form's
  // ActionList.OnUpdate event to fire. (This is where form validation takes place)
  pnlCaption.Visible := True;
  pnlCaption.Height := 0 ;

end;

procedure TFormTIFormMgrForm.FormDestroy(Sender: TObject);
begin
  // The currently active form will have it's parent set. If the application is
  // shutting down, then the parent will be destroyed when the main form
  // is destroyed. This will cause the active form to be destroyed twice,
  // with the associated AV. The If statement below stops this.
  if FormMgr.IndexOf(Self) <> -1 then
    FormMgr.RemoveForm(Self);
  FreeAndNil(FDataBuffer) ;
  inherited;
end;

procedure TFormTIFormMgrForm.SetData(const Value: TtiObject);
begin
  FAL.OnUpdate := nil ;
  try
    FData := Value;
    FreeAndNil(FDataBuffer) ;
    if FData <> nil then
      FDataBuffer := FData.Clone ;
  finally
    FAL.OnUpdate := ALUpdate ;
  end ;
end;

procedure TFormTIFormMgrForm.alUpdate(Action: TBasicAction; var Handled: Boolean);
var
  lFormIsDirty : Boolean;
  lFormIsValid : Boolean;
begin
  Handled := true ;
  Assert( FData.TestValid(TtiObject, true), cTIInvalidObjectError + ' <FData>') ;
  Assert( FDataBuffer.TestValid(TtiObject, true ), cTIInvalidObjectError + '<FDataBuffer>') ;
  lFormIsDirty := FormIsDirty;
  lFormIsValid := FormIsValid;
  if FUpdateButtons then
  begin
    case FButtonsVisible of
    btnVisNone      : ; // Do nothing
    btnVisReadOnly  : ; // Do nothing
    btnVisReadWrite : begin
                        FaUndo.Enabled := ( FData <> nil ) and lFormIsDirty ;
                        FaSaveClose.Enabled := ( FData <> nil ) and lFormIsValid and lFormIsDirty ;
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
  Assert( DataBuffer.TestValid(TtiObject, true), cTIInvalidObjectError );
  if DataBuffer <> nil then
  begin
    Result := DataBuffer.IsValid(ls);
    if ls <> FFormErrorMessage then
      FormErrorMessage := ls;
  end else
    result := true ;
end;


procedure TFormTIFormMgrForm.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons ;
end;

procedure TFormTIFormMgrForm.SetupButtons ;
begin
  if not FUpdateButtons then
   Exit ; //==>
  // No data
  if ( FData = nil ) or
     ( FDataBuffer = nil ) then
    ButtonsVisible := btnVisReadOnly
  else
  begin
    // Only show the Save and Cancel buttons if the form is 'Modal'
    if FIsModal then
      ButtonsVisible := btnVisReadWrite
    else
      ButtonsVisible := btnVisReadOnly;
  end;
end ;

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
  result := lblCaption.Caption ;
end;

procedure TFormTIFormMgrForm.SetFormCaption(const Value: string);
begin
  lblCaption.Caption := Value ;
end;

procedure TFormTIFormMgrForm.aSaveCloseExecute(Sender: TObject);
begin
  Assert( FData <> nil, 'FData not assigned' ) ;
  Assert( FDataBuffer <> nil, 'FDataBuffer not assigned' ) ;
  try
    FAL.OnUpdate := nil ;
    Visible := False ;
    DoBeforeSave ;
    FDataBuffer.Dirty := true ;
    FData.Assign( FDataBuffer ) ;
    FDataBuffer.ObjectState := Data.ObjectState ;
    DoAfterSave;
  finally
    FormMgr.CloseForm(Self);
  end;
end;

procedure TFormTIFormMgrForm.aCancelCloseExecute(Sender: TObject);
begin
  try
    FAL.OnUpdate := nil ;
    Visible := False ;
    DoAfterDiscard;
  finally
    FormMgr.CloseForm(Self);
  end;
end;

procedure TFormTIFormMgrForm.aUndoExecute(Sender: TObject);
var
  lFocusControl: TWinControl;
begin
  FAL.OnUpdate := nil ;
  try
    lFocusControl := Screen.ActiveControl;
    SetData( Data ) ;
    if (lFocusControl <> nil) and
       (lFocusControl.CanFocus) then
    begin
      Self.SetFocus;
      lFocusControl.SetFocus;
    end;
  finally
    FAL.OnUpdate := ALUpdate ;
  end;
end;

procedure TFormTIFormMgrForm.aCloseExecute(Sender: TObject);
begin
  aCancelCloseExecute(nil);
end;

procedure TFormTIFormMgrForm.SetButtonsVisible(const Value: TtiButtonsVisible);
begin
//  FButtonsSetup := True;
  FButtonsVisible := Value;
  case FButtonsVisible of
  btnVisNone      : begin
                      FaClose.Visible       := False;
                      FaUndo.Visible        := False;
                      FaSaveClose.Visible   := False;
                      FaCancelClose.Visible := False;
                    end;
  btnVisReadOnly  : begin
                      FaClose.Visible       := True;
                      FaUndo.Visible        := False;
                      FaSaveClose.Visible   := False;
                      FaCancelClose.Visible := False;
                    end;
  btnVisReadWrite : begin
                      FaClose.Visible       := False;
                      FaUndo.Visible        := True;
                      FaSaveClose.Visible   := True;
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

function TFormTIFormMgrForm.FormIsDirty: boolean;
begin
  Assert( Data.TestValid(TtiBaseObject, true), cTIInvalidObjectError );
  Assert( DataBuffer.TestValid(TtiBaseObject, true), cTIInvalidObjectError );
  if ( Data <> nil ) and ( DataBuffer <> nil ) then
    Result := ( not Data.Equals( DataBuffer ))
  else
    Result := False;
end;

function TFormTIFormMgrForm.GetBorderColor: TColor;
begin
  result := pnlCaption.Color ;
end;

procedure TFormTIFormMgrForm.SetBorderColor(const Value: TColor);
begin
  pnlCaption.Color := Value ;
end;

function TFormTIFormMgrForm.AddAction(
  const pCaption: string;
  const pOnExecute:   TNotifyEvent;
  const pHint:        string = '';
        pImageIndex:  Integer = -1;
        pHelpContext: Integer = -1 ): TAction;
begin
  Result := TAction.Create( FAL ) ;
  Result.ActionList := FAL ;
  Result.Caption   := pCaption ;
  Result.OnExecute := pOnExecute ;
  Result.Hint      := pHint ;
  Result.ImageIndex := pImageIndex;
  Result.HelpContext := pHelpContext;
end;

function TFormTIFormMgrForm.AddAction(const pCaption, pHint: string;
  const pOnExecute: TNotifyEvent; pShortCutKey: Word; pShortCutShiftState: TShiftState): TAction;
begin
  Result := AddAction(pCaption, pOnExecute, pHint );
  Result.ShortCut  := Shortcut(Word(pShortCutKey), pShortCutShiftState);
end;

function TFormTIFormMgrForm.Databuffer: TtiObject;
begin
  Result := FDataBuffer;
end;

procedure TFormTIFormMgrForm.SetFormErrorMessage(const Value: string);
begin
  FFormErrorMessage := Value;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormErrorMessage, tiufmtError);
end;

procedure TtiFormMgr.Add(const pForm: TFormTIFormMgrForm);
begin
  pForm.BorderStyle := bsNone ;
  pForm.Parent := ParentPnl ;
  pForm.Align := alClient ;
  FForms.Add(pForm);
end;

procedure TtiFormMgr.BringToFront(const pForm: TFormTIFormMgrForm ; pFocusFirstControl : Boolean );
begin
  Assert(pForm<>nil, 'pForm not assigned');
  if pForm = FActiveForm then
    Exit ; //==>

  // Move to the end of the list
  FForms.Extract(pForm);
  FForms.Add(pForm);

  pForm.Visible := true ;
  pForm.SetFocus;

  if ( pForm.LastActiveControl <> nil ) then
          pForm.LastActiveControl.SetFocus
  else
    pForm.SelectFirstControl;

  if FActiveForm <> nil then
    FActiveForm.Visible := false ;
  FActiveForm := pForm ;
  DoOnShowForm(FActiveForm);
end;

procedure TtiFormMgr.CloseAllForms;
var
  i : Integer ;
begin
  for i := FForms.Count - 1 downto 0 do
    if (FForms.Items[i] as TFormTIFormMgrForm).ButtonsVisible in [btnVisReadOnly, btnVisReadWrite] then
      DoCloseForm( Forms[i] ) ;
end;

procedure TtiFormMgr.DoCloseForm(const pForm: TFormTIFormMgrForm);
begin
  if ActiveForm = pForm then
  begin
    if GetPrevForm <> nil then
      ActiveForm := GetPrevForm
    else
      ActiveForm := GetNextForm ;
  end ;
  FForms.Extract( pForm ) ;
  if FModalForms.IndexOf(pForm) <> -1 then
    FModalForms.Extract(pForm);
  if ActiveForm = pForm then
    HideActiveForm;
  DoOnShowForm(FActiveForm);
  pForm.Free;
end;

constructor TtiFormMgr.Create;
begin
  inherited ;
  FForms := TObjectList.Create(false) ;
  FModalForms := TObjectList.Create(false) ;
end;

destructor TtiFormMgr.Destroy;
var
  i : integer ;
  lForm : TFormTIFormMgrForm ;
  lName : string ;
begin
  for i := FForms.Count - 1 downto 0 do
  begin
    lForm := FForms.Items[i] as TFormTIFormMgrForm ;
    lName := lForm.ClassName ;
    FForms.Extract(lForm);
    try
      FreeAndNil(lForm) ;
    except
      on e:exception do
        ShowMessage( 'Error destroying form <' + lName + '>' + Cr +
                     'Message: ' + e.Message + Cr +
                     'Location: ' + ClassName + '.Destroy' ) ;
    end ;
  end ;
  FreeAndNil(FForms) ;
  FreeAndNil(FModalForms);
  inherited;
end;

function TtiFormMgr.FindForm(const pFormClass: TFormTIFormMgrFormClass ; const pData : TtiObject ) : TFormTIFormMgrForm ;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FForms.Count - 1 do
    if ( FForms.Items[i] is pFormClass ) and
       ( TFormTIFormMgrForm( FForms.Items[i] ).Data = pData ) then
    begin
      result := FForms.Items[i] as TFormTIFormMgrForm ;
      Exit ; //==>
    end ;
end;

function TtiFormMgr.GetActiveForm: TFormTIFormMgrForm;
begin
  result := FActiveForm ;
end;

function TtiFormMgr.GetForms(i: integer): TFormTIFormMgrForm;
begin
  result := FForms[i] as TFormTIFormMgrForm ;
end;

function TtiFormMgr.GetNextForm: TFormTIFormMgrForm;
begin
  if FForms.Count > 0 then
    result := Forms[0]
  else
    result := nil ;
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

procedure TtiFormMgr.SetActiveForm(const Value: TFormTIFormMgrForm);
begin
  if Value = nil then
  begin
    FActiveForm := Value ;
    Exit ; //==>
  end ;
  ShowForm(Value, Value.Data);
end;

procedure TtiFormMgr.SetForms(i: integer; const Value: TFormTIFormMgrForm);
begin
  FForms[i] := Value ;
end;

procedure  TtiFormMgr.ShowForm(
      const pForm          : TFormTIFormMgrForm;
      const pData          : TtiObject = nil;
            pModal         : Boolean = False;
      const pOnEditsSave   : TtiObjectEvent = nil;
      const pOnEditsCancel : TtiObjectEvent = nil;
            pReadOnly      : boolean = False ) ;
begin
  ShowForm( TFormTIFormMgrFormClass(pForm.ClassType), pData, pModal, pOnEditsSave, pOnEditsCancel, pReadOnly );
end ;

procedure TtiFormMgr.ShowForm(
      const pFormClass      : TFormTIFormMgrFormClass ;
      const pData : TtiObject = nil ;
            pModal: Boolean = False ;
      const pOnEditsSave   : TtiObjectEvent = nil;
      const pOnEditsCancel : TtiObjectEvent = nil;
            pReadOnly : boolean = False
      );
var
  lForm : TFormTIFormMgrForm ;
begin
  Assert( ParentPnl <> nil, 'ParentPnl not assigned' ) ;
  tiAutoCursor;
  DoBeginUpdate;
  try
    if ( FActiveForm <> nil ) and
       ( not FActiveForm.IsModal ) and
       ( not CloseCurrentFormActivatePreviousForm(false) ) then
      Exit ; //==>
    lForm := FindForm( pFormClass, pData ) ;
    if lForm <> nil then
      BringToFront( lForm, false )
    else
    begin
      HideActiveForm;
      lForm := pFormClass.Create(nil);
      lForm.BorderColor    := BorderColor ;
      lForm.OnFormMessage  := FOnFormMessageEvent;
      lForm.OnEditsSave    := pOnEditsSave;
      lForm.OnEditsCancel  := pOnEditsCancel;
      lForm.IsModal        := pModal;
      lForm.FormMgr        := Self;
      // ToDo: Not sure of the need for FModalForms any more
      if pModal then
        FModalForms.Add(lForm);
      Add(lForm);
      if lForm.Data <> pData then
        lForm.Data := pData ;
      lForm.SetupButtons;
      BringToFront( lForm, true );
    end ;
  finally
    DoEndUpdate;
  end;
end;

procedure TtiFormMgr.CloseForm(const pForm: TFormTIFormMgrForm);
begin
  Assert( FActiveForm = pForm, 'Close for can only be called with currently active form');
  CloseCurrentFormActivatePreviousForm( true ) ;
end;

function TtiFormMgr.CloseCurrentFormActivatePreviousForm( pClose : boolean ) : boolean;
var
  lForm : TFormTIFormMgrForm ;
  lActiveForm : TFormTIFormMgrForm ;
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
      lForm := FActiveForm ;
      HideActiveForm;
      FForms.Extract( lForm ) ;
      FreeAndNil(lForm);
      ActiveForm := lActiveForm;
      result := true ;
      DoOnShowForm(ActiveForm);
    end else
    begin
      if pClose then
      begin
        DoCloseForm(FActiveForm);
        result := true ;
      end
      else begin
        HideActiveForm ;
        result := true ;
        DoOnShowForm(Nil);
      end ;
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
  lForm := GetNextForm ;
  if lForm = nil then
    ShowForm(nil, nil)
  else
  begin
    lActiveForm := ActiveForm;
    FForms.Extract(lActiveForm);
    FForms.Add(lActiveForm);
    ShowForm(lForm, lForm.Data) ;
  end;
end;

procedure TtiFormMgr.ShowPrevForm;
var
  lForm : TFormTIFormMgrForm;
  lActiveForm: TFormTIFormMgrForm;
begin
  lForm := GetPrevForm ;
  if lForm = nil then
    ShowForm(nil, nil)
  else
  begin
    lActiveForm := ActiveForm;
    FForms.Extract(lActiveForm);
    FForms.Insert(0, lActiveForm);
    ShowForm(lForm, lForm.Data) ;
  end;
end;

procedure TtiFormMgr.SetBorderColor(const Value: TColor);
var
  i : integer ;
begin
  FBorderColor := Value;
  for i := 0 to FForms.Count - 1 do
    ( FForms.Items[i] as TFormTIFormMgrForm ).BorderColor := FBorderColor ;
end;

procedure TtiFormMgr.AssignFormList(const pStrings: TStrings);
var
  i : integer ;
begin
  pStrings.Clear ;
  for i := 0 to FForms.Count - 1 do
    if FForms.Items[i] <> ActiveForm then
      pStrings.AddObject(( FForms.Items[i] as TFormTIFormMgrForm ).FormCaption,
                          FForms.Items[i] ) ;
end;

procedure TtiFormMgr.DoOnShowForm(const pForm : TFormTIFormMgrForm);
begin
  if assigned(FOnShowForm) then
    FOnShowForm(pForm);
  if ( pForm <> nil ) then
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


procedure TtiFormMgr.SetParentPnl(const Value: TPanel);
begin
  FParentPnl := Value;
end;

procedure TtiFormMgr.ShowFormModal(
  const pFormClass : TFormTIFormMgrFormClass ;
  const pData : TtiObject ;
  const pOnEditsSave   : TtiObjectEvent = nil;
  const pOnEditsCancel : TtiObjectEvent = nil);
begin
  ShowForm( pFormClass, pData, True, pOnEditsSave, pOnEditsCancel, False );
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

procedure TFormTIFormMgrForm.AssignActions(const pList: TList);
var
  i : Integer;
begin
  pList.Clear;
  for i := 0 to FAL.ActionCount - 1 do
    if (FAL.Actions[i] <> FaDummy) and
       ((FAL.Actions[i] as TAction).Visible) then
      pList.Add(FAL.Actions[i]);
end;


procedure TFormTIFormMgrForm.SetIsModal(const Value: Boolean);
begin
  FIsModal := Value;
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
    FActiveForm := nil ;
  end;
end;

function TtiFormMgr.GetFormCount: Integer;
begin
  Result := FForms.Count ;
end;

function TFormTIFormMgrForm.GetData: TtiObject;
begin
  Result := FData;
end;

procedure TFormTIFormMgrForm.SetFormInfoMessage(const Value: string);
begin
  FFormInfoMessage := Value;
  if Assigned(FOnFormMessageEvent) then
    FOnFormMessageEvent(FFormInfoMessage, tiufmtInfo);
end;

procedure TFormTIFormMgrForm.SelectFirstControl;
begin
  SelectFirst;
end;

end.




unit FtiPopupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, tiSpeedButton, ExtCtrls, tiRoundedPanel, tiObject, ActnList,
  tiDataFormData, StdCtrls;

type

  TPopupDisplayPosition = (
    pdpCoveringAndAbove,
    pdpCoveringAndBelow,// was pdpAbove
    pdpBelow);

  TFormTIPopupData = class(TForm)
    pnlBorder: TtiRoundedPanel;
    pnlButtons: TPanel;
    pnlMain: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure DoALUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFormData: TtiDataFormData;

    FTriggeredByRect: TRect;
    FFormDisplayPosition: TPopupDisplayPosition;
    FLastErrorMessage: string;

    FAL        : TActionList;
    FaOK       : TAction;
    FaCancel   : TAction;

    FDoOnPopupOK: TNotifyEvent;
    FDoOnPopupCancel: TNotifyEvent;
    FDeactivatePopupResult: TModalResult;

    function  FormDataIsEdited : boolean;
    procedure SetFormPosition;
  protected
    function  CreateFormData: TtiDataFormData; virtual; abstract;
    procedure DoaCancelExecute(Sender: TObject); virtual;
    procedure DoaOKExecute(Sender: TObject); virtual;
    function  FormIsValid(out AMessage: string) : boolean; virtual;
    function  GetData: TtiObject;
    procedure SetData(const AValue: TtiObject);
    property  FormData: TtiDataFormData read FFormData;
    procedure DoSetControlDataBindings; virtual;
  public
    destructor Destroy; override;

    property Data: TtiObject read GetData write SetData;
    property TriggeredByRect: TRect read FTriggeredByRect write FTriggeredByRect;
    property FormDisplayPosition: TPopupDisplayPosition read FFormDisplayPosition write FFormDisplayPosition;
    property DoOnPopupOK: TNotifyEvent read FDoOnPopupOK write FDoOnPopupOK;
    property DoOnPopupCancel: TNotifyEvent read FDoOnPopupCancel write FDoOnPopupCancel;
    property DeactivatePopupResult: TModalResult read FDeactivatePopupResult write FDeactivatePopupResult;

    class function Execute(const AOwner: TWinControl;
        const AData : TtiObject;
        const AFormPopupButton: TtiSpeedButton;
        const AFormDisplayPosition: TPopupDisplayPosition;
        const ADoOnPopupOK: TNotifyEvent; const ADoOnPopupCancel: TNotifyEvent;
        const ADeactivatePopupResult: TModalResult): TFormTIPopupData; overload; virtual;

    class function Execute(const AOwner: TWinControl;
        const AData : TtiObject;
        const ATriggeredByRect: TRect;
        const AFormDisplayPosition: TPopupDisplayPosition;
        const ADoOnPopupOK: TNotifyEvent; const ADoOnPopupCancel: TNotifyEvent;
        const ADeactivatePopupResult: TModalResult): TFormTIPopupData; overload; virtual;
  end;

implementation
uses
  tiApplicationMenuSystem,
  tiConstants;

{$R *.dfm}


destructor TFormTIPopupData.Destroy;
begin
  FFormData.Free;
  inherited;
end;

procedure TFormTIPopupData.DoaCancelExecute(Sender: TObject);
begin
  if Assigned(FDoOnPopupCancel) then
    FDoOnPopupCancel(Sender);

  ModalResult := mrCancel;
  Close;
end;

procedure TFormTIPopupData.DoALUpdate(Action: TBasicAction; var Handled: Boolean);
var
  LFormIsValid: boolean;
  LMessage: string;
begin
  Assert(FaOK <> nil, 'FaOK must be assigned');
  Assert(FaCancel <> nil, 'FaCancel must be assigned');
  LFormIsValid:= FormIsValid(LMessage);
  FaOK.Enabled := LFormIsValid and FormDataIsEdited;
  if LMessage <> FLastErrorMessage then
  begin
    gAMS.FormErrorMessage:= LMessage;
    FLastErrorMessage:= LMessage;
  end;
end;

procedure TFormTIPopupData.DoaOKExecute(Sender: TObject);
begin
  Assert(FormData.TestValid(TtiObject), CTIErrorInvalidObject);
  FormData.PrepareSave;

  if Assigned(FDoOnPopupOK) then
    FDoOnPopupOK(Sender);

  ModalResult := mrOK;
  Close;
end;

procedure TFormTIPopupData.DoSetControlDataBindings;
begin
  // Implement in the concrete
end;

class function TFormTIPopupData.Execute(const AOwner: TWinControl;
  const AData: TtiObject;
  const AFormPopupButton: TtiSpeedButton;
  const AFormDisplayPosition: TPopupDisplayPosition;
  const ADoOnPopupOK: TNotifyEvent; const ADoOnPopupCancel: TNotifyEvent;
  const ADeactivatePopupResult: TModalResult): TFormTIPopupData;
var
  LTopLeft: TPoint;
  LBottomRight: TPoint;
  LRect: TRect;
begin
  LTopLeft:= AFormPopupButton.Parent.ClientToScreen(AFormPopupButton.BoundsRect.TopLeft);
  LBottomRight:= AFormPopupButton.Parent.ClientToScreen(AFormPopupButton.BoundsRect.BottomRight);
  LRect:= Rect(LTopLeft, LBottomRight);
  result:=
    Execute(
      AOwner,
      AData,
      LRect,
      AFormDisplayPosition,
      ADoOnPopupOK,
      ADoOnPopupCancel,
      ADeactivatePopupResult);
end;

procedure TFormTIPopupData.FormCreate(Sender: TObject);
begin
  FFormData := CreateFormData;
  FLastErrorMessage:= '';

  FAL := TActionList.Create(Self);
  FAL.OnUpdate := DoALUpdate;

  FaOK := TAction.Create(FAL);
  FaOK.ActionList := FAL;
  FaOK.Caption  := '&OK';
  FaOK.OnExecute := DoaOKExecute;
  btnOK.Action := FaOK;

  FaCancel := TAction.Create(FAL);
  FaCancel.ActionList := FAL;
  FaCancel.Caption  := '&Cancel';
  FaCancel.OnExecute := DoaCancelExecute;
  btnCancel.Action := FaCancel;
end;

function TFormTIPopupData.FormDataIsEdited: boolean;
begin
  Assert(FormData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result:= FormData.IsDirty;
end;

procedure TFormTIPopupData.FormDeactivate(Sender: TObject);
  procedure _OnDeactivate(const AOnDeactivateAction: TAction);
  begin
    Assert(AOnDeactivateAction <> nil, Format('%s must be assigned', [AOnDeactivateAction.Name]));
    AOnDeactivateAction.Execute;
  end;
begin
  FAL.OnUpdate:= nil;
  gAMS.FormErrorMessage:= '';
  case FDeactivatePopupResult of
    mrOk:     _OnDeactivate(FaOK);
    mrCancel: _OnDeactivate(FaCancel);
  end;
end;

procedure TFormTIPopupData.FormHide(Sender: TObject);
begin
  GAMS.FormMgr.ActiveForm.SetEscapeKeyEnabled(True);
end;

function TFormTIPopupData.FormIsValid(out AMessage: string): boolean;
begin
  Assert(FormData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := FormData.IsValid(AMessage);
end;

procedure TFormTIPopupData.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    DoaCancelExecute(nil);
end;

procedure TFormTIPopupData.FormShow(Sender: TObject);
begin
  GAMS.FormMgr.ActiveForm.SetEscapeKeyEnabled(False);
end;

function TFormTIPopupData.GetData: TtiObject;
begin
  result:= FFormData.Data;
end;

class function TFormTIPopupData.Execute(const AOwner: TWinControl;
  const AData: TtiObject; const ATriggeredByRect: TRect;
  const AFormDisplayPosition: TPopupDisplayPosition; const ADoOnPopupOK,
  ADoOnPopupCancel: TNotifyEvent;
  const ADeactivatePopupResult: TModalResult): TFormTIPopupData;
begin
  Result := Create(AOwner);
  Result.Data := AData;
  Result.DoOnPopupOK := ADoOnPopupOK;
  Result.DoOnPopupCancel := ADoOnPopupCancel;
  Result.DeactivatePopupResult := ADeactivatePopupResult;
  Result.TriggeredByRect:= ATriggeredByRect;
  Result.FormDisplayPosition := AFormDisplayPosition;
  Result.SetFormPosition;
  Result.Show;
end;

procedure TFormTIPopupData.SetData(const AValue: TtiObject);
begin
  FFormData.Data:= AValue;
  DoSetControlDataBindings;
end;

procedure TFormTIPopupData.SetFormPosition;
begin
  Left := TriggeredByRect.Left;
  case FFormDisplayPosition of
    pdpCoveringAndAbove: Top:= TriggeredByRect.Bottom - Height;
    pdpCoveringAndBelow: Top := TriggeredByRect.Top;
    pdpBelow: Top := TriggeredByRect.Bottom;
  end;
end;

end.

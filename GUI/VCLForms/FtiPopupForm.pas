unit FtiPopupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, tiSpeedButton, ExtCtrls, tiRoundedPanel, tiObject, ActnList;

type

  TPopupDisplayPosition = (
    pdpAbove,
    pdpBelow);

  TFormTIPopupData = class(TForm)
    pnlBorder: TtiRoundedPanel;
    pnlButtons: TPanel;
    btnOK: TtiSpeedButton;
    btnCancel: TtiSpeedButton;
    pnlMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure DoALUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FData: TtiObject;
    FEditedData: TtiObject;

    FFormPopupButton: TtiSpeedButton;
    FFormDisplayPosition: TPopupDisplayPosition;

    FAL        : TActionList;
    FaOK       : TAction;
    FaCancel   : TAction;

    FDoOnPopupOK: TNotifyEvent;
    FDoOnPopupCancel: TNotifyEvent;
    FDeactivatePopupResult: TModalResult;

    function  FormDataIsEdited : boolean;
    procedure SetFormPosition;
  protected
    procedure DoaCancelExecute(Sender: TObject); virtual;
    procedure DoaOKExecute(Sender: TObject); virtual;
    procedure SetData(const AValue: TtiObject); virtual;
    function  EditedData: TtiObject;
    function  FormIsValid : boolean; virtual;
  public
    destructor Destroy; override;
    property Data : TtiObject read FData write SetData;

    property FormPopupButton: TtiSpeedButton read FFormPopupButton write FFormPopupButton;
    property FormDisplayPosition: TPopupDisplayPosition read FFormDisplayPosition write FFormDisplayPosition;
    property DoOnPopupOK: TNotifyEvent read FDoOnPopupOK write FDoOnPopupOK;
    property DoOnPopupCancel: TNotifyEvent read FDoOnPopupCancel write FDoOnPopupCancel;
    property DeactivatePopupResult: TModalResult read FDeactivatePopupResult write FDeactivatePopupResult;

    class function Execute(const AOwner: TWinControl;
        const AData : TtiObject;
        const AFormPopupButton: TtiSpeedButton;
        const AFormDisplayPosition: TPopupDisplayPosition;
        const ADoOnPopupOK: TNotifyEvent; const ADoOnPopupCancel: TNotifyEvent;
        const ADeactivatePopupResult: TModalResult): TFormTIPopupData; virtual;
  end;

implementation
uses
  tiApplicationMenuSystem,
  tiConstants;

{$R *.dfm}


destructor TFormTIPopupData.Destroy;
begin
  FreeAndNil(FEditedData);
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
begin
  Assert(FaOK <> nil, 'FaOK must be assigned');
  Assert(FaCancel <> nil, 'FaCancel must be assigned');
  FaOK.Enabled := FormIsValid and FormDataIsEdited;
end;

procedure TFormTIPopupData.DoaOKExecute(Sender: TObject);
begin
  Assert(Data.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Data.Assign(EditedData);
  Data.Dirty := true;

  if Assigned(FDoOnPopupOK) then
    FDoOnPopupOK(Sender);

  ModalResult := mrOK;
  Close;
end;

class function TFormTIPopupData.Execute(const AOwner: TWinControl;
  const AData: TtiObject;
  const AFormPopupButton: TtiSpeedButton;
  const AFormDisplayPosition: TPopupDisplayPosition;
  const ADoOnPopupOK: TNotifyEvent; const ADoOnPopupCancel: TNotifyEvent;
  const ADeactivatePopupResult: TModalResult): TFormTIPopupData;
begin
  Result := Create(AOwner);
  Result.Data := AData;

  Result.DoOnPopupOK := ADoOnPopupOK;
  Result.DoOnPopupCancel := ADoOnPopupCancel;
  Result.DeactivatePopupResult := ADeactivatePopupResult;

  Result.FormPopupButton := AFormPopupButton;
  Result.FormDisplayPosition := AFormDisplayPosition;
  Result.SetFormPosition;
  Result.Show;
end;

procedure TFormTIPopupData.FormCreate(Sender: TObject);
begin
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
  Assert(Data.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := not Data.Equals(EditedData);
end;

procedure TFormTIPopupData.FormDeactivate(Sender: TObject);
  procedure _OnDeactivate(const AOnDeactivateAction: TAction);
  begin
    Assert(AOnDeactivateAction <> nil, Format('%s must be assigned', [AOnDeactivateAction.Name]));
    AOnDeactivateAction.Execute;
  end;
begin
  case FDeactivatePopupResult of
    mrOk:     _OnDeactivate(FaOK);
    mrCancel: _OnDeactivate(FaCancel);
  end;
end;

procedure TFormTIPopupData.FormHide(Sender: TObject);
begin
  GAMS.FormMgr.ActiveForm.SetEscapeKeyEnabled(True);
end;

function TFormTIPopupData.FormIsValid: boolean;
begin
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := true;
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

function TFormTIPopupData.EditedData: TtiObject;
begin
  Assert(FEditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := FEditedData;
end;

procedure TFormTIPopupData.SetData(const AValue: TtiObject);
begin
  FData := AValue;
  FEditedData := FData.Clone;
  Assert(FData.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(FEditedData.TestValid(TtiObject), CTIErrorInvalidObject);
end;

procedure TFormTIPopupData.SetFormPosition;
var
  LBtnClientPos: TPoint;
  LBtnScreenPos: TPoint;
begin
  Assert(FFormPopupButton <> nil, 'Cannot set popup form position if FormPopupButton is nil');

  LBtnClientPos.X := FFormPopupButton.BoundsRect.Left;
  case FFormDisplayPosition of
    pdpAbove: LBtnClientPos.Y := FFormPopupButton.BoundsRect.Top;
    pdpBelow: LBtnClientPos.Y := FFormPopupButton.BoundsRect.Bottom;
  end;

  LBtnScreenPos := FFormPopupButton.Parent.ClientToScreen(LBtnClientPos);

  Left := LBtnScreenPos.X;
  case FFormDisplayPosition of
    pdpAbove: Top := LBtnScreenPos.Y - Height;
    pdpBelow: Top := LBtnScreenPos.Y;
  end;

end;

end.

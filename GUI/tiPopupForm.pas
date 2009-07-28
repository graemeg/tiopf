unit tiPopupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, tiSpeedButton, ExtCtrls, tiRoundedPanel, tiObject, ActnList;

type

  TPopupDisplayPosition = (
    pdpAbove,
    pdpBelow);

  TtiPopupDataForm = class(TForm)
    pnlBorder: TtiRoundedPanel;
    pnlButtons: TPanel;
    btnOK: TtiSpeedButton;
    btnCancel: TtiSpeedButton;
    pnlMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure DoALUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FData: TtiObject;
    FEditedData: TtiObject;

    FFormPopupButton: TtiSpeedButton;
    FFormDisplayPosition: TPopupDisplayPosition;

    FAL        : TActionList;
    FaOK       : TAction;
    FaCancel   : TAction;
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

    class function Execute(const AOwner: TWinControl;
        const AData : TtiObject;
        const AFormPopupButton: TtiSpeedButton;
        const AFormDisplayPosition: TPopupDisplayPosition): TtiPopupDataForm; virtual;
  end;

var
  tiPopupDataForm: TtiPopupDataForm;

implementation

{$R *.dfm}

uses
  tiConstants;

destructor TtiPopupDataForm.Destroy;
begin
  FreeAndNil(FEditedData);
  inherited;
end;

procedure TtiPopupDataForm.DoaCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TtiPopupDataForm.DoALUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  Assert(FaOK <> nil, 'FaOK must be assigned');
  Assert(FaCancel <> nil, 'FaCancel must be assigned');
  FaOK.Enabled := FormIsValid and FormDataIsEdited;
end;

procedure TtiPopupDataForm.DoaOKExecute(Sender: TObject);
begin
  Assert(Data.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Data.Assign(EditedData);
  Data.Dirty := true;
  ModalResult := mrOK;
  Close;
end;

class function TtiPopupDataForm.Execute(const AOwner: TWinControl;
  const AData: TtiObject;
  const AFormPopupButton: TtiSpeedButton;
  const AFormDisplayPosition: TPopupDisplayPosition): TtiPopupDataForm;
begin
  Result := Create(AOwner);
  Result.Data := AData;

  Result.FormPopupButton := AFormPopupButton;
  Result.FormDisplayPosition := AFormDisplayPosition;
  Result.SetFormPosition;
  Result.Show;
end;

procedure TtiPopupDataForm.FormCreate(Sender: TObject);
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

function TtiPopupDataForm.FormDataIsEdited: boolean;
begin
  Assert(Data.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := not Data.Equals(EditedData);
end;

procedure TtiPopupDataForm.FormDeactivate(Sender: TObject);
begin
  Assert(FaCancel <> nil, 'FaCancel must be assigned');
  FaCancel.Execute;
end;

function TtiPopupDataForm.FormIsValid: boolean;
begin
  Assert(EditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := true;
end;

function TtiPopupDataForm.EditedData: TtiObject;
begin
  Assert(FEditedData.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := FEditedData;
end;

procedure TtiPopupDataForm.SetData(const AValue: TtiObject);
begin
  FData := AValue;
  FEditedData := FData.Clone;
  Assert(FData.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(FEditedData.TestValid(TtiObject), CTIErrorInvalidObject);
end;

procedure TtiPopupDataForm.SetFormPosition;
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

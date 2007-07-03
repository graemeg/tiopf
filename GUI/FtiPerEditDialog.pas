unit FtiPerEditDialog;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiObject, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly;

type

// Paste this into the concrete class
//  protected
//    procedure SetData(const AValue: TtiObject); override;
//    function  FormIsValid : boolean; override;

  TFormTIPerEditDialog = class(TFormTiDialogAbs)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbEnterAsTab: TCheckBox;
    procedure aCancelExecute(Sender: TObject);
    procedure alUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    FAL        : TActionList;
    FaOK       : TAction;
    FaCancel   : TAction;
    FData      : TtiObject;
    FDataBuffer : TtiObject;

  protected

    procedure aOKExecute(Sender: TObject);virtual;

    property  Databuffer : TtiObject read FDataBuffer write FDataBuffer;

    // Implement these in the concrete...
    procedure SetData(const AValue: TtiObject); virtual;
    function  FormIsValid : boolean; virtual;
    function  FormIsEdited : boolean; virtual;
    procedure SetupButtons;

  public
    property Data : TtiObject read FData write SetData;
    class function Execute(const AData : TtiObject; pReadOnly : boolean = false): boolean; virtual;
  end;

implementation
uses
   tiUtils
  ,tiGUIINI
 ;

{$R *.DFM}

class function TFormTIPerEditDialog.Execute(const AData: TtiObject; pReadOnly : boolean = false): boolean;
var
  lForm : TFormTIPerEditDialog;
begin
  lForm := Create(nil);
  try
    lForm.Data := AData;
    result := lForm.ShowModal = mrOK;
  finally
    lForm.Free;
  end;
end;

procedure TFormTIPerEditDialog.FormCreate(Sender: TObject);
begin
  inherited;

  FAL := TActionList.Create(Self);
  FAL.OnUpdate := ALUpdate;

  FaOK := TAction.Create(FAL);
  FaOK.ActionList := FAL;
  FaOK.Caption  := '&OK';
  FaOK.OnExecute := aOKExecute;
  btnOK.Action := FaOK;

  FaCancel := TAction.Create(FAL);
  FaCancel.ActionList := FAL;
  FaCancel.Caption  := '&Cancel';
  FaCancel.OnExecute := aCancelExecute;
  btnCancel.Action := FaCancel;

  cbEnterAsTab.Checked := gGUIINI.ReadBool(Name, 'EnterAsTab', False);

end;

procedure TFormTIPerEditDialog.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataBuffer.Free;
  gGUIINI.WriteBool(Name, 'EnterAsTab', cbEnterAsTab.Checked);
end;

procedure TFormTIPerEditDialog.aOKExecute(Sender: TObject);
begin
  Assert(FData <> nil, 'FData not assigned');
  Assert(FDataBuffer <> nil, 'FDataBuffer not assigned');
  FData.Assign(FDataBuffer);
  FData.Dirty := true;
  ModalResult := mrOK;
end;

procedure TFormTIPerEditDialog.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormTIPerEditDialog.SetData(const AValue: TtiObject);
begin
  FData := AValue;
  FreeAndNil(FDataBuffer);
  if FData <> nil then
    FDataBuffer := FData.Clone;
  SetupButtons;
end;

procedure TFormTIPerEditDialog.alUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  FaOK.Enabled := FormIsValid and FormIsEdited;
  btnCancel.Default := (not cbEnterAsTab.Checked) and (not FaOK.Enabled);
  Handled := true;
end;

function TFormTIPerEditDialog.FormIsValid: boolean;
begin
  result := true;
end;


procedure TFormTIPerEditDialog.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons;
end;

procedure TFormTIPerEditDialog.SetupButtons;
begin
  // No data
  if (FData = nil) or
     (FDataBuffer = nil) then
  begin
    FaOK.Enabled := false;
    btnCancel.Default := true;
    Exit; //==>
  end;

  btnCancel.Default := false;
  btnOK.Default := not cbEnterAsTab.Checked;
  KeyPreview := cbEnterAsTab.Checked;

end;

procedure TFormTIPerEditDialog.FormKeyPress(Sender: TObject; var Key: Char);
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

procedure TFormTIPerEditDialog.FormShow(Sender: TObject);
begin
  SelectFirst;
end;

function TFormTIPerEditDialog.FormIsEdited: boolean;
begin
  result := not FData.Equals(FDataBuffer);
end;

end.




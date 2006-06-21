{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpContractPartPaymentForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ActnList,

  jclStrings,

  pbpBusinessClasses, pbpUtils;

type
  TContractPartPaymentForm = class(TForm)
    Panel1: TPanel;
    Label2: TLabel;
    PaymentTypesComboBox: TComboBox;
    DetailsEdit: TEdit;
    ValueEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    ButtonPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    Bevel1: TBevel;
    ActionList1: TActionList;
    OKAction: TAction;
    PaymentTypesEdit: TEdit;
    procedure OKActionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure PaymentTypesComboBoxChange(Sender: TObject);
  private
    function GetPaymentType: TContractPaymentType;
    function GetPaymentValue: Currency;
    procedure SetPaymentType(const Value: TContractPaymentType);
    procedure SetPaymentValue(const Value: Currency);
    procedure AssignPaymentTypesComboBoxItems;
    function GetPaymentDetails: string;
    procedure SetPaymentDetails(const Value: string);
  public
    procedure AfterConstruction; override;
    function Execute: Boolean;
    property PaymentDetails: string read GetPaymentDetails write SetPaymentDetails;
    property PaymentType: TContractPaymentType read GetPaymentType write SetPaymentType;
    property PaymentValue: Currency read GetPaymentValue write SetPaymentValue;
  end;

var
  ContractPartPaymentForm: TContractPartPaymentForm;

implementation

{$R *.DFM}

{ TContractPartPaymentForm }

procedure TContractPartPaymentForm.AfterConstruction;
begin
  inherited;
  PaymentTypesEdit.Top := PaymentTypesComboBox.Top;
  PaymentTypesEdit.Left := PaymentTypesComboBox.Left;
  PaymentTypesEdit.Width := PaymentTypesComboBox.Width;
  PaymentTypesEdit.Height := PaymentTypesComboBox.Height;

  PaymentDetails := '';
  PaymentValue := 0.00;
  AssignPaymentTypesComboBoxItems;
  if PaymentTypesComboBox.Items.Count = 1 then
  begin
    PaymentTypesComboBox.Visible := False;
    PaymentTypesEdit.Visible := True;
  end
  else
  begin
    PaymentTypesComboBox.Visible := True;
    PaymentTypesEdit.Visible := False;
  end;

  ValueEdit.OnKeyPress := TEditKeyPressUtility.OnEditKeyPress_DisallowNonNumerics;
end;

procedure TContractPartPaymentForm.AssignPaymentTypesComboBoxItems;
var
  PaymentType: TContractPaymentType;
  Counter: Integer;
begin
  Assert(PawnBroker.SystemValues.DefaultContractPaymentType <> nil);

  PaymentTypesComboBox.Clear;
  for Counter := 0 to PawnBroker.ContractPaymentTypes.Count-1 do
  begin
    PaymentType := PawnBroker.ContractPaymentTypes.Items[Counter];
    PaymentTypesComboBox.Items.AddObject(PaymentType.Name, PaymentType);
  end;
  PaymentTypesComboBox.ItemIndex := PaymentTypesComboBox.Items.IndexOf(PawnBroker.SystemValues.DefaultContractPaymentType.Name);
  PaymentTypesEdit.Text := PaymentTypesComboBox.Items[PaymentTypesComboBox.ItemIndex];
end;

function TContractPartPaymentForm.Execute: Boolean;
begin
  Result := ShowModal = mrOK;
end;

function TContractPartPaymentForm.GetPaymentDetails: string;
begin
  Result := DetailsEdit.Text;
end;

function TContractPartPaymentForm.GetPaymentType: TContractPaymentType;
var
  Index: Integer;
begin
  Index := PaymentTypesComboBox.ItemIndex;
  if Index <> -1 then
  begin
    Result := TContractPaymentType(PaymentTypesComboBox.Items.Objects[Index]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TContractPartPaymentForm.GetPaymentValue: Currency;
begin
  Assert(ValueEdit.Text <> '');
  Assert(StrIsNumber(ValueEdit.Text));
  Result := StrToFloat(ValueEdit.Text);
end;

procedure TContractPartPaymentForm.SetPaymentDetails(const Value: string);
begin
  DetailsEdit.Text := Value;
end;

procedure TContractPartPaymentForm.SetPaymentType(const Value: TContractPaymentType);
var
  Index: Integer;
begin
  Index := PaymentTypesComboBox.Items.IndexOfObject(Value);
  Assert(Index <> -1, 'Value is not in payment types list.');
  PaymentTypesComboBox.ItemIndex := Index;
end;

procedure TContractPartPaymentForm.SetPaymentValue(const Value: Currency);
begin
  ValueEdit.Text := StrStripNonNumberChars(Format('%m', [Value]));
end;

procedure TContractPartPaymentForm.OKActionExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TContractPartPaymentForm.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action = OKAction then
  begin
    OKAction.Enabled := (ValueEdit.Text <> '') and  StrIsNumber(ValueEdit.Text);
  end;
end;

procedure TContractPartPaymentForm.PaymentTypesComboBoxChange(
  Sender: TObject);
begin
  PaymentTypesEdit.Text := PaymentTypesComboBox.Text;
end;

end.

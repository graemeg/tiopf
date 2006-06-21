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
unit pbpContractAdditionalAmountForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  jclStrings,

  pbpUtils;

type
  TContractAdditionalAmountForm = class(TForm)
    AdditionalAmountEdit: TEdit;
    Label2: TLabel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    OKButton: TButton;
    procedure FinishPageFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
  private
    procedure SetAmount(const Value: Currency);
    function GetAmount: Currency;
  public
    procedure AfterConstruction; override;
    function Execute: Boolean;
    property Amount: Currency read GetAmount write SetAmount;
  end;

var
  ContractAdditionalAmountForm: TContractAdditionalAmountForm;

implementation

{$R *.DFM}

procedure TContractAdditionalAmountForm.AfterConstruction;
begin
  inherited;
  AdditionalAmountEdit.Text := '0.00';
  AdditionalAmountEdit.OnKeyPress := TEditKeyPressUtility.OnEditKeyPress_DisallowNonNumerics;
end;

function TContractAdditionalAmountForm.Execute: Boolean;
begin
  ActiveControl := AdditionalAmountEdit;
  Result := ShowModal = mrOK;
end;

procedure TContractAdditionalAmountForm.FinishPageFinishButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  Stop := True;
  ModalResult := mrOK;
end;

procedure TContractAdditionalAmountForm.SetAmount(const Value: Currency);
begin
  AdditionalAmountEdit.Text := StrStripNonNumberChars(Format('%m', [Value]));
end;

function TContractAdditionalAmountForm.GetAmount: Currency;
begin
  if AdditionalAmountEdit.Text = '' then
    Result := 0
  else
    Result := StrToFloat(AdditionalAmountEdit.Text);
end;

end.

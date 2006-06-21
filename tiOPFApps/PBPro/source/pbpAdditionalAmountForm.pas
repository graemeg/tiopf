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
unit pbpAdditionalAmountForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  StdCtrls,

  KWizard, RouteMapNodes,

  jclStrings;

type
  TAdditionalAmountForm = class(TForm)
    KWizard: TKWizard;
    KWizardRouteMapNodes1: TKWizardRouteMapNodes;
    EnterAmountPage: TKWizardInteriorPage;
    FinishPage: TKWizardInteriorPage;
    AdditionalAmountEdit: TEdit;
    Label2: TLabel;
    procedure FinishPageFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
  private
    procedure SetAmount(const Value: Currency);
    function GetAmount: Currency;
    { Private declarations }
  public
    procedure AfterConstruction; override;
    function Execute: Boolean;
    property Amount: Currency read GetAmount write SetAmount;
  end;

var
  AdditionalAmountForm: TAdditionalAmountForm;

implementation

{$R *.DFM}


procedure TAdditionalAmountForm.AfterConstruction;
begin
  inherited;
  AdditionalAmountEdit.Text := '0.00';
end;

function TAdditionalAmountForm.Execute: Boolean;
begin
  ActiveControl := AdditionalAmountEdit;
  Result := ShowModal = mrOK;
end;

procedure TAdditionalAmountForm.FinishPageFinishButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  Stop := True;
  ModalResult := mrOK;
end;

procedure TAdditionalAmountForm.SetAmount(const Value: Currency);
begin
  AdditionalAmountEdit.Text := StrStripNonNumberChars(Format('%m', [Value]));
end;

function TAdditionalAmountForm.GetAmount: Currency;
begin
  if AdditionalAmountEdit.Text = '' then
    Result := 0
  else
    Result := StrToFloat(AdditionalAmountEdit.Text);
end;

end.

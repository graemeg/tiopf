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
unit pbpSelectDateDialogForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, KWizard, RouteMapNodes;

type
  TSelectDateDialogForm = class(TForm)
    KWizard1: TKWizard;
    KWizardInteriorPage2: TKWizardInteriorPage;
    DateTimePicker: TDateTimePicker;
    Label1: TLabel;
    procedure KWizardInteriorPage2FinishButtonClick(Sender: TObject;
      var Stop: Boolean);
  private
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
  public
    function Execute: Boolean;
    property Date: TDate read GetDate write SetDate;
  end;

var
  SelectDateDialogForm: TSelectDateDialogForm;

implementation

{$R *.DFM}


function TSelectDateDialogForm.Execute: Boolean;
begin
  DateTimePicker.Date := Date;
  Result := ShowModal = mrOK;
end;

function TSelectDateDialogForm.GetDate: TDate;
begin
  Result := DateTimePicker.Date;
end;

procedure TSelectDateDialogForm.KWizardInteriorPage2FinishButtonClick(
  Sender: TObject; var Stop: Boolean);
begin
  Stop := True;
  ModalResult := mrOK;
end;

procedure TSelectDateDialogForm.SetDate(const Value: TDate);
begin
  DateTimePicker.Date := Value;
end;

end.

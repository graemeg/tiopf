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
unit pbpDateRangeDialogForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, KWizard, RouteMapNodes;

type
  TDateRangeDialogForm = class(TForm)
    KWizard1: TKWizard;
    KWizardInteriorPage2: TKWizardInteriorPage;
    StartDateTimePicker: TDateTimePicker;
    EndDateTimePicker: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    procedure KWizardInteriorPage2FinishButtonClick(Sender: TObject;
      var Stop: Boolean);
  private
    function GetEndDate: TDate;
    function GetStartDate: TDate;
    procedure SetEndDate(const Value: TDate);
    procedure SetStartDate(const Value: TDate);
  public
    function Execute: Boolean;
    property StartDate: TDate read GetStartDate write SetStartDate;
    property EndDate: TDate read GetEndDate write SetEndDate;
  end;

var
  DateRangeDialogForm: TDateRangeDialogForm;

implementation

{$R *.DFM}


function TDateRangeDialogForm.Execute: Boolean;
begin
  StartDateTimePicker.Date := Date - 1;
  EndDateTimePicker.Date := Date;
  Result := ShowModal = mrOK;
end;

function TDateRangeDialogForm.GetEndDate: TDate;
begin
  Result := EndDateTimePicker.Date;
end;

function TDateRangeDialogForm.GetStartDate: TDate;
begin
  Result := StartDateTimePicker.Date;
end;

procedure TDateRangeDialogForm.KWizardInteriorPage2FinishButtonClick(
  Sender: TObject; var Stop: Boolean);
begin
  Stop := True;
  ModalResult := mrOK;
end;

procedure TDateRangeDialogForm.SetEndDate(const Value: TDate);
begin
  EndDateTimePicker.Date := Value;
end;

procedure TDateRangeDialogForm.SetStartDate(const Value: TDate);
begin
  StartDateTimePicker.Date := Value
end;

end.

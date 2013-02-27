{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

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
unit pbpTasksForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EsGrad, ImgList, Buttons, ActnList,

{ TODO -oTT -cReview : 
References to these forms should be moved into a user experience controller }
  pbpContractCreationWizardForm, pbpClientCreationWizardForm, pbpReportCashInCashOut,
  pbpDateRangeDialogForm, pbpReportActiveAndDueContracts, pbpSelectDateDialogForm, pbpPoliceReport;

type
  TTasksForm = class(TForm)
    Gradient: TEsGradient;
    Shape1: TShape;
    Shape2: TShape;
    Label1: TLabel;
    Shape3: TShape;
    Shape4: TShape;
    Label4: TLabel;
    ActionList1: TActionList;
    CreateNewContractAction: TAction;
    SpeedButton1: TSpeedButton;
    ImageList1: TImageList;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    CashInReportAction: TAction;
    CashOutReportAction: TAction;
    ActiveTicketsReportAction: TAction;
    SpeedButton7: TSpeedButton;
    DueForRedemptionReportAction: TAction;
    PoliceReportAction: TAction;
    SpeedButton8: TSpeedButton;
    CreateNewClientAction: TAction;
    procedure CreateNewContractActionExecute(Sender: TObject);
    procedure CashInReportActionExecute(Sender: TObject);
    procedure CashOutReportActionExecute(Sender: TObject);
    procedure ActiveTicketsReportActionExecute(Sender: TObject);
    procedure DueForRedemptionReportActionExecute(Sender: TObject);
    procedure PoliceReportActionExecute(Sender: TObject);
    procedure CreateNewClientActionExecute(Sender: TObject);
  private
    
  public
    { Public declarations }
  end;

var
  TasksForm: TTasksForm;

implementation

{$R *.DFM}

procedure TTasksForm.CreateNewClientActionExecute(Sender: TObject);
var
  WizardForm: TClientCreationWizardForm;
begin
  WizardForm := TClientCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;

procedure TTasksForm.CreateNewContractActionExecute(Sender: TObject);
var
  WizardForm: TContractCreationWizardForm;
begin
  WizardForm := TContractCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;

procedure TTasksForm.CashInReportActionExecute(Sender: TObject);
var
  ReportDataModule: TCashInCashOutReportDataModule;
  DateRangeDialogForm: TDateRangeDialogForm;
begin
  DateRangeDialogForm := TDateRangeDialogForm.Create(nil);
  try
    DateRangeDialogForm.Caption := 'Cash In';
    if DateRangeDialogForm.Execute then
    begin
      ReportDataModule := TCashInCashOutReportDataModule.Create(nil);
      try
        ReportDataModule.StartDate := DateRangeDialogForm.StartDate;
        ReportDataModule.EndDate := DateRangeDialogForm.EndDate;
        ReportDataModule.CashIn := True;
        ReportDataModule.Preview;
      finally
        ReportDataModule.Free;
      end;
    end;
  finally
    DateRangeDialogForm.Free;
  end;
end;

procedure TTasksForm.CashOutReportActionExecute(Sender: TObject);
var
  ReportDataModule: TCashInCashOutReportDataModule;
  DateRangeDialogForm: TDateRangeDialogForm;
begin
  DateRangeDialogForm := TDateRangeDialogForm.Create(nil);
  try
    DateRangeDialogForm.Caption := 'Cash Out';
    if DateRangeDialogForm.Execute then
    begin
      ReportDataModule := TCashInCashOutReportDataModule.Create(nil);
      try
        ReportDataModule.StartDate := DateRangeDialogForm.StartDate;
        ReportDataModule.EndDate := DateRangeDialogForm.EndDate;
        ReportDataModule.CashIn := False;
        ReportDataModule.Preview;
      finally
        ReportDataModule.Free;
      end;
    end;
  finally
    DateRangeDialogForm.Free;
  end;
end;

procedure TTasksForm.ActiveTicketsReportActionExecute(Sender: TObject);
var
  ReportDataModule: TContractListReportDataModule;
begin

  ReportDataModule := TContractListReportDataModule.Create(nil);
  try
    ReportDataModule.PrintActiveContractsReport(True);
  finally
    ReportDataModule.Free;
  end;
end;

procedure TTasksForm.DueForRedemptionReportActionExecute(Sender: TObject);
var
  ReportDataModule: TContractListReportDataModule;
  SelectDateDialogForm: TSelectDateDialogForm;
begin
  SelectDateDialogForm := TSelectDateDialogForm.Create(nil);
  try
    SelectDateDialogForm.Caption := 'Due for Redemption';
    if SelectDateDialogForm.Execute then
    begin
      ReportDataModule := TContractListReportDataModule.Create(nil);
      try
        ReportDataModule.PrintDueContractsReport(SelectDateDialogForm.Date, True);
      finally
        ReportDataModule.Free;
      end;
    end;
  finally
    SelectDateDialogForm.Free;
  end;
end;

procedure TTasksForm.PoliceReportActionExecute(Sender: TObject);
var
  ReportDataModule: TPoliceReportDataModule;
  DateRangeDialogForm: TDateRangeDialogForm;
begin
  DateRangeDialogForm := TDateRangeDialogForm.Create(nil);
  try
    DateRangeDialogForm.Caption := 'Police Report';
    if DateRangeDialogForm.Execute then
    begin
      ReportDataModule := TPoliceReportDataModule.Create(nil);
      try
        ReportDataModule.StartDate := DateRangeDialogForm.StartDate;
        ReportDataModule.EndDate := DateRangeDialogForm.EndDate;
        ReportDataModule.Preview;
      finally
        ReportDataModule.Free;
      end;
    end;
  finally
    DateRangeDialogForm.Free;
  end;
end;

end.

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
unit pbpReportCashInCashOut;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, IBCustomDataSet, IBQuery, FR_DSet, FR_DBSet, IBDatabase, FR_Class,

  tiOpfDataset,

  pbpBusinessClasses, pbpPersistence;

type
  TCashInCashOutReportDataModule = class(TDataModule)
    CashInCashOutReport: TfrReport;
    TransactionDataset: TfrDBDataSet;
    TransactionQuery: TIBQuery;
    TransactionQueryOID: TIntegerField;
    TransactionQueryOWNER_OID: TIntegerField;
    TransactionQueryDETAILS: TIBStringField;
    TransactionQueryEXTENSION_NUMBER: TIntegerField;
    TransactionQueryTRANSACTION_TIMESTAMP: TDateTimeField;
    TransactionQueryTRANSACTION_TYPE: TIntegerField;
    TransactionQueryTRANSACTION_VALUE: TFloatField;
    TransactionQueryPAYMENT_TYPE_OID: TIntegerField;
    TransactionQueryCLIENT_ADDRESS_OID: TIntegerField;
    TransactionQueryOID1: TIntegerField;
    TransactionQueryCONTRACT_FEE: TFloatField;
    TransactionQueryCONTRACT_NUMBER: TIntegerField;
    TransactionQueryEND_DATE: TDateTimeField;
    TransactionQueryEXTENSION_NUMBER1: TIntegerField;
    TransactionQueryINTEREST_RATE: TFloatField;
    TransactionQueryCONTRACT_STATE: TIntegerField;
    TransactionQuerySTART_DATE: TDateTimeField;
    TransactionQueryCLIENT_OID: TIntegerField;
    TransactionQueryOID2: TIntegerField;
    TransactionQueryCLIENT_NUMBER: TIntegerField;
    TransactionQueryEMAIL_ADDRESS: TIBStringField;
    TransactionQueryFAMILY_NAME: TIBStringField;
    TransactionQueryGIVEN_NAMES: TIBStringField;
    TransactionQueryDATE_OF_BIRTH: TDateTimeField;
    TransactionQueryPHONE_HOME: TIBStringField;
    TransactionQueryPHONE_MOBILE: TIBStringField;
    TransactionQueryPHONE_WORK: TIBStringField;
    TransactionQueryNOTES: TMemoField;
    TransactionQueryPHOTO: TBlobField;
    TransactionQueryUNDESIRABLE: TIBStringField;
    TransactionQueryUNDESIRABLE_CODE: TIBStringField;
    TransactionQueryUNDESIRABLE_NOTES: TMemoField;
    TransactionQueryOPERATION: TStringField;
    Database: TIBDatabase;
    IBTransaction1: TIBTransaction;
    TransactionQueryCURRENT_ADDRESS_OID: TIntegerField;
    procedure CashInCashOutReportGetValue(const ParName: String;
      var ParValue: Variant);
    procedure TransactionQueryCalcFields(DataSet: TDataSet);
  private
    FTitle: string;
    FStartDate: TDate;
    FEndDate: TDate;
    FCashIn: Boolean;
    FTransactionList: TContractTransactionListCashFlowReportQuery;
    procedure Prepare;
    procedure Unprepare;
  public
    procedure AfterConstruction; override;
    procedure Print;
    procedure Preview;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property CashIn: Boolean read FCashIn write FCashIn;
  end;

var
  CashInCashOutReportDataModule: TCashInCashOutReportDataModule;

implementation

{$R *.DFM}

{ TCashInCashOutReportDataModule }

procedure TCashInCashOutReportDataModule.AfterConstruction;
begin
  inherited;
  Database.DatabaseName := gDatabaseName;
end;

procedure TCashInCashOutReportDataModule.CashInCashOutReportGetValue(
  const ParName: String; var ParValue: Variant);
begin
  if ParName = 'TITLE' then
    ParValue := FTitle;
end;

procedure TCashInCashOutReportDataModule.TransactionQueryCalcFields(
  DataSet: TDataSet);
begin
  DataSet.FieldByName('OPERATION').AsString := ContractTransactionTypeToStr(
    TContractTransactionType(DataSet.FieldByName('TRANSACTION_TYPE').AsInteger));
end;

procedure TCashInCashOutReportDataModule.Prepare;
begin
  if CashIn then
    FTitle := 'Cash In Report: '
  else
    FTitle := 'Cash Out Report: ';

  FTitle := FTitle + DateToStr(FStartDate) + ' to ' + DateToStr(EndDate);

  if CashIn then
    TransactionQuery.SQL.Add('AND TRANSACTION_VALUE > 0')
  else
    TransactionQuery.SQL.Add('AND TRANSACTION_VALUE < 0');

  TransactionQuery.SQL.Add('ORDER BY TRANSACTION_TIMESTAMP');
  TransactionQuery.ParamByName('STARTTIME').AsDateTime := StartDate;
  TransactionQuery.ParamByName('ENDTIME').AsDateTime := EndDate + 0.999999;
end;

procedure TCashInCashOutReportDataModule.Preview;
begin
  Prepare;
  try
    CashInCashOutReport.ShowReport;
  finally
    Unprepare;
  end;
end;

procedure TCashInCashOutReportDataModule.Print;
begin
  Prepare;
  try
    CashInCashOutReport.PrepareReport;
    CashInCashOutReport.PrintPreparedReport('', 1);
  finally
    Unprepare;
  end;
end;

procedure TCashInCashOutReportDataModule.Unprepare;
begin

end;



end.

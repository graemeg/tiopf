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
unit pbpReportActiveAndDueContracts;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, IBCustomDataSet, IBQuery, IBDatabase,

  jclStrings,

  FR_DSet, FR_DBSet, FR_Class,

  pbpBusinessClasses, pbpPersistence;

type
  TContractListReportDataModule = class(TDataModule)
    ContractDataset: TfrDBDataSet;
    ContractQuery: TIBQuery;
    ContractsReport: TfrReport;
    ContractQueryDataSource: TDataSource;
    ContractQueryOID: TIntegerField;
    ContractQueryCONTRACT_FEE: TFloatField;
    ContractQueryCONTRACT_NUMBER: TIntegerField;
    ContractQueryEND_DATE: TDateTimeField;
    ContractQueryEXTENSION_NUMBER: TIntegerField;
    ContractQueryINTEREST_RATE: TFloatField;
    ContractQueryCONTRACT_STATE: TIntegerField;
    ContractQuerySTART_DATE: TDateTimeField;
    ContractQueryCLIENT_OID: TIntegerField;
    ContractQueryOID1: TIntegerField;
    ContractQueryCLIENT_NUMBER: TIntegerField;
    ContractQueryEMAIL_ADDRESS: TIBStringField;
    ContractQueryFAMILY_NAME: TIBStringField;
    ContractQueryGIVEN_NAMES: TIBStringField;
    ContractQueryDATE_OF_BIRTH: TDateTimeField;
    ContractQueryPHONE_HOME: TIBStringField;
    ContractQueryPHONE_MOBILE: TIBStringField;
    ContractQueryPHONE_WORK: TIBStringField;
    ContractQueryNOTES: TMemoField;
    ContractQueryPHOTO: TBlobField;
    ContractQueryUNDESIRABLE: TIBStringField;
    ContractQueryUNDESIRABLE_CODE: TIBStringField;
    ContractQueryUNDESIRABLE_NOTES: TMemoField;
    ContractQueryLOAN_VALUE: TCurrencyField;
    ContractQueryREDEMPTION_VALUE: TCurrencyField;
    ContractQueryCURRENT_PAYMENTS: TCurrencyField;
    ContractQueryPREVIOUS_PAYMENTS: TCurrencyField;
    ContractQuerySUBTOTAL: TCurrencyField;
    TempQuery: TIBQuery;
    Database: TIBDatabase;
    IBTransaction1: TIBTransaction;
    ContractQueryCURRENT_ADDRESS_OID: TIntegerField;
    procedure ContractQueryCalcFields(DataSet: TDataSet);
    procedure ContractsReportGetValue(const ParName: String;
      var ParValue: Variant);
  private
    FTitle: string;
  public
    procedure AfterConstruction; override;
    procedure PrintActiveContractsReport(Preview: Boolean = False);
    procedure PrintDueContractsReport(DueDate: TDate; Preview: Boolean = False);
  end;

var
  ContractListReportDataModule: TContractListReportDataModule;

implementation

uses
  pbpContractPrint;

{$R *.DFM}

{ TContractListReportDataModule }

procedure TContractListReportDataModule.AfterConstruction;
begin
  inherited;
  Database.DatabaseName := gDatabaseName;
end;

procedure TContractListReportDataModule.PrintActiveContractsReport(
  Preview: Boolean);
begin
  FTitle := 'Active Contracts';

  ContractQuery.SQL.Add('AND CONTRACT_STATE = ' + IntToStr(ord(csActive)));
  ContractQuery.SQL.Add(' ORDER BY CONTRACT_NUMBER');

  ContractsReport.ShowReport;
end;

procedure TContractListReportDataModule.ContractQueryCalcFields(
  DataSet: TDataSet);
var
  ContractFee: Currency;
  LoanValue: Currency;
  InterestValue: Currency;
  CurrentPayments: Currency;
  PreviousPayments: Currency;
  RedemptionValue: Currency;
  Subtotal: Currency;
  OID: Integer;
  ExtensionNumber: Integer;
begin
  OID := DataSet.FieldByName('OID').AsInteger;
  ExtensionNumber := DataSet.FieldByName('EXTENSION_NUMBER').AsInteger;

  ContractFee := DataSet.FieldByName('CONTRACT_FEE').AsFloat;

  // Calc Subtotal
  TempQuery.Close;
  TempQuery.SQL.Clear;
  TempQuery.SQL.Add('SELECT SUM(CONTRACT_ITEM_VALUE) FROM CONTRACT_ITEM WHERE OWNER_OID = ' + IntToStr(OID));
  TempQuery.Open;
  Assert(TempQuery.RecordCount = 1);
  Subtotal := TempQuery.Fields[0].AsFloat;
  TempQuery.Close;

  // Calc Current payments
  TempQuery.SQL.Clear;
  TempQuery.SQL.Text :=
    'SELECT ' +
    '  SUM(TRANSACTION_VALUE) ' +
    '  FROM ' +
    '  CONTRACT_TRANSACTION ' +
    'WHERE ' +
    '  TRANSACTION_TYPE = 4 AND ' +
    '  OWNER_OID = ' + IntToStr(OID) + ' AND ' +
    '  EXTENSION_NUMBER = ' + IntToStr(ExtensionNumber);
  TempQuery.Open;
  Assert(TempQuery.RecordCount = 1);
  CurrentPayments := TempQuery.Fields[0].AsFloat;
  TempQuery.Close;

  // Calc Previous payments
  TempQuery.SQL.Clear;
  TempQuery.SQL.Text :=
    'SELECT ' +
    '  SUM(TRANSACTION_VALUE) ' +
    '  FROM ' +
    '  CONTRACT_TRANSACTION ' +
    'WHERE ' +
    '  TRANSACTION_TYPE = 4 AND ' +
    '  OWNER_OID = ' + IntToStr(OID) + ' AND ' +
    '  EXTENSION_NUMBER < ' + IntToStr(ExtensionNumber);
  TempQuery.Open;
  Assert(TempQuery.RecordCount = 1);
  PreviousPayments := TempQuery.Fields[0].AsFloat;
  TempQuery.Close;

  LoanValue := Subtotal - PreviousPayments;

  InterestValue := (DataSet.FieldByName('INTEREST_RATE').AsFloat / 100) * LoanValue;

  DataSet.FieldByName('LOAN_VALUE').AsFloat := LoanValue;
  DataSet.FieldByName('REDEMPTION_VALUE').AsFloat := LoanValue + ContractFee + InterestValue - CurrentPayments;
  DataSet.FieldByName('PREVIOUS_PAYMENTS').AsFloat := PreviousPayments;
  DataSet.FieldByName('CURRENT_PAYMENTS').AsFloat := CurrentPayments;
  DataSet.FieldByName('SUBTOTAL').AsFloat := Subtotal;
end;

procedure TContractListReportDataModule.ContractsReportGetValue(
  const ParName: String; var ParValue: Variant);
begin
  if ParName = 'TITLE' then
    ParValue := FTitle;
end;

procedure TContractListReportDataModule.PrintDueContractsReport(DueDate: TDate; Preview: Boolean);
begin
  FTitle := 'Due for redemption on or before ' + DateTimeToStr(DueDate);

  ContractQuery.SQL.Add('AND CONTRACT_STATE = ' + IntToStr(ord(csActive)));
  ContractQuery.SQL.Add('AND END_DATE <= ' + StrSingleQuote(FormatDateTime('m/d/yyyy', DueDate)));
  ContractQuery.SQL.Add(' ORDER BY END_DATE');

  ContractsReport.ShowReport;
end;



end.

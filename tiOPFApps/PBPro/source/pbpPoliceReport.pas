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
unit pbpPoliceReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, IBCustomDataSet, IBQuery,

  FR_Class, Db, FR_DSet, FR_DBSet, FR_Rich,

  tiOpfDataset, tiPtnVisPerObj,

  pbpBusinessClasses, pbpPersistence;

type
  TPoliceReportDataModule = class(TDataModule)
    PawnbrokerObjectDataset: TOpfDataset;
    PawnbrokerObjectDatasetName: TStringField;
    PawnbrokerReportDataset: TfrDBDataSet;
    ContractObjectDataset: TOpfDataset;
    ContractReportDataset: TfrDBDataSet;
    ContractReport: TfrReport;
    PawnbrokerObjectDatasetContactDetails: TStringField;
    ContractItemObjectDataset: TOpfDataset;
    ContractItemReportDataset: TfrDBDataSet;
    ClientAddressObjectDataset: TOpfDataset;
    ClientAddressReportDataset: TfrDBDataSet;
    ClientIdentityRecordObjectDataset: TOpfDataset;
    ClientIdentityRecordReportDataset: TfrDBDataSet;
    SystemValuesObjectDataset: TOpfDataset;
    SystemValuesReportDataset: TfrDBDataSet;
    SystemValuesObjectDatasetCaption: TStringField;
    SystemValuesObjectDatasetContractPeriod: TIntegerField;
    SystemValuesObjectDatasetContractFee: TFloatField;
    SystemValuesObjectDatasetContractInterestRate: TFloatField;
    SystemValuesObjectDatasetContractTerms: TStringField;
    ContractQuery: TIBQuery;
    Database: TIBDatabase;
    IBTransaction1: TIBTransaction;
    ContractItemQuery: TIBQuery;
    ContractDataSource: TDataSource;
    ContractItemDataset: TfrDBDataSet;
    IdentityRecordQuery: TIBQuery;
    IdentityRecordDataset: TfrDBDataSet;
    procedure ContractReportGetValue(const ParName: String;
      var ParValue: Variant);
    procedure ContractObjectDatasetAfterScroll(DataSet: TDataSet);
  private
    FClientAddressList: TPerObjList;
    FPawnBrokerList: TPerObjList;
    FContractList: TPerObjList;
    FData: TContract;
    FSystemValuesList: TPerObjList;
    FStartDate: TDate;
    FEndDate: TDate;
    function CreateNewObjectList: TPerObjList;
    procedure Prepare;
    procedure Unprepare;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Preview;
    procedure Print(const NumCopies: Integer = 1);
    property Data: TContract read FData write FData;
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
  end;

var
  PoliceReportDataModule: TPoliceReportDataModule;

implementation

{$R *.DFM}

{ TReportsDataModule }

procedure TPoliceReportDataModule.AfterConstruction;
begin
  inherited;
  Database.DatabaseName := gDatabaseName;
  FClientAddressList := CreateNewObjectList;
  FPawnBrokerList := CreateNewObjectList;
  FContractList := CreateNewObjectList;
  FSystemValuesList := CreateNewObjectList;
end;

procedure TPoliceReportDataModule.BeforeDestruction;
begin
  FSystemValuesList.Free;
  FClientAddressList.Free;
  FPawnBrokerList.Free;
  FContractList.Free;
  inherited;
end;

function TPoliceReportDataModule.CreateNewObjectList: TPerObjList;
begin
  Result := TPerObjList.Create;
  Result.AutoSetItemOwner := False;
  Result.OwnsObjects := False;
end;

procedure TPoliceReportDataModule.Prepare;
var
  Counter: Integer;
  Contract: TContract;
begin
  ContractQuery.ParamByName('StartDate').AsDateTime := FStartDate;
  ContractQuery.ParamByName('EndDate').AsDateTime := FEndDate + 0.999999;

  FSystemValuesList.Add(pbpBusinessClasses.PawnBroker.SystemValues);

  FPawnBrokerList.Add(pbpBusinessClasses.PawnBroker);

  for Counter := 0 to Pred(Pawnbroker.Contracts.Count) do
  begin
    Contract := Pawnbroker.Contracts.Items[Counter];
    if ((Contract.StartDate >= FStartDate) and (Contract.StartDate <= FEndDate)) or
       ((Contract.EndDate >= FStartDate) and (Contract.StartDate <= FEndDate)) then
    begin
      FContractList.Add(Contract);
    end;
  end;


//  if FData.ClientAddress <> nil then
//  begin
//    FClientAddressList.Add(FData.ClientAddress)
//  end;

  PawnbrokerObjectDataset.ObjectList := FPawnbrokerList;
  ContractObjectDataset.ObjectList := FContractList;
//  ClientAddressObjectDataset.ObjectList := FClientAddressList;
//  ClientIdentityRecordObjectDataset.ObjectList := FData.ClientIdentityRecordProxies;
//  ContractItemObjectDataset.ObjectList := FData.Items;
end;

procedure TPoliceReportDataModule.Preview;
begin
  Prepare;
  try
    ContractReport.ShowReport;
  finally
    Unprepare;
  end;
end;

procedure TPoliceReportDataModule.Print(const NumCopies: Integer = 1);
begin
  Prepare;
  try
    ContractReport.PrepareReport;
    ContractReport.PrintPreparedReport('', NumCopies);
  finally
    Unprepare;
  end;
end;

procedure TPoliceReportDataModule.Unprepare;
begin
  FPawnBrokerList.Clear;
  FContractList.Clear;
  FSystemValuesList.Clear;
end;

procedure TPoliceReportDataModule.ContractReportGetValue(
  const ParName: String; var ParValue: Variant);
begin
  if ParName = 'CONTRACT_TERMS' then
    ParValue := Pawnbroker.SystemValues.ContractTerms;
end;

procedure TPoliceReportDataModule.ContractObjectDatasetAfterScroll(
  DataSet: TDataSet);
var
  Contract: TContract;
begin
  Contract := (DataSet as TOpfDataset).GetActiveItem as TContract;
  if Assigned(Contract.Client) and (Contract.Client.ObjectState = posPK) then
    Contract.Client.Read;
  if Contract.ObjectState = posPK then
    Contract.Read;
  ClientIdentityRecordObjectDataset.ObjectList := Contract.ClientIdentityRecordProxies;
end;

end.

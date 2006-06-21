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
unit pbpContractPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  FR_Class, Db, FR_DSet, FR_DBSet, FR_Rich,

  tiOpfDataset, tiPtnVisPerObj,

  pbpBusinessClasses;

type
  TContractPrintDataModule = class(TDataModule)
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
    procedure ContractReportGetValue(const ParName: String;
      var ParValue: Variant);
  private
    FClientAddressList: TPerObjList;
    FPawnBrokerList: TPerObjList;
    FContractList: TPerObjList;
    FData: TContract;
    FSystemValuesList: TPerObjList;
    function CreateNewObjectList: TPerObjList;
    procedure Prepare;
    procedure Unprepare;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Preview;
    procedure Print(const NumCopies: Integer = 1);
    property Data: TContract read FData write FData;
  end;

var
  ContractPrintDataModule: TContractPrintDataModule;

implementation

{$R *.DFM}

{ TReportsDataModule }

procedure TContractPrintDataModule.AfterConstruction;
begin
  inherited;
  FClientAddressList := CreateNewObjectList;
  FPawnBrokerList := CreateNewObjectList;
  FContractList := CreateNewObjectList;
  FSystemValuesList := CreateNewObjectList;
end;

procedure TContractPrintDataModule.BeforeDestruction;
begin
  FSystemValuesList.Free;
  FClientAddressList.Free;
  FPawnBrokerList.Free;
  FContractList.Free;
  inherited;
end;

function TContractPrintDataModule.CreateNewObjectList: TPerObjList;
begin
  Result := TPerObjList.Create;
  Result.AutoSetItemOwner := False;
  Result.OwnsObjects := False;
end;

procedure TContractPrintDataModule.Prepare;
begin
  Assert(FData <> nil);

  FSystemValuesList.Add(pbpBusinessClasses.PawnBroker.SystemValues);

  FPawnBrokerList.Add(pbpBusinessClasses.PawnBroker);
  FContractList.Add(FData);
  if FData.ClientAddress <> nil then
  begin
    FClientAddressList.Add(FData.ClientAddress)
  end;

  PawnbrokerObjectDataset.ObjectList := FPawnbrokerList;
  ContractObjectDataset.ObjectList := FContractList;
  ClientAddressObjectDataset.ObjectList := FClientAddressList;
  ClientIdentityRecordObjectDataset.ObjectList := FData.ClientIdentityRecordProxies;
  ContractItemObjectDataset.ObjectList := FData.Items;
end;

procedure TContractPrintDataModule.Preview;
begin
  Prepare;
  try
    ContractReport.ShowReport;
  finally
    Unprepare;
  end;
end;

procedure TContractPrintDataModule.Print(const NumCopies: Integer = 1);
begin
  if NumCopies > 0 then
  begin
    Prepare;
    try
      ContractReport.PrepareReport;
      ContractReport.PrintPreparedReport('', NumCopies);
    finally
      Unprepare;
    end;
  end;
end;

procedure TContractPrintDataModule.Unprepare;
begin
  FPawnBrokerList.Clear;
  FContractList.Clear;
  FSystemValuesList.Clear;
end;

procedure TContractPrintDataModule.ContractReportGetValue(
  const ParName: String; var ParValue: Variant);
begin
  if ParName = 'CONTRACT_TERMS' then
    ParValue := Pawnbroker.SystemValues.ContractTerms;
end;

end.

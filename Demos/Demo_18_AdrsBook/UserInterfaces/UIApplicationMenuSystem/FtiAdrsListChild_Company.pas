{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiAdrsListChild_Company;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, tiPerAwareCtrls, StdCtrls, ComCtrls, Adrs_BOM, tiListView,
  tiSplitter, tiFocusPanel,FtiFormMgrForm, tiReadOnly, Buttons,
  tiVisitorDB;

type
  TFormEditCompany = class(TFormTIFormMgrForm)
    gbName: TGroupBox;
    paeCompanyName: TtiPerAwareEdit;
    tiSplitterPanel1: TtiSplitterPanel;
    tiSplitterPanel2: TtiSplitterPanel;
    Label1: TLabel;
    lvEAddress: TtiListView;
    Label2: TLabel;
    lvAddress: TtiListView;
    paeNotes: TtiPerAwareMemo;
    procedure FormShow(Sender: TObject);
    procedure lvEAddressFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure lvEAddressItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvAddressItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvEAddressItemEdit(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvAddressItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvEAddressItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvAddressItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure FormCreate(Sender: TObject);
  private
  protected
    function  FormIsValid: boolean; override;
    procedure DoBeforeDisgard; override;
    function  Company: TCompany;
  public
    procedure SetData(const Value: TtiObject); override;
  end;

procedure EditNewCompany;

implementation
uses
   tiVisitorDB_Cli
  ,FEdit_Addrs
  ,FEdit_EAddrs
  ,tiUtils
  ,tiFormMgr
 ;

{$R *.DFM}

procedure EditNewCompany;
var
  lData: TCompany;
begin
  lData:= TCompany.CreateNew;
  gAdrsBook.Companies.Add(lData);
  gFormMgr.ShowForm(TFormEditCompany, lData, false);
end;


{ TForm1 }

procedure TFormEditCompany.SetData(const Value: TtiObject);
begin
  inherited SetData(Value);
  if Value = nil then
    exit; //=>

  paeCompanyName.LinkToData(Company, 'CompanyName');
  paeNotes.LinkToData(Company, 'Notes');
  LVEAddress.Data  := Company.EAddressList.List;
  LVAddress.Data   := Company.AddressList.List;
end;

procedure TFormEditCompany.FormShow(Sender: TObject);
begin
  paeCompanyName.SetFocus;
end;

procedure TFormEditCompany.lvEAddressFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  pbInclude:= not (pData as TtiObject).Deleted;
end;

procedure TFormEditCompany.lvEAddressItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete(pData as TtiObject) then
    pLV.Refresh;
end;

procedure TFormEditCompany.lvAddressItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete(pData as TtiObject) then
    pLV.Refresh;
end;

procedure TFormEditCompany.lvEAddressItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEdit_EAdrs.Execute(pData as TEAdrs) then
    pLV.Refresh;
end;

procedure TFormEditCompany.lvAddressItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEdit_Adrs.Execute(pData as TAdrs) then
    pLV.Refresh;
end;

procedure TFormEditCompany.lvEAddressItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData: TEAdrs;
begin
  lData:= TEAdrs.CreateNew;
  if TFormEdit_EAdrs.Execute(lData) then
  begin
    Company.EAddressList.Add(lData);
    LVEAddress.Refresh;
  end else
    lData.Free;
end;

procedure TFormEditCompany.lvAddressItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData: TAdrs;
begin
  lData:= TAdrs.CreateNew;
  if TFormEdit_Adrs.Execute(lData) then
  begin
    Company.AddressList.Add(lData);
    LVAddress.Refresh;
  end else
    lData.Free;
end;

procedure TFormEditCompany.FormCreate(Sender: TObject);
begin
  inherited;
  FormCaption:= 'Edit a companies contact details';
end;

procedure TFormEditCompany.DoBeforeDisgard;
begin
  if DataBuffer.ObjectState = posCreate then
    gAdrsBook.Companies.Remove(Data);
end;

function TFormEditCompany.Company: TCompany;
begin
  result:= DataBuffer as TCompany;
end;

function TFormEditCompany.FormIsValid: boolean;
begin
  result:= Company.IsValid;
end;

end.

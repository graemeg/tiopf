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

  Change history:
    Created: Jan 2000

  Notes: Dialog for editing a person's details and browsing their
         Addresses and EAddresses. This dialog is used as the LH
         pane of the main form and is controled by the TtiTreeViewPlus.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiAdrsListChild_Person;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Adrs_BOM, ComCtrls, tiListView, StdCtrls, tiPerAwareCtrls,
  tiSplitter, ExtCtrls, tiFocusPanel, FtiFormMgrForm, tiReadOnly, Buttons,
  tiPtnVisPerObj
  ;

type
  TFormEditPerson = class(TFormTIFormMgrForm)
    GroupBox1: TGroupBox;
    paeLastName: TtiPerAwareEdit;
    paeFirstName: TtiPerAwareEdit;
    paeInitials: TtiPerAwareEdit;
    tiSplitterPanel1: TtiSplitterPanel;
    tiSplitterPanel2: TtiSplitterPanel;
    lvEAddress: TtiListView;
    Label1: TLabel;
    Label2: TLabel;
    lvAddress: TtiListView;
    paeNotes: TtiPerAwareMemo;
    paeTitle: TtiPerAwareComboBoxStatic;
    procedure LVEAddressFilterData(pData: TPersistent; var pbInclude: Boolean);
    procedure FormShow(Sender: TObject);
    procedure lvEAddressItemEdit(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvEAddressItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvEAddressItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvAddressItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvAddressItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function  Person : TPerson ;
  protected
    function  FormIsValid : boolean ; override ;
    procedure DoBeforeDisgard ; override ;
  public
    procedure SetData(const Value: TPerObjAbs); override ;

  end;


procedure EditNewPerson ;

implementation
uses
  tiUtils
  ,FEdit_Addrs
  ,FEdit_EAddrs
  ,tiDialogs
  ,tiFormMgr
  ;

{$R *.DFM}

procedure EditNewPerson ;
var
  lData : TPerson ;
begin
  lData := TPerson.CreateNew ;
  gAdrsBook.People.Add(lData);
  gFormMgr.ShowForm(TFormEditPerson, lData, false) ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormEditPerson
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Form's OnShow event
//------------------------------------------------------------------------------
procedure TFormEditPerson.FormShow(Sender: TObject);
begin
  paeTitle.SetFocus;
end;

// Data's Set method. Write data to the appropriate GUI controls.
//------------------------------------------------------------------------------
procedure TFormEditPerson.SetData(const Value: TPerObjAbs);
begin
  inherited SetData(Value);
  if Value = nil then 
    exit ; //=>

  paeLastName.LinkToData(  DataBuffer, 'LastName' ) ;
  paeFirstName.LinkToData( DataBuffer, 'FirstName' ) ;
  paeInitials.LinkToData(  DataBuffer, 'Initials' ) ;
  paeTitle.LinkToData(     DataBuffer, 'Title' ) ;
  paeNotes.LinkToData(     DataBuffer, 'Notes' ) ;

  LVEAddress.Data   := (DataBuffer as TPerson).EAddressList.List ;
  LVAddress.Data    := (DataBuffer as TPerson).AddressList.List ;

end;

// The LastName was changed, so change the treeNode's text

// The form's Valid property is called by the TtiTreeViewPlus before allowing
// the node to change. Form validation is performed here.
//------------------------------------------------------------------------------

// TtiListViewPlus on filter event to filter deleted e-addresses
//------------------------------------------------------------------------------
procedure TFormEditPerson.LVEAddressFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end ;

procedure TFormEditPerson.lvEAddressItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEdit_EAdrs.Execute( pData as TEAdrs) then
    pLV.Refresh ;
end;

procedure TFormEditPerson.lvEAddressItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TEAdrs ;
begin
  lData := TEAdrs.CreateNew ;
  if TFormEdit_EAdrs.Execute( lData ) then
  begin
    Person.EAddressList.Add( lData ) ;
    LVEAddress.Refresh ;
  end else
    lData.Free ;
end;

procedure TFormEditPerson.lvEAddressItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TAdrsAbs ;
begin
  lData := pData as TAdrsAbs ;
  if tiAppConfirmation( 'Are you sure you want to delete <%s - %s> ?',
                        [lData.AdrsType.Text, lData.Caption] ) then
  begin
    lData.Deleted := true ;
    pLV.Refresh ;
  end ;
end;

procedure TFormEditPerson.lvAddressItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEdit_Adrs.Execute( pData as TAdrs ) then
    pLV.Refresh ;
end;

procedure TFormEditPerson.lvAddressItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TAdrs ;
begin
  lData := TAdrs.CreateNew ;
  if TFormEdit_Adrs.Execute( lData ) then
  begin
    Person.AddressList.Add( lData ) ;
    LVAddress.Refresh ;
  end else
    lData.Free ;
end;

procedure TFormEditPerson.FormCreate(Sender: TObject);
begin
  inherited;
  FormCaption := 'Edit a person''s contact details';
end;

procedure TFormEditPerson.FormDestroy(Sender: TObject);
begin
  inherited;
//
end;

function TFormEditPerson.Person: TPerson;
begin
  result := DataBuffer as TPerson ;
end;

function TFormEditPerson.FormIsValid: boolean;
begin
  result := Person.IsValid;
end;

procedure TFormEditPerson.DoBeforeDisgard;
begin
  if DataBuffer.ObjectState = posCreate then
    gAdrsBook.People.Remove(Data);
end;

end.

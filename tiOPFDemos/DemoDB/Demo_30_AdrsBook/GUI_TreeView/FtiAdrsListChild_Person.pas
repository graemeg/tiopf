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
  tiSplitter, ExtCtrls, tiFocusPanel
  ;

type
  TFormEditPerson = class(TForm)
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
    procedure paeLastNameChange(Sender: TObject);
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
  private
    FData     : TPerson ;
    FTreeNode : TTreeNode;
    procedure SetData(const Value: TPersistent);
    function  GetData : TPersistent;
    function  GetValid: boolean;
  published
    // These published properties are required by the TtiTreeViewPlus
    property Data : TPersistent read GetData write SetData ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid    : boolean   read GetValid ;
  public

  end;

implementation
uses
  tiPtnVisPerObj
  ,tiUtils
  ,FEdit_Addrs
  ,FEdit_EAddrs
  ,tiDialogs
  ;

{$R *.DFM}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormEditPerson
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Form's OnShow event
//------------------------------------------------------------------------------
procedure TFormEditPerson.FormShow(Sender: TObject);
begin
  if FData.ObjectState = posCreate then
    paeLastName.SetFocus ;
end;

// Data's Get method
//------------------------------------------------------------------------------
function TFormEditPerson.GetData: TPersistent;
begin
  result := FData ;
end;

// Data's Set method. Write data to the appropriate GUI controls.
//------------------------------------------------------------------------------
procedure TFormEditPerson.SetData(const Value: TPersistent);
begin
  if Value = nil then begin
    FData := nil ;
    exit ; //=>
  end ;

  FData := Value as TPerson ;

  paeLastName.LinkToData( FData, 'LastName' ) ;
  paeFirstName.LinkToData( FData, 'FirstName' ) ;
  paeInitials.LinkToData( FData, 'Initials' ) ;
  paeTitle.LinkToData( FData, 'Title' ) ;
  paeNotes.LinkToData( FData, 'Notes' ) ;

  LVEAddress.Data   := FData.EAddressList.List ;
  LVAddress.Data    := FData.AddressList.List ;

end;

// The LastName was changed, so change the treeNode's text

// The form's Valid property is called by the TtiTreeViewPlus before allowing
// the node to change. Form validation is performed here.
//------------------------------------------------------------------------------
function TFormEditPerson.GetValid: boolean;
begin
  result := true ;
  if FData = nil then
    exit ; //==>

  // Has there been a change made ?
  //if ( FData.LastName    = eLastName.Text    ) and
  if ( not paeLastName.Dirty    ) and
     ( not paeFirstName.Dirty   ) and
     ( not paeInitials.Dirty    ) and
     ( not paeTitle.Dirty       ) and
     ( not paeNotes.Dirty ) then
    exit ;

  // Check for a valid lastName
  if SameText( paeLastName.Value, EmptyStr ) then begin
    MessageDlg( 'Please enter a last name',
                mtInformation,
                [mbOK], 0 ) ;
    paeLastName.SetFocus ;
    result := false ;
    exit ; //==>
  end ;

end;

// TtiListViewPlus on filter event to filter deleted e-addresses
//------------------------------------------------------------------------------
procedure TFormEditPerson.LVEAddressFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not ( pData as TPerObjAbs ).Deleted ;
end ;

procedure TFormEditPerson.paeLastNameChange(Sender: TObject);
begin
  FData.Dirty := true ;
  TreeNode.Text := FData.Caption ;
end;

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
    FData.EAddressList.Add( lData ) ;
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
    FData.AddressList.Add( lData ) ;
    LVAddress.Refresh ;
  end else
    lData.Free ;
end;

end.

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
unit FormPerson;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Adrs_BOM, ComCtrls, tiListView, StdCtrls, tiSplitter, ExtCtrls, Grids,
  DBGrids, tiPtnVisPerObj, tiDialogs, DBCtrls, Mask, Db, FormAddress,
  FormContact, tiDataset;


type
  TFormPerson = class(TForm)
    GroupBox1: TGroupBox;
    tbPeople: TTiRecordDataset;
    dsPeople: TDataSource;
    Label3: TLabel;
    EditLastName: TDBEdit;
    Label4: TLabel;
    EditFirstName: TDBEdit;
    Label5: TLabel;
    EditInitials: TDBEdit;
    Label6: TLabel;
    EditTitle: TDBComboBox;
    dsEAdrList: TDataSource;
    dsAdrList: TDataSource;
    tiSplitterPanel2: TtiSplitterPanel;
    Panel2: TPanel;
    Label7: TLabel;
    tiSplitterPanel1: TtiSplitterPanel;
    GroupBox2: TGroupBox;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    GroupBox3: TGroupBox;
    DBGrid2: TDBGrid;
    DBNavigator2: TDBNavigator;
    DBMemo1: TDBMemo;
    tbContact: TTiNestedDataset;
    tbAddress: TTiNestedDataset;
    tbPeopleEAddressList: TDataSetField;
    tbPeopleAddressList: TDataSetField;
    tbPeopleCaption: TStringField;
    tbPeopleNotes: TStringField;
    tbPeopleLastName: TStringField;
    tbPeopleFirstName: TStringField;
    tbPeopleTitle: TStringField;
    tbPeopleInitials: TStringField;
    procedure FormShow(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid2DblClick(Sender: TObject);
    procedure tbContactAfterEdit(DataSet: TDataSet);
    procedure tbAddressAfterEdit(DataSet: TDataSet);
    procedure tbPeopleAfterPost(DataSet: TDataSet);
  private
    FData: TPerObjAbs;
    FTreeNode: TTreeNode;
    procedure SetData(const Value: TPerObjAbs);
    function GetValid: boolean;
  published
    //These published properties are required by the TtiTreeViewPlus
    property Data: TPerObjAbs read FData write SetData;
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
    property Valid: boolean read GetValid;
  public

  end;

implementation


{$R *.DFM}

{ TFormEditPerson }

procedure TFormPerson.FormShow(Sender: TObject);
begin
  EditLastName.SetFocus;
end;

procedure TFormPerson.SetData(const Value: TPerObjAbs);
begin
  FData := Value;
  tbPeople.Close;
  tbPeople.oRecord := Value;
  tbPeople.Active := Assigned(Value);
end;

function TFormPerson.GetValid: boolean;
begin
  Result := true;
  if FData = nil
  then exit;

  if tbPeople.State in dsEditModes
  then tbPeople.Post;

  // Has there been a change made ?
  if not FData.Dirty
  then exit;

  // Check for a valid lastName
  if SameText(EditLastName.Text, EmptyStr) then begin
    MessageDlg('Please enter a last name', mtInformation,[mbOK], 0);
    EditLastName.SetFocus;
    Result := false;
  end;
end;

procedure TFormPerson.DBGrid1DblClick(Sender: TObject);
begin
  tbContact.Edit;
end;

procedure TFormPerson.DBGrid2DblClick(Sender: TObject);
begin
  tbAddress.Edit;
end;

procedure TFormPerson.tbPeopleAfterPost(DataSet: TDataSet);
begin
  TreeNode.Text := FData.Caption;
end;

procedure TFormPerson.tbContactAfterEdit(DataSet: TDataSet);
begin
  with TFormContact.Create(Self)do try
    SetData(tbContact.GetActiveItem);
    if Showmodal = mrOk
    then tbContact.Post
    else tbContact.Cancel;
  finally
    Free;
  end;
end;

procedure TFormPerson.tbAddressAfterEdit(DataSet: TDataSet);
begin
  with TFormAddress.Create(Self)do try
    SetData(tbAddress.GetActiveItem);
    if Showmodal = mrOk
    then tbAddress.Post
    else tbAddress.Cancel;
  finally
    Free;
  end;
end;

end.


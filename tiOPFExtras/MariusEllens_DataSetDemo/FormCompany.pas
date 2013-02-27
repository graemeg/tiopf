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
unit FormCompany;

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, tiSplitter,
  tiPtnVisPerObj, DBCtrls, Db, FormAddress,FormContact, tiDataset, Grids,
  DBGrids, ExtCtrls, Mask;

type
  TFormCompany = class(TForm)
    gbName: TGroupBox;
    EditCompanyName: TDBEdit;
    tiSplitterPanel1: TtiSplitterPanel;
    tiSplitterPanel2: TtiSplitterPanel;
    Label1: TLabel;
    lvContact: TDBGrid;
    Label2: TLabel;
    lvAddress: TDBGrid;
    paeNotes: TDBEdit;
    tbCompany: TTiRecordDataset;
    dsCompany: TDataSource;
    tbAddress: TTiNestedDataset;
    tbContact: TTiNestedDataset;
    dsContact: TDataSource;
    dsAddress: TDataSource;
    Label3: TLabel;
    DBNavigator2: TDBNavigator;
    tbCompanyCaption: TStringField;
    tbCompanyEAddressList: TDataSetField;
    tbCompanyAddressList: TDataSetField;
    tbCompanyNotes: TStringField;
    tbCompanyCompanyName: TStringField;
    tbCompanyPeople: TDataSetField;
    DBNavigator1: TDBNavigator;
    procedure lvContactDblClick(Sender: TObject);
    procedure lvAddressDblClick(Sender: TObject);
    procedure tbContactAfterEdit(DataSet: TDataSet);
    procedure tbAddressAfterEdit(DataSet: TDataSet);
    procedure tbCompanyAfterPost(DataSet: TDataSet);
  private
    FData: TPerObjAbs;
    FTreeNode: TTreeNode;
    procedure SetData(const Value: TPerObjAbs);
    function GetValid: boolean;
  published
    //These published properties are required by the TtiTreeViewPlus
    property Valid: boolean read GetValid;
    property Data: TPerObjAbs read FData write SetData;
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
  end;

implementation

{$R *.DFM}

{ TFormEditCompany }

function TFormCompany.GetValid: boolean;
begin
  Result := true;
  if FData = nil
  then exit; //==>

  if tbCompany.State in dsEditModes
  then tbCompany.Post;

  //Has there been a change made ?
  if not FData.Dirty
  then exit;
  
  //Check for a valid lastName
  if SameText(EditCompanyName.Text , EmptyStr) then begin
    MessageDlg('Please enter a company name',mtInformation,[mbOK], 0);
    EditCompanyName.SetFocus;
    Result := false;
  end;
end;

procedure TFormCompany.SetData(const Value: TPerObjAbs);
begin
  FData := Value;
  tbCompany.Close;
  tbCompany.oRecord := Value;
  tbCompany.Active := Assigned(Value);
end;


procedure TFormCompany.lvContactDblClick(Sender: TObject);
begin
  tbContact.Edit;
end;

procedure TFormCompany.lvAddressDblClick(Sender: TObject);
begin
  tbAddress.Edit;
end;

procedure TFormCompany.tbCompanyAfterPost(DataSet: TDataSet);
begin
  TreeNode.Text := FData.Caption;
end;

procedure TFormCompany.tbContactAfterEdit(DataSet: TDataSet);
begin
  with TFormContact.Create(Self) do try
    SetData(tbContact.GetActiveItem);
    if ShowModal = mrOk
    then tbContact.Post
    else tbContact.Cancel;
  finally
    Free;
  end;
end;

procedure TFormCompany.tbAddressAfterEdit(DataSet: TDataSet);
begin
  with TFormAddress.Create(Self) do try
    SetData(tbAddress.GetActiveItem);
    if ShowModal = mrOk
    then tbAddress.Post
    else tbAddress.Cancel;
  finally
    Free;
  end;
end;


end.

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

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiReadOnly, tiListView, tiPerAwareCtrls, ExtCtrls, ComCtrls,
  Contnrs, tiTreeView, tiFocusPanel ;

type

  TPerson = class( TPersistent )
  private
    FName: string;
    FList : TObjectList ;
    function GetList : TList ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
  published
    property Name : string read FName write FName ;
    property List : TList read GetList ;
  end ;

  TAdrs = class( TPersistent )
  private
    FAddress: string;
  published
    property Address : string read FAddress write FAddress ;
  end ;

  TFormMain = class(TForm)
    RO: TtiReadOnly;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Edit1: TEdit;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    tiPerAwareEdit1: TtiPerAwareEdit;
    tiPerAwareMemo1: TtiPerAwareMemo;
    tiPerAwareComboBoxStatic1: TtiPerAwareComboBoxStatic;
    tiPerAwareDateTimePicker1: TtiPerAwareDateTimePicker;
    tiPerAwareCheckBox1: TtiPerAwareCheckBox;
    tiPerAwareFloatEdit1: TtiPerAwareFloatEdit;
    tiListView1: TtiListView;
    cbReadOnly: TCheckBox;
    tiTreeView1: TtiTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tiListView1ItemEdit(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure tiListView1ItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure tiListView1ItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure tiTVMappingAdrsOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingAdrsOnEdit(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingAdrsOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
  private
    FData : TObjectList ;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  typinfo
  ;

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
var
  i, j : integer ;
  lPerson : TPerson ;
  lAdrs   : TAdrs ;
begin
  FData := TObjectList.Create ;
  for i := 0 to 9 do
  begin
    lPerson := TPerson.Create ;
    lPerson.Name := 'Person number: ' + IntToStr( Trunc( Random * 100 )) ;
    FData.Add( lPerson ) ;
    for j := 0 to 9 do
    begin
      lAdrs := TAdrs.Create ;
      lAdrs.Address := 'Address number: ' + IntToStr( Trunc( Random * 100 )) ;
      lPerson.List.Add( lAdrs ) ;
    end ;
  end ;
  tiListView1.Data := FData ;
  tiTreeView1.Data := lPerson ;

  ListBox1.ItemIndex := 1 ;
  ComboBox1.ItemIndex := 1 ;
  tiPerAwareComboBoxStatic1.Value := 'Two' ;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  tiListView1.Data := nil ;
  FData.Free ;
end;

procedure TFormMain.tiListView1ItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  ShowMessage( 'Edit' ) ;
end;

procedure TFormMain.tiListView1ItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  ShowMessage( 'Insert' ) ;
end;

procedure TFormMain.tiListView1ItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  ShowMessage( 'Delete' ) ;
end;

procedure TFormMain.cbReadOnlyClick(Sender: TObject);
begin
  RO.ReadOnly := cbReadOnly.Checked ;
end;

{ TPerson }

constructor TPerson.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

destructor TPerson.Destroy;
begin
  FList.Free ;
  inherited;
end;

function TPerson.GetList: TList;
begin
  result := FList ;
end;

procedure TFormMain.tiTVMappingAdrsOnDelete(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  ShowMessage( 'Delete' ) ;
end;

procedure TFormMain.tiTVMappingAdrsOnEdit(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  ShowMessage( 'Edit' ) ;
end;

procedure TFormMain.tiTVMappingAdrsOnInsert(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  ShowMessage( 'Insert' ) ;
end;

end.

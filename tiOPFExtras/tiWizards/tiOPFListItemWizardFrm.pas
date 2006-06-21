{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: The main form for an Open Tools API Wizard for Delphi to assist
  in the creation of a unit defining a TPerObjList which owns TPerObjAbs items

  Revision History:
    March 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFListItemWizardFrm;

interface

uses
  Windows, Messages,
  {$IFDEF VER140}
  Variants,
  {$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList, ToolsAPI, ComCtrls,
  Grids, tiOPFProperty;

const
  PROPVIS   = 0;
  PROPNAME  = 1;
  PROPTYPE  = 2;
  PROPREAD  = 3;
  PROPWRITE = 4;

type
  TfrmWizard = class(TForm)
    pagWizard: TPageControl;
    ActionList1: TActionList;
    actGenerate: TAction;
    actGenOneFile: TAction;
    Image1: TImage;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnPrevious: TBitBtn;
    btnNext: TBitBtn;
    tabClasses: TTabSheet;
    gbListOwner: TGroupBox;
    Label4: TLabel;
    edListOwner: TEdit;
    Label5: TLabel;
    rbListOwnerThisUnit: TRadioButton;
    rbListOwnerOtherUnit: TRadioButton;
    edListOwnerUnit: TEdit;
    Label3: TLabel;
    gbList: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edList: TEdit;
    rbListThisUnit: TRadioButton;
    rbListOtherUnit: TRadioButton;
    edListUnit: TEdit;
    gbItem: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edItem: TEdit;
    rbItemThisUnit: TRadioButton;
    rbItemOtherUnit: TRadioButton;
    edItemUnit: TEdit;
    Label12: TLabel;
    edListOwnerCaption: TEdit;
    tabProperties: TTabSheet;
    btnItemPropertyAdd: TButton;
    lblItemClassProperties: TLabel;
    btnItemPropertyEdit: TButton;
    btnItemPropertyRemove: TButton;
    btnItemPropertyClear: TButton;
    sgItemProperties: TStringGrid;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure actGenerateExecute(Sender: TObject);
    procedure actGenerateUpdate(Sender: TObject);
    procedure rbListOwnerThisUnitClick(Sender: TObject);
    procedure rbListOwnerOtherUnitClick(Sender: TObject);
    procedure rbListThisUnitClick(Sender: TObject);
    procedure rbListOtherUnitClick(Sender: TObject);
    procedure rbItemThisUnitClick(Sender: TObject);
    procedure rbItemOtherUnitClick(Sender: TObject);
    procedure btnItemPropertyAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tabPropertiesShow(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnItemPropertyEditClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnItemPropertyRemoveClick(Sender: TObject);
    procedure btnItemPropertyClearClick(Sender: TObject);
  private
    procedure DisplayPropertyGrid;
    procedure SetNavButtons;
  public
    { Public declarations }
    ItemProperties: TProperties;
  end;

implementation

{$R *.dfm}

uses
  tiOPFWizardsGlobal, tiOPFPropertyFrm;

{ TfrmInvWiz }

procedure TfrmWizard.actGenerateExecute(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmWizard.actGenerateUpdate(Sender: TObject);
begin
  actGenerate.Enabled := True;
end;

procedure TfrmWizard.rbListOwnerThisUnitClick(Sender: TObject);
begin
  if edListOwnerCaption.Text = '' then
    edListOwnerCaption.Text := edListOwner.Text;
  edListOwnerUnit.Text := '';
  edListOwnerCaption.Enabled := True;
  edListOwnerUnit.Enabled := False;
  edListOwnerCaption.SetFocus;
end;

procedure TfrmWizard.rbListOwnerOtherUnitClick(Sender: TObject);
begin
  if edListOwnerUnit.Text = '' then
    edListOwnerUnit.Text := edListOwner.Text;
  edListOwnerCaption.Text := '';
  edListOwnerCaption.Enabled := False;
  edListOwnerUnit.Enabled := True;
  edListOwnerUnit.SetFocus;
end;

procedure TfrmWizard.rbListThisUnitClick(Sender: TObject);
begin
  edListUnit.Text := '';
  edListUnit.Enabled := False;
end;

procedure TfrmWizard.rbListOtherUnitClick(Sender: TObject);
begin
  if edListUnit.Text = '' then
    edListUnit.Text := edList.Text;
  edListUnit.Enabled := True;
  edListUnit.SetFocus;
end;

procedure TfrmWizard.rbItemThisUnitClick(Sender: TObject);
begin
  edItemUnit.Text := '';
  edItemUnit.Enabled := False;
end;

procedure TfrmWizard.rbItemOtherUnitClick(Sender: TObject);
begin
  if edItemUnit.Text = '' then
    edItemUnit.Text := edItem.Text;
  edItemUnit.Enabled := True;
  edItemUnit.SetFocus;
end;

procedure TfrmWizard.btnItemPropertyAddClick(Sender: TObject);
var
  PropertyForm: TFrmProperty;
  TempProps: TProperties;
  NewProperty: TProperty;
begin
  // Create a temporary collection
  TempProps := TProperties.Create(TProperty);
  try
    // Instantiate a Item Property to play with in the temporary collection
    NewProperty := TProperty.Create(TempProps);
    try
      PropertyForm := TfrmProperty.Create(nil, NewProperty);
      try
        // Get the user's requirements for the new property
        if PropertyForm.ShowModal = mrOK then
        begin
          // Add this to the real list
          ItemProperties.Add(NewProperty);
          DisplayPropertyGrid;
        end;
      finally
        PropertyForm.Free;
      end;
    finally
      NewProperty.Free;
    end;
  finally
    TempProps.Free;
  end;
end;

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  ItemProperties := TProperties.Create(TProperty);
  pagWizard.ActivePage := tabClasses;
  // Set up the Item Property grid
  sgItemProperties.Cols[PROPNAME].Text := 'Property Name';
  sgItemProperties.ColWidths[PROPNAME] := 120;
  sgItemProperties.Cols[PROPVIS].Text := 'Visibility';
  sgItemProperties.ColWidths[PROPVIS] := 80;
  sgItemProperties.Cols[PROPTYPE].Text := 'Type';
  sgItemProperties.ColWidths[PROPTYPE] := 80;
  sgItemProperties.Cols[PROPREAD].Text := 'Read Access';
  sgItemProperties.ColWidths[PROPREAD] := 90;
  sgItemProperties.Cols[PROPWRITE].Text := 'Write Access';
  sgItemProperties.ColWidths[PROPWRITE] := 90;
end;

procedure TfrmWizard.tabPropertiesShow(Sender: TObject);
begin
  lblItemClassProperties.Caption := 'T' + edItem.Text + ' Properties';
end;

procedure TfrmWizard.btnNextClick(Sender: TObject);
begin
  if pagWizard.ActivePageIndex < pagWizard.PageCount-1 then
    pagWizard.ActivePageIndex := pagWizard.ActivePageIndex + 1;
  SetNavButtons; // pagWizard OnChange not firing for some reason
end;

procedure TfrmWizard.btnPreviousClick(Sender: TObject);
begin
  if pagWizard.ActivePageIndex > 0 then
    pagWizard.ActivePageIndex := pagWizard.ActivePageIndex - 1;
  SetNavButtons; // pagWizard OnChange not firing for some reason
end;

procedure TfrmWizard.SetNavButtons;
begin
  btnNext.Enabled := not (pagWizard.ActivePage = tabProperties);
  btnPrevious.Enabled := not (pagWizard.ActivePage = tabClasses);
end;

procedure TfrmWizard.btnItemPropertyEditClick(Sender: TObject);
var
  PropertyForm: TfrmProperty;
  OldProperty, NewProperty: TProperty;
  TempProps: TProperties;
begin
  TempProps := TProperties.Create(TProperty);
  try
    OldProperty := ItemProperties.FindByName(sgItemProperties.Cells[PROPNAME, sgItemProperties.Row]);
    if OldProperty <> nil then
    begin
      NewProperty := TempProps.Add(OldProperty);
      PropertyForm := TfrmProperty.Create(nil, NewProperty);
      try
        if PropertyForm.ShowModal = mrOK then
        begin
          ItemProperties.DeleteByName(OldProperty.Name);
          ItemProperties.Add(NewProperty);
          DisplayPropertyGrid;
        end;
      finally
        PropertyForm.Free;
      end;
    end;
  finally
    TempProps.Free;
  end;
end;

procedure TfrmWizard.FormDestroy(Sender: TObject);
begin
  ItemProperties.Free;
end;

procedure TfrmWizard.btnItemPropertyRemoveClick(Sender: TObject);
begin
  if MessageDlg('Do you want to delete property "' + sgItemProperties.Cells[PROPNAME, sgItemProperties.Row] + '"?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then
  begin
    ItemProperties.DeleteByName(sgItemProperties.Cells[sgItemProperties.Col, sgItemProperties.Row]);
    DisplayPropertyGrid;
  end;
end;


procedure TfrmWizard.DisplayPropertyGrid;
var
  i: Integer;
begin
  // Simply redisplays the grid from scratch
  // Not the most optimised way of doing it...

  // Get rid of the current values
  for i := 1 to sgItemProperties.RowCount - 1 do
    sgItemProperties.Rows[i].Clear;
  sgItemProperties.RowCount := 2; // Minimum rows we can have

  // Display the rows
  for i := 0 to ItemProperties.Count-1 do
  begin
    if i > 0 then
      sgItemProperties.RowCount := sgItemProperties.RowCount+1;
    sgItemProperties.Cells[PROPNAME, i+1] := TProperty(ItemProperties.Items[i]).Name;
    sgItemProperties.Cells[PROPVIS, i+1] := TProperty(ItemProperties.Items[i]).VisibilityAsString;
    sgItemProperties.Cells[PROPTYPE, i+1] := TProperty(ItemProperties.Items[i]).DataType;
    sgItemProperties.Cells[PROPREAD, i+1] := TProperty(ItemProperties.Items[i]).ReadAccessAsString;
    sgItemProperties.Cells[PROPWRITE, i+1] := TProperty(ItemProperties.Items[i]).WriteAccessAsString;
  end;

  // Set the grid and button visibilities according to whether we have properties
  sgItemProperties.Visible := ItemProperties.Count > 0;
  btnItemPropertyEdit.Enabled := ItemProperties.Count > 0;
  btnItemPropertyRemove.Enabled := ItemProperties.Count > 0;
  btnItemPropertyClear.Enabled := ItemProperties.Count > 0;
end;

procedure TfrmWizard.btnItemPropertyClearClick(Sender: TObject);
begin
  ItemProperties.Clear;
  DisplayPropertyGrid;
end;

end.


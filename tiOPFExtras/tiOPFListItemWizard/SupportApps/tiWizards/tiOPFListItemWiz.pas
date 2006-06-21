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

  Purpose: An Open Tools API Wizard for Delphi to assist in the creation
  of a unit defining a TPerObjList which owns TPerObjAbs items

  Revision History:
    March 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFListItemWiz;

interface

uses
  ToolsAPI
  , Windows
  , classes
  , ActnList
  , ComCtrls
  , tiOPFWizard
  , tiOPFListItemWizardFrm
  ;

type
  TtiOPFListItemWizard = class(TtiOPFWizard)
  private
    procedure Execute;
  public
    constructor Create;
  end;

  procedure Register;

implementation

{$R tiOPFListItemWizard.dcr}

uses
  SysUtils, Controls, Graphics, INIFiles
  , tiOPFUnitStructure, tiOPFPlaceholders, Dialogs;

procedure Register;
begin
  RegisterPackageWizard(TtiOPFListItemWizard.Create as IOTAWizard);
end;

{ TtiOPFListItemWizard }

constructor TtiOPFListItemWizard.Create;
begin
  inherited;
  Name := 'tiOPF List-Item Wizard';
  Author := 'Chris Latta';
  IDString := 'tiOPF.ListItemWizard';
  Comment := 'tiOPF List-Item Class Generation Wizard';
  UnitTemplate := 'C:\TechInsite\SupportApps\tiWizards\tiOPFListItem.txt';
  GlyphResourceName := 'TIOPFLISTITEMWIZ';
  Toolbutton.ResourceName := 'TIOPFLISTITEM';
  Toolbutton.TransparentColor := clFuchsia;
end;

procedure TtiOPFListItemWizard.Execute;
var
  WizardForm: TfrmWizard;
  NewUnit: TUnitStructure;

  function AddPlaceholder(SearchText, ReplaceText: string): TPlaceholder;
  begin
    Result := NewUnit.PlaceHolders.Add('%' + SearchText + '%', ReplaceText);
  end;

begin
  WizardForm := TfrmWizard.Create(nil);
  try
    if WizardForm.ShowModal = mrOK then
    begin
      NewUnit := TUnitStructure.Create;
      try
        AddPlaceholder('ListOwner', WizardForm.edListOwner.Text);
        AddPlaceholder('ListOwnerUnit', WizardForm.edListOwnerUnit.Text);
        AddPlaceholder('ListOwnerCaption', WizardForm.edListOwnerCaption.Text);
        AddPlaceholder('List', WizardForm.edList.Text);
        AddPlaceholder('ListUnit', WizardForm.edListUnit.Text);
        with AddPlaceholder('Item', WizardForm.edItem.Text) do
        begin
          Properties.Clone(WizardForm.ItemProperties);
          PlaceholderType := ptClass
        end;
        AddPlaceholder('ItemUnit', WizardForm.edItemUnit.Text);
        CreateUnit(NewUnit);
      finally
        NewUnit.Free;
      end;
    end;
  finally
    WizardForm.Free;
  end;
end;

end.


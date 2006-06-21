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
  of a form to display the currently selected item in a TtiTreeView

  Revision History:
    April 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFTreeViewFormWizardClass;

interface

uses
  ToolsAPI
  , Windows
  , tiOPFUnitStructure
  , tiOPFTreeViewFormWizardFrm
  ;

const
  cgPathToFormTemplate = 'C:\TechInsite\SupportApps\tiWizards\tiOPFTreeViewFormDFM.txt';
  cgPathToUnitTemplate = 'C:\TechInsite\SupportApps\tiWizards\tiOPFTreeViewFormPAS.txt';

type
  TtiOPFTreeViewFormWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard,
                               IOTARepositoryWizard, IOTAFormWizard)
  private
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  private
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  private
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    {$IFDEF VER140}
    function GetGlyph: cardinal;
    {$ELSE}
    function GetGlyph: HICON;
    {$ENDIF}
    procedure CreateUnit(UnitStructure: TUnitStructure);
  end;

  TtiOPFTreeViewFormUnitCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  private
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  private
    FUnitStructure: TUnitStructure;
  public
    property AncestorName: string read GetAncestorName;
    property UnitStructure: TUnitStructure read FUnitStructure;
    constructor Create(UnitStructure: TUnitStructure);
  end;

  TtiOPFTreeViewFormDFM = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TtiOPFTreeViewFormUnitCreator;
  private
    // IOTAFile
    function GetSource: string; virtual;
    function GetAge: TDateTime;
  public
    constructor Create(const ACreator: TtiOPFTreeViewFormUnitCreator);
    property Creator: TtiOPFTreeViewFormUnitCreator read FCreator;
  end;

  TtiOPFTreeViewFormPAS = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TtiOPFTreeViewFormUnitCreator;
  private
    // IOTAFile
    function GetSource: string; virtual;
    function GetAge: TDateTime;
  public
    constructor Create(const ACreator: TtiOPFTreeViewFormUnitCreator);
    property Creator: TtiOPFTreeViewFormUnitCreator read FCreator;
  end;

  procedure Register;

implementation

{$R tiOPFTreeViewFormWizard.dcr}

uses
  SysUtils, Classes, Controls, Dialogs
  , tiOPFWizardsGlobal, tiOPFPlaceholders;

procedure Register;
begin
  RegisterPackageWizard(TtiOPFTreeViewFormWizard.Create as IOTAWizard);
end;

{ TtiOPFTreeViewFormWizard }

procedure TtiOPFTreeViewFormWizard.AfterSave;
begin
  // Do nothing.
end;

procedure TtiOPFTreeViewFormWizard.BeforeSave;
begin
  // Do nothing.
end;

procedure TtiOPFTreeViewFormWizard.CreateUnit;
begin
{$IFDEF VER140}
  with BorlandIdeServices as IOTAModuleServices do
    CreateModule(TtiOPFTreeViewFormUnitCreator.Create(UnitStructure)).MarkModified;
{$ELSE}
 // Not sure what this should be in D5
  with BorlandIdeServices as IOTAModuleServices do
    CreateModule(TtiOPFTreeViewFormUnitCreator.Create(UnitStructure));
{$ENDIF}
end;

procedure TtiOPFTreeViewFormWizard.Destroyed;
begin
  // Do nothing.
end;

procedure TtiOPFTreeViewFormWizard.Execute;
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
        AddPlaceholder('Form', WizardForm.edFormName.Text);
        AddPlaceholder('FormCaption', WizardForm.edFormCaption.Text);
        AddPlaceholder('TreeNode', WizardForm.edTreeNode.Text);
        AddPlaceholder('TreeNodeUnit', WizardForm.edTreeNodeUnit.Text);

        CreateUnit(NewUnit);
      finally
        NewUnit.Free;
      end;
    end;
  finally
    WizardForm.Free;
  end;
end;

function TtiOPFTreeViewFormWizard.GetAuthor: string;
begin
  Result := 'Chris Latta';
end;

function TtiOPFTreeViewFormWizard.GetComment: string;
begin
  Result := 'tiOPF TreeView Form Generation Wizard';
end;

{$IFDEF VER140}
function TtiOPFTreeViewFormWizard.GetGlyph: cardinal;
begin
  Result := LoadIcon(HINSTANCE, 'TIOPFTREEVIEWFORMWIZ');
end;
{$ELSE}
function TtiOPFTreeViewFormWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(HINSTANCE, 'TIOPFTREEVIEWFORMWIZ');
end;
{$ENDIF}

function TtiOPFTreeViewFormWizard.GetIDString: string;
begin
  Result := 'tiOPF.TreeViewFormWizard';
end;

function TtiOPFTreeViewFormWizard.GetName: string;
begin
  Result := 'tiOPF TreeView Form Wizard';
end;

function TtiOPFTreeViewFormWizard.GetPage: string;
begin
  Result := 'TechInsite';
end;

function TtiOPFTreeViewFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TtiOPFTreeViewFormWizard.Modified;
begin
  // Do nothing.
end;

{ TtiOPFTreeViewFormUnitCreator }

procedure TtiOPFTreeViewFormUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Do nothing.
end;

function TtiOPFTreeViewFormUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TtiOPFTreeViewFormUnitCreator.GetCreatorType: string;
begin
  Result := 'sForm';
end;

function TtiOPFTreeViewFormUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TtiOPFTreeViewFormUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TtiOPFTreeViewFormUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TtiOPFTreeViewFormUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TtiOPFTreeViewFormUnitCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProject;
end;

function TtiOPFTreeViewFormUnitCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TtiOPFTreeViewFormUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TtiOPFTreeViewFormUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TtiOPFTreeViewFormUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TtiOPFTreeViewFormDFM.Create(Self);
end;

function TtiOPFTreeViewFormUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TtiOPFTreeViewFormUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TtiOPFTreeViewFormPAS.Create(Self);
end;

function TtiOPFTreeViewFormUnitCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TtiOPFTreeViewFormUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

constructor TtiOPFTreeViewFormUnitCreator.Create(
  UnitStructure: TUnitStructure);
begin
  inherited Create;
  FUnitStructure := UnitStructure;
end;

{ TtiOPFTreeViewFormDFM }

constructor TtiOPFTreeViewFormDFM.Create(
  const ACreator: TtiOPFTreeViewFormUnitCreator);
begin
  inherited Create;
  FCreator := ACreator;
end;

function TtiOPFTreeViewFormDFM.GetAge: TDateTime;
begin
  Result := -1;
end;

function TtiOPFTreeViewFormDFM.GetSource: string;
begin
  Result := Creator.UnitStructure.ProcessTemplate(cgPathToFormTemplate);
end;

{ TtiOPFTreeViewFormPAS }

constructor TtiOPFTreeViewFormPAS.Create
  (const ACreator: TtiOPFTreeViewFormUnitCreator);
begin
  inherited Create;
  FCreator := ACreator;
end;

function TtiOPFTreeViewFormPAS.GetAge: TDateTime;
begin
  Result := -1;
end;

function TtiOPFTreeViewFormPAS.GetSource: string;
begin
  Result := Creator.UnitStructure.ProcessTemplate(cgPathToUnitTemplate);
end;

end.


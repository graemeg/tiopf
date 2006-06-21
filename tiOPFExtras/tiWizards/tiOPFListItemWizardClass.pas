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
unit tiOPFListItemWizardClass;

interface

uses
  ToolsAPI
  , Windows
  , classes
  , ActnList
  , ComCtrls
  , tiOPFUnitStructure
  , tiOPFListItemWizardFrm
  ;

const
  cgPathToTemplateFile = 'C:\TechInsite\SupportApps\tiWizards\tiOPFListItem.txt' ;

type
  TtiOPFListItemWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard,
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
  private
    // tiOPF List-Item Wizard declarations
    Action: TAction;     // The action of this wizard, for toolbuttion creation
    ImageIndex: Integer; // The image index for the toolbutton image
    procedure DoExecute(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemoveAction(Action: TAction; ToolBar: TToolBar);
  end;

  TtiOPFListItemUnitCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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

  TtiOPFListItemImplUnit = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TtiOPFListItemUnitCreator;
  private
    // IOTAFile
    function GetSource: string; virtual;
    function GetAge: TDateTime;
  public
    constructor Create(const ACreator: TtiOPFListItemUnitCreator);
    property Creator: TtiOPFListItemUnitCreator read FCreator;
  end;

  procedure Register;

implementation

{$R tiOPFListItemWizard.dcr}

uses
  SysUtils, Controls, Graphics, INIFiles
  , tiOPFWizardsGlobal, tiOPFPlaceholders, Dialogs;

procedure Register;
begin
  RegisterPackageWizard(TtiOPFListItemWizard.Create as IOTAWizard);
end;

{ TtiOPFListItemWizard }

procedure TtiOPFListItemWizard.AfterSave;
begin
  // Do nothing.
end;

procedure TtiOPFListItemWizard.BeforeSave;
begin
  // Do nothing.
end;

constructor TtiOPFListItemWizard.Create;
var
  Services: INTAServices;
  Bmp: TBitmap;
begin
  inherited;

  Supports(BorlandIDEServices, INTAServices, Services);
  // Add the tiOPF List-Item Wizard toolbutton image to the image list
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, 'TIOPFLISTITEM');
  ImageIndex := Services.AddMasked(Bmp, clFuchsia,
                                  'tiOPF.ListItemWizard');
  Bmp.Free;
  // Add the action so users can use a toolbutton for it
  Action := TAction.Create(nil);
  Action.ActionList := Services.ActionList;
  Action.Caption    := 'tiOPF List-Item Wizard';
  Action.Hint       := 'Run tiOPF List-Item Wizard';
  Action.ImageIndex := ImageIndex;
  Action.OnExecute  := DoExecute;
end;


procedure TtiOPFListItemWizard.RemoveAction(Action: TAction; ToolBar: TToolBar);
var
  I: Integer;
  Btn: TToolButton;
begin
  // Removes the toolbutton from the toolbar
  for I := ToolBar.ButtonCount - 1 downto 0 do
  begin
    Btn := ToolBar.Buttons[I];
    if Btn.Action = Action then
    begin
      { Remove "Btn" from "ToolBar" }
      ToolBar.Perform(CM_CONTROLCHANGE, WPARAM(Btn), 0);
      Btn.Free;
    end;
  end;
end;

destructor TtiOPFListItemWizard.Destroy;
var
  Services: INTAServices;
begin
  Supports(BorlandIDEServices, INTAServices, Services);
  // Check all the toolbars, and remove any buttons that use this action
  RemoveAction(Action, Services.ToolBar[sCustomToolBar]);
  RemoveAction(Action, Services.ToolBar[sDesktopToolBar]);
  RemoveAction(Action, Services.ToolBar[sStandardToolBar]);
  RemoveAction(Action, Services.ToolBar[sDebugToolBar]);
  RemoveAction(Action, Services.ToolBar[sViewToolBar]);
  RemoveAction(Action, Services.ToolBar['InternetToolBar']);

  Action.Free;

  inherited;
end;

procedure TtiOPFListItemWizard.CreateUnit;
begin
  {$IFDEF VER140}
    with BorlandIdeServices as IOTAModuleServices do
      CreateModule(TtiOPFListItemUnitCreator.Create(UnitStructure)).MarkModified;
  {$ELSE}
    // Not sure what this should be in D5
    with BorlandIdeServices as IOTAModuleServices do
      CreateModule(TtiOPFListItemUnitCreator.Create(UnitStructure));
  {$ENDIF}
end;

procedure TtiOPFListItemWizard.Destroyed;
begin
  // Do nothing.
end;

procedure TtiOPFListItemWizard.DoExecute(Sender: TObject);
begin
  Execute;
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

function TtiOPFListItemWizard.GetAuthor: string;
begin
  Result := 'Chris Latta';
end;

function TtiOPFListItemWizard.GetComment: string;
begin
  Result := 'tiOPF List-Item Class Generation Wizard';
end;


{$IFDEF VER140}
function TtiOPFListItemWizard.GetGlyph: cardinal;
begin
  Result := LoadIcon(HINSTANCE, 'TIOPFLISTITEMWIZ');
end;
{$ELSE}
function TtiOPFListItemWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(HINSTANCE, 'TIOPFLISTITEMWIZ');
end;
{$ENDIF}

function TtiOPFListItemWizard.GetIDString: string;
begin
  Result := 'tiOPF.ListItemWizard';
end;

function TtiOPFListItemWizard.GetName: string;
begin
  Result := 'tiOPF List-Item Wizard';
end;

function TtiOPFListItemWizard.GetPage: string;
begin
  Result := 'TechInsite';
end;

function TtiOPFListItemWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TtiOPFListItemWizard.Modified;
begin
  // Do nothing.
end;

{ TtiOPFListItemUnitCreator }

procedure TtiOPFListItemUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Do nothing.
end;

function TtiOPFListItemUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TtiOPFListItemUnitCreator.GetCreatorType: string;
begin
  Result := 'sUnit';
end;

function TtiOPFListItemUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TtiOPFListItemUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TtiOPFListItemUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TtiOPFListItemUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TtiOPFListItemUnitCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProject;
end;

function TtiOPFListItemUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TtiOPFListItemUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TtiOPFListItemUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TtiOPFListItemUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TtiOPFListItemUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TtiOPFListItemUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TtiOPFListItemImplUnit.Create(Self);;
end;

function TtiOPFListItemUnitCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TtiOPFListItemUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

constructor TtiOPFListItemUnitCreator.Create(
  UnitStructure: TUnitStructure);
begin
  inherited Create;
  FUnitStructure := UnitStructure;
end;

{ TtiOPFListItemImplUnit }

constructor TtiOPFListItemImplUnit.Create(const ACreator: TtiOPFListItemUnitCreator);
begin
  inherited Create;
  FCreator := ACreator;
end;

function TtiOPFListItemImplUnit.GetAge: TDateTime;
begin
  Result := -1;
end;

function TtiOPFListItemImplUnit.GetSource: string;
begin
  Result := Creator.UnitStructure.ProcessTemplate(cgPathToTemplateFile);
end;

end.


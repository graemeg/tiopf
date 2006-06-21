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

  Purpose: An ancestor Wizard class for the tiOPF to assist in the creation
  of other wizards.

  Revision History:
    May 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFWizard;

interface

uses
  ToolsAPI
  , Windows
  , classes
  , ActnList
  , ComCtrls
  , Graphics
  , tiOPFUnitStructure
  ;

type
  TtiOPFWizardToolbutton = class
  public
    ResourceName: string;
    TransparentColor: TColor;
  end;

  TtiOPFWizard = class(TInterfacedObject, IOTANotifier, IOTAWizard,
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
    procedure Execute; virtual; abstract;
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
  private
    // tiOPF Wizard declarations
    Action: TAction;     // The action of this wizard, for toolbuttion creation
    ImageIndex: Integer; // The image index for the toolbutton image
    procedure DoExecute(Sender: TObject);
    procedure RemoveAction(Action: TAction; ToolBar: TToolBar);
  protected
    procedure CreateUnit(UnitStructure: TUnitStructure);
  public
    Author: string;
    Comment: string;
    FormTemplate: string;
    GlyphResourceName: PChar;
    IDString: string;
    Name: string;
    Toolbutton: TtiOPFWizardToolbutton;
    UnitTemplate: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TtiOPFUnitCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
    FFormTemplate, FUnitTemplate: string;
  public
    property AncestorName: string read GetAncestorName;
    property UnitStructure: TUnitStructure read FUnitStructure;
    constructor Create(UnitStructure: TUnitStructure; FormTemplate, UnitTemplate: string);
  end;

  TtiOPFImplUnit = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TtiOPFUnitCreator;
  private
    // IOTAFile
    function GetSource: string; virtual;
    function GetAge: TDateTime;
  public
    FTemplateFile: string;
    constructor Create(const ACreator: TtiOPFUnitCreator; TemplateFile: string);
    property Creator: TtiOPFUnitCreator read FCreator;
  end;

implementation

uses
  SysUtils, Controls, INIFiles
  , tiOPFWizardsGlobal, tiOPFPlaceholders, Dialogs;

{ TtiOPFWizard }

procedure TtiOPFWizard.AfterSave;
begin
  // Do nothing.
end;

procedure TtiOPFWizard.BeforeSave;
begin
  // Do nothing.
end;

constructor TtiOPFWizard.Create;
var
  Services: INTAServices;
  Bmp: TBitmap;
begin
  inherited;

  Toolbutton := TtiOPFWizardToolbutton.Create;

  Supports(BorlandIDEServices, INTAServices, Services);
  // Add this tiOPF Wizard toolbutton image to the image list
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, Toolbutton.ResourceName);
  ImageIndex := Services.AddMasked(Bmp, Toolbutton.TransparentColor, IDString);
  Bmp.Free;
  // Add the action so users can use a toolbutton for it
  Action := TAction.Create(nil);
  Action.ActionList := Services.ActionList;
  Action.Caption    := Name;
  Action.Hint       := Comment;
  Action.ImageIndex := ImageIndex;
  Action.OnExecute  := DoExecute;
end;


procedure TtiOPFWizard.RemoveAction(Action: TAction; ToolBar: TToolBar);
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

destructor TtiOPFWizard.Destroy;
var
  Services: INTAServices;
begin
  Toolbutton.Free;

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

procedure TtiOPFWizard.CreateUnit;
begin
  {$IFDEF VER140}
    with BorlandIdeServices as IOTAModuleServices do
      CreateModule(TtiOPFUnitCreator.Create(UnitStructure, FormTemplate, UnitTemplate)).MarkModified;
  {$ELSE}
    // Not sure what this should be in D5
    with BorlandIdeServices as IOTAModuleServices do
      CreateModule(TtiOPFUnitCreator.Create(UnitStructure, FormTemplate, UnitTemplate));
  {$ENDIF}
end;

procedure TtiOPFWizard.Destroyed;
begin
  // Do nothing.
end;

procedure TtiOPFWizard.DoExecute(Sender: TObject);
begin
  Execute;
end;

function TtiOPFWizard.GetAuthor: string;
begin
  Result := Author;
end;

function TtiOPFWizard.GetComment: string;
begin
  Result := Comment;
end;

{$IFDEF VER140}
function TtiOPFWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HINSTANCE, GlyphResourceName);
end;
{$ELSE}
function TtiOPFWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(HINSTANCE, GlyphResourceName);
end;
{$ENDIF}

function TtiOPFWizard.GetIDString: string;
begin
  Result := IDString;
end;

function TtiOPFWizard.GetName: string;
begin
  Result := Name;
end;

function TtiOPFWizard.GetPage: string;
begin
  Result := 'TechInsite';
end;

function TtiOPFWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TtiOPFWizard.Modified;
begin
  // Do nothing.
end;

{ TtiOPFUnitCreator }

procedure TtiOPFUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Do nothing.
end;

function TtiOPFUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TtiOPFUnitCreator.GetCreatorType: string;
begin
  Result := 'sUnit';
end;

function TtiOPFUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TtiOPFUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TtiOPFUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TtiOPFUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TtiOPFUnitCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProject;
end;

function TtiOPFUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TtiOPFUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TtiOPFUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TtiOPFUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string):
    IOTAFile;
begin
  Result := nil;
end;

function TtiOPFUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
    AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TtiOPFUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
    AncestorIdent: string): IOTAFile;
begin
  Result := TtiOPFImplUnit.Create(Self, FUnitTemplate);;
end;

function TtiOPFUnitCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TtiOPFUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

constructor TtiOPFUnitCreator.Create(UnitStructure: TUnitStructure; FormTemplate, UnitTemplate: string);
begin
  inherited Create;
  FUnitStructure := UnitStructure;
  FFormTemplate := FormTemplate;
  FUnitTemplate := UnitTemplate;
end;

{ TtiOPFImplUnit }

constructor TtiOPFImplUnit.Create(const ACreator: TtiOPFUnitCreator; TemplateFile: string);
begin
  inherited Create;
  FCreator := ACreator;
  FTemplateFile := TemplateFile;
end;

function TtiOPFImplUnit.GetAge: TDateTime;
begin
  Result := -1;
end;

function TtiOPFImplUnit.GetSource: string;
begin
  Result := Creator.UnitStructure.ProcessTemplate(FTemplateFile);
end;

end.


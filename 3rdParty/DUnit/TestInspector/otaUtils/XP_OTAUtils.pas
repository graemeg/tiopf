unit XP_OTAUtils;

{
 $Source: /usr/local/cvs/EPC/DelphiExperts/Common/XP_OTAUtils.pas,v $
 $Revision: 1.23 $
 $Date: 2004/03/28 07:14:02 $
 Last amended by $Author: paul $
 $State: Exp $

 XP_OTAUtils:
 Utility methods and base classes for OpenTools API

 Copyright (c) 2001,2002 by The Excellent Programming Company Pty Ltd
 (ABN 27 005 394 918). All rights reserved.

 Contact Paul Spain via email: paul@xpro.com.au

 This unit is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This unit is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this unit; if not, the license can be viewed at:
 http://www.gnu.org/copyleft/lesser.html
 or write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA  02111-1307  USA
 }

interface

{$I jedi.inc}   // used solely for delphi version definitions

uses
  ToolsAPI,
  SysUtils,
  Classes;

function ExtractWhiteSpace(const AString: string): string;
function ExtractFileStem(const AFileName: string): string;

function SplitPair(const APair: string;
  out AFirst, ASecond: string; const ASeparator: char): boolean;
function SplitDelimitedText(const ADelimitedText: string;
  const ATokens: TStrings; const ADelimiter: Char): boolean;

procedure MessageViewAdd(AMessage: string);
procedure MessageViewAddFmt(const MessageFormat: string;
  const Args: array of const);
procedure EventLogAdd(AMessage: string);
procedure EventLogAddFmt(const MessageFormat: string;
  const Args: array of const);

function IsMetaModule(const AModule: IOTAModule): boolean;

function GetCurrentProject(out Project: IOTAProject): boolean;
function GetCurrentProjectGroup(out ProjectGroup: IOTAProjectGroup): boolean;
function GetCurrentUnitName(out AbsoluteFileSpec: string): boolean;
function GetCurrentProjectName(out AbsoluteFileSpec: string): boolean;
function GetProjectName(const Project: IOTAProject; out AbsoluteFileSpec: string): boolean;
function GetCurrentProjectGroupName(out AbsoluteFileSpec: string): boolean;
function GetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): boolean;
// Returns true for at least one editor found. Parameters are nil if not found
function GetCurrentEditors(out SourceEditor: IOTASourceEditor;
  out FormEditor: IOTAFormEditor): boolean;

function GetProjectRawSearchPaths(const AProject: IOTAProject; const ASearchPaths: TStrings): boolean;
function GetProjectAbsoluteSearchPaths(const AProject: IOTAProject; const ASearchPaths: TStrings): boolean;
function OpenFileInIDE(const AFilePath: string; const ALineNumber: integer = 1): boolean;
function GetIDEDelphiLibraryPath(const ALibraryPath: TStrings): boolean;
function GetTopView(out EditView: IOTAEditView): boolean;
function ExpandEnvironmentVariableReferences(var AString: string): boolean;

function EditPosToFilePos(const View: IOTAEditView;
  EditPos: TOTAEditPos): longint;

function CreateModule(const Creator: IOTACreator): boolean;

function AddWizard(const Wizard: IOTAWizard; out Handle: integer): boolean;
procedure DeleteWizard(const Handle: integer);

{$IFDEF DEBUG}
procedure DebugMessage(const AMessage: string);
procedure DebugMessageFmt(const AMessageFormat: string;
  const Args: array of const);
{$ENDIF}

function GetIDEEnvironmentOptions(const AOutputFilePath: string): boolean;
function GetEnvVarValue(const VarName: string): string;
function WriteToFile(const AText: string; const AFilePath: string): boolean;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAFile declaration
//////////////////////////////////////////////////////////////////////////////

type

  IXP_OTAFile = interface(IOTAFile)
    ['{709205C1-C959-48EA-A58B-F74DE1059F50}']
    function GetFileName: string;
    procedure SetFileName(const AFileName: string);

    property FileName: string
      read GetFileName write SetFileName;
  end;

  TXP_OTAFile = class (TInterfacedObject, IOTAFile, IXP_OTAFile)
  protected

    FFileName: string;

    function GetSource: string; virtual; abstract;
    function GetAge: TDateTime; virtual;

    function GetFileName: string; virtual;
    procedure SetFileName(const AFileName: string); virtual;

  public

    constructor Create(const AFileName: string = ''); virtual;

  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTAMessage declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTAMessage = class (TInterfacedObject, IOTACustomMessage)
  protected

    FLineText: string;
    FFileName: string;
    FLineNumber: integer;
    FColumnNumber: integer;

    function GetColumnNumber: Integer; virtual;
    function GetFileName: string; virtual;
    function GetLineNumber: Integer; virtual;
    function GetLineText: string; virtual;
    procedure ShowHelp; virtual;

  public

    constructor Create(const ALineText: string; const AFileName: string = '';
      const ALineNumber: integer = 0; const AColumnNumber: integer = 0); virtual;

  end;

//////////////////////////////////////////////////////////////////////////////
///     TXP_OTANotifier declaration
//////////////////////////////////////////////////////////////////////////////

  TXP_OTANotifier = class (TInterfacedObject, IOTANotifier)
  public
    constructor Create;
    destructor Destroy; override;
  protected
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
  end;


implementation

{$IFDEF DELPHI6_UP}
uses
  Dialogs,
  Windows,      // HKEY_CURRENT_USER
  Registry,
  Variants;
{$ELSE}
uses
  Variants,
  FileCtrl;                   // ForceDirectories
{$ENDIF}

{$IFDEF DEBUG}

procedure DebugMessage(const AMessage: string);
begin
  EventLogAdd(AMessage);
end;

procedure DebugMessageFmt(const AMessageFormat: string;
  const Args: array of const);
begin
  EventLogAddFmt(AMessageFormat, Args);
end;

{$ENDIF}

function ExtractWhiteSpace(const AString: string): string;
var
  Src, Dst: pchar;

begin
  System.SetLength(Result, System.Length(AString));

  if System.Length(Result) > 0 then
  begin
    Src := @AString[1];
    Dst := @Result[1];

    while Src^ <> #0 do
    begin

{$IFDEF DELPHI2009_UP}
      if not SysUtils.CharInSet(Src^, [#09..#13, #32]) then
{$ELSE}
      if not (Src^ in [#09..#13, #32]) then
{$ENDIF}
      begin
        Dst^ := Src^;
        System.Inc(Dst);
      end;

      System.Inc(Src);
    end;

    // Copy null terminator (#0);
    Dst^ := Src^;
    // Copy to new string, length based on position of null terminator
    Result := pchar(Result);
  end;

end;

function ExtractFileStem(const AFileName: string): string;
begin
  Result := SysUtils.ChangeFileExt( SysUtils.ExtractFileName(AFileName), '' );
end;

function SplitDelimitedText(const ADelimitedText: string;
  const ATokens: TStrings; const ADelimiter: Char): boolean;
var
  i: integer;
begin
  Assert(Assigned(ATokens));
  ATokens.Clear;
  ATokens.Delimiter := ADelimiter;
  ATokens.StrictDelimiter := true;
  ATokens.DelimitedText := ADelimitedText;

  for i := 0 to ATokens.Count - 1 do
    ATokens[i] := Trim(ATokens[i]);

  Result := true;
end;

function SplitPair(const APair: string;
  out AFirst, ASecond: string; const ASeparator: char): boolean;
var
  LSeparatorPos: integer;
begin
  LSeparatorPos := Pos(ASeparator, APair);

  if LSeparatorPos > 0 then
  begin
    Result := true;
    AFirst := Copy(APair, 1, LSeparatorPos - 1);
    ASecond := Copy(APair, LSeparatorPos + 1, Length(APair));
  end
  else
  begin
    Result := false;
    AFirst := APair;
    ASecond := '';
  end;
end;

procedure MessageViewAdd(AMessage: string);
var
  MessageServices: IOTAMessageServices;

begin

  if SysUtils.Supports(BorlandIDEServices, IOTAMessageServices,
    MessageServices) then
    MessageServices.AddTitleMessage(AMessage);

end;

procedure MessageViewAddFmt(const MessageFormat: string;
  const Args: array of const);
begin
  MessageViewAdd(SysUtils.Format(MessageFormat, Args));
end;

procedure EventLogAdd(AMessage: string);
var
  DebuggerServices: IOTADebuggerServices;

begin

  if SysUtils.Supports(BorlandIDEServices, IOTADebuggerServices,
    DebuggerServices) then
{$IFDEF DELPHI7_UP}
    DebuggerServices.LogString(AMessage, litDefault);
{$ELSE}
    DebuggerServices.LogString(AMessage);
{$ENDIF}
end;

procedure EventLogAddFmt(const MessageFormat: string;
  const Args: array of const);
begin
  EventLogAdd(SysUtils.Format(MessageFormat, Args));
end;


function IsMetaModule(const AModule: IOTAModule): boolean;
var
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;

begin
  Result := SysUtils.Supports(AModule, IOTAProject, Project)
    or SysUtils.Supports(AModule, IOTAProjectGroup, ProjectGroup);
end;

function GetCurrentUnitName(out AbsoluteFileSpec: string): boolean;
var
  SourceEditor: IOTASourceEditor;

begin
  Result := GetCurrentSourceEditor(SourceEditor);

  if Result then
    AbsoluteFileSpec := SourceEditor.FileName
  else
    AbsoluteFileSpec := '';

end;

function GetCurrentProjectName(out AbsoluteFileSpec: string): boolean;
var
  Project: IOTAProject;

begin
  Result := GetCurrentProject(Project);

  if Result then
    AbsoluteFileSpec := Project.FileName
  else
    AbsoluteFileSpec := '';

end;

function GetProjectName(const Project: IOTAProject; out AbsoluteFileSpec: string): boolean;
begin
  if Assigned(Project) then
  begin
    AbsoluteFileSpec := Project.FileName;
    Result := true;
  end
  else
    Result := false;
end;

function GetCurrentProjectGroupName(out AbsoluteFileSpec: string): boolean;
var
  idx: integer;
  ProjectGroup: IOTAProjectGroup;
  SourceEditor: IOTASourceEditor;

begin

  if GetCurrentProjectGroup(ProjectGroup) then
  begin
    idx := ProjectGroup.GetModuleFileCount - 1;

    // Iterate over modules till we find a source editor or list exhausted
    while not ((idx < 0)
      or SysUtils.Supports(ProjectGroup.GetModuleFileEditor(idx),
      IOTASourceEditor, SourceEditor)) do
      System.Dec(idx);

    // Success if list wasn't ehausted.
    if idx >= 0 then
    begin
      AbsoluteFileSpec := SourceEditor.FileName;
      Result := true;
      exit;
    end;

  end;

  Result := false;
  AbsoluteFileSpec := '';
end;

function GetCurrentSourceEditor(out SourceEditor: IOTASourceEditor): boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  idx: integer;

begin
  Result := false;
  SourceEditor := nil;

  if SysUtils.Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    Module := ModuleServices.CurrentModule;

    if System.Assigned(Module) then
    begin
      idx := Module.GetModuleFileCount - 1;

      // Iterate over modules till we find a source editor or list exhausted
      while not ((idx < 0) or SysUtils.Supports(Module.GetModuleFileEditor(idx),
        IOTASourceEditor, SourceEditor)) do
        System.Dec(idx);

      // Success if list wasn't ehausted.
      Result := idx >= 0;
    end;

  end;

end;

function GetCurrentEditors(out SourceEditor: IOTASourceEditor;
  out FormEditor: IOTAFormEditor): boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  idx: integer;

begin
  SourceEditor := nil;
  FormEditor := nil;

  if SysUtils.Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    Module := ModuleServices.CurrentModule;

    if System.Assigned(Module) then

      for idx := Module.GetModuleFileCount - 1 downto 0 do

        if not SysUtils.Supports(Module.GetModuleFileEditor(idx),
          IOTASourceEditor, SourceEditor) then
          SysUtils.Supports(Module.GetModuleFileEditor(idx),
            IOTAFormEditor, FormEditor);
  end;

  Result := Assigned(SourceEditor) or Assigned(FormEditor);
end;

function GetTopView(out EditView: IOTAEditView): boolean;
var
  EditorServices: IOTAEditorServices;

begin
  // EditorServices.TopView AV's if there are no buffers open. Workaround is to
  // check for EditorServices.TopBuffer first.
  Result := SysUtils.Supports(
    BorlandIDEServices, IOTAEditorServices, EditorServices)
    and System.Assigned(EditorServices.TopBuffer);

  if Result then
    EditView := EditorServices.TopView
  else
    EditView := nil;

end;

function EditPosToFilePos(const View: IOTAEditView;
  EditPos: TOTAEditPos): longint;
var
  CharPos: TOTACharPos;

begin

  if System.Assigned(View) then
  begin
    View.ConvertPos(true, EditPos, CharPos);
    Result := View.CharPosToPos(CharPos);
  end
  else
    Result := 0;

end;


function GetCurrentProject(out Project: IOTAProject): boolean;
var
  ModuleServices: IOTAModuleServices;

begin
  Result := false;
  Project := nil;

  if SysUtils.Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    Project := ModuleServices.GetActiveProject;
    Result := Assigned(Project);
  end;

end;

function GetCurrentProjectGroup(out ProjectGroup: IOTAProjectGroup): boolean;
var
  ModuleServices: IOTAModuleServices;

begin
  Result := false;
  ProjectGroup := nil;

  if SysUtils.Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    ProjectGroup := ModuleServices.MainProjectGroup;
    Result := Assigned(ProjectGroup);
  end;

end;

function GetProjectRawSearchPaths(const AProject: IOTAProject; const ASearchPaths: TStrings): boolean;
const
  cSourceSearchPath = 'SrcDir';
  cObjectSearchPath = 'ObjDir';
  // also UnitDir
  cSearchPathDelimiter = ';';
var
  delimitedSearchPaths: string;
begin
  Result := false;
  if Assigned(AProject) and Assigned(AProject.ProjectOptions) and Assigned(ASearchPaths) then
  begin
    delimitedSearchPaths := VarToStr(AProject.ProjectOptions.Values[cSourceSearchPath]);
    Result := SplitDelimitedText(delimitedSearchPaths, ASearchPaths, cSearchPathDelimiter);
  end;

end;

function GetProjectAbsoluteSearchPaths(const AProject: IOTAProject; const ASearchPaths: TStrings): boolean;
const
  cRootFolder = '\';
var
  ProjectFolder: string;
  i: integer;
begin
  Result := false;
  if GetProjectRawSearchPaths(AProject, ASearchPaths) and
    GetProjectName(AProject, ProjectFolder) then
  begin
    ProjectFolder := ExtractFileDir(ProjectFolder);

    for i := 0 to ASearchPaths.Count - 1 do
      // check for relative search paths
    begin

      if (Length(ExtractFileDrive(ASearchPaths[i])) = 0) or (ASearchPaths[i][1] <> cRootFolder) then
      begin
        // is a relative search path -> make relative to active project absolute folder
        ASearchPaths[i] := Format('%s\%s',[ProjectFolder, ASearchPaths[i]]);
      end;

      ASearchPaths[i] := IncludeTrailingPathDelimiter(ExpandFileName(ASearchPaths[i]));
    end;

    // add project folder to search path
    ASearchPaths.Insert(0, IncludeTrailingPathDelimiter(ProjectFolder));
    Result := true;
  end;

end;

function GetIDEDelphiLibraryPath(const ALibraryPath: TStrings): boolean;
const
  cDelphiWin32LibraryPath = 'LibraryPath';
  cLibraryPathDelimiter = ';';
var
  EnvironmentOptions: IOTAEnvironmentOptions;
  DelimitedSearchPath: string;
  i: integer;

begin
  Result := false;
  EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;

  if Assigned(EnvironmentOptions) then
  begin
    DelimitedSearchPath := VarToStr(EnvironmentOptions.Values[cDelphiWin32LibraryPath]);

    if ExpandEnvironmentVariableReferences(DelimitedSearchPath) and
      SplitDelimitedText(DelimitedSearchPath, ALibraryPath, cLibraryPathDelimiter) then
    begin

      for i := 0 to ALibraryPath.Count - 1 do
        ALibraryPath[i] := IncludeTrailingPathDelimiter(ALibraryPath[i]);

      Result := true;
    end;

  end;

end;

function ExpandEnvironmentVariableReferences(var AString: string): boolean;
var
  LDelphiKeys: TRegIniFile;
  LDelphiBaseRegKey: string;
  LEnvironmentVariables: TStrings;
  LVariableNameReference: string;
  i: integer;

const
  cRegRootKey = HKEY_CURRENT_USER;
  cEnvironmentVariablesKey = 'Environment Variables';
  cBDS = 'BDS';
  cBDSCOMMONDIR = 'BDSCOMMONDIR';

begin
  LDelphiKeys := nil;
  LEnvironmentVariables := nil;
  LDelphiBaseRegKey := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;

  try
    LEnvironmentVariables := TStringList.Create;
    // TODO: minimal list of environment variables for OPDMS
    // - expand to include all codegear vars and environment variables
    LEnvironmentVariables.Values[cBDS] := (BorlandIDEServices as IOTAServices).GetRootDirectory;
    LEnvironmentVariables.Values[cBDSCOMMONDIR] := GetEnvironmentVariable(cBDSCOMMONDIR);
    // apply user variables and overrides - will clobber any same-named vars from above...
    LDelphiKeys := TRegIniFile.Create(LDelphiBaseRegKey);
    LDelphiKeys.ReadSectionValues(cEnvironmentVariablesKey, LEnvironmentVariables);

      for i := 0 to LEnvironmentVariables.Count - 1 do
      begin
        LVariableNameReference := Format('$(%s)', [LEnvironmentVariables.Names[i]]);
        AString := StringReplace(AString, LVariableNameReference,
          LEnvironmentVariables.ValueFromIndex[i], [rfReplaceAll, rfIgnoreCase]);
      end;

    Result := true;
  finally
    LDelphiKeys.Free;
    LEnvironmentVariables.Free;
  end;

end;

function GetIDEEnvironmentOptions(const AOutputFilePath: string): boolean;
var
  EnvironmentOptions: IOTAEnvironmentOptions;
  OptionNames: TOTAOptionNameArray;
  i: integer;
  OutputText: string;
begin
  Result := false;
  EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;

  if Assigned(EnvironmentOptions) then
  begin
    OptionNames := EnvironmentOptions.GetOptionNames;

    for i := Low(OptionNames) to High(OptionNames) do
      FmtStr(OutputText,'%s%s=%s'#13#10, [OutputText, OptionNames[i].Name,
        VarToStr(EnvironmentOptions.Values[OptionNames[i].Name])]);

    Result := WriteToFile(OutputText, AOutputFilePath);
  end;

end;

function WriteToFile(const AText: string; const AFilePath: string): boolean;
var
  OutputFile: TextFile;

begin
  Result := false;

  try
    AssignFile(OutputFile, AFilePath);

    try
      Rewrite(OutputFile);
      Write(OutputFile, AText);
    finally
      CloseFile(OutputFile);
    end;

    Result := true;
  except
    on E: EInOutError do ;  // swallow file errors
  end;

end;

// http://www.delphidabbler.com/articles?article=6#getenvvarvalue
function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  // Get required buffer size (inc. terminal #0)
  BufSize := GetEnvironmentVariable(
    PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName),
      PChar(Result), BufSize);
  end
  else
    // No such environment variable
    Result := '';
end;

function OpenFileInIDE(const AFilePath: string; const ALineNumber: integer): boolean;
var
  LEditPos: TOTAEditPos;
  LView: IOTAEditView;
begin
  Result := false;

  if FileExists(AFilePath) and (ALineNumber > 0) then
  begin
    LEditPos.Col := 1;
    LEditPos.Line := ALineNumber;
    (BorlandIDEServices as IOTAActionServices).OpenFile(AFilePath);
    LView := (BorlandIDEServices as IOTAEditorServices60).TopView;
    LView.CursorPos := LEditPos;
    LView.MoveViewToCursor;
    LView.Paint;
    Result := true;
  end;

end;

function CreateModule(const Creator: IOTACreator): boolean;
var
  ModuleServices: IOTAModuleServices;

begin
  Result :=  SysUtils.Supports(ToolsAPI.BorlandIDEServices, IOTAModuleServices,
    ModuleServices) and System.Assigned(Creator);

  if Result then
    ModuleServices.CreateModule(Creator);

end;

function AddWizard(const Wizard: IOTAWizard; out Handle: integer): boolean;
var
  WizardServices: IOTAWizardServices;

begin
  Result :=  SysUtils.Supports(ToolsAPI.BorlandIDEServices, IOTAWizardServices,
    WizardServices) and System.Assigned(Wizard);

  if Result then
    Handle := WizardServices.AddWizard(Wizard);

end;

procedure DeleteWizard(const Handle: integer);
var
  WizardServices: IOTAWizardServices;

begin

  if SysUtils.Supports(ToolsAPI.BorlandIDEServices, IOTAWizardServices,
    WizardServices) then
    WizardServices.RemoveWizard(Handle);

end;

{ TXP_OTAFile }

constructor TXP_OTAFile.Create(const AFileName: string);
begin
  inherited Create;
  SetFileName(AFileName);
end;

function TXP_OTAFile.GetAge: TDateTime;
begin
  // New file
  Result := -1;
end;

function TXP_OTAFile.GetFileName: string;
begin
  Result := FFileName
end;

procedure TXP_OTAFile.SetFileName(const AFileName: string);
begin
  FFileName := AFileName;
end;

{ TXP_OTAMessage }

constructor TXP_OTAMessage.Create(const ALineText, AFileName: string;
  const ALineNumber, AColumnNumber: integer);
begin
  inherited Create;
  FLineText := ALineText;
  FFileName := AFileName;
  FLineNumber := ALineNumber;
  FColumnNumber := AColumnNumber;
end;

function TXP_OTAMessage.GetColumnNumber: Integer;
begin
  Result := FColumnNumber;
end;

function TXP_OTAMessage.GetFileName: string;
begin
  Result := FFileName;
end;

function TXP_OTAMessage.GetLineNumber: Integer;
begin
  Result := FLineNumber;
end;

function TXP_OTAMessage.GetLineText: string;
begin
  Result := FLineText;
end;

procedure TXP_OTAMessage.ShowHelp;
begin
  // Do nothing;
end;

{ TXP_OTANotifier }

procedure TXP_OTANotifier.AfterSave;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: TXP_OTANotifier.AfterSave.', [ClassName]);
  {$ENDIF}
end;

procedure TXP_OTANotifier.BeforeSave;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: TXP_OTANotifier.BeforeSave.', [ClassName]);
  {$ENDIF}
end;

constructor TXP_OTANotifier.Create;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: Entering TXP_OTANotifier.Create.', [ClassName]);
  {$ENDIF}
  inherited;
end;

destructor TXP_OTANotifier.Destroy;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: Entering TXP_OTANotifier.Destroy.', [ClassName]);
  {$ENDIF}
  inherited;
end;

procedure TXP_OTANotifier.Destroyed;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: TXP_OTANotifier.Destroyed.', [ClassName]);
  {$ENDIF}
end;

procedure TXP_OTANotifier.Modified;
begin
  {$IFDEF DEBUG}
  DebugMessageFmt('%s: TXP_OTANotifier.Modified.', [ClassName]);
  {$ENDIF}
end;

end.



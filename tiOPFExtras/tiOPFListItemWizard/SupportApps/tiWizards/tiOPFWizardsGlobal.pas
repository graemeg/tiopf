unit tiOPFWizardsGlobal;

interface

uses
  SysUtils, ToolsAPI;

const
  CRLF = #13#10;

  cgPathToHeaderFile = 'C:\TechInsite\SupportApps\tiWizards\header.txt';
  cgWizardINIFile = 'C:\TechInsite\SupportApps\tiWizards\tiOPFWizards.ini';

type
  TModuleNames = class(TObject)
  public
    UnitName,
    FormName,
    FileName: string;
    constructor Create;
  end;

  function GetCurrentProject: IOTAProject;
  function GetProjectGroup: IOTAProjectGroup;

implementation

function GetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
begin
  Result := nil;

  IProjectGroup := GetProjectGroup;
  if not Assigned(IProjectGroup) then
    Exit;

  Result := IProjectGroup.ActiveProject;
end;

function GetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(BorlandIDEServices));

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  Result := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if Supports(IModule, IOTAProjectGroup, Result) then
      Break;
  end;
end;

{ TModuleNames }

constructor TModuleNames.Create;
var
  OTAModuleServices: IOTAModuleServices;
begin
  inherited;
  if BorlandIDEServices.QueryInterface(IOTAModuleServices, OTAModuleServices) = S_OK then
    OTAModuleServices.GetNewModuleAndClassName('Unit', UnitName, FormName, FileName);
end;


end.


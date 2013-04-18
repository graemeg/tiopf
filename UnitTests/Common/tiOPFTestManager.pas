unit tiOPFTestManager;

{$I tiDefines.inc}
{.$Define gDEBUG}   // remove . (dot) to enable define

interface
uses
   Classes  // needed for TStringList
  ,tiObject
  ,tiPersistenceLayers
  ,tiOID
 ;

const
  cINIPerLayersToTest = 'PerLayersToTest';


type
  TtiOPFTestManager     = class;
  TtiOPFTestSetupData   = class;

  TtiOPFTestSetupData = class(TtiObject)
  private
    FPersistenceLayerDefaults: TtiPersistenceLayerDefaults;
    FPersistenceLayerClass: TtiPersistenceLayerClass;
    function GetCanCreateDatabase: boolean;
    function GetCanDropDatabase: boolean;
    function GetDatabaseName: string;
    function GetPassword: string;
    function GetPersistenceLayerName: string;
    function GetUserName: string;
    function GetParams: string;
  protected
    FSelected: Boolean;
    // Gives you the chance to override default database, username
    // and password values for the unit tests.
    function  ReadFromReg(const pPerLayer, pProp, pDefault : string): string;
  public
    constructor Create(const APersistenceLayer: TtiPersistenceLayer); reintroduce;
    destructor  Destroy; override;
    procedure   Read; override;
    procedure   Save; override;

    property    PersistenceLayerClass: TtiPersistenceLayerClass read FPersistenceLayerClass;
    property    PersistenceLayerName : string read GetPersistenceLayerName;
    property    DBName       : string read GetDatabaseName;
    property    Username     : string read GetUserName;
    property    Password     : string read GetPassword;
    property    Params       : string read GetParams;
    property    CanCreateDatabase : boolean read GetCanCreateDatabase;
    property    CanDropDatabase : boolean read GetCanDropDatabase;
    procedure   ForceTestDataDirectory;
    property    Selected     : boolean read FSelected write FSelected;

  end;


  TtiOPFTestManager = class(TtiObjectList)
  private
    FTestAll: boolean;
    FTestNonPersistentClasses: boolean;
  protected
    function    GetItems(i: integer):TtiOPFTestSetupData; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiOPFTestSetupData); reintroduce;
  public
    constructor Create; override;
    property    TestNonPersistentClasses : boolean read FTestNonPersistentClasses write FTestNonPersistentClasses;
    function    ToRun(const pClassID : string):boolean;
    property    TestAll : boolean read FTestAll;
    property    Items[i:integer]: TtiOPFTestSetupData read GetItems write SetItems;
    procedure   Add(AObject: TtiOPFTestSetupData); reintroduce;
    function    IsRegistered(const APersistenceLayerName : string): boolean;
    function    FindByPersistenceLayerName(const APersistenceLayerName : string): TtiOPFTestSetupData;
    procedure   UnloadPersistenceLayersNotSelected;
    procedure   DeleteDatabaseFiles;
    procedure   Read; override;
    procedure   Save; override;

    function    DefaultOIDGeneratorClass: TtiOIDGeneratorClass;
  end;

function  GTIOPFTestManager: TtiOPFTestManager;

implementation
uses
  SysUtils
  ,tiOPFManager
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiUtils
  ,tiDUnitINI
  ,tiConstants
  ,tiOIDForTesting
  {$IFDEF gDEBUG},dbugintf{$ENDIF}
 ;

const
  CDefaultTestDataDirectory = '..\_Data';

var
  UTIOPFTestManager: TtiOPFTestManager;

function GTIOPFTestManager: TtiOPFTestManager;
begin
  if UTIOPFTestManager = nil then
    UTIOPFTestManager := TtiOPFTestManager.Create;
  result := UTIOPFTestManager;
end;

{ TtiOPFTestManager }

procedure TtiOPFTestManager.Add(AObject: TtiOPFTestSetupData);
begin
  inherited Add(AObject);
end;


constructor TtiOPFTestManager.Create;
var
  L: TtiOPFTestSetupData;
  i: integer;
begin
  inherited;
  FTestNonPersistentClasses:= True;
  FTestAll := FindCmdLineSwitch('All', ['-', '/'], true);
  {$IFDEF gDEBUG}SendDebug('Persistence layers found:');{$ENDIF}
  for i := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
  begin
    {$IFDEF gDEBUG}SendDebug('    ' + GTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName); {$ENDIF}
    L:= TtiOPFTestSetupData.Create(GTIOPFManager.PersistenceLayers.Items[i]);
    Add(L);
  end;
end;


function TtiOPFTestManager.DefaultOIDGeneratorClass: TtiOIDGeneratorClass;
begin
  result:= TtiOIDGeneratorForTesting;
end;

procedure TtiOPFTestManager.DeleteDatabaseFiles;
var
  i: integer;
  LDatabaseName:string;
begin
  for i := 0 to Count - 1 do
    if Items[i].CanCreateDatabase then
    begin
      LDatabaseName:= Items[i].DBName;
      if FileExists(LDatabaseName) then
        tiDeleteFile(LDatabaseName)
      else if DirectoryExists(LDatabaseName) then
        tiForceRemoveDir(LDatabaseName)
    end;
end;

function TtiOPFTestManager.FindByPersistenceLayerName(const APersistenceLayerName: string): TtiOPFTestSetupData;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].PersistenceLayerName, APersistenceLayerName) then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;


function TtiOPFTestManager.GetItems(i: integer): TtiOPFTestSetupData;
begin
  result := TtiOPFTestSetupData(inherited GetItems(i));
end;


function TtiOPFTestManager.IsRegistered(const APersistenceLayerName : string): boolean;
begin
  result := FindByPersistenceLayerName(APersistenceLayerName) <> nil;
end;


procedure TtiOPFTestManager.Read;
var
  i: Integer;
begin
  {$IFDEF gDEBUG}
  SendDebug('DUnit ini file: ' + gDUnitINICommon.FileName);
  {$ENDIF}
  if not gDUnitINICommon.ValueExists(cINIPerLayersToTest, 'NonPersistent') then
    gDUnitINICommon.WriteBool(cINIPerLayersToTest, 'NonPersistent', True);
  FTestNonPersistentClasses := gDUnitINICommon.ReadBool(cINIPerLayersToTest, 'NonPersistent', True);
  for i:= 0 to Count - 1 do
    Items[i].Read;
end;


procedure TtiOPFTestManager.Save;
var
  i: Integer;
begin
  gDUnitINICommon.WriteBool(cINIPerLayersToTest, 'NonPersistent', FTestNonPersistentClasses);
  for i:= 0 to Count - 1 do
    Items[i].Save;
end;


procedure TtiOPFTestManager.SetItems(i: integer;const AValue: TtiOPFTestSetupData);
begin
  inherited SetItems(i, AValue);
end;


function TtiOPFTestManager.ToRun(const pClassID: string): boolean;
var
  lSetup : TtiOPFTestSetupData;
begin
  result := IsRegistered(pClassID);
  result := result or FTestAll;
  if not result then
    Exit; //==>
  lSetup := FindByPersistenceLayerName(pClassID);
  result := (lSetup.Selected or FTestAll);
end;


procedure TtiOPFTestManager.UnloadPersistenceLayersNotSelected;
var
  i: integer;
  LPersistenceLayerName: string;
  LTestSetupData: TtiOPFTestSetupData;
begin
  for i := Count - 1 downto 0 do
  begin
    LTestSetupData := Items[i];
    LPersistenceLayerName:= LTestSetupData.PersistenceLayerName;
    if not LTestSetupData.Selected then
    begin
      if GTIOPFManager.PersistenceLayers.IsLoaded(LPersistenceLayerName) then
        GTIOPFManager.PersistenceLayers.UnLoadPersistenceLayer(LPersistenceLayerName);
      Delete(i);
    end;
  end;
end;

{ TtiOPFTestSetupData }
constructor TtiOPFTestSetupData.Create(const APersistenceLayer: TtiPersistenceLayer);
begin
  inherited Create;
  FPersistenceLayerClass:= TtiPersistenceLayerClass(APersistenceLayer.ClassType);
  FPersistenceLayerDefaults:= TtiPersistenceLayerDefaults.Create;
  APersistenceLayer.AssignPersistenceLayerDefaults(FPersistenceLayerDefaults);
end;

destructor TtiOPFTestSetupData.destroy;
begin
  FPersistenceLayerDefaults.Free;
  inherited;
end;


procedure TtiOPFTestSetupData.ForceTestDataDirectory;
var
  lDir : string;
begin
  if not CanCreateDatabase then
    Exit; //==>
  lDir := ExtractFilePath(DBName);
  if not DirectoryExists(lDir) then
    ForceDirectories(lDir);
  if not DirectoryExists(lDir) then
    raise exception.Create('Can not create directory <' +
                            lDir +
                            '> called in ' +
                            ClassName + '.Create');
end;


procedure TtiOPFTestSetupData.Read;
begin
  FSelected := gDUnitINICommon.ReadBool(cINIPerLayersToTest, PersistenceLayerName, True);
end;


function TtiOPFTestSetupData.GetCanCreateDatabase: boolean;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  result:= FPersistenceLayerDefaults.CanCreateDatabase;
end;

function TtiOPFTestSetupData.GetCanDropDatabase: boolean;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  result:= FPersistenceLayerDefaults.CanDropDatabase;
end;

function TtiOPFTestSetupData.GetDatabaseName: string;
var
  LDefault: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  if FPersistenceLayerDefaults.IsDatabaseNameFilePath then
    LDefault:= ExpandFileName(tiAddTrailingSlash(CDefaultTestDataDirectory) + FPersistenceLayerDefaults.DatabaseName)
  else
    LDefault:= FPersistenceLayerDefaults.DatabaseName;
  Result:= ReadFromReg(
    FPersistenceLayerDefaults.PersistenceLayerName,
    'DBName',
    LDefault);
end;

function TtiOPFTestSetupData.GetPassword: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  Result:= ReadFromReg(
    FPersistenceLayerDefaults.PersistenceLayerName,
    'Password',
    FPersistenceLayerDefaults.Password);
end;

function TtiOPFTestSetupData.GetPersistenceLayerName: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  Result:= FPersistenceLayerDefaults.PersistenceLayerName;
end;

function TtiOPFTestSetupData.GetUserName: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  Result:= ReadFromReg(
    FPersistenceLayerDefaults.PersistenceLayerName,
    'UserName',
    FPersistenceLayerDefaults.UserName);
end;

function TtiOPFTestSetupData.GetParams: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  Result:= ReadFromReg(
    FPersistenceLayerDefaults.PersistenceLayerName,
    'Params',
    FPersistenceLayerDefaults.Params);
end;

function TtiOPFTestSetupData.ReadFromReg(const pPerLayer, pProp, pDefault: string): string;
var
  LDUnitLocalSettings: TDUntiLocalSettings;
begin
  LDUnitLocalSettings:= TDUntiLocalSettings.Create;
  try
    Result:= LDUnitLocalSettings.ReadString('DB_' + pPerLayer, pProp);
  finally
    LDUnitLocalSettings.Free;
  end;
  if result = '' then
    result := pDefault;
end;


procedure TtiOPFTestSetupData.Save;
begin
  gDUnitINICommon.WriteBool(cINIPerLayersToTest, PersistenceLayerName, FSelected);
end;

initialization

finalization
  UTIOPFTestManager.Free;

end.


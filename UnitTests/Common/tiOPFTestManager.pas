unit tiOPFTestManager;

{$I tiDefines.inc}

interface
uses
   Classes  // needed for TStringList
  ,tiObject
  ,tiPersistenceLayers
  ,tiOID
  {$IFDEF FPC}
  ,TestRegistry
  {$ENDIF}
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
    function GetDatabaseName: string;
    function GetPassword: string;
    function GetPersistenceLayerName: string;
    function GetUserName: string;
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
    property    PerLayerName : string read GetPersistenceLayerName;
    property    DBName       : string read GetDatabaseName;
    property    Username     : string read GetUserName;
    property    Password     : string read GetPassword;
    property    CanCreateDatabase : boolean read GetCanCreateDatabase;
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
    procedure   Add(AObject : TtiOPFTestSetupData ; ADefDispOrdr : boolean = true); reintroduce;
    function    IsRegistered(const APerLayerName : string): boolean;
    function    FindByPerLayerName(const APerLayerName : string): TtiOPFTestSetupData;
    procedure   UnloadPersistenceLayersNotSelected;
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
  ,tiOIDGUID
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

procedure TtiOPFTestManager.Add(AObject: TtiOPFTestSetupData;ADefDispOrdr: boolean);
begin
  inherited Add(AObject, ADefDispOrdr);
end;


constructor TtiOPFTestManager.Create;
var
  L: TtiOPFTestSetupData;
  i: integer;
begin
  inherited;
  FTestNonPersistentClasses:= True;
  FTestAll := FindCmdLineSwitch('All', ['-', '/'], true);
  for i := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
  begin
    L:= TtiOPFTestSetupData.Create(GTIOPFManager.PersistenceLayers.Items[i]);
    Add(L);
  end;
end;


function TtiOPFTestManager.DefaultOIDGeneratorClass: TtiOIDGeneratorClass;
begin
  result:= TtiOIDGeneratorGUID;
end;

function TtiOPFTestManager.FindByPerLayerName(const APerLayerName: string): TtiOPFTestSetupData;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].PerLayerName, APerLayerName) then
    begin
      result := Items[i];
      Exit; //==>
    end; 
end;


function TtiOPFTestManager.GetItems(i: integer): TtiOPFTestSetupData;
begin
  result := TtiOPFTestSetupData(inherited GetItems(i));
end;


function TtiOPFTestManager.IsRegistered(const APerLayerName : string): boolean;
begin
  result := FindByPerLayerName(APerLayerName) <> nil;
end;


procedure TtiOPFTestManager.Read;
var
  i: Integer;
begin
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
  lSetup := FindByPerLayerName(pClassID);
  result := (lSetup.Selected or FTestAll);
end;


procedure TtiOPFTestManager.UnloadPersistenceLayersNotSelected;
var
  i: integer;
  LPerLayerName: string;
  LPerFrameworkSetup: TtiOPFTestSetupData;
begin
  for i := Count - 1 downto 0 do
  begin
    LPerFrameworkSetup := Items[i];
    LPerLayerName:= LPerFrameworkSetup.PerLayerName;
    if not LPerFrameworkSetup.Selected then
    begin
      if gTIOPFManager.PersistenceLayers.IsLoaded(LPerLayerName) then
        gTIOPFManager.PersistenceLayers.UnLoadPersistenceLayer(LPerLayerName);
      Delete(i);
    end;
  end;
end;

{ TtiOPFTestSetupData }
constructor TtiOPFTestSetupData.Create(const APersistenceLayer: TtiPersistenceLayer);
const
  cUnknown = 'Unknown';
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
  FSelected := gDUnitINICommon.ReadBool(cINIPerLayersToTest, PerLayerName, True);
end;


function TtiOPFTestSetupData.GetCanCreateDatabase: boolean;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  result:= FPersistenceLayerDefaults.CanCreateDatabase;
end;

function TtiOPFTestSetupData.GetDatabaseName: string;
var
  LDefault: string;
begin
  Assert(FPersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  LDefault:= ExpandFileName(tiAddTrailingSlash(CDefaultTestDataDirectory) + FPersistenceLayerDefaults.DatabaseName);
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
  gDUnitINICommon.WriteBool(cINIPerLayersToTest, PerLayerName, FSelected);
end;

initialization

finalization
  UTIOPFTestManager.Free;

end.


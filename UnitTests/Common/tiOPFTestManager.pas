unit tiOPFTestManager;

{$I tiDefines.inc}

interface
uses
   Classes  // needed for TStringList
  ,tiObject
  {$IFDEF FPC}
  ,TestRegistry
  {$ELSE}
  ,TestFramework
  {$ENDIF}
 ;


const
  cINIPerLayersToTest = 'PerLayersToTest';


type
  TtiOPFTestManager     = class;
  TtiOPFTestSetupData   = class;

  TtiOPFTestSetupData = class(TtiObject)
  private
    function   GetToRun: boolean;
  protected
    FSelected: Boolean;
    FPerLayerName : string;
    FDBName      : string;
    FUsername    : string;
    FPassword    : string;
    FEnabled: boolean;
    FCanCreateDatabase: boolean;
    // Gives you the chance to override default database, username
    // and password values for the unit tests.
    function  ReadFromReg(const pPerLayer, pProp, pDefault : string): string;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Read; override;
    procedure   Save; override;
    property    PerLayerName : string read FPerLayerName;
    property    DBName       : string read FDBName    ;
    property    Username     : string read FUsername  ;
    property    Password     : string read FPassword  ;
    property    CanCreateDatabase : boolean read FCanCreateDatabase;
    procedure   ForceTestDataDirectory;
    property    Selected     : boolean read FSelected write FSelected;
    property    Enabled      : boolean read FEnabled;
    property    ToRun        : boolean read GetToRun;
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
  end;

var
  gTestDataRoot: string;


implementation
uses
  tiCommandLineParams
  ,SysUtils
  ,tiOPFManager
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiLog
  ,tiUtils
  ,tiQuery
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiDUnitINI
 ;

{ TtiOPFTestManager }

procedure TtiOPFTestManager.Add(AObject: TtiOPFTestSetupData;ADefDispOrdr: boolean);
begin
  inherited Add(AObject, ADefDispOrdr);
end;


constructor TtiOPFTestManager.Create;
begin
  inherited;
  FTestNonPersistentClasses:= True;
  FTestAll := FindCmdLineSwitch('All', ['-', '/'], true);
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
  result := lSetup.Enabled and (lSetup.Selected or FTestAll);
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

constructor TtiOPFTestSetupData.Create;
const
  cUnknown = 'Unknown';
begin
  inherited;
  FPerLayerName    := cUnknown;
  FDBName          := cUnknown;
  FUsername        := cUnknown;
  FPassword        := cUnknown;
  FEnabled         := false;
  FCanCreateDatabase:= false;
end;


destructor TtiOPFTestSetupData.destroy;
begin
  inherited;
end;


procedure TtiOPFTestSetupData.ForceTestDataDirectory;
var
  lDir : string;
begin
//  Exit;   // ?????
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
  if not gDUnitINICommon.ValueExists(cINIPerLayersToTest, PerLayerName) then
    gDUnitINICommon.WriteBool(cINIPerLayersToTest, PerLayerName, Enabled);
  FSelected := gDUnitINICommon.ReadBool(cINIPerLayersToTest, PerLayerName, Enabled);
end;


function TtiOPFTestSetupData.GetToRun: boolean;
begin
  result := Selected and Enabled;
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
  {$IFDEF UNIX}
  gTestDataRoot := '../_Data/Demo';
  {$ELSE}
  gTestDataRoot := '..\_Data\Demo';
  {$ENDIF}

end.

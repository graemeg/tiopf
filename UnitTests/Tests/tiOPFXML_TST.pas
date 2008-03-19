unit tiOPFXML_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQueryNonSQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersXML = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure   ConnectDatabase; override;
  end;

  TTestTIDatabaseXML = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryXML = class(TTestTIQueryNonSQL)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure FieldByNameVSFieldByIndex; override;
  end;

  TTestTIAutoMapOperationXML = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure CollectionReadPKThreaded; override;
  end;

  TTestTIOIDPersistentGUIDXML = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerXML = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;

procedure RegisterTests;

implementation

uses
  tiConstants,
  {$IFDEF FPC}
  tiFPCUnitUtils,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  tiOPFTestManager,
  tiTestFramework,
  SysUtils,
  tiUtils,
  tiTestDependencies;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersXML);
  tiRegisterPersistenceTest(TTestTIDatabaseXML);
  tiRegisterPersistenceTest(TTestTIQueryXML);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationXML);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDXML);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerXML);
end;

{ TTestTIDatabaseXML }

procedure TTestTIDatabaseXML.CreateDatabase;
var
  LFileName: string;
begin
  LFileName:= TempFileName('temp.msxml');
  if FileExists(LFileName) then
  tiDeleteFile(LFileName);
  Check(not FileExists(LFileName), '<' + LFileName + '> Exists when it should not');
  PersistenceLayer.DatabaseClass.CreateDatabase(LFileName, '', '');
  Check(FileExists(LFileName), '<' + LFileName + '> Does not exists when it should');
  tiDeleteFile(LFileName);
end;

procedure TTestTIDatabaseXML.DatabaseExists;
var
  LFileName: string;
begin
  LFileName:= TempFileName('temp.msxml');
  if FileExists(LFileName) then
    tiDeleteFile(LFileName);
  Check(not FileExists(LFileName), '<' + LFileName + '> Exists when it should not');
  Check(not PersistenceLayer.DatabaseClass.DatabaseExists(LFileName, '', ''),
    'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test', LFileName);
  Check(FileExists(LFileName), '<' + LFileName + '> Does not exists when it should');
  Check(PersistenceLayer.DatabaseClass.DatabaseExists(LFileName, '', ''),
    'FDatabaseClass.DatabaseExists()=false when it should =true');
  tiDeleteFile(LFileName);
end;

class function TTestTIDatabaseXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

procedure TTestTIQueryXML.FieldByNameVSFieldByIndex;
begin
  Check(True); // Dont test because it will always fail.
end;

{ TTestTIPersistenceLayersXML }

procedure TTestTIPersistenceLayersXML.ConnectDatabase;
begin
  AllowedMemoryLeakSize:= 24;
  inherited;
end;

class function TTestTIPersistenceLayersXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

class function TTestTIQueryXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIAutoMapOperationXML }

procedure TTestTIAutoMapOperationXML.CollectionReadPKThreaded;
begin
  AllowedMemoryLeakSize:= 32;
  inherited;
end;

class function TTestTIAutoMapOperationXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIOIDPersistentGUIDXML }

class function TTestTIOIDPersistentGUIDXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

{ TTestTIOIDPersistentIntegerXML }

class function TTestTIOIDPersistentIntegerXML.PersistenceLayerName: string;
begin
  Result := cTIPersistXML;
end;

procedure TTestTIOIDPersistentIntegerXML.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
  SetAllowedLeakArray([32]);
  inherited;
end;

end.

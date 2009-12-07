unit tiOPFDBISASM4_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiAutomapCriteria_TST,
  tiOID_TST;

type

  TTestTIPersistenceLayersDBISAM4 = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseDBISAM4 = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  end;

  TTestTIQueryDBISAM4 = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  published
    // Testing of stream support under construction
    // procedure ParamAsStream; override;
  end;

  TTestTIAutoMapOperationDBISAM4 = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestAutomappingCriteriaDBISAM4 = class(TTestAutomappingCriteria)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDDBISAM4 = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerDBISAM4 = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
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
  SysUtils,
  tiUtils,
  tiTestDependencies,
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF}
  tiTestFramework;

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersDBISAM4);
  tiRegisterPersistenceTest(TTestTIDatabaseDBISAM4);
  tiRegisterPersistenceTest(TTestTIQueryDBISAM4);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDDBISAM4);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerDBISAM4);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationDBISAM4);
  tiRegisterPersistenceTest(TTestAutomappingCriteriaDBISAM4);
end;

{ TtiOPFTestSetupDataDBISAM4 }

procedure TTestTIDatabaseDBISAM4.CreateDatabase;
var
  LDir: string;
begin
  LDir := tiGetTempFile('');
  try
    tiForceRemoveDir(LDir);
    Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
    PersistenceLayer.DatabaseClass.CreateDatabase(LDir, 'null', 'null');
    Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
  finally
    tiForceRemoveDir(LDir);
  end;
end;

procedure TTestTIDatabaseDBISAM4.DatabaseExists;
var
  LDir: string;
begin
  LDir := tiSwapExt(TempFileName, '');
  try
    tiForceRemoveDir(LDir);
    Check(not DirectoryExists(LDir), '<' + LDir + '> Exists when it should not');
    Check(not PersistenceLayer.DatabaseClass.DatabaseExists(LDir, 'null', 'null'),
      'FDatabaseClass.DatabaseExists()=true when it should =false');
    ForceDirectories(LDir);
    Check(DirectoryExists(LDir), '<' + LDir + '> Does not exists when it should');
    Check(PersistenceLayer.DatabaseClass.DatabaseExists(LDir, 'null', 'null'),
      'FDatabaseClass.DatabaseExists()=false when it should =true');
  finally
    tiForceRemoveDir(LDir);
  end;
end;

class function TTestTIDatabaseDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestTIPersistenceLayersDBISAM4 }

class function TTestTIPersistenceLayersDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestTIQueryDBISAM4 }

class function TTestTIQueryDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestTIAutoMapOperationDBISAM4 }

class function TTestTIAutoMapOperationDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestAutomappingCriteriaDBISAM4 }

class function TTestAutomappingCriteriaDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestTIOIDPersistentGUIDDBISAM4 }

class function TTestTIOIDPersistentGUIDDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

{ TTestTIOIDPersistentIntegerDBISAM4 }

class function TTestTIOIDPersistentIntegerDBISAM4.PersistenceLayerName: string;
begin
  Result := cTIPersistDBISAM4;
end;

end.

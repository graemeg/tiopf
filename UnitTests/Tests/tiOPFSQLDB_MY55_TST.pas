unit tiOPFSQLDB_MY55_TST;

{$I tiDefines.inc}

interface
uses
   tiQuery_TST
  ,tiQuerySQL_TST
  ,tiAutoMap_TST
  ,tiOID_TST
  ;


type

  TTestTIPersistenceLayersSQLDB_MY55 = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIDatabaseSQLDB_MY55 = class(TTestTIDatabase)
  protected
    procedure CreateDatabase; override;
    procedure DatabaseExists; override;
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIQuerySQLDB_MY55 = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIAutoMapOperationSQLDB_MY55 = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentGUIDSQLDB_MY55 = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;


  TTestTIOIDPersistentIntegerSQLDB_MY55 = class(TTestTIOIDPersistentInteger)
  public
    class function PersistenceLayerName: string; override;
  end;



procedure RegisterTests;


implementation
uses
  tiConstants
  ,TestFramework
  ,tiOPFTestManager
  ,SysUtils
  ,tiTestDependencies
  ;

procedure RegisterTests ;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersSQLDB_MY55);
  tiRegisterPersistenceTest(TTestTIDatabaseSQLDB_MY55);
  tiRegisterPersistenceTest(TTestTIQuerySQLDB_MY55);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDSQLDB_MY55);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerSQLDB_MY55);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationSQLDB_MY55);
end ;

{ TTestTIDatabaseSQLDB_MY55 }

procedure TTestTIDatabaseSQLDB_MY55.CreateDatabase;
begin
  // it's a remote database. Do nothing.
end;

procedure TTestTIDatabaseSQLDB_MY55.DatabaseExists;
begin
  // it's a remote database. Do nothing.
end;

class function TTestTIDatabaseSQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;

{ TTestTIPersistenceLayersSQLDB_MY55 }

class function TTestTIPersistenceLayersSQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;

{ TTestTIQuerySQLDB_IB }

class function TTestTIQuerySQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;

{ TTestTIAutoMapOperationSQLDB_MY55 }

class function TTestTIAutoMapOperationSQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;

{ TTestTIOIDPersistentGUIDSQLDB_MY55 }

class function TTestTIOIDPersistentGUIDSQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;

{ TTestTIOIDPersistentIntegerSQLDB_MY55 }

class function TTestTIOIDPersistentIntegerSQLDB_MY55.PersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMySQL55;
end;


end.

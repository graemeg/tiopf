unit tiOPFRemote_TST;

{$I tiDefines.inc}

interface

uses
  tiQuery_TST,
  tiQuerySQL_TST,
  tiAutoMap_TST,
  tiOID_TST;

const
  cErrorCanNotLoadtiDBProxyServer = 'Can not load tiDBProxyServer'#13'' +
    'File location: %s'#13'' +
    'Error message: %s';

  // ToDo: cErrorFileNotFound should be a generic exception
  cErrorFileNotFound = 'File not found';

type

  TTestTIPersistenceLayersRemote = class(TTestTIPersistenceLayers)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIDatabaseRemote = class(TTestTIDatabase)
  public
    class function PersistenceLayerName: string; override;
  published
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
    procedure Transaction_TimeOut;
  end;

  TTestTIQueryRemote = class(TTestTIQuerySQL)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIAutoMapOperationRemote = class(TTestTIAutoMapOperation)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentGUIDRemote = class(TTestTIOIDPersistentGUID)
  public
    class function PersistenceLayerName: string; override;
  end;

  TTestTIOIDPersistentIntegerRemote = class(TTestTIOIDPersistentInteger)
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
  tiOPFTestManager,
  SysUtils,
  tiTestDependencies,
  tiQuery,
  tiTestFramework;

const
  cRemoteServerMainFormName = 'TFormMainTIDBProxyServer';

procedure RegisterTests;
begin
  tiRegisterPersistenceTest(TTestTIPersistenceLayersRemote);
  tiRegisterPersistenceTest(TTestTIDatabaseRemote);
  tiRegisterPersistenceTest(TTestTIQueryRemote);
  tiRegisterPersistenceTest(TTestTIOIDPersistentGUIDRemote);
  tiRegisterPersistenceTest(TTestTIOIDPersistentIntegerRemote);
  tiRegisterPersistenceTest(TTestTIAutoMapOperationRemote);
end;

{ TTestTIDatabaseRemote }

procedure TTestTIDatabaseRemote.CreateDatabase;
begin
//  try
//    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
//    Fail('Exception not raised when it should have been');
//  except
//    on e: Exception do
//    begin
//      CheckIs(e, EAssertionFailed);
//      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message) <> 0);
//    end;
//  end;
end;

procedure TTestTIDatabaseRemote.DatabaseExists;
begin
//  try
//    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password);
//    Fail('Exception not raised when it should have been');
//  except
//    on e: Exception do
//    begin
//      CheckIs(e, EAssertionFailed);
//      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message) <> 0);
//    end;
//  end;
end;

class function TTestTIDatabaseRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIDatabaseRemote.Transaction_TimeOut;
var
  lQuery: TtiQuery;
begin
//  FDatabase.Connect(PerFrameworkSetup.DBName,
//    PerFrameworkSetup.UserName,
//    PerFrameworkSetup.Password,
//    '');
//  try
//    FDatabase.DropTable(cTableNameTestGroup)
//  except
//  end;
//
//  CreateTableTestGroup(FDatabase);
//
//  FDatabase.StartTransaction;
//  try
//    InsertIntoTestGroup(FDatabase, 1);
//    FDatabase.Commit;
//    lQuery := FPersistenceLayer.QueryClass.Create;
//    try
//      lQuery.AttachDatabase(FDatabase);
//      FDatabase.StartTransaction;
//      lQuery.SelectRow(cTableNameTestGroup, nil);
//      Check(not lQuery.EOF, 'Transaction not committed');
//      lQuery.Next;
//      Check(lQuery.EOF, 'Wrong number of records');
//      Sleep(Trunc(cDBProxyServerTimeOut * 60000 * 1.5));
//      try
//        FDatabase.Commit;
//        Fail('tiDBProxyServer did not time out as expected');
//      except
//        on e: Exception do
//          Check(Pos('TIMED OUT', UpperCase(e.message)) <> 0,
//            'tiDBProxyServer did not raise the right exception. Exception message: ' + e.message);
//      end;
//    finally
//      lQuery.Free;
//    end;
//  finally
//    FDatabase.DropTable(cTableNameTestGroup);
//  end;
Assert(False, 'Under construction');
end;

{ TTestTIPersistenceLayersRemote }

class function TTestTIPersistenceLayersRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

{ TTestTIQueryRemote }

class function TTestTIQueryRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

{ TTestTIAutoMapOperationRemote }

class function TTestTIAutoMapOperationRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

{ TTestTIOIDPersistentGUIDRemote }

class function TTestTIOIDPersistentGUIDRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

{ TTestTIOIDPersistentIntegerRemote }

class function TTestTIOIDPersistentIntegerRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

end.

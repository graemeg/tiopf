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
    procedure DatabaseExists; override;
    procedure CreateDatabase; override;
  published
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
  tiTestFramework,
  tiOPFTestCase;

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
  // Not supported in remote persistence layer
end;

procedure TTestTIDatabaseRemote.DatabaseExists;
begin
  // Not supported in remote persistence layer
end;

class function TTestTIDatabaseRemote.PersistenceLayerName: string;
begin
  Result := cTIPersistRemote;
end;

procedure TTestTIDatabaseRemote.Transaction_TimeOut;
var
  LQuery: TtiQuery;
  LDatabase: TtiDatabase;
begin
  DropTable(cTableNameTestGroup);
  CreateTableTestGroup;
  InsertIntoTestGroup(1);

  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.StartTransaction;
    LQuery := LDatabase.CreateAndAttachTIQuery;
    try
      LDatabase.StartTransaction;
      LQuery.SelectRow(cTableNameTestGroup, nil);
      Check(not LQuery.EOF, 'Transaction not committed');
      LQuery.Next;
      Check(LQuery.EOF, 'Wrong number of records');
      Sleep(Trunc(cDBProxyServerTimeOut * 60000 * 1.5));
      try
        lDatabase.Commit;
        Fail('tiDBProxyServer did not time out as expected');
      except
        on e: Exception do
          Check(Pos('TIMED OUT', UpperCase(e.message)) <> 0,
            'tiDBProxyServer did not raise the right exception. Exception message: ' + e.message);
      end;
    finally
      LQuery.Free;
    end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
  DropTable(cTableNameTestGroup);
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

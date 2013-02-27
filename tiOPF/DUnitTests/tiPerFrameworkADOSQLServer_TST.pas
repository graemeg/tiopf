unit tiPerFrameworkADOSQLServer_TST;

{$I tiDefines.inc}

interface
uses
   tiQueryAbs_TST
  ,tiQuerySQL_TST
  ,tiDBConnectionSetupAbs_TST
  ,tiPersistAbs_TST
  ;

type

  // Parent found in tiDBConnectionSetupAbs_TST.pas
  TPerFrameworkSetupADOSQLServer = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupADOSQLServer = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseADOSQLServer = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIDatabaseConnectionADOSQLServer = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryADOSQLServer = class( TTestTIQuerySQL ) ;

procedure RegisterTests;

implementation
uses
  cTIPersist
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiDUnitDependencies
  ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOSQLServer.Create( TTestTIDatabaseADOSQLServer.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOSQLServer.Create( TTestTIDatabaseConnectionADOSQLServer.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOSQLServer.Create( TTestTIQueryADOSQLServer.Suite ));
end ;

{ TPerFrameworkSetupADOSQLServer }

constructor TPerFrameworkSEtupADOSQLServer.create;
begin
  inherited;
  PerLayerName := cTIPersistADOSQLServer ;
  DBName   := ReadFromReg( cTIPersistADOSQLServer, 'DBName', 'EnterValueHere' ) ;
  UserName := ReadFromReg( cTIPersistADOSQLServer, 'UserName', 'null') ;
  Password := ReadFromReg( cTIPersistADOSQLServer, 'Password', 'null') ;
  CanCreateDatabase := false ;
end;

{ TTestTIDatabaseADOSQLServer }

procedure TTestTIDatabaseADOSQLServer.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

procedure TTestTIDatabaseADOSQLServer.DatabaseExists;
begin
  try
    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

{ TTestDBConnectionSetupADOSQLServer }

function TTestDBConnectionSetupADOSQLServer.PerLayerID : ShortString ;
begin
  Result := cTIPersistADOSQLServer ;
end;

end.

unit tiPerFrameworkDOA_TST;

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
  TPerFrameworkSetupDOA = class( TPerFrameworkSetup )
  public
    constructor Create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupDOA = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseDOA = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIDatabaseConnectionDOA = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryDOA = class( TTestTIQuerySQL ) ;

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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupDOA.Create( TTestTIDatabaseDOA.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupDOA.Create( TTestTIDatabaseConnectionDOA.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupDOA.Create( TTestTIQueryDOA.Suite ));
end ;

{ TPerFrameworkSetupDOA }

constructor TPerFrameworkSetupDOA.create;
begin
  inherited;
  PerLayerName := cTIPersistDOA ;
  DBName   := ReadFromReg( cTIPersistDOA, 'DBName',   'EnterValueHere' ) ;
  UserName := ReadFromReg( cTIPersistDOA, 'UserName', 'EnterValueHere') ;
  Password := ReadFromReg( cTIPersistDOA, 'Password', 'EnterValueHere') ;
  CanCreateDatabase := false ;
end;

{ TTestTIDatabaseDOA }

procedure TTestTIDatabaseDOA.CreateDatabase;
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

procedure TTestTIDatabaseDOA.DatabaseExists;
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

{ TTestDBConnectionSetupDOA }

function TTestDBConnectionSetupDOA.PerLayerID : ShortString ;
begin
  Result := cTIPersistDOA ;
end;

end.

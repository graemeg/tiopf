unit tiPerFrameworkIBO_TST;

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
  TPerFrameworkSetupIBO = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupIBO = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseIBO = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIDatabaseConnectionIBO = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryIBO = class( TTestTIQuerySQL ) ;

procedure RegisterTests;

implementation
uses
   cTIPersist
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiDUnitDependencies
  ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBO.Create( TTestTIDatabaseIBO.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBO.Create( TTestTIDatabaseConnectionIBO.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBO.Create( TTestTIQueryIBO.Suite ));
end ;

{ TPerFrameworkSetupIBO }

constructor TPerFrameworkSetupIBO.create;
begin
  inherited;
  PerLayerName := cTIPersistIBO ;
  DBName   := ReadFromReg( cTIPersistIBO, 'DBName', gTestDataRoot + '.ib' ) ;
  UserName := ReadFromReg( cTIPersistIBO, 'UserName', 'SYSDBA') ;
  Password := ReadFromReg( cTIPersistIBO, 'Password', 'masterkey') ;
  CanCreateDatabase := false ;
end;

{ TTestTIDatabaseIBO }

// Assumes database is on same machine as tests are running.
procedure TTestTIDatabaseIBO.CreateDatabase;
var
  lDB : string ;
  lDBExists : boolean ;
begin
  lDB := ExpandFileName( PerFrameworkSetup.DBName ) ;
  lDB := tiSwapExt( lDB, 'tmp' ) ;
  if FileExists( lDB ) then
  begin
    SysUtils.DeleteFile( lDB ) ;
    if FileExists( lDB ) then
      Fail( 'Can not remove old database file' ) ;
  end ;

  Check( not FileExists( lDB ), 'Database exists when it should not' ) ;
  FDatabaseClass.CreateDatabase(
    lDB,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) ;
  Check( FileExists( lDB ), 'Database not created' ) ;

  lDBExists :=
    FDatabaseClass.DatabaseExists(
      lDB,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;

  Check( lDBExists, 'Database does not exist when it should do' ) ;
  SysUtils.DeleteFile( lDB ) ;
end;

procedure TTestTIDatabaseIBO.DatabaseExists;
var
  lDB : string ;
  lDBExists : boolean ;
begin
  lDB := PerFrameworkSetup.DBName ;
  Check( FileExists( lDB ), 'Database file not found so test can not be performed' ) ;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
  Check( lDBExists, 'DBExists returned false when it should return true' ) ;
  Check( not FileExists( lDB + 'Tmp' ), 'Database file found so test can not be performed' ) ;
  lDBExists :=
    FDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName + 'Tmp',
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
  Check( not lDBExists, 'DBExists returned true when it should return false' ) ;
end;

{ TTestDBConnectionSetupIBO }

function TTestDBConnectionSetupIBO.PerLayerID: ShortString;
begin
 Result := cTIPersistIBO ;
end;

end.

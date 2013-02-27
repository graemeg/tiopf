unit tiPerFrameworkIBX_TST;

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
  TPerFrameworkSetupIBX = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupIBX = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseIBX = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIDatabaseConnectionIBX = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryIBX = class( TTestTIQuerySQL ) ;

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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBX.Create( TTestTIDatabaseIBX.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBX.Create( TTestTIDatabaseConnectionIBX.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupIBX.Create( TTestTIQueryIBX.Suite ));
end ;

{ TPerFrameworkSetupIBX }

constructor TPerFrameworkSetupIBX.create;
begin
  inherited;
  PerLayerName := cTIPersistIBX ;
  // This will fail if there is an IP address or machine name in the databasename
  DBName   := ExpandFileName( ReadFromReg( cTIPersistIBX, 'DBName', gTestDataRoot + '.ib' )) ;
  Username := ReadFromReg( cTIPersistIBX, 'Username', 'SYSDBA' ) ;
  Password := ReadFromReg( cTIPersistIBX, 'Password', 'masterkey' );
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseIBX }

// Assumes database is on same machine as tests are running.
procedure TTestTIDatabaseIBX.CreateDatabase;
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

procedure TTestTIDatabaseIBX.DatabaseExists;
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

{ TTestDBConnectionSetupIBX }

function TTestDBConnectionSetupIBX.PerLayerID : ShortString ;
begin
  result := cTIPersistIBX ;
end;

end.

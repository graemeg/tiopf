unit tiPerFrameworkADOAccess_TST;

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
  TPerFrameworkSetupADOAccess = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupADOAccess = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  // ThreadedDBConnectionPool will hang for the second call, probably because
  // of a bug in tiCOM which is calling CoCreate.
  TTestTIDatabaseADOAccess = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end;

  TTestTIDatabaseConnectionADOAccess = class( TTestTIDatabaseConnectionAbs )
  published
    procedure ThreadedDBConnectionPool ; override ;
  end ;

  TTestTIQueryADOAccess = class( TTestTIQuerySQL )
  published
    // Testing of stream support under construction
    // procedure ParamAsStream ; override ;
  end ;


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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOAccess.Create( TTestTIDatabaseADOAccess.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOAccess.Create( TTestTIDatabaseConnectionADOAccess.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupADOAccess.Create( TTestTIQueryADOAccess.Suite ));
end ;

{ TPerFrameworkSetupADOAccess }

constructor TPerFrameworkSetupADOAccess.create;
begin
  inherited;
  PerLayerName  := cTIPersistADOAccess ;
  DBName   := ReadFromReg( cTIPersistADOAccess, 'DBUserName', gTestDataRoot + '.mdb' ) ;
  UserName := ReadFromReg( cTIPersistADOAccess, 'UserName', 'null' ) ;
  Password := ReadFromReg( cTIPersistADOAccess, 'Password', 'null' ) ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseADOAccess }

procedure TTestTIDatabaseADOAccess.CreateDatabase;
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

procedure TTestTIDatabaseADOAccess.DatabaseExists;
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

procedure TTestTIDatabaseConnectionADOAccess.ThreadedDBConnectionPool;
begin
  // Do nothing. This test will fail, or at least run very slowly and will
  // appear to lockup the machine. I'm told this is due to the JetEngine, but
  // don't know enough about ADO to fix.

  // Well, we have had a shot at fixing this buy adding an extra call to
  // CoInitialize(nil) in the initialization section of tiCOM, but it has
  // not fixed the problem.
  // However, it is only noticed when the ADO layer is being tested at the
  // same time as another layer (say BDEParadox).
  // Ask on the mailing list (www.techinsite.com.au\tiOPF\MailingList.htm)
  // for more information if you think you can help out with a fix.
  LogWarning( 'There are problems with multithreading and the ADO persistence layer.' ) ;
  LogWarning( 'See comments in TTestPerFrameworkADOAccess.ThreadedDBConnectionPool for more information.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestDBConnectionSetupADOAccess }

function  TTestDBConnectionSetupADOAccess.PerLayerID : ShortString ;
begin
  result := cTIPersistADOAccess ;
end;

end.

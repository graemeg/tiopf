unit tiPerFrameworkTAB_TST;

{$I tiDefines.inc}

interface
uses
   tiQueryAbs_TST
  ,tiQueryNonSQL_TST
  ,tiDBConnectionSetupAbs_TST
  ,tiPersistAbs_TST
  ;
  
type

  // Parent found in tiDBConnectionSetupAbs_TST.pas
  TPerFrameworkSetupTAB = class( TPerFrameworkSetup )
  public
    constructor Create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupTAB = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseTAB = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end;

  TTestTIDatabaseConnectionTAB = class( TTestTIDatabaseConnectionAbs )
  published
    procedure ThreadedDBConnectionPool ; override ;
  end ;

  TTestTIQueryTAB = class( TTestTIQueryNonSQL )
  published
  end;


procedure RegisterTests;

implementation
uses
  cTIPersist
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiLog
  ,tiDUnitDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupTAB.Create( TTestTIDatabaseTAB.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupTAB.Create( TTestTIDatabaseConnectionTAB.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupTAB.Create( TTestTIQueryTAB.Suite ));
end ;

{ TPerFrameworkSetupTAB }

constructor TPerFrameworkSetupTAB.create;
begin
  inherited;
  PerLayerName := cTIPersistTAB ;
  DBName   := ExpandFileName(ReadFromReg( cTIPersistTAB, 'DBName', gTestDataRoot + 'TAB' )) ;
  UserName := ReadFromReg( cTIPersistTAB, 'UserName', 'null') ;
  Password := ReadFromReg( cTIPersistTAB, 'Password', 'null') ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseTAB }

procedure TTestTIDatabaseTAB.CreateDatabase;
var
  lDir : string ;
begin
  lDir := PerFrameworkSetup.DBName ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseTAB.DatabaseExists;
var
  lDir : string ;
begin
  lDir := PerFrameworkSetup.DBName ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseConnectionTAB.ThreadedDBConnectionPool;
begin
  LogWarning( 'The TAB persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestDBConnectionSetupTAB }

function TTestDBConnectionSetupTAB.PerLayerID: ShortString;
begin
  result := cTIPersistTAB ;
end;

end.

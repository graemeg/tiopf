unit tiPerFrameworkCSV_TST;

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
  TPerFrameworkSetupCSV = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupCSV = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseCSV = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end;

  TTestTIDatabaseConnectionCSV = class( TTestTIDatabaseConnectionAbs )
  published
    procedure ThreadedDBConnectionPool ; override ;
  end ;

  TTestTIQueryCSV = class( TTestTIQueryNonSQL )
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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupCSV.Create( TTestTIDatabaseCSV.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupCSV.Create( TTestTIDatabaseConnectionCSV.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupCSV.Create( TTestTIQueryCSV.Suite ));
end ;

{ TPerFrameworkSetupCSV }

constructor TPerFrameworkSetupCSV.create;
begin
  inherited;
  PerLayerName := cTIPersistCSV ;
  DBName   := ExpandFileName( ReadFromReg( cTIPersistCSV, 'DBName', gTestDataRoot + 'CSV' )) ;
  UserName := ReadFromReg( cTIPersistCSV, 'UserName', 'null') ;
  Password := ReadFromReg( cTIPersistCSV, 'Password', 'null') ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseCSV }

procedure TTestTIDatabaseCSV.CreateDatabase;
var
  lDir : string ;
begin
  lDir := PerFrameworkSetup.DBName ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseCSV.DatabaseExists;
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

procedure TTestTIDatabaseConnectionCSV.ThreadedDBConnectionPool;
begin
  LogWarning( 'The CSV persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestDBConnectionSetupCSV }

function TTestDBConnectionSetupCSV.PerLayerID: ShortString;
begin
  result := cTIPersistCSV ;
end;

end.

unit tiPerFrameworkBDEParadox_TST;

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
  TPerFrameworkSetupBDEParadox = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupBDEParadox = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseBDEParadox = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure Transaction_RollBack ; override ;
  end ;

  TTestTIDatabaseConnectionBDEParadox = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryBDEParadox = class( TTestTIQuerySQL )
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
  ,tiDUnitDependencies
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiLog
  ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupBDEParadox.Create( TTestTIDatabaseBDEParadox.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupBDEParadox.Create( TTestTIDatabaseConnectionBDEParadox.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupBDEParadox.Create( TTestTIQueryBDEParadox.Suite ));
end ;

{ TPerFrameworkSetupBDEParadox }

constructor TPerFrameworkSetupBDEParadox.create;
begin
  inherited;
  PerLayerName := cTIPersistBDEParadox ;
  DBName   := ExpandFileName(ReadFromReg( cTIPersistBDEParadox, 'DBName', gTestDataRoot + 'Paradox' ));
  UserName := ReadFromReg( cTIPersistBDEParadox, 'UserName', 'null' ) ;
  Password := ReadFromReg( cTIPersistBDEParadox, 'Password', 'null' ) ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseBDEParadox }

procedure TTestTIDatabaseBDEParadox.CreateDatabase;
var
  lDir : string ;
begin
  lDir := tiUtils.tiGetTempFile('') ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(lDir, 'null', 'null' );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseBDEParadox.DatabaseExists;
var
  lDir : string ;
begin
  lDir := tiUtils.tiGetTempFile('') ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(lDir, 'null', 'null' ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(lDir);
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(lDir, 'null', 'null' ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseBDEParadox.Transaction_RollBack;
begin
  // inherited;
  // RollBack not supported in Paradox tables without indexes.
  // See note in TtiDatabaseBDEParadox.RollBack
  // Not may users of the BDEParadox layer these days so we won't spend the
  // time implementing a test.
  LogWarning( 'TtiDatabaseBDEParadox.RollBack not tested' ) ;
end;

{ TTestDBConnectionSetupBDEParadox }

function TTestDBConnectionSetupBDEParadox.PerLayerID : ShortString ;
begin
  result := cTIPersistBDEParadox ;
end;

end.

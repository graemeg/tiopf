unit tiPerFrameworkNx1_TST;

{$I tiDefines.inc}

interface
uses
  tiPerFrameworkAbs_TST
  ,tiDBConnectionSetupAbs_TST
  ,tiPersistAbs_TST
  ;

type

  // Parent found in tiDBConnectionSetupAbs_TST.pas
  TPerFrameworkSetupNx1 = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupNx1 = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseNx1 = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end ;

  TTestTIDatabaseConnectionNx1 = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryNx1 = class( TTestTIQuerySQL );
  

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
  ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupNx1.Create( TTestTIDatabaseNx1.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupNx1.Create( TTestTIDatabaseConnectionNx1.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupNx1.Create( TTestTIQueryNx1.Suite ));
end ;

{ TPerFrameworkSetupNx1 }

constructor TPerFrameworkSetupNx1.create;
begin
  inherited;
  PerLayer := cTIPersistNx1 ;
  { Don't use relative file paths here. }
  DBName   := ReadFromReg( cTIPersistNx1, 'DBName',
          ExpandFileName( gTestDataRoot + 'NexusDb1' ));
  UserName := ReadFromReg( cTIPersistNx1, 'UserName', 'null' ) ;
  Password := ReadFromReg( cTIPersistNx1, 'Password', 'null' ) ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseNx1 }

procedure TTestTIDatabaseNx1.CreateDatabase;
var
  lDir : string ;
begin
  lDir := tiUtils.tiGetTempFile('') ;
  tiForceRemoveDir(lDir);
  Check(not DirectoryExists(lDir), '<' + lDir + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(lDir, 'null', 'null' );
  Check(DirectoryExists(lDir), '<' + lDir + '> Does not exists when it should');
end;

procedure TTestTIDatabaseNx1.DatabaseExists;
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

{ TTestDBConnectionSetupNx1 }

function TTestDBConnectionSetupNx1.PerLayerID : ShortString ;
begin
  result := cTIPersistNx1 ;
end;

end.

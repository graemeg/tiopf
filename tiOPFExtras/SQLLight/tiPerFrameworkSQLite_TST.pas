unit tiPerFrameworkSQLite_TST;


interface
uses
   tiQueryAbs_TST
  ,tiQuerySQL_TST
  ,tiDBConnectionSetupAbs_TST
  ,tiPersistAbs_TST
  ;
{$I tiDefines.inc}

type

  // Parent found in tiDBConnectionSetupAbs_TST.pas
  TPerFrameworkSetupSQLite = class(TPerFrameworkSetup)
  public
    constructor create; override;
  end;
  
  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupSQLite = class(TTestDBConnectionSetup)
  protected
    function PerLayerID: ShortString; override;
  end;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseSQLite = class(TTestTIDatabaseAbs)
  published
    procedure CreateDatabase; override;
    procedure DatabaseExists; override;
  end;

  TTestTIDatabaseConnectionSQLite = class(TTestTIDatabaseConnectionAbs)
  end;

  TTestTIQuerySQLite = class(TTestTIQuerySQL)
  end;
  

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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupSQLite.Create( TTestTIDatabaseSQLite.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupSQLite.Create( TTestTIDatabaseConnectionSQLite.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupSQLite.Create( TTestTIQuerySQLite.Suite ));
end ;

{ TPerFrameworkSetupSQLite }

{
*************************** TPerFrameworkSetupSQLite ***************************
}
constructor TPerFrameworkSetupSQLite.create;
begin
  inherited;
  PerLayerName := cTIPersistSQLite ;
  DBName   := ExpandFileName(ReadFromReg( cTIPersistSQLite, 'DBName', gTestDataRoot + 'SQLite' ));
  UserName := ReadFromReg( cTIPersistSQLite, 'UserName', 'null' ) ;
  Password := ReadFromReg( cTIPersistSQLite, 'Password', 'null' ) ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseSQLite }

{
**************************** TTestTIDatabaseSQLite *****************************
}
procedure TTestTIDatabaseSQLite.CreateDatabase;
var
  lFile: string;
begin
  lFile := tiUtils.tiGetTempFile('');
  SysUtils.DeleteFile( lFile );
  Check(not FileExists(lFile), 'TTestTIDatabaseSQLite: <' + lFile + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(lFile, 'null', 'null' );
  Check(FileExists(lFile), 'TTestTIDatabaseSQLite: <' + lFile + '> Does not exists when it should');
end;

procedure TTestTIDatabaseSQLite.DatabaseExists;
var
  lFile: string;
begin
  lFile := tiUtils.tiGetTempFile('') ;
  SysUtils.DeleteFile( lFile );
  Check(not FileExists(lFile), 'TTestTIDatabaseSQLite: <' + lFile + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(lFile, 'null', 'null' ),
        'TTestTIDatabaseSQLite: FDatabaseClass.DatabaseExists()=true when it should =false');
  ForceDirectories(ExtractFileDir(lFile));
  FDatabaseClass.CreateDatabase(lFile, 'null', 'null' );
  Check(FileExists(lFile), 'TTestTIDatabaseSQLite: <' + lFile + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(lFile, 'null', 'null' ),
        'TTestTIDatabaseSQLite: FDatabaseClass.DatabaseExists()=false when it should =true');
end;

function TTestDBConnectionSetupSQLite.PerLayerID: ShortString;
begin
  result := cTIPersistSQLite ;
end;


end.

unit tiPerFrameworkXMLLight_TST;

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
  TPerFrameworkSetupXMLLight = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupXMLLight = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseXMLLight = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end;

  TTestTIDatabaseConnectionXMLLight = class( TTestTIDatabaseConnectionAbs )
  published
    procedure ThreadedDBConnectionPool ; override ;
  end ;

  TTestTIQueryXMLLight = class( TTestTIQueryNonSQL );

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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXMLLight.Create( TTestTIDatabaseXMLLight.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXMLLight.Create( TTestTIDatabaseConnectionXMLLight.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXMLLight.Create( TTestTIQueryXMLLight.Suite ));
end ;

{ TPerFrameworkSetupXMLLight }

constructor TPerFrameworkSetupXMLLight.create;
begin
  inherited;
  PerLayerName := cTIPersistXMLLight ;
  DBName   := ExpandFileName(ReadFromReg( cTIPersistXMLLight, 'DBName', gTestDataRoot + '1.XML' )) ;
  Username := ReadFromReg( cTIPersistXMLLight, 'Username', 'null') ;
  Password := ReadFromReg( cTIPersistXMLLight, 'Password', 'null') ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseXMLLight }

procedure TTestTIDatabaseXMLLight.CreateDatabase;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXMLLight.DatabaseExists;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  Check(not FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=true when it should =false');
  tiStringToFile('test',lFileName);
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
  Check(FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password ),
        'FDatabaseClass.DatabaseExists()=false when it should =true');
end;

procedure TTestTIDatabaseConnectionXMLLight.ThreadedDBConnectionPool;
begin
  LogWarning( 'The XMLLight persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestDBConnectionSetupXMLLight }

function TTestDBConnectionSetupXMLLight.PerLayerID : ShortString ;
begin
  result := cTIPersistXMLLight ;
end;

end.

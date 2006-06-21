unit tiPerFrameworkXML_TST;

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
  TPerFrameworkSetupXML = class( TPerFrameworkSetup )
  public
    constructor create ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupXML = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseXML = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
  end;

  TTestTIDatabaseConnectionXML = class( TTestTIDatabaseConnectionAbs )
  published
    procedure ThreadedDBConnectionPool ; override ;
  end ;

  TTestTIQueryXML = class( TTestTIQueryNonSQL );

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
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXML.Create( TTestTIDatabaseXML.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXML.Create( TTestTIDatabaseConnectionXML.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupXML.Create( TTestTIQueryXML.Suite ));
end ;

{ TPerFrameworkSetupXML }

constructor TPerFrameworkSetupXML.create;
begin
  inherited;
  PerLayerName := cTIPersistXML ;
  DBName   := ExpandFileName(ReadFromReg( cTIPersistXML, 'DBName', gTestDataRoot + '.xml' )) ;
  Username := ReadFromReg( cTIPersistXML, 'Username', 'null') ;
  Password := ReadFromReg( cTIPersistXML, 'Password', 'null') ;
  CanCreateDatabase := true ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseXML }

procedure TTestTIDatabaseXML.CreateDatabase;
var
  lFileName : string ;
begin
  lFileName := PerFrameworkSetup.DBName ;
  SysUtils.DeleteFile(lFileName);
  Check(not FileExists(lFileName), '<' + lFileName + '> Exists when it should not');
  FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
  Check(FileExists(lFileName), '<' + lFileName + '> Does not exists when it should');
end;

procedure TTestTIDatabaseXML.DatabaseExists;
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

procedure TTestTIDatabaseConnectionXML.ThreadedDBConnectionPool;
begin
  LogWarning( 'The XML persistence layer can only manage one thread.' ) ;
  DoThreadedDBConnectionPool( 1 ) ;
end;

{ TTestDBConnectionSetupXML }

function TTestDBConnectionSetupXML.PerLayerID : ShortString ;
begin
  result := cTIPersistXML ;
end;

end.

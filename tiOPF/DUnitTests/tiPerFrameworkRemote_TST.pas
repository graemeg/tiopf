unit tiPerFrameworkRemote_TST;

{$I tiDefines.inc}

interface
uses
   tiQueryAbs_TST
  ,tiQuerySQL_TST
  ,tiDBConnectionSetupAbs_TST
  ,tiPersistAbs_TST
  ;

const
  cErrorCanNotLoadtiDBProxyServer = 'Can not load tiDBProxyServer'#13'' +
                                    'File location: %s'#13'' +
                                    'Error message: %s' ;

  // ToDo: cErrorFileNotFound should be a generic exception
  cErrorFileNotFound = 'File not found' ;

type

  // Parent found in tiDBConnectionSetupAbs_TST.pas
  TPerFrameworkSetupRemote = class( TPerFrameworkSetup )
  private
    FHadToLoadServer : boolean ;
  public
    constructor create    ; override ;
    destructor  destroy   ; override ;
    procedure   DoRunOnce ; override ;
  end ;

  // Parent found in tiPersistAbs_TST.pas
  TTestDBConnectionSetupRemote = class( TTestDBConnectionSetup )
  protected
    function  PerLayerID : ShortString ; override ;
  end ;

  // Parent found in tiQueryAbs_TST.pas
  TTestTIDatabaseRemote = class( TTestTIDatabaseAbs )
  published
    procedure DatabaseExists ; override ;
    procedure CreateDatabase ; override ;
    procedure Transaction_TimeOut ;
  end ;

  TTestTIDatabaseConnectionRemote = class( TTestTIDatabaseConnectionAbs );

  TTestTIQueryRemote = class( TTestTIQuerySQL ) ;

procedure RegisterTests;

implementation
uses
  cTIPersist
  ,TestFramework
  ,SysUtils
  ,tiUtils
  ,tiDUnitDependencies
  ,tiQuery
  ,Windows
  ,Messages
  ,tiDialogs
  ;

const
  cRemoteServerMainFormName = 'TFormMainTIDBProxyServer' ;

procedure RegisterTests ;
begin
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupRemote.Create( TTestTIDatabaseRemote.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupRemote.Create( TTestTIDatabaseConnectionRemote.Suite ));
  RegisterTest( cTIQueryTestName, TTestDBConnectionSetupRemote.Create( TTestTIQueryRemote.Suite ));
end ;

{ TPerFrameworkSetupRemote }

constructor TPerFrameworkSetupRemote.create;
begin
  inherited;
  FHadToLoadServer := false ;
  PerLayerName := cTIPersistRemote ;
  DBName   := ReadFromReg( cTIPersistRemote, 'DBName',    cLocalHost ) ;
  Username := ReadFromReg( cTIPersistRemote, 'Username', 'null'    ) ;
  Password := ReadFromReg( cTIPersistRemote, 'Password', 'null' );
  CanCreateDatabase := false ;
  ForceTestDataDirectory ;
end;

{ TTestTIDatabaseRemote }

// Assumes database is on same machine as tests are running.
procedure TTestTIDatabaseRemote.CreateDatabase;
begin
  try
    FDatabaseClass.CreateDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('CreateDatabase not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

procedure TTestTIDatabaseRemote.DatabaseExists;
begin
  try
    FDatabaseClass.DatabaseExists(PerFrameworkSetup.DBName, PerFrameworkSetup.Username, PerFrameworkSetup.Password );
    Fail('Exception not raised when it should have been');
  except
    on e:exception do
    begin
      CheckIs(e, EAssertionFailed);
      Check(Pos('DatabaseExists not implemented in ' + FDatabaseClass.ClassName, e.Message)<>0);
    end ;
  end ;
end;

destructor TPerFrameworkSetupRemote.destroy;
var
  lHandle : THandle ;
begin
  lHandle := FindWindow(nil, pchar(cRemoteServerMainFormCaption)) ;
  if FHadToLoadServer and ( lHandle <> 0 ) then
    PostMessage(lHandle, WM_Quit, 0, 0);
  inherited;
end;

procedure TPerFrameworkSetupRemote.DoRunOnce;
var
  lEXE : string ;
  lHandle : THandle ;
begin
{
  try
    lHandle := FindWindow(nil, pchar(cRemoteServerMainFormCaption)) ;
    if lHandle = 0 then
    begin
      lEXE := ExtractFilePath(ParamStr(0)) + '\' + 'tiDBProxyServer.exe' ;
      if not FileExists(lEXE) then
        raise exception.create(cErrorFileNotFound);
      tiShellExecute(lEXE);
      FHadToLoadServer := true ;
    end;
  except
    on e:exception do
      tiAppError(Format(cErrorCanNotLoadtiDBProxyServer,[lEXE, e.message]));
  end;
}  
end;

{ TTestDBConnectionSetupRemote }

function TTestDBConnectionSetupRemote.PerLayerID : ShortString ;
begin
  result := cTIPersistRemote ;
end;

procedure TTestTIDatabaseRemote.Transaction_TimeOut;
var
  lQuery : TtiQuery ;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  try FDatabase.DropTable(cTableNameTestGroup) except end ;

  CreateTableTestGroup(FDatabase);

  FDatabase.StartTransaction ;
  try
    InsertIntoTestGroup(FDatabase, 1);
    FDatabase.Commit ;
    lQuery := FRegPerLayer.tiQueryClass.Create;
    try
      lQuery.AttachDatabase(FDatabase);
      FDatabase.StartTransaction ;
      lQuery.SelectRow(cTableNameTestGroup, nil);
      Check( not lQuery.EOF, 'Transaction not committed' ) ;
      lQuery.Next ;
      Check( lQuery.EOF, 'Wrong number of records' ) ;
      Sleep( Trunc( cDBProxyServerTimeOut * 60000 * 1.5 )) ;
      try
        FDatabase.Commit ;
        Fail('tiDBProxyServer did not time out as expected');
      except
        on e:exception do
          Check( Pos( 'TIMED OUT', UpperCase( e.message )) <> 0,
                 'tiDBProxyServer did not raise the right exception. Exception message: ' + e.message ) ;
      end ;
    finally
      lQuery.Free;
    end;
  finally
    FDatabase.DropTable(cTableNameTestGroup);
  end;
end;

end.

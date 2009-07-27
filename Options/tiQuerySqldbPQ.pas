{
  This persistence layer uses standard Free Pascal SQLDB (PostGreSQL) components.

  The connection string format is show below.

  eg:
    GTIOPFManager.ConnectDatabase('192.168.0.20:mydatabasename',
        'myusername', 'mypassword', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbPQ;

{$I tiDefines.inc}

// for debug purposes only
{.$define LOGSQLDB}

interface

uses
  tiQuery
  ,Classes
  ,db
  ,sqldb
  ,PQConnection
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type

  { TtiPersistenceLayerSqldPQ }

  TtiPersistenceLayerSqldPQ = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;
  { TtiDatabaseSQLDBPQ }

  TtiDatabaseSQLDBPQ = Class(TtiDatabaseSQLDB)
  protected
    Class Function CreateSQLConnection : TSQLConnection; override;
    function    HasNativeLogicalType: boolean; override;
  end;



implementation

uses
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  tiOPFManager,
  tiConstants;


{ TtiPersistenceLayerSqldPQ }

procedure TtiPersistenceLayerSqldPQ.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbPQ;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.Username:= 'SYSDBA';
  APersistenceLayerDefaults.Password:= 'masterkey';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldPQ.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBPQ;
end;

function TtiPersistenceLayerSqldPQ.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbPQ;
end;


{ TtiDatabaseSQLDBPQ }

Class function TtiDatabaseSQLDBPQ.CreateSQLConnection: TSQLConnection;
begin
  Result:=TPQConnection.Create(Nil);
end;


function TtiDatabaseSQLDBPQ.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldPQ);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbPQ);

end.

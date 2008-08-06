{
  This persistence layer uses standard Free Pascal SQLDB (Oracle) components.

  The connection string format is the same as the standard Oracle persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('oracleconn','admin', 'admin', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbOracle;

{$I tiDefines.inc}

interface

uses
  tiQuery
  ,Classes
  ,db
  ,sqldb
  ,OracleConnection
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type

  { TtiPersistenceLayerSqldOracle }

  TtiPersistenceLayerSqldOracle = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;
  { TtiDatabaseSQLDBOracle }

  TtiDatabaseSQLDBOracle = Class(TtiDatabaseSQLDB)
  protected
    Class Function CreateSQLConnection : TSQLConnection; override;
    function    HasNativeLogicalType: boolean; override;
  end;



implementation

{ $define LOGSQLDB}
uses
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  tiOPFManager,
  tiConstants;


{ TtiPersistenceLayerSqldOracle }

procedure TtiPersistenceLayerSqldOracle.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbOracle;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.Username:= 'admin';
  APersistenceLayerDefaults.Password:= 'admin';
  APersistenceLayerDefaults.CanCreateDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldOracle.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBOracle;
end;

function TtiPersistenceLayerSqldOracle.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbOracle;
end;


{ TtiDatabaseSQLDBOracle }

Class function TtiDatabaseSQLDBOracle.CreateSQLConnection: TSQLConnection;
begin
  Result:=TOracleConnection.Create(Nil);
end;


function TtiDatabaseSQLDBOracle.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldOracle);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbOracle);

end.

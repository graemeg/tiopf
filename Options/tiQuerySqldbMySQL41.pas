{
  This persistence layer uses standard Free Pascal SQLDB (MySQL 41) components.

  The connection string format is the same as the standard MySQL persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('test','root', '', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbMySQL41;

{$I tiDefines.inc}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,MySQL41Conn
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type

  { TtiPersistenceLayerSqldMySQL41 }

  TtiPersistenceLayerSqldMySQL41 = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;
  { TtiDatabaseSQLDBMySQL41 }

  TtiDatabaseSQLDBMySQL41 = Class(TtiDatabaseSQLDB)
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


{ TtiPersistenceLayerSqldMySQL41 }

procedure TtiPersistenceLayerSqldMySQL41.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbMySQL41;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.Username:= 'root';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldMySQL41.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBMySQL41;
end;

function TtiPersistenceLayerSqldMySQL41.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbMySQL41;
end;


{ TtiDatabaseSQLDBMySQL41 }

Class function TtiDatabaseSQLDBMySQL41.CreateSQLConnection: TSQLConnection;
begin
  Result:=TMySQL41Connection.Create(Nil);
end;


function TtiDatabaseSQLDBMySQL41.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldMySQL41);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbMySQL41);

end.

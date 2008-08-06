{
  This persistence layer uses standard Free Pascal SQLDB (MySQL 40) components.

  The connection string format is the same as the standard MySQL persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('test','root', '', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbMySQL40;

{$I tiDefines.inc}

interface

uses
  tiQuery
  ,Classes
  ,db
  ,sqldb
  ,MySQL40Conn
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type

  { TtiPersistenceLayerSqldMySQL40 }

  TtiPersistenceLayerSqldMySQL40 = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;
  { TtiDatabaseSQLDBMySQL40 }

  TtiDatabaseSQLDBMySQL40 = Class(TtiDatabaseSQLDB)
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


{ TtiPersistenceLayerSqldMySQL40 }

procedure TtiPersistenceLayerSqldMySQL40.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbMySQL40;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.Username:= 'root';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldMySQL40.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBMySQL40;
end;

function TtiPersistenceLayerSqldMySQL40.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbMySQL40;
end;


{ TtiDatabaseSQLDBMySQL40 }

Class function TtiDatabaseSQLDBMySQL40.CreateSQLConnection: TSQLConnection;
begin
  Result:=TMySQL40Connection.Create(Nil);
end;


function TtiDatabaseSQLDBMySQL40.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldMySQL40);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbMySQL40);

end.

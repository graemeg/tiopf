{
  This persistence layer uses standard Free Pascal SQLDB (MySQL 50) components.

  The connection string format is the same as the standard MySQL persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('test','root', '', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbMySQL50;

{$I tiDefines.inc}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,MySQL50Conn
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type

  { TtiPersistenceLayerSqldMySQL50 }

  TtiPersistenceLayerSqldMySQL50 = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;
  { TtiDatabaseSQLDBMySQL50 }

  TtiDatabaseSQLDBMySQL50 = Class(TtiDatabaseSQLDB)
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


{ TtiPersistenceLayerSqldMySQL50 }

procedure TtiPersistenceLayerSqldMySQL50.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbMySQL50;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.Username:= 'root';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldMySQL50.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBMySQL50;
end;

function TtiPersistenceLayerSqldMySQL50.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbMySQL50;
end;


{ TtiDatabaseSQLDBMySQL50 }

Class function TtiDatabaseSQLDBMySQL50.CreateSQLConnection: TSQLConnection;
begin
  Result:=TMySQL50Connection.Create(Nil);
end;


function TtiDatabaseSQLDBMySQL50.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldMySQL50);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbMySQL50);

end.

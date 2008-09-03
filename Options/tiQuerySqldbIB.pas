{
  This persistence layer uses standard Free Pascal sqlDB (interbase) components.

  The connection string format is the same as the standard Interbase/Firebird
  persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', '');


  Authors:  Graeme Geldenhuys (graemeg@gmail.com) - Feb 2006
            Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbIB;

{$I tiDefines.inc}

{.$Define LOGSQLDB}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,IBConnection
  ,tiPersistenceLayers
  ,tiQuerySqldb
  ;

type
  TtiPersistenceLayerSqldIB = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseSQLDBIB = class(TtiDatabaseSQLDB)
  protected
    class function CreateSQLConnection: TSQLConnection; override;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    function HasNativeLogicalType: Boolean; override;
  end;


implementation

uses
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  tiOPFManager,
  tiConstants;


{ TtiPersistenceLayerSqldIB }

procedure TtiPersistenceLayerSqldIB.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistSqldbIB;
  APersistenceLayerDefaults.DatabaseName      := CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.Username          := 'SYSDBA';
  APersistenceLayerDefaults.Password          := 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase   := True;
  APersistenceLayerDefaults.CanCreateDatabase := True;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL     := True;
end;

function TtiPersistenceLayerSqldIB.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseSQLDBIB;
end;

function TtiPersistenceLayerSqldIB.GetPersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;


{ TtiDatabaseSQLDBIB }

class function TtiDatabaseSQLDBIB.CreateSQLConnection: TSQLConnection;
begin
  Result := TIBConnection.Create(nil);
  TIBConnection(Result).Dialect := 3;
end;

function TtiDatabaseSQLDBIB.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
  if AFieldMetaData.Kind = qfkDateTime then
  begin
    // Take into account dialect
    if TIBConnection(SQLConnection).Dialect <> 1 then
      Result := 'TIMESTAMP'
    else
      Result := 'Date';
  end
  else
    Result := inherited FieldMetaDataToSQLCreate(AFieldMetaData);
end;

function TtiDatabaseSQLDBIB.HasNativeLogicalType: Boolean;
begin
  Result := False;
end;


initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldIB);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbIB);

end.


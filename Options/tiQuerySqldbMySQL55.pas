{
  This persistence layer uses standard Free Pascal SQLDB (MySQL 5.5) components.

  The connection string format is the same as the standard MySQL persistence layers.

  eg:
    GTIOPFManager.ConnectDatabase('test','root', '', '');

}

unit tiQuerySqldbMySQL55;

{$I tiDefines.inc}

interface

uses
  tiQuery
  ,Classes
  ,SysUtils
  ,sqldb
  ,MySQL55Conn
  ,tiPersistenceLayers
  ,tiQuerySqldb
  ;

type

  TtiPersistenceLayerSqldMySQL55 = class(TtiPersistenceLayerSqldDB)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseSQLDBMySQL55 = class(TtiDatabaseSQLDB)
  protected
    class function CreateSQLConnection: TSQLConnection; override;
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    function HasNativeLogicalType: boolean; override;
  end;



implementation

{ $define LOGSQLDB}
uses
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  tiOPFManager,
  tiConstants,
  tiExcept;


{ TtiPersistenceLayerSqldMySQL55 }

procedure TtiPersistenceLayerSqldMySQL55.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistSqldbMySQL55;
  APersistenceLayerDefaults.DatabaseName := CDefaultDatabaseName;
  APersistenceLayerDefaults.IsDatabaseNameFilePath := False;
  APersistenceLayerDefaults.Username := 'root';
  APersistenceLayerDefaults.Password := '';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
end;

function TtiPersistenceLayerSqldMySQL55.GetDatabaseClass: TtiDatabaseClass;
begin
  result := TtiDatabaseSQLDBMySQL55;
end;

function TtiPersistenceLayerSqldMySQL55.GetPersistenceLayerName: string;
begin
  result := cTIPersistSqldbMySQL55;
end;


{ TtiDatabaseSQLDBMySQL55 }

Class function TtiDatabaseSQLDBMySQL55.CreateSQLConnection: TSQLConnection;
begin
  Result := TMySQL55Connection.Create(Nil);
end;

function TtiDatabaseSQLDBMySQL55.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;

  case AFieldMetaData.Kind of
    qfkString: result := 'VarChar( ' + IntToStr(AFieldMetaData.Width) + ' )';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: result := 'DOUBLE PRECISION';
    qfkDateTime: result := 'datetime';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical    : result := 'Char(1) default ''F''';
    {$ELSE}
    qfkLogical    : result := 'tinyint(1) default ''0''';
    {$ENDIF}
    qfkBinary: result := 'Blob';
    qfkLongString: result := 'longtext';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseSQLDBMySQL55.HasNativeLogicalType: boolean;
begin
  Result := False;
end;


initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldMySQL55);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbMySQL55);

end.

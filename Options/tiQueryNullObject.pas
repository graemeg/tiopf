{
  This is a persistence layer that doesn't actually do anything. You can use
  this when you want to use gTIOPFManager.VisitorManager for visitor management,
  but don't actually need persistence to a file or database.

  As an example, this persistence layer has been used when consuming a REST API.
}
unit tiQueryNullObject;

{$I tiDefines.inc}

interface

uses
  Classes,
  tiObject,
  tiQuery,
  tiPersistenceLayers,
  tiCriteria;

type

  TtiPersistenceLayerNullObject = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseNullObject = class(TtiDatabase)
  private
    FConnected: boolean;
    FInTransaction: boolean;
  protected
    procedure SetConnected(AValue: boolean); override;
    function GetConnected: boolean; override;
  public
    constructor Create; override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    procedure StartTransaction; override;
    function InTransaction: boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    function Test: boolean; override;
    function TIQueryClass: TtiQueryClass; override;
    procedure ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    procedure DropTable(const ATableMetaData: TtiDBMetaDataTable); override;
    procedure CreateTable(const ATableMetaData: TtiDBMetaDataTable); override;
  end;


  TtiQueryNullObject = class(TtiQueryNonSQL)
  private
    FActive: boolean;
  protected
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function GetEOF: boolean; override;

    function GetFieldAsString(const AName: string): string; override;
    function GetFieldAsFloat(const AName: string): extended; override;
    function GetFieldAsBoolean(const AName: string): boolean; override;
    function GetFieldAsInteger(const AName: string): int64; override;
    function GetFieldAsDateTime(const AName: string): TDateTime; override;
    function GetFieldIsNull(const AName: string): boolean; override;

    function GetFieldAsStringByIndex(AIndex: integer): string; override;
    function GetFieldAsFloatByIndex(AIndex: integer): extended; override;
    function GetFieldAsBooleanByIndex(AIndex: integer): boolean; override;
    function GetFieldAsIntegerByIndex(AIndex: integer): int64; override;
    function GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime; override;
    function GetFieldIsNullByIndex(AIndex: integer): boolean; override;
  public
    constructor Create; override;
    procedure Open; override;
    procedure Close; override;
    function ExecSQL: integer; override;

    procedure SelectRow(const ATableName: string; const AWhere: TtiQueryParams; const ACriteria: TtiCriteria); override;
    procedure SelectRow(const ATableName: string; const AWhere: TtiQueryParams); override;
    procedure InsertRow(const ATableName: string; const AParams: TtiQueryParams); override;
    procedure DeleteRow(const ATableName: string; const AWhere: TtiQueryParams); override;
    procedure UpdateRow(const ATableName: string; const AParams: TtiQueryParams; const AWhere: TtiQueryParams); override;

    procedure Next; override;

    function FieldCount: integer; override;
    function FieldName(AIndex: integer): string; override;
    function FieldIndex(const AName: string): integer; override;
    function FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function FieldSize(AIndex: integer): integer; override;
    function HasNativeLogicalType: boolean; override;
  end;


implementation

uses
  tiOPFManager,
  tiConstants,
  tiLog;

{ TtiPersistenceLayerNullObject }

function TtiPersistenceLayerNullObject.GetPersistenceLayerName: string;
begin
  Result := cTIPersistNullObject;
end;

function TtiPersistenceLayerNullObject.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseNullObject;
end;

function TtiPersistenceLayerNullObject.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryNullObject;
end;

procedure TtiPersistenceLayerNullObject.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistNullObject;
  APersistenceLayerDefaults.DatabaseName  := cLocalHost;
  APersistenceLayerDefaults.IsDatabaseNameFilePath := False;
  APersistenceLayerDefaults.Username      := 'null';
  APersistenceLayerDefaults.Password      := 'null';
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := False;
  APersistenceLayerDefaults.CanSupportSQL := False;
end;

{ TtiDatabaseNullObject }

procedure TtiDatabaseNullObject.SetConnected(AValue: boolean);
begin
  FConnected := AValue;
end;

function TtiDatabaseNullObject.GetConnected: boolean;
begin
  Result := FConnected;
end;

constructor TtiDatabaseNullObject.Create;
begin
  inherited Create;
  FConnected := False;
end;

class function TtiDatabaseNullObject.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string): boolean;
begin
  Result := False;
  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
end;

class procedure TtiDatabaseNullObject.CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
begin
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;

class procedure TtiDatabaseNullObject.DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

procedure TtiDatabaseNullObject.StartTransaction;
begin
  Log(ClassName + ': [Start Trans]', lsSQL);
  FInTransaction := True;
end;

function TtiDatabaseNullObject.InTransaction: boolean;
begin
  Result := FInTransaction;
end;

procedure TtiDatabaseNullObject.Commit;
begin
  FInTransaction := False;
end;

procedure TtiDatabaseNullObject.RollBack;
begin
  FInTransaction := False;
end;

function TtiDatabaseNullObject.Test: boolean;
begin
  Result := False;
  Assert(False, 'Under construction');
end;

function TtiDatabaseNullObject.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQueryNullObject;
end;

procedure TtiDatabaseNullObject.ReadMetaDataTables(AData: TtiDBMetaData);
begin
  // do nothing
end;

procedure TtiDatabaseNullObject.ReadMetaDataFields(AData: TtiDBMetaDataTable);
begin
  // do nothing
end;

procedure TtiDatabaseNullObject.DropTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  // do nothing
end;

procedure TtiDatabaseNullObject.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  // do nothing
end;

{ TtiQueryNullObject }

function TtiQueryNullObject.GetSQL: TStrings;
begin
  Assert(False, ClassName + '.GetSQL not implemented');
  Result := nil;
end;

procedure TtiQueryNullObject.SetSQL(const AValue: TStrings);
begin
  Assert(False, ClassName + '.SetSQL not implemented');
end;

function TtiQueryNullObject.GetActive: boolean;
begin
  Result := FActive;
end;

procedure TtiQueryNullObject.SetActive(const AValue: boolean);
begin
  FActive := AValue;
end;

function TtiQueryNullObject.GetEOF: boolean;
begin
  Result := True;
end;

function TtiQueryNullObject.GetFieldAsString(const AName: string): string;
begin
  Result := '';
end;

function TtiQueryNullObject.GetFieldAsFloat(const AName: string): extended;
begin
  Result := 0;
end;

function TtiQueryNullObject.GetFieldAsBoolean(const AName: string): boolean;
begin
  Result := False;
end;

function TtiQueryNullObject.GetFieldAsInteger(const AName: string): int64;
begin
  Result := 0;
end;

function TtiQueryNullObject.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  Result := 0.0;
end;

function TtiQueryNullObject.GetFieldIsNull(const AName: string): boolean;
begin
  Result := False;
end;

function TtiQueryNullObject.GetFieldAsStringByIndex(AIndex: integer): string;
begin
  Result := '';
end;

function TtiQueryNullObject.GetFieldAsFloatByIndex(AIndex: integer): extended;
begin
  Result := 0;
end;

function TtiQueryNullObject.GetFieldAsBooleanByIndex(AIndex: integer): boolean;
begin
  Result := False;
end;

function TtiQueryNullObject.GetFieldAsIntegerByIndex(AIndex: integer): int64;
begin
  Result := 0;
end;

function TtiQueryNullObject.GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime;
begin
  Result := 0;
end;

function TtiQueryNullObject.GetFieldIsNullByIndex(AIndex: integer): boolean;
begin
  Result := False;
end;

constructor TtiQueryNullObject.Create;
begin
  inherited Create;
  FActive := False;
end;

procedure TtiQueryNullObject.Open;
begin
  Active := True;
end;

procedure TtiQueryNullObject.Close;
begin
  Active := False;
end;

function TtiQueryNullObject.ExecSQL: integer;
begin
  Result := 0;
end;

procedure TtiQueryNullObject.SelectRow(const ATableName: string; const AWhere: TtiQueryParams; const ACriteria: TtiCriteria);
begin
  // do nothing
end;

procedure TtiQueryNullObject.SelectRow(const ATableName: string; const AWhere: TtiQueryParams);
begin
  // do nothing
end;

procedure TtiQueryNullObject.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
begin
  // do nothing
end;

procedure TtiQueryNullObject.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
begin
  // do nothing
end;

procedure TtiQueryNullObject.UpdateRow(const ATableName: string; const AParams: TtiQueryParams; const AWhere: TtiQueryParams);
begin
  // do nothing
end;

procedure TtiQueryNullObject.Next;
begin
  // do nothing
end;

function TtiQueryNullObject.FieldCount: integer;
begin
  Result := 0;
end;

function TtiQueryNullObject.FieldName(AIndex: integer): string;
begin
  Result := '';
end;

function TtiQueryNullObject.FieldIndex(const AName: string): integer;
begin
  Result := -1;
end;

function TtiQueryNullObject.FieldKind(AIndex: integer): TtiQueryFieldKind;
begin
  Result := qfkString;
end;

function TtiQueryNullObject.FieldSize(AIndex: integer): integer;
begin
  Result := 0;
end;

function TtiQueryNullObject.HasNativeLogicalType: boolean;
begin
  Result := False;
end;


initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerNullObject);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistNullObject);

end.


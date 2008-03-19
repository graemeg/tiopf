unit tiQueryASqlite3;

{
 uses the Asqlite library from
   www.aducom.com/sqlite

Persistance Layer writen by
Sean Cross
Source IT Ltd
www.sourceitsoftware.com

}
interface
uses
   tiQuery
  ,Classes
  ,tiAutoMap
  ,tiObject
  ,ASGSQLite3
  ,tiPersistenceLayers
 ;

type

  TtiDatabaseASqlite3 = class(TtiDatabaseSQL)
  private
    FDatabase     : TASqlite3DB;
//    FIgnoreTransactionErrors : Boolean;
  protected
    property  Database : TASqlite3DB read FDatabase write FDatabase;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure   SetupDBParams; 
    function    FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string):boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
//    procedure       CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
//    procedure       DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
  end;


  TtiQueryASqlite3 = class(TtiQuerySQL)
  private
    FQuery : TASqlite3Query;
  protected

    function    GetFieldAsString(const AName: string): string  ; override;
    function    GetFieldAsFloat(const AName: string): extended ; override;
    function    GetFieldAsBoolean(const AName: string): boolean   ; override;
    function    GetFieldAsInteger(const AName: string): Int64; override;
    function    GetFieldAsDateTime(const AName: string):TDateTime; override;

    function    GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function    GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
    function    GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function    GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function    GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function    GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    function    GetActive: boolean; override;
    procedure   SetActive(const AValue: boolean); override;
    function    GetEOF: boolean; override;
    function    GetParamAsString(const AName: string): string; override;
    function    GetParamAsBoolean(const AName: string): boolean; override;
    function    GetParamAsFloat(const AName: string): extended;override;
    function    GetParamAsInteger(const AName: string): Int64;override;
    procedure   SetParamAsString(const AName, AValue: string); override;
    procedure   SetParamAsBoolean(const AName: string;const AValue: boolean);override;
    procedure   SetParamAsFloat(const AName: string; const AValue: extended);override;
    procedure   SetParamAsInteger(const AName: string;const AValue: Int64);override;
    function    GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure   SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;

    function    GetParamIsNull(const AName: String): Boolean; override;
    procedure   SetParamIsNull(const AName: String; const AValue: Boolean); override;
    function    GetFieldIsNull(const AName: string): Boolean; override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   Next   ; override;
    procedure   ExecSQL; override;

    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;

    procedure   AssignParamToStream(const AName : string; const AStream : TStream); override;
    procedure   AssignParamFromStream(const AName : string; const AStream : TStream); override;
    procedure   AssignFieldAsStream(const AName : string; const AStream : TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AValue : TStream); override;

    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase;  override;
    procedure   Reset; override;

    function    FieldCount : integer; override;
    function    FieldName(AIndex : integer): string; override;
    function    FieldIndex(const AName : string): integer; override;
    function    FieldKind(AIndex : integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex : integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

  end;

  TtiPersistenceLayerASqlite3 = class(TtiPersistenceLayer)
  protected
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetPersistenceLayerName: string; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


implementation
uses
  tiLog
  ,tiUtils
  ,tiExcept
  ,SysUtils
  ,DB
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
 ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryASqlite3
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryASqlite3.Create;
begin
  inherited;
  FQuery := TASqlite3Query.Create(nil);
//  FQuery.UniDirectional:= true;
end;

destructor TtiQueryASqlite3.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TtiQueryASqlite3.Close;
begin
  Active := false;
end;

procedure TtiQueryASqlite3.ExecSQL;
begin
  FQuery.UniDirectional:= false;
  FQuery.ExecSQL;
end;

function TtiQueryASqlite3.GetFieldAsBoolean(const AName: string): boolean;
var
  lsValue : string;
begin
  lsValue := upperCase(FQuery.FieldByName(AName).AsString);
  result := (lsValue = 'T'   ) or
            (lsValue = 'TRUE') or
            (lsValue = 'Y'   ) or
            (lsValue = 'YES' ) or
            (lsValue = '1'   );
end;

function TtiQueryASqlite3.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.FieldByName(AName).AsDateTime;
end;

function TtiQueryASqlite3.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQuery.FieldByName(AName).AsFloat;
end;

function TtiQueryASqlite3.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FQuery.FieldByName(AName).AsInteger;
end;

function TtiQueryASqlite3.GetFieldAsString(const AName: string): string;
var
  lField : TField;
  lStream : TStringStream;
begin
  lField := FQuery.FieldByName(AName);
  if lField is TMemoField then
  begin
    lStream := TStringStream.Create('');
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0;
      result := lStream.DataString;
    finally
      lStream.Free;
    end;
  end
  else
    result := lField.AsString;
end;

function TtiQueryASqlite3.GetActive: boolean;
begin
  result := FQuery.Active;
end;

function TtiQueryASqlite3.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryASqlite3.GetParamAsBoolean(const AName: string): boolean;
begin
  result := FQuery.Params.ParamByName(AName).AsBoolean;
end;

function TtiQueryASqlite3.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.Params.ParamByName(AName).AsDateTime;
end;

function TtiQueryASqlite3.GetParamAsFloat(const AName: string): extended;
begin
  result := FQuery.Params.ParamByName(AName).AsFloat;
end;

function TtiQueryASqlite3.GetParamAsInteger(const AName: string): Int64;
begin
  result := FQuery.Params.ParamByName(AName).AsInteger;
end;

function TtiQueryASqlite3.GetParamAsString(const AName: string): string;
begin
  result := FQuery.Params.ParamByName(AName).AsString;
end;

function TtiQueryASqlite3.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryASqlite3.Next;
begin
  FQuery.Next;
end;

procedure TtiQueryASqlite3.Open;
begin
  FQuery.UniDirectional:= true;
  Active := true;
end;

function TtiQueryASqlite3.ParamCount: integer;
begin
  result := FQuery.Params.Count;
end;

function TtiQueryASqlite3.ParamName(AIndex: integer): string;
begin
  result := FQuery.Params.Items[ AIndex ].Name;
end;

procedure TtiQueryASqlite3.SetActive(const AValue: boolean);
begin
  FQuery.Active := AValue;
end;

procedure TtiQueryASqlite3.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  FQuery.Params.ParamByName(AName).AsBoolean := AValue;
end;

procedure TtiQueryASqlite3.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  FQuery.Params.ParamByName(AName).AsDateTime := AValue;
end;

procedure TtiQueryASqlite3.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  FQuery.Params.ParamByName(AName).AsFloat := AValue;
end;

procedure TtiQueryASqlite3.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  FQuery.Params.ParamByName(AName).AsInteger := AValue;
end;

procedure TtiQueryASqlite3.SetParamAsString(const AName, AValue: string);
var
  lParam : TParam;
begin
  lParam := FQuery.Params.ParamByName(AName);
  if length(AValue) <= 255 then
    lParam.AsString := AValue
  else
    lParam.AsMemo := AValue;
end;

procedure TtiQueryASqlite3.SetSQL(const AValue: TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;

procedure TtiQueryASqlite3.AssignFieldAsStream(const AName: string; const AStream: TStream);
var
  lField : TField;
begin
  lField := FQuery.FieldByName(AName);
  Assert(lField is TBlobField, 'Field <' + AName + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AStream);
end;

procedure TtiQueryASqlite3.AttachDatabase(ADatabase: TtiDatabase);
begin
  FQuery.Connection := TtiDatabaseASqlite3(ADatabase).Database;
  Database := ADatabase;
end;

procedure TtiQueryASqlite3.DetachDatabase;
begin
  inherited DetachDatabase;
end;

function TtiQueryASqlite3.FieldCount: integer;
begin
  result := FQuery.FieldCount;
end;

function TtiQueryASqlite3.FieldName(AIndex: integer): string;
begin
  result := FQuery.Fields[AIndex].FieldName;
end;

procedure TtiQueryASqlite3.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.Params.Clear;
end;

function TtiQueryASqlite3.FieldIndex(const AName: string): integer;
begin
  result := FQuery.FieldByName(AName).Index;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseASqlite3
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseASqlite3.Create;
begin
  inherited Create;
  FDatabase := TASqlite3DB.Create(nil);
  FDatabase.TimeOut:= 200;
//  FDatabase.
//  FDatabase.Name := FDatabase.DatabaseName;
//  FIgnoreTransactionErrors:= false;
end;

destructor TtiDatabaseASqlite3.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

//procedure TtiDatabaseASqlite3.DropTable(
//  const ATableMetaData: TtiDBMetaDataTable);
//begin
//  // override existing as DropTable automatically commits the transaction
//  FIgnoreTransactionErrors:= true;
//  inherited DropTable(ATableMetaData);
//end;

class procedure TtiDatabaseASqlite3.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  lFileName : string;
  lDb: TASQLite3DB;
begin
  if not DirectoryExists(ExtractFilePath(ADatabaseName)) then
    tiForceDirectories(ExtractFilePath(ADatabaseName));

  lFileName := ExpandFileName(ADatabaseName);
  if DatabaseExists(lFileName,AUserName,APassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistASqlite3, ADatabaseName, AUserName, APassword);

  lDb:= TASQLite3DB.Create(nil);
  try
    ldb.Database:= lFileName;
    ldb.MustExist:= false;  // for auto creation
    ldb.Open;
    ldb.Close;
  finally
    lDb.Free;
  end;

  if not DatabaseExists(lFileName,AUserName,APassword) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistASqlite3, ADatabaseName, AUserName, APassword);
end;

//procedure TtiDatabaseASqlite3.CreateTable(
//  const ATableMetaData: TtiDBMetaDataTable);
//begin
//  // override existing as  CreateTable automatically commits the transaction
//  FIgnoreTransactionErrors:= true;
//  inherited CreateTable(ATableMetaData);
//end;

class function TtiDatabaseASqlite3.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
var
  lFileName : string;
begin
  lFileName := ExpandFileName(ADatabaseName);
  result := FileExists(lFileName);
end;

function TtiDatabaseASqlite3.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VARCHAR(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'INTEGER' ;
    qfkFloat     : result := 'FLOAT';
    qfkDateTime  : result := 'DATETIME' ;
    qfkLogical   : result := 'BOOLEAN' ;
    qfkBinary    : result := 'BLOB' ;
    qfkLongString : result := 'MEMO' ;
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

procedure TtiDatabaseASqlite3.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lDelphiTable : TASqlite3Table;
  i : integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TASqlite3Table.Create(nil);
  try
    lDelphiTable.Connection := FDatabase;
    lDelphiTable.TableName := lTable.Name;
    lDelphiTable.FieldDefs.Update;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := lDelphiTable.FieldDefs[i].Name;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
  finally
    lDelphiTable.Free;
  end;
end;

procedure TtiDatabaseASqlite3.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  lsl : TStringList;
  i : integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    FDatabase.GetTableNames(lsl);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := tiExtractFileNameOnly(lsl.Strings[i]);
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lMetaData.ObjectState := posClean;
    end;
  finally
    lsl.Free;
  end;
end;

procedure TtiDatabaseASqlite3.SetupDBParams;
begin
  FDatabase.Database:= DatabaseName;
end;

function TtiDatabaseASqlite3.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseASqlite3.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryASqlite3;
end;

procedure TtiDatabaseASqlite3.Commit;
begin
  if  InTransaction then
    FDatabase.Commit
  else // if not FIgnoreTransactionErrors then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

//  FIgnoreTransactionErrors:= false;
end;

function TtiDatabaseASqlite3.InTransaction: boolean;
begin
  result := FDatabase.InTransaction;
end;

procedure TtiDatabaseASqlite3.RollBack;
begin
  if  InTransaction then
    FDatabase.Rollback
  else //if not FIgnoreTransactionErrors then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

//  FIgnoreTransactionErrors:= false;
end;

procedure TtiDatabaseASqlite3.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FDatabase.StartTransaction;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryASqlite3.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lDataType : TFieldType;
begin
  lDataType := FQuery.Fields[ AIndex ].DataType;

  // These are the available field types for a TDataSet descendant
//  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
//    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
//    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
//    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid);

  // These are the available TtiQueryFieldKind(s)
//  ,
//  qfkInteger,
//  qfkFloat,
//  qfkDateTime,
//  qfkLogical,
//  qfkBinary,
//  qfkMacro,
//  qfkLongString

    case lDataType of
    ftString, ftWideString :                    result := qfkString  ;
    ftSmallint, ftInteger, ftWord, ftLargeint : result := qfkInteger ;
    ftBoolean :                                 result := qfkLogical ;
    ftFloat, ftCurrency, ftBCD :                result := qfkFloat   ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime;
    ftBlob, ftGraphic :                         result := qfkBinary  ;
    ftMemo, ftFmtMemo :                         result := qfkLongString;
    else
      raise EtiOPFInternalException.Create(
        'Invalid FQuery.Fields[ AIndex ].DataType <' +
                      GetEnumName(TypeInfo(TFieldType), Ord(lDataType)) + '>');
    end;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

function TtiQueryASqlite3.FieldSize(AIndex: integer): integer;
begin
  case FieldKind(AIndex) of
    qfkString    : result := FQuery.FieldDefs[ AIndex ].Size;
    qfkLongString : result := 0;
    qfkInteger   : result := 0;
    qfkFloat     : result := 0;
    qfkDateTime  : result := 0;
    qfkBinary    : result := 0;
    qfkLogical   : result := 0;
  else
    raise EtiOPFInternalException.Create('Invalid field type');
  end;
end;

function TtiQueryASqlite3.GetParamIsNull(const AName: String): Boolean;
begin
  result := FQuery.Params.ParamByName(AName).IsNull;
end;

procedure TtiQueryASqlite3.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  FQuery.Params.ParamByName(AName).Clear;
end;

function TtiDatabaseASqlite3.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseASqlite3.SetConnected(AValue: boolean);
var
  lsErrorMessage : string;
begin

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FDatabase.Connected := false;
    Exit; //==>
  end;

  SetupDBParams;
  lsErrorMessage := '';
  try
    FDatabase.Connected := true;
  except

    on e:AsgError do
    begin
      lsErrorMessage := '';

      lsErrorMessage := lsErrorMessage;

      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, e.message);
  end;
end;

function TtiQueryASqlite3.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FQuery.FieldByName(AName).IsNull;
end;

procedure TtiQueryASqlite3.AssignParamToStream(const AName: string; const AStream : TStream);
var
  ls : string;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  ls := FQuery.Params.ParamByName(AName).Value;
  tiStringToStream(ls, AStream);
end;

procedure TtiQueryASqlite3.AssignParamFromStream(const AName: string; const AStream : TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FQuery.Params.ParamByName(AName).LoadFromStream(AStream, ftBlob);
end;

function TtiQueryASqlite3.HasNativeLogicalType: boolean;
begin
  result := true;
end;

procedure TtiQueryASqlite3.AssignFieldAsStreamByIndex(AIndex: Integer;const AValue: TStream);
var
  lField : TField;
begin
  lField := FQuery.Fields[AIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(AIndex) + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AValue);
end;

function TtiQueryASqlite3.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lsValue : string;
begin
  lsValue := upperCase(FQuery.Fields[ AIndex ].AsString);
  result := (lsValue = 'T'   ) or
            (lsValue = 'TRUE') or
            (lsValue = 'Y'   ) or
            (lsValue = 'YES' ) or
            (lsValue = '1'   );
end;

function TtiQueryASqlite3.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ AIndex ].AsDateTime;
end;

function TtiQueryASqlite3.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FQuery.Fields[ AIndex ].AsFloat;
end;

function TtiQueryASqlite3.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FQuery.Fields[ AIndex ].AsInteger;
end;

function TtiQueryASqlite3.GetFieldAsStringByIndex(AIndex: Integer): string;
var
  lField : TField;
  lStream : TStringStream;
begin
  lField := FQuery.Fields[AIndex];
  if lField is TMemoField then
  begin
    lStream := TStringStream.Create('');
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0;
      result := lStream.DataString;
    finally
      lStream.Free;
    end;
  end
  else
    result := lField.AsString;
end;

function TtiQueryASqlite3.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ AIndex ].IsNull;
end;

{ TtiPersistenceLayerASqlite3 }

procedure TtiPersistenceLayerASqlite3.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistASqlite3;
  APersistenceLayerDefaults.DatabaseName:= 'Demo.sqb';
  APersistenceLayerDefaults.Username:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= false;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerASqlite3.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseASqlite3;
end;

function TtiPersistenceLayerASqlite3.GetPersistenceLayerName: string;
begin
  result:= cTIPersistASqlite3;
end;

function TtiPersistenceLayerASqlite3.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryASqlite3;
end;



Initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerASqlite3);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistASqlite3);


end.




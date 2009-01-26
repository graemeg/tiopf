unit tiQueryDBISAM4;

interface
uses
   tiQuery
  ,Classes
  ,tiAutoMap
  ,tiObject
  ,dbisamtb
  ,tiPersistenceLayers
 ;

type

  TtiDatabaseDBISAM4 = class(TtiDatabaseSQL)
  private
    FDatabase     : TDBISAMDataBase;
    FSession      : TDBISAMSession;
    FIgnoreTransactionErrors : Boolean;
  protected
    property  Database : TDBISAMDataBase read FDatabase write FDatabase;
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
    procedure       CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure       DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
  end;


  TtiQueryDBISAM4 = class(TtiQuerySQL)
  private
    FQuery : TDBISAMQuery;
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
    function    ExecSQL: integer; override;

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

  TtiPersistenceLayerDBISAM4 = class(TtiPersistenceLayer)
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
// * TtiQueryDBISAM4
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryDBISAM4.Create;
begin
  inherited;
  FQuery := TDBISAMQuery.Create(nil);
end;

destructor TtiQueryDBISAM4.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TtiQueryDBISAM4.Close;
begin
  Active := false;
end;

function TtiQueryDBISAM4.ExecSQL: integer;
begin
  FQuery.ExecSQL;
  Result := -1;
  { TODO :
When implementing RowsAffected,
please return correct result
and put FSupportsRowsAffected := True; in TtiQueryXXX.Create;}
end;

function TtiQueryDBISAM4.GetFieldAsBoolean(const AName: string): boolean;
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

function TtiQueryDBISAM4.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.FieldByName(AName).AsDateTime;
end;

function TtiQueryDBISAM4.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQuery.FieldByName(AName).AsFloat;
end;

function TtiQueryDBISAM4.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FQuery.FieldByName(AName).AsInteger;
end;

function TtiQueryDBISAM4.GetFieldAsString(const AName: string): string;
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

function TtiQueryDBISAM4.GetActive: boolean;
begin
  result := FQuery.Active;
end;

function TtiQueryDBISAM4.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryDBISAM4.GetParamAsBoolean(const AName: string): boolean;
begin
  result := FQuery.ParamByName(AName).AsBoolean;
end;

function TtiQueryDBISAM4.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.ParamByName(AName).AsDateTime;
end;

function TtiQueryDBISAM4.GetParamAsFloat(const AName: string): extended;
begin
  result := FQuery.ParamByName(AName).AsFloat;
end;

function TtiQueryDBISAM4.GetParamAsInteger(const AName: string): Int64;
begin
  result := FQuery.ParamByName(AName).AsInteger;
end;

function TtiQueryDBISAM4.GetParamAsString(const AName: string): string;
begin
  result := FQuery.ParamByName(AName).AsString;
end;

function TtiQueryDBISAM4.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryDBISAM4.Next;
begin
  FQuery.Next;
end;

procedure TtiQueryDBISAM4.Open;
begin
  Active := true;
end;

function TtiQueryDBISAM4.ParamCount: integer;
begin
  result := FQuery.ParamCount;
end;

function TtiQueryDBISAM4.ParamName(AIndex: integer): string;
begin
  result := FQuery.Params.Items[ AIndex ].Name;
end;

procedure TtiQueryDBISAM4.SetActive(const AValue: boolean);
begin
  FQuery.Active := AValue;
end;

procedure TtiQueryDBISAM4.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsBoolean := AValue;
end;

procedure TtiQueryDBISAM4.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsDateTime := AValue;
end;

procedure TtiQueryDBISAM4.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsFloat := AValue;
end;

procedure TtiQueryDBISAM4.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsInteger := AValue;
end;

procedure TtiQueryDBISAM4.SetParamAsString(const AName, AValue: string);
var
  lParam : TDBISAMParam;
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  lParam := FQuery.ParamByName(AName);
  if length(AValue) <= 255 then
    lParam.AsString := AValue
  else
    lParam.AsMemo := AValue;
end;

procedure TtiQueryDBISAM4.SetSQL(const AValue: TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;

procedure TtiQueryDBISAM4.AssignFieldAsStream(const AName: string; const AStream: TStream);
var
  lField : TField;
begin
  lField := FQuery.FieldByName(AName);
  Assert(lField is TBlobField, 'Field <' + AName + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AStream);
end;

procedure TtiQueryDBISAM4.AttachDatabase(ADatabase: TtiDatabase);
begin
  FQuery.DatabaseName := TtiDatabaseDBISAM4(ADatabase).Database.DatabaseName;
  FQuery.SessionName:= TtiDatabaseDBISAM4(ADatabase).Database.SessionName;
  Database := ADatabase;
end;

procedure TtiQueryDBISAM4.DetachDatabase;
begin
  inherited DetachDatabase;
end;

function TtiQueryDBISAM4.FieldCount: integer;
begin
  result := FQuery.FieldCount;
end;

function TtiQueryDBISAM4.FieldName(AIndex: integer): string;
begin
  result := FQuery.Fields[AIndex].FieldName;
end;

procedure TtiQueryDBISAM4.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.Params.Clear;
end;

function TtiQueryDBISAM4.FieldIndex(const AName: string): integer;
begin
  result := FQuery.FieldByName(AName).Index;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseDBISAM4
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseDBISAM4.Create;
begin
  inherited Create;
  FDatabase := TDBISAMDataBase.Create(nil);
  FDatabase.LoginPrompt := false;
  FSession      := TDBISAMSession.Create(nil);

  FSession.AutoSessionName:= True;
  FDatabase.SessionName := FSession.SessionName;

  // Must come up with a better way of doing this.
  FDatabase.DatabaseName := 'DB_' + FDatabase.SessionName;
  FDatabase.Name := FDatabase.DatabaseName; 
  FIgnoreTransactionErrors:= false;
end;

destructor TtiDatabaseDBISAM4.Destroy;
begin
  FDatabase.Free;
  FSession.Free;
  inherited;
end;

procedure TtiDatabaseDBISAM4.DropTable(
  const ATableMetaData: TtiDBMetaDataTable);
begin
  // override existing as DropTable automatically commits the transaction
  FIgnoreTransactionErrors:= true;
  inherited DropTable(ATableMetaData);
end;

class procedure TtiDatabaseDBISAM4.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  lDir : string;
begin
  lDir := ExpandFileName(ADatabaseName);
  if DatabaseExists(lDir,AUserName,APassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistDBISAM4, ADatabaseName, AUserName, APassword);
  if not ForceDirectories(lDir) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistDBISAM4, ADatabaseName, AUserName, APassword);
end;

procedure TtiDatabaseDBISAM4.CreateTable(
  const ATableMetaData: TtiDBMetaDataTable);
begin
  // override existing as  CreateTable automatically commits the transaction
  FIgnoreTransactionErrors:= true;
  inherited CreateTable(ATableMetaData);
end;

class function TtiDatabaseDBISAM4.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
var
  lDir : string;
begin
  lDir := ExpandFileName(ADatabaseName);
  result := DirectoryExists(lDir);
end;

function TtiDatabaseDBISAM4.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VARCHAR(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'INTEGER' ;
    qfkFloat     : result := 'FLOAT';
    qfkDateTime  : result := 'TIMESTAMP' ;
    qfkLogical   : result := 'BOOLEAN' ;
    qfkBinary    : result := 'BLOB' ;
    qfkLongString : result := 'MEMO' ;
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

procedure TtiDatabaseDBISAM4.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lDelphiTable : TDBISAMTable;
  i : integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TDBISAMTable.Create(nil);
  try
    lDelphiTable.DatabaseName := DatabaseName;
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

procedure TtiDatabaseDBISAM4.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  lsl : TStringList;
  i : integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    tiFilesToStringList(DatabaseName,
                         '*.dat',
                         lsl,
                         false);
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

procedure TtiDatabaseDBISAM4.SetupDBParams;
begin
  FDatabase.Directory:= DatabaseName;
  FSession.PrivateDir:= tiGetTempDir;
end;

function TtiDatabaseDBISAM4.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseDBISAM4.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDBISAM4;
end;

procedure TtiDatabaseDBISAM4.Commit;
begin
  if  InTransaction then
    FDatabase.Commit
  else if not FIgnoreTransactionErrors then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

  FIgnoreTransactionErrors:= false;
end;

function TtiDatabaseDBISAM4.InTransaction: boolean;
begin
  result := FDatabase.InTransaction;
end;

procedure TtiDatabaseDBISAM4.RollBack;
begin
  if  InTransaction then
    FDatabase.Rollback
  else if not FIgnoreTransactionErrors then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

  FIgnoreTransactionErrors:= false;
end;

procedure TtiDatabaseDBISAM4.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FDatabase.StartTransaction;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryDBISAM4.FieldKind(AIndex: integer): TtiQueryFieldKind;
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

function TtiQueryDBISAM4.FieldSize(AIndex: integer): integer;
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

function TtiQueryDBISAM4.GetParamIsNull(const AName: String): Boolean;
begin
  result := FQuery.ParamByName(AName).IsNull;
end;

procedure TtiQueryDBISAM4.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  FQuery.ParamByName(AName).Clear;
end;

function TtiDatabaseDBISAM4.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseDBISAM4.SetConnected(AValue: boolean);
var
  lsErrorMessage : string;
begin

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FDatabase.Connected := false;
    FSession.Active := false;
    Exit; //==>
  end;

  SetupDBParams;
  lsErrorMessage := '';
  try
    FDatabase.Connected := true;
    FSession.Active := true;
  except

    on e:EDBISAMEngineError do
    begin
      lsErrorMessage := '';

      lsErrorMessage := lsErrorMessage +
                        'Error class: '   + e.classname + Cr +
                        'Error message: ' + e.Message + Cr +
                        'Error Code: ' + IntToStr(e.ErrorCode) + Cr;

      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, e.message);
  end;
end;

function TtiQueryDBISAM4.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FQuery.FieldByName(AName).IsNull;
end;

procedure TtiQueryDBISAM4.AssignParamToStream(const AName: string; const AStream : TStream);
var
  ls : string;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  ls := FQuery.ParamByName(AName).Value;
  tiStringToStream(ls, AStream);
end;

procedure TtiQueryDBISAM4.AssignParamFromStream(const AName: string; const AStream : TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FQuery.ParamByName(AName).LoadFromStream(AStream, ftBlob);
end;

function TtiQueryDBISAM4.HasNativeLogicalType: boolean;
begin
  result := true;
end;

procedure TtiQueryDBISAM4.AssignFieldAsStreamByIndex(AIndex: Integer;const AValue: TStream);
var
  lField : TField;
begin
  lField := FQuery.Fields[AIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(AIndex) + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AValue);
end;

function TtiQueryDBISAM4.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
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

function TtiQueryDBISAM4.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ AIndex ].AsDateTime;
end;

function TtiQueryDBISAM4.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FQuery.Fields[ AIndex ].AsFloat;
end;

function TtiQueryDBISAM4.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FQuery.Fields[ AIndex ].AsInteger;
end;

function TtiQueryDBISAM4.GetFieldAsStringByIndex(AIndex: Integer): string;
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

function TtiQueryDBISAM4.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ AIndex ].IsNull;
end;

{ TtiPersistenceLayerDBISAM4 }

procedure TtiPersistenceLayerDBISAM4.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistDBISAM4;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + 'DBISAM';
  APersistenceLayerDefaults.Username:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
end;

function TtiPersistenceLayerDBISAM4.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseDBISAM4;
end;

function TtiPersistenceLayerDBISAM4.GetPersistenceLayerName: string;
begin
  result:= cTIPersistDBISAM4;
end;

function TtiPersistenceLayerDBISAM4.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDBISAM4;
end;



Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDBISAM4);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistDBISAM4);


end.




unit tiQueryDOA;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiDBConnectionPool
  ,tiAutoMap
  ,tiObject
  ,tiBaseObject
  ,tiExcept
  ,Classes
  ,Contnrs
  ,Oracle
  ,tiPersistenceLayers
 ;

const
  CtiQueryOptionDOAReadBuffer = 'ReadBuffer';

type

  TtiPersistenceLayerDOA = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  EtiOPFDOAException = EtiOPFException;

  TtiDatabaseDOA = class(TtiDatabaseSQL)
  private
    FOracleSession: TOracleSession;
    FInTransaction : boolean;
  protected
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    function  FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    constructor     Create; override;
    destructor      Destroy; override;
    property        OracleSession : TOracleSession read FOracleSession;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): boolean; override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    procedure       StartTransaction; override;
    function        InTransaction : boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiDOABinParamItem = class(TtiBaseObject)
  private
    FParamName: string;
    FLOBLocator: TLOBLocator;
  public
    constructor Create(ASession: TOracleSession; AOracleDataType: Byte);
    destructor  Destroy; override;
    property    ParamName : string read FParamName write FParamName;
    property    LOBLocator: TLOBLocator Read FLOBLocator;
    procedure   LoadFromStream(const AStream : TStream);
    procedure   SaveToStream(const AStream : TStream);
  end;

  TtiQueryDOA = class(TtiQuerySQL)
  private
    FQuery : TOracleQuery;
    FbActive : boolean;
    FslVariables : TStringList;
    FDOABinParamList : TObjectList;
    procedure DeclareVariable(const AName : string; piOracleType : integer);
    function  OracleErrorMessage : string;
    function  FindCreateBinParam(const AName: string; AOracleDataType: byte): TtiDOABinParamItem;
  protected
    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function  GetEOF: boolean; override;

    function  GetFieldAsString(const AName: string): string     ; override;
    function  GetFieldAsFloat(const AName: string): extended    ; override;
    function  GetFieldAsBoolean(const AName: string): boolean   ; override;
    function  GetFieldAsInteger(const AName: string): Int64     ; override;
    function  GetFieldAsDateTime(const AName: string):TDateTime ; override;
    function  GetFieldIsNull(const AName: string): Boolean      ; override;

    function  GetFieldAsStringByIndex(AIndex: Integer) : string  ; override;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function  GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function  GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function  GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function  GetParamAsString(const AName: string): string; override;
    procedure SetParamAsString(const AName, AValue: string); override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    procedure SetParamAsBoolean(const AName: string;const AValue: boolean);override;
    function  GetParamAsFloat(const AName: string): extended;override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended);override;
    function  GetParamAsInteger(const AName: string): Int64;override;
    procedure SetParamAsInteger(const AName: string;const AValue: Int64);override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;

    procedure SetParamAsMacro(const AName: string;
                               const AValue: string); override;

    function  GetParamIsNull(const AName: String): Boolean; override;
    procedure SetParamIsNull(const AName: String; const AValue: Boolean); override;

    procedure DoChangeOptions(Sender: TObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   Next   ; override;
    function    ExecSQL: integer; override;

    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;

    function    FieldCount : integer; override;
    function    FieldName(AIndex : integer): string; override;
    function    FieldIndex(const AName : string): integer; override;
    function    FieldKind(AIndex : integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex : integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

    procedure   AssignParamFromStream(const AName : string ; const AValue : TStream); override;
    procedure   AssignParamToStream(  const AName : string ; const AValue : TStream); override;
    procedure   AssignFieldAsStream(  const AName : string ; const AValue : TStream); override;
    procedure   AssignFieldAsStreamByIndex( AIndex : integer; const AValue : TStream); override;

    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

  end;


implementation
uses
   tiLog
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,Forms
  ,Windows
  ,Controls
  ,SysUtils
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,OracleCI
  ;

const
  cSavePoint = 'DOA_Save_Point';

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryDOA
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryDOA.Create;
begin
  inherited;
  FQuery      := TOracleQuery.Create(nil);
  FslVariables := TStringList.Create;
  FbActive    := false;
end;

destructor TtiQueryDOA.Destroy;
begin
  FQuery.ClearVariables;
  FQuery.Free;
  FslVariables.Free;
  FDOABinParamList.Free;
  inherited;
end;

procedure TtiQueryDOA.Close;
begin
  Active := false;
end;

function TtiQueryDOA.ExecSQL: integer;
begin
  try
    FQuery.Execute;
    if Assigned(FDOABinParamList) then
      FDOABinParamList.Clear;
    Result := -1;
  { TODO :
When implementing RowsAffected,
please return correct result
and put FSupportsRowsAffected := True; in TtiQueryXXX.Create;}
  except
    on e:exception do
    begin
      Database.ErrorInLastCall := true;
      raise EtiOPFDOAException.Create(e.message + ' ' + #13 + OracleErrorMessage);
    end;
  end;
end;

function TtiQueryDOA.GetFieldAsBoolean(const AName: string): boolean;
var
  lsValue : string;
begin
  lsValue := upperCase(FQuery.Field(AName));
  result := (lsValue = 'T'   ) or
            (lsValue = 'TRUE') or
            (lsValue = 'Y'   ) or
            (lsValue = 'YES' ) or
            (lsValue = '1'   );
end;

function TtiQueryDOA.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.FieldAsDate(AName);
end;

function TtiQueryDOA.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQuery.FieldAsFloat(AName);
end;

function TtiQueryDOA.GetFieldAsInteger(const AName: string): Int64;
var
  lr : double;
begin
  // Delphi real types
  // Real48	2.9 x 10^–39 .. 1.7 x 10^38	11–12	6
  // Single	1.5 x 10^–45 .. 3.4 x 10^38	7–8	4
  // Double	5.0 x 10^–324 .. 1.7 x 10^308	15–16	8
  // Extended	3.6 x 10^–4951 .. 1.1 x 10^4932	19–20	10
  // An OID is Number(15) so we must read it as a double, then convert to an integer.
  lr := FQuery.Field(AName);
  result := Trunc(lr);
end;

function TtiQueryDOA.GetFieldAsString(const AName: string): string;
begin
  result := FQuery.FieldAsString(AName);
end;

function TtiQueryDOA.GetActive: boolean;
begin
  result := FbActive;
end;

function TtiQueryDOA.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryDOA.GetParamAsBoolean(const AName: string): boolean;
var
  ls : string;
begin
  ls := FQuery.GetVariable(AName);
  result :=
    SameText(ls, 'TRUE') or
    SameText(ls, 'T') or
    SameText(ls, 'YES') or
    SameText(ls, 'Y') or
    (ls = '1');
end;

function TtiQueryDOA.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.GetVariable(AName);
end;

function TtiQueryDOA.GetParamAsFloat(const AName: string): extended;
begin
  result := FQuery.GetVariable(AName);
end;

function TtiQueryDOA.GetParamAsInteger(const AName: string): Int64;
var
  lr : real;
begin
  lr := FQuery.GetVariable(AName);
  result := trunc(lr);
end;

function TtiQueryDOA.GetParamAsString(const AName: string): string;
begin
  result := FQuery.GetVariable(AName);
end;

function TtiQueryDOA.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryDOA.Next;
begin
  try
    FQuery.Next;
  except
    on e:exception do
    begin
      Database.ErrorInLastCall := true;
      raise;
    end;
  end;
end;

procedure TtiQueryDOA.Open;
begin
  Active := true;
end;

function TtiQueryDOA.ParamCount: integer;
begin
  result := FQuery.VariableCount;
end;

function TtiQueryDOA.ParamName(AIndex: integer): string;
var
  ls : string;
begin
  ls := FQuery.VariableName(AIndex);
  result := tiStrTran(ls, ':', '');
end;

procedure TtiQueryDOA.SetActive(const AValue: boolean);
begin
  if AValue then
  begin
    try
      FQuery.Execute;
      FbActive := true;
    except
      on e:exception do
        raise EtiOPFDOAException.Create(SQLAndParamsAsString + #13 + 'Message: ' + e.message);
    end;
  end else
  begin
    FQuery.Close;
    FbActive := false;
  end;
end;

procedure TtiQueryDOA.SetParamAsBoolean(const AName: string;
  const AValue: boolean);
begin
  DeclareVariable(AName, otString);
  if AValue then
    FQuery.SetVariable(AName, 'T')
  else
    FQuery.SetVariable(AName, 'F')
end;

procedure TtiQueryDOA.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  DeclareVariable(AName, otDate);
  FQuery.SetVariable(AName, DateTimeToStr(AValue));

  // This was causing conversion error in StrToDateTime when called in Oracle.pas
  //FQuery.SetVariable(AName, FormatDateTime('dd/mm/yyyy hh:nn:ss', AValue));

  // Not sure why this was commented out. Think it might have been because of
  // problems converting TDateTime (float)
  //FQuery.SetVariable(AName, AValue);
end;

procedure TtiQueryDOA.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  DeclareVariable(AName, otFloat);
  FQuery.SetVariable(AName, AValue);
end;

procedure TtiQueryDOA.SetParamAsInteger(const AName: string;
  const AValue: Int64);
var
  lr : real;
begin
  DeclareVariable(AName, otFloat);
  lr := AValue;
  FQuery.SetVariable(AName, lr);
end;

procedure TtiQueryDOA.SetParamAsString(const AName, AValue: string);
var
  lBinParam : TtiDOABinParamItem;
begin
  if Length(AValue) < 4000 then
  begin
    DeclareVariable(AName, otString);
    FQuery.SetVariable(AName, AValue);
  end else
  begin
    DeclareVariable(AName, otCLOB);
    lBinParam := FindCreateBinParam(AName, otCLOB);
    lBinParam.LOBLocator.AsString := AValue;
    FQuery.SetComplexVariable(AName, LBinParam.LOBLocator);
  end;
end;

procedure TtiQueryDOA.SetSQL(const AValue: TStrings);
begin
  FQuery.DeleteVariables;
  FslVariables.Clear;
  FQuery.SQL.Assign(AValue);
end;

{
function TtiQueryDOA.GetFieldAsStream(const AName: string): TStream;
const
  cmiBuffer = 12000;
var
  liOffset : integer;
  liLen   : integer;
  lBuffer : array [0..cmiBuffer-1] of byte;
begin

  // If the stream does not exist, then create it
//  if FStream = nil then
//    FStream := TMemoryStream.Create
//  else
//    FStream.Clear;

  if FStream <> nil then
    FStream.Free;
  FStream := TMemoryStream.Create;

  liOffset := 0;
  repeat
    liLen := FQuery.GetLongField(AName,
                                  @lBuffer, liOffset, cmiBuffer);
    FStream.Write(lBuffer, liLen);
    inc(liOffset, cmiBuffer);
  until liLen < cmiBuffer;

  result := FStream;

end;
}

procedure TtiQueryDOA.AssignFieldAsStream(const AName: string; const AValue: TStream);
var
  LLOB: TLOBLocator;
begin
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Size := 0;
  LLOB := FQuery.LOBField(AName);
  if not LLOB.IsNull then
    AValue.CopyFrom(LLOB, LLOB.Size);
  AValue.Position := 0;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseDOA
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabaseDOA.Commit;
begin
  try
    Assert(FInTransaction, 'Attempt to call commit when a transaction is not open');
    try
      // mmm what happens if a commit fails. Should we roll back?
      FOracleSession.Commit;
    finally
      FInTransaction := false;
    end;
  except
    on e:exception do
    begin
      ErrorInLastCall := true;
      raise;
    end;
  end;
end;

constructor TtiDatabaseDOA.Create;
begin
  inherited;
  FOracleSession := TOracleSession.Create(nil);
  // If these are commented out, DOA will determine which version of OCI to use
  // FOracleSession.UseOCI80 := false;
  // FOracleSession.Preferences.UseOCI7 := true;
  FInTransaction := false;
end;

destructor TtiDatabaseDOA.destroy;
begin
  // Swallow any exceptions as we might be destroying a DBConnection from
  // the pool after Oracle has gone down. An exception will be raised by
  // DOA in this case.
  try
    FOracleSession.Connected := false;
  except end;
  FOracleSession.Free;
  inherited;
end;

procedure TtiQueryDOA.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  FQuery.Session := TtiDatabaseDOA(ADatabase).OracleSession;
end;

procedure TtiQueryDOA.DetachDatabase;
begin
  inherited DetachDatabase;
  FQuery.Session := nil;
end;

procedure TtiQueryDOA.DoChangeOptions(Sender: TObject);
begin
  if Options.IndexOfName(CtiQueryOptionDOAReadBuffer) <> -1 then
    FQuery.ReadBuffer:= StrToInt(Options.Values[CtiQueryOptionDOAReadBuffer]);
end;

procedure TtiDatabaseDOA.StartTransaction;
begin
  try
    Assert(not FInTransaction, 'Attempt to start a transaction when one is already open');
  //  FOracleSession.SavePoint(cSavePoint);
    FInTransaction := true;
  except
    on e:exception do
    begin
      ErrorInLastCall := true;
      raise;
    end;
  end;
end;

function TtiDatabaseDOA.InTransaction: boolean;
begin
  try
    result := FInTransaction;
  //  result := FOracleSession.InTransaction;
  except
    on e:exception do
    begin
      ErrorInLastCall := true;
      raise;
    end;
  end;
end;

procedure TtiDatabaseDOA.RollBack;
begin
  if not FInTransaction then
    Exit; //==>
  try
    // FOracleSession.RollBackToSavePoint(cSavePoint);
    // The DBConnection might be broken, so a rollback will raise an
    // exception. Assume the DB will rollback if the connection has been
    // broken.
    try
      FOracleSession.Rollback;
    except
      on e:exception do
        ErrorInLastCall := true;
    end;
  finally
    FInTransaction := false;
  end;
end;

function TtiQueryDOA.FieldCount: integer;
begin
  result := FQuery.FieldCount;
end;

function TtiQueryDOA.FieldName(AIndex: integer): string;
begin
  result := FQuery.FieldName(AIndex);
end;

// Reset the query, same as calling SQL.Clear and DeleteVariables
procedure TtiQueryDOA.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.DeleteVariables;
  FslVariables.Clear;
end;

function TtiQueryDOA.FieldIndex(const AName: string): integer;
begin
  result := FQuery.FieldIndex(AName);
end;

procedure TtiQueryDOA.SetParamAsMacro(const AName, AValue: string);
begin
  SQLText :=
    tiCIStrTran(SQLText,
                 cgtiQueryMacroChr + AName,
                 AValue);
end;

function  TtiQueryDOA.GetParamIsNull(const AName: String): Boolean;
var
  AValue: Variant;
begin
  AValue := FQuery.GetVariable(AName);
  Result := VarIsNull(AValue) or VarIsEmpty(AValue);
end;

procedure TtiQueryDOA.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  if AValue then
  begin
    // Keep an eye on this for errors. A variable must be declared before it can be used.
    // If a var is is set to nill in the first call, then a value in the second, and it's not a
    // string, there may be problems...
    DeclareVariable(AName, otString);
    FQuery.SetVariable(AName, null)
  end
  else
    raise EtiOPFDOAException.Create('Parameter <' + AName + '> cannot be explicitly set to non-null');
end;

function TtiQueryDOA.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lPrecision : integer;
  lScale : integer;
begin
  case FQuery.FieldType(AIndex) of
  otString : result := qfkString    ;
  otInteger : result := qfkInteger   ;
  otFloat  : result := qfkFloat     ;
  otDate   : result := qfkDateTime  ;
  otLong   : result := qfkLongString;
  otCLOB   : result := qfkLongString;
  otLongRaw : result := qfkBinary    ;
  otBLOB   : result := qfkBinary    ;
  else
    raise EtiOPFDOAException.Create('Invalid oracle field type <' +
                    IntToStr(FQuery.FieldType(AIndex)) + '>');
  end;

  if (result = qfkFloat) then
  begin
    lPrecision := FQuery.FieldPrecision(AIndex);
    lScale    := FQuery.FieldScale(AIndex);
    if (lPrecision <> 0) and (lScale = 0) then
      result := qfkInteger;
  end;

end;

function TtiQueryDOA.FieldSize(AIndex: integer): integer;
//var
//  lValue : string;
begin
  case FieldKind(AIndex) of
    qfkString    : begin
                      result := FQuery.FieldSize(AIndex);
                      //  Can not detect field size for a logical as it will be stored as a string
                      //  qfkLogical   : result := 0;
                      // ToDo: What it it's a logical, but there is no data
                      //       returned. Can't check the field width then
(*
                      if (result = 1) or (result = 5) then
                      begin
                        lValue := FieldAsString[FieldName(AIndex)];
                        {$IFDEF BOOLEAN_CHAR_1}
                           if (result = 1) and
                              (SameText(lValue, 'T') or SameText(lValue, 'F')) then
                             result := 0;
                        {$ELSE}
                           if (result = 5) and
                              (SameText(lValue, 'TRUE') or SameText(lValue, 'FALSE')) then
                             result := 0;
                        {$ENDIF}
                      end;
*)                      
                    end;
    qfkLongString : result := 0;
    qfkInteger   : result := 0;
    qfkFloat     : result := 0;
    qfkDateTime  : result := 0;
    qfkBinary    : result := 0;
  else
    raise EtiOPFDOAException.CreateFmt('Invalid oracle field type <%s>',
                    [FQuery.FieldType(AIndex)]);
  end;
end;

procedure TtiQueryDOA.DeclareVariable(const AName: string; piOracleType: integer);
var
  lsName : string;
begin
  lsName := UpperCase(AName);
  // Not sure if this check before declaring a parameter is necessary. Test later.
  if FslVariables.IndexOf(lsName) = -1 then
  begin
    FslVariables.Add(lsName);
    FQuery.DeclareVariable(AName, piOracleType);
  end;
//  FQuery.DeclareVariable(AName, otFloat);
//  FQuery.DeclareVariable(AName, otString);
//  FQuery.DeclareVariable(AName, otDate);
//  FQuery.DeclareVariable(AName, otLong);
//  FQuery.DeclareVariable(AName, otLongRaw);

end;

procedure TtiDatabaseDOA.SetConnected(AValue: boolean);
var
  FOldCursor : TCursor;
begin

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FOracleSession.Connected := false;
    FInTransaction := false;
    Exit; //==>
  end;

  FOracleSession.LogonDatabase := DatabaseName;
  FOracleSession.LogonUserName := UserName;
  FOracleSession.LogonPassword := Password;

  try
    FOldCursor := FOracleSession.Cursor;
    if GetCurrentThreadId <> MainThreadID then
      FOracleSession.Cursor := Screen.Cursor;
    try
      FOracleSession.Connected    := true;
    finally
      FOracleSession.Cursor := FOldCursor;
    end;
  except
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create(ctiPersistDOA, DatabaseName, UserName, Password, e.message);
  end;

end;

function TtiDatabaseDOA.GetConnected: boolean;
begin
  result := FOracleSession.Connected;
end;

function TtiQueryDOA.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FQuery.FieldIsNull(FieldIndex(AName));
end;

procedure TtiDatabaseDOA.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery : TtiQuery;
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    lQuery.AttachDatabase(Self);
    lQuery.SQLText :=
    'select ' +
    '   table_name ' +
    'from ' +
    '  all_tables ' +
    'where ' +
    '    not table_name like ''RM$%'' ' +
    'and not table_name like ''RM_%'' ' +
    'and not table_name like ''SDD_%'' ' +
    'and not table_name like ''SDW_%'' ' +
    'and not table_name like ''CDI_%'' ' +
    'and not table_name like ''CK_%'' ' +
    'and not table_name like ''O_%'' ' +
    'and not table_name like ''COPY_RULE%'' ' +
    'and owner <> ''SYS'' ' +
    ' ' +
    'order by ' +
    '  table_name ';

{
    'select ' +
    '   table_name ' +
    'from ' +
// ZL Change - was all_tables
// In all tables unique key is TABLE_NAME+OWNER. We are not using OWNER so
// we have to change to USER_TABLES and than uk is only TABLE_NAME
    '  user_tables ' +
    'where ' +
    '    not table_name like ''RM$%'' ' +
    'and not table_name like ''RM_%'' ' +
    'and not table_name like ''SDD_%'' ' +
    'and not table_name like ''SDW_%'' ' +
    'and not table_name like ''CDI_%'' ' +
    'and not table_name like ''CK_%'' ' +
    'and not table_name like ''O_%'' ' +
    'and not table_name like ''COPY_RULE%'' ' +
// ZL Change - in user_tables there is no such column
//    'and owner <> ''SYS'' ' +
    ' ' +
    'order by ' +
    '  table_name ';
}
    lQuery.Open;
    while not lQuery.EOF do
    begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := lQuery.FieldAsString[ 'table_name' ];
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lQuery.Next;
    end;
    lQuery.DetachDatabase;
    lMetaData.ObjectState := posClean;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseDOA.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lQuery : TtiQuery;
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    lQuery.AttachDatabase(Self);
    lQuery.SQLText :=
      'select ' +
      '     COLUMN_NAME ' +
      '    ,DATA_TYPE            ' +
      '    ,DATA_LENGTH ' +
      '    ,DATA_PRECISION       ' +
      '    ,NULLABLE ' +
      'from ' +
// ZL Change - was all_tab_columns - but than we can see all columns
// of all tables and we should use also OWNER
// (unique key is OWNER+TABLE_NAME+COLUMN_NAME) in user_Tab_columns unique key
// is only TABLE_NAME+COLUMN_NAME
      '   user_tab_columns ' +
      'where ' +
      '   table_name = ''' + lTable.Name + '''' {+
      'order by ' +
      '    column_name ' +
      ' ' };
    lQuery.Open;
    while not lQuery.EOF do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := lQuery.FieldAsString[ 'COLUMN_NAME' ];
      lField.ObjectState := posClean;
      lTable.Add(lField);
      lQuery.Next;
    end;
    lQuery.DetachDatabase;
    lTable.ObjectState := posClean;
  finally
    lQuery.Free;
  end;
end;

function TtiQueryDOA.FindCreateBinParam(const AName: string; AOracleDataType: byte): TtiDOABinParamItem;
var
  i : integer;
begin
  result := nil;
  if FDOABinParamList = nil then
    FDOABinParamList := TObjectList.Create(True);
  for i := 0 to FDOABinParamList.Count - 1 do
    if SameText(TtiDOABinParamItem(FDOABinParamList.Items[i]).ParamName, AName) then
    begin
      result := TtiDOABinParamItem(FDOABinParamList.Items[i]);
      Break; //==>
    end;
  if result = nil then
  begin
    result := TtiDOABinParamItem.Create((Database as TtiDatabaseDOA).OracleSession, AOracleDataType);
    result.ParamName := AName;
    FDOABinParamList.Add(result);
  end;
end;

procedure TtiQueryDOA.AssignParamFromStream(const AName: string; const AValue: TStream);
var
  LBinParam : TtiDOABinParamItem;
begin
  DeclareVariable(AName, otBLOB);
  LBinParam := FindCreateBinParam(AName, otBLOB);
  LBinParam.LoadFromStream(AValue);
  FQuery.SetComplexVariable(AName, LBinParam.LOBLocator);
end;

procedure TtiQueryDOA.AssignParamToStream(const AName: string; const AValue: TStream);
var
  lBinParam : TtiDOABinParamItem;
begin
  lBinParam := FindCreateBinParam(AName, otBLOB);
  lBinParam.SaveToStream(AValue);
end;

function TtiDatabaseDOA.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName : string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString    : result := 'VarChar2(' + IntToStr(AFieldMetaData.Width) + ')';
// Change by ZL
// was Number(12,0) - Number(12) looks cleaner and is used by Oracle it self
    qfkInteger   : result := 'Number(12)';
//Change by ZL
// was Numeric(11,5) - better Number than we can cover whole Real range.
// this is equal to Number(*) and means Maximal precision and scale.
// Numeric(11,5) is subset og Number so there is back-compability
    qfkFloat     : result := 'Number';
    qfkDateTime  : result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical   : result := 'Char(1) default ''F'' check(' + lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical   : result := 'VarChar(5) default ''FALSE'' check(' + lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary    : result := 'BLOB';
    qfkLongString : result := 'CLOB';
  else
    raise EtiOPFDOAException.Create('Invalid FieldKind');
  end;
end;

class procedure TtiDatabaseDOA.CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDOA.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string):boolean;
begin
  result := false;
  Assert(false, 'DatabaseExists not implemented in ' + ClassName);
end;

class procedure TtiDatabaseDOA.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string = '');
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

function TtiQueryDOA.HasNativeLogicalType: boolean;
begin
  result := false;
end;

function TtiQueryDOA.OracleErrorMessage: string;
var
  lRow : integer;
  lCol : integer;
  lArrow : string;
  i : integer;
  ls : string;
begin
  ls    := SQLAndParamsAsString;
  lRow  := FQuery.ErrorLine;
  lCol  := FQuery.ErrorPosition;

  if (lRow <> 0) and (lCol <> 0) then
  begin
    result := '';
    lArrow := tiReplicate('-', lCol) + '---^';
    for i := 1 to tiNumToken(ls, Cr) do
    begin
      if i = lRow+2 then
        result := result + Cr + lArrow;
      result := result + Cr + tiToken(ls, Cr, i);
    end;
  end else
    result := ls;

  result :=
    result + ' ' + Cr(2) + 
    'Error row: ' + IntToStr(lRow) + ' ' +
    'Error col: ' + IntToStr(lCol);

end;

{ TtiDOABinParamItem }

constructor TtiDOABinParamItem.Create(ASession: TOracleSession; AOracleDataType: Byte);
begin
  inherited Create;
  FLOBLocator := TLOBLocator.CreateTemporary(ASession, AOracleDataType, True);
end;

destructor TtiDOABinParamItem.destroy;
begin
  FLOBLocator.Free;
//  System.FreeMem(FBuffer);
  inherited;
end;

function TtiDatabaseDOA.Test: boolean;
var
  lQuery : TtiQuery;
begin
  result := false;
  try
    lQuery := CreateTIQuery;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText := 'select null from dual';
      lQuery.Open;
      while (not lQuery.EOF) and
            (not result) do
        result := true;
      lQuery.DetachDatabase;
    finally
      lQuery.Free;
    end;
  except
    on e:exception do
      result := false;
  end;
  if not result then
    ErrorInLastCall := true;
end;

function TtiDatabaseDOA.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDOA;
end;

function TtiQueryDOA.GetFieldAsStringByIndex(AIndex: Integer): string;
begin
  result := FQuery.Field(AIndex);
end;

function TtiQueryDOA.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lsValue : string;
begin
  lsValue := upperCase(FQuery.Field(AIndex));
  result := (lsValue = 'T'   ) or
            (lsValue = 'TRUE') or
            (lsValue = 'Y'   ) or
            (lsValue = 'YES' ) or
            (lsValue = '1'   );
end;

function TtiQueryDOA.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FQuery.FieldAsDate(AIndex);
end;

function TtiQueryDOA.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FQuery.FieldAsFloat(AIndex);
end;

function TtiQueryDOA.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
var
  lr : double;
begin
  // Delphi real types
  // Real48	2.9 x 10^–39 .. 1.7 x 10^38	11–12	6
  // Single	1.5 x 10^–45 .. 3.4 x 10^38	7–8	4
  // Double	5.0 x 10^–324 .. 1.7 x 10^308	15–16	8
  // Extended	3.6 x 10^–4951 .. 1.1 x 10^4932	19–20	10
  // An OID is Number(15) so we must read it as a double, then convert to an integer.
  lr := FQuery.Field(AIndex);
  result := Trunc(lr);
end;

function TtiQueryDOA.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FQuery.FieldIsNull(AIndex);
end;

procedure TtiQueryDOA.AssignFieldAsStreamByIndex(AIndex: Integer;const AValue: TStream);
var
  LLOB: TLOBLocator;
  LPosition: Integer;
begin
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Size := 0;
  LLOB := FQuery.LOBField(AIndex);
  LPosition := LLOB.Position;
  LLOB.Position := 0;
  if not LLOB.IsNull then
    AValue.CopyFrom(LLOB, LLOB.Size);
  AValue.Position := 0;
  LLOB.Position := LPosition;
end;

procedure TtiDOABinParamItem.LoadFromStream(const AStream: TStream);
var
  LPosition: Integer;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  LPosition:= AStream.Position;
  AStream.Position := 0;
  FLOBLocator.Size := 0;
  FLOBLocator.CopyFrom(AStream, AStream.Size);
  AStream.Position := LPosition;
end;

procedure TtiDOABinParamItem.SaveToStream(const AStream: TStream);
begin
  Assert(AStream<>nil, 'AStream not assigned');
  FLOBLocator.Position := 0;
  AStream.Size := 0;
  AStream.CopyFrom(FLOBLocator, FLOBLocator.Size);
  AStream.Position := 0;
end;

{ TtiPersistenceLayerDOA }

procedure TtiPersistenceLayerDOA.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistDOA;
  APersistenceLayerDefaults.DatabaseName:= 'ocrl';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.Username:= 'scott';
  APersistenceLayerDefaults.Password:= 'tiger';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerDOA.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseDOA;
end;

function TtiPersistenceLayerDOA.GetPersistenceLayerName: string;
begin
  result:= cTIPersistDOA;
end;

function TtiPersistenceLayerDOA.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDOA;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerDOA);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistDOA);

end.



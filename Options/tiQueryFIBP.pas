unit tiQueryFIBP;

{$I tiDefines.inc}

interface
uses
  tiPersistenceLayers,
  tiQuery,
  Classes,

  ibase,

  FIBQuery,
  pFIBDatabase,
  FIB;

type

  TtiPersistenceLayerFIBP = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseFIBP = class(TtiDatabaseSQL)
  private
    FDatabase: TpFIBDatabase;
    FTransaction: TpFIBTransaction;
    FTraceLevel: integer;
  protected
    procedure SetConnected(AValue: boolean); override;
    function  GetConnected: boolean; override;
    function  FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string); override;
    property        NativeDatabase: TpFIBDatabase read FDatabase write FDatabase;
    property        TraceLevel  : integer read FTraceLevel    write FTraceLevel  ;
    procedure       StartTransaction; override;
    function        InTransaction: boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    procedure       ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;

  end;

  // friend class to access protected member of TFIBQuery
  THackFibQuery = class( TFIBQuery)
  end;

  TtiQueryFIBP = class(TtiQuerySQL)
  private
    FIBSQL: TFIBQuery;
    FbActive: boolean;
    function FieldKindToTIFieldKind(AData : PXSQLVAR): TtiQueryFieldKind;
    procedure Prepare;
  protected

    function  GetFieldAsString(const AName: string): string; override;
    function  GetFieldAsFloat(const AName: string): extended; override;
    function  GetFieldAsBoolean(const AName: string): boolean; override;
    function  GetFieldAsInteger(const AName: string): Int64; override;
    function  GetFieldAsDateTime(const AName: string): TDateTime; override;

    function  GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function  GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function  GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function  GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function  GetEOF: boolean; override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsFloat(const AName: string): extended; override;
    function  GetParamAsInteger(const AName: string): Int64; override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: Int64); override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;

    function  GetParamIsNull(const AName: string): Boolean; override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;
    function  GetFieldIsNull(const AName: string): Boolean; override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    function    ExecSQL: integer; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure   AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AStream : TStream); override;
    procedure   AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

  end;

implementation
uses
  tiUtils,
  tiLog,
  TypInfo,
  SysUtils,
{$IFDEF DELPHI6ORABOVE}
  Variants,
{$ENDIF}
  tiOPFManager,
  tiObject,
  tiConstants,
  tiExcept;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryFIBP
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiQueryFIBP.Create;
begin
  inherited;
  FIBSQL := TFIBQuery.Create(nil);
end;


destructor TtiQueryFIBP.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;


procedure TtiQueryFIBP.Close;
begin
  Active := false;
end;


function TtiQueryFIBP.ExecSQL: integer;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Prepare;
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  FIBSQL.ExecQuery;
  Result := -1;
  { TODO :
When implementing RowsAffected,
please return correct result
and put FSupportsRowsAffected := True; in TtiQueryXXX.Create;}
end;


function TtiQueryFIBP.GetFieldAsBoolean(const AName: string): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(upperCase(FIBSQL.FieldByName(UpperCase(AName)).AsString));
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryFIBP.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsDateTime;
end;


function TtiQueryFIBP.GetFieldAsFloat(const AName: string): extended;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsDouble;
end;


function TtiQueryFIBP.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsInt64;
end;

function TtiQueryFIBP.GetFieldAsString(const AName: string): string;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsString;
end;

function TtiQueryFIBP.GetFieldAsStringByIndex(AIndex: Integer): string    ;
begin
  Result := FIBSQL.Fields[AIndex].AsString;
end;

function TtiQueryFIBP.GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ;
begin
  Result := FIBSQL.Fields[AIndex].AsDouble;
end;

function TtiQueryFIBP.GetFieldAsBooleanByIndex(AIndex: Integer): boolean ;
var
  lsValue: string;
begin
  lsValue := Trim(FIBSQL.Fields[AIndex].AsString);
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQueryFIBP.GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ;
begin
  Result := FIBSQL.Fields[AIndex].AsInt64;
end;

function TtiQueryFIBP.GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime;
begin
  Result := FIBSQL.Fields[AIndex].AsDateTime;
end;

function TtiQueryFIBP.GetFieldIsNullByIndex(AIndex: Integer):Boolean      ;
begin
  Result := FIBSQL.Fields[AIndex].IsNull;
end;

function TtiQueryFIBP.GetActive: boolean;
begin
  result := FbActive;
end;

function TtiQueryFIBP.GetEOF: boolean;
begin
  result := FIBSQL.EOF;
end;

function TtiQueryFIBP.GetParamAsBoolean(const AName: string): boolean;
var
  lValue: string;
begin
  lValue := FIBSQL.Params.ByName[UpperCase(AName)].AsString;
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
end;


function TtiQueryFIBP.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FIBSQL.Params.ByName[ UpperCase(AName)].AsDateTime;
end;


function TtiQueryFIBP.GetParamAsFloat(const AName: string): extended;
begin
  result := FIBSQL.Params.ByName[ UpperCase(AName)].AsDouble;
end;


function TtiQueryFIBP.GetParamAsInteger(const AName: string): Int64;
begin
  result := FIBSQL.Params.ByName[ UpperCase(AName)].AsInt64;
end;

function TtiQueryFIBP.GetParamAsString(const AName: string): string;
begin
  result := FIBSQL.Params.ByName[ UpperCase(AName)].AsString;
end;

function TtiQueryFIBP.GetSQL: TStrings;
begin
  result := FIBSQL.SQL;
end;

procedure TtiQueryFIBP.Next;
begin
  FIBSQL.Next;
end;

procedure TtiQueryFIBP.Open;
begin
  Log(ClassName + ': ' + tiNormalizeStr(self.SQLText), lsSQL);
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  Active := true;
end;


function TtiQueryFIBP.ParamCount: integer;
begin
  result := FIBSQL.Params.Count;
end;


function TtiQueryFIBP.ParamName(AIndex: integer): string;
begin
  result := FIBSQL.Params[AIndex].Name;
end;


procedure TtiQueryFIBP.SetActive(const AValue: boolean);
begin
  Assert(Database.TestValid(TtiDatabase), CTIErrorInvalidObject);
  if AValue then
  begin
    FIBSQL.ExecQuery;
    FbActive := true;
  end else
  begin
    FIBSQL.Close;
    FbActive := false;
  end;
end;


procedure TtiQueryFIBP.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  Prepare;
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FIBSQL.Params.ByName[ UpperCase(AName)].AsString := 'T'
  else
    FIBSQL.Params.ByName[ UpperCase(AName)].AsString := 'F';
{$ELSE}
  {$IFDEF BOOLEAN_NUM_1}
    if AValue then
      FIBSQL.ParamByName(AName).AsInt64 := 1
    else
      FIBSQL.ParamByName(AName).AsInt64 := 0;
  {$ELSE}
    if AValue then
      FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'TRUE'
    else
      FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'FALSE';
  {$ENDIF}
{$ENDIF} // BOOLEAN_CHAR_1
end;


procedure TtiQueryFIBP.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
  Prepare;
  FIBSQL.Params.ByName[ UpperCase(AName)].AsDateTime := AValue;
end;


procedure TtiQueryFIBP.SetParamAsFloat(const AName: string; const AValue: extended);
begin
  Prepare;
  FIBSQL.Params.ByName[ UpperCase(AName)].AsDouble := AValue;
end;


procedure TtiQueryFIBP.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  Prepare;
  FIBSQL.Params.ByName[ UpperCase(AName)].AsInt64 :=  AValue;
end;

procedure TtiQueryFIBP.SetParamAsString(const AName, AValue: string);
begin
  Prepare;
  FIBSQL.Params.ByName[ UpperCase(AName)].AsString := AValue;
end;

procedure TtiQueryFIBP.SetSQL(const AValue: TStrings);
begin
  FIBSQL.SQL.Assign(AValue);
end;

procedure TtiQueryFIBP.Prepare;
begin
  if FIBSQL.Prepared then
    Exit; //==>
  FIBSQL.Prepare;
end;

procedure TtiQueryFIBP.AssignParamFromStream(const AName: string; const AStream: TStream);
var
  LPos: integer;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  Prepare;
  LPos:= AStream.Position;
  FIBSQL.Params.ByName[ UpperCase(AName)].LoadFromStream(AStream);

  // need this as without it the streamed param
  // is in intermediate buffer and cannot be read back
  THackFibQuery(FIBSQL).SaveStreamedParams;

  AStream.Position:= LPos;
end;

procedure TtiQueryFIBP.AssignParamToStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FIBSQL.Params.ByName[ UpperCase(AName)].SaveToStream(AStream);
  AStream.Position := 0;
end;


procedure TtiQueryFIBP.AssignFieldAsStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Size:= 0 ;
  FIBSQL.FieldByName( UpperCase(AName)).SaveToStream(AStream);
  AStream.Position:= 0 ;
end;

procedure TtiQueryFIBP.AssignFieldAsStreamByIndex(AIndex : integer; const AStream : TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Size:= 0 ;
  FIBSQL.Fields[AIndex].SaveToStream(AStream);
  AStream.Position := 0;
end;

procedure TtiQueryFIBP.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  FIBSQL.Database := TtiDatabaseFIBP(ADatabase).NativeDatabase;
end;


procedure TtiQueryFIBP.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Open then
    FIBSQL.Close;
  FIBSQL.Transaction := nil;
  FIBSQL.Database := nil;
end;

function TtiQueryFIBP.FieldCount: integer;
begin
  if FIBSQL.EOF then
    result := 0
  else
    result := FIBSQL.Current.Count;
end;


function TtiQueryFIBP.FieldName(AIndex: integer): string;
begin
  result := FIBSQL.Fields[AIndex].Name;
end;


procedure TtiQueryFIBP.Reset;
begin
  Active := false;
  FIBSQL.SQL.Clear;
end;

function TtiQueryFIBP.FieldIndex(const AName: string): integer;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).Index;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseFIBP
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiDatabaseFIBP.Create;
begin
  inherited Create;
  FDatabase := TpFIBDatabase.Create(nil);
  FDatabase.UseLoginPrompt := false;
  FDatabase.SQLDialect := 3; // IK added.
  FTransaction := TpFIBTransaction.Create(nil);
  with FTransaction.TRParams do
  begin
    Add('read_committed');
    Add('rec_version');
    Add('nowait');
  end;

  FDatabase.DefaultTransaction := FTransaction;
  FTransaction.DefaultDatabase := FDatabase;
end;


destructor TtiDatabaseFIBP.Destroy;
begin
  try
    FTransaction.Active := false;
    FDatabase.Connected := false;
    FDatabase.DefaultTransaction := nil;
    FTransaction.DefaultDatabase := nil;
    FTransaction.Free;
    FDatabase.Free;
  except
    on e: exception do
      LogError(e.message);
  end;
  inherited;
end;


procedure TtiDatabaseFIBP.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  Log(ClassName + ': [Commit Trans]', lsSQL);
  FTransaction.Commit;
end;


function TtiDatabaseFIBP.InTransaction: boolean;
begin
  result := FTransaction.InTransaction;
end;


procedure TtiDatabaseFIBP.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  FTransaction.RollBack;
end;


procedure TtiDatabaseFIBP.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  Log(ClassName + ': [Start Trans]', lsSQL);
  FTransaction.StartTransaction;
end;

function TtiQueryFIBP.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  result := FieldKindToTIFieldKind( FIBSQL.Fields[AIndex].AsXSQLVAR);

  if (result = qfkString) then
  begin
    lValue := FIBSQL.Fields[AIndex].AsString;
    // ToDo: How to detect a logical field in a SQL database
    //       where logicals are represented as VarChar or Char?
    //       In the IBX layer we are currently looking for a value of
    //       'TRUE' or 'FALSE', but this is not reliable as it will not
    //       detect a null. Also, other ways of representing booleans
    //       might be used like 'T', 'F', '0', '1', etc...
    {$IFDEF BOOLEAN_CHAR_1}
      if SameText(lValue, 'T') or
         SameText(lValue, 'F') then
        result := qfkLogical;
    {$ELSE}
      if SameText(lValue, 'TRUE') or
         SameText(lValue, 'TRUE ') or
         SameText(lValue, 'FALSE') then
    {$ENDIF} // BOOLEAN_CHAR_1
  end;
end;

function TtiQueryFIBP.FieldKindToTIFieldKind(AData : PXSQLVAR): TtiQueryFieldKind;
begin
  case (AData^.sqltype and (not 1))of
    SQL_TEXT, SQL_VARYING:                       result := qfkString;
    SQL_LONG, SQL_SHORT, SQL_INT64, SQL_QUAD:    if AData.SqlScale = 0 then
                                                   result := qfkInteger
                                                 else
                                                   result := qfkFloat;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:          result := qfkFloat;
    SQL_BLOB: begin
                case AData.sqlsubtype of
                isc_blob_untyped:                result := qfkBinary;
                isc_blob_text:                   result := qfkLongString;
//  isc_blob_blr                   =          2;
//  isc_blob_acl                   =          3;
//  isc_blob_ranges                =          4;
//  isc_blob_summary               =          5;
//  isc_blob_format                =          6;
//  isc_blob_tra                   =          7;
//  isc_blob_extfile               =          8;
                else
                  raise EtiOPFInternalException.Create('Invalid Interbase/FireBird/IBX sqlsubtype');
                end;
              end;
    SQL_TIMESTAMP, SQL_TYPE_TIME, SQL_TYPE_DATE: result := qfkDateTime;
  else
    raise EtiOPFInternalException.Create('Invalid Interbase sqltype');
  end;
  // These constants are defined in IBHeader.pas
//  SQL_VARYING                    =        448;
//  SQL_TEXT                       =        452;
//  SQL_DOUBLE                     =        480;
//  SQL_FLOAT                      =        482;
//  SQL_LONG                       =        496;
//  SQL_SHORT                      =        500;
//  SQL_TIMESTAMP                  =        510;
//  SQL_BLOB                       =        520;
//  SQL_D_FLOAT                    =        530;
//  SQL_ARRAY                      =        540;
//  SQL_QUAD                       =        550;
//  SQL_TYPE_TIME                  =        560;
//  SQL_TYPE_DATE                  =        570;
//  SQL_INT64                      =        580;
//  SQL_DATE                       =        SQL_TIMESTAMP;

//  isc_blob_untyped               =          0;
//  isc_blob_text                  =          1;
//  isc_blob_blr                   =          2;
//  isc_blob_acl                   =          3;
//  isc_blob_ranges                =          4;
//  isc_blob_summary               =          5;
//  isc_blob_format                =          6;
//  isc_blob_tra                   =          7;
//  isc_blob_extfile               =          8;

end;

function TtiQueryFIBP.FieldSize(AIndex: integer): integer;
begin
  if FieldKind(AIndex) in [ qfkInteger, qfkFloat, qfkDateTime,
                            qfkLogical, qfkLongString ] then
    result := 0
  else
    result := FIBSQL.Fields[AIndex].Size;
end;

function TtiQueryFIBP.GetParamIsNull(const AName: string): Boolean;
begin
  result := FIBSQL.Params.ByName[ UpperCase(AName)].IsNull;
end;

procedure TtiQueryFIBP.SetParamIsNull(const AName: string; const AValue: Boolean);
begin
  Prepare;
  FIBSQL.Params.ByName[ UpperCase(AName)].IsNull := true;
end;

function TtiDatabaseFIBP.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseFIBP.SetConnected(AValue: boolean);
var
  lMessage: string;
  i: integer;
begin

  try
    if (not AValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      FDatabase.Connected := false;
      Exit; //==>
    end;

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.DBParams.Values['user_name'] := UserName;
    FDatabase.DBParams.Values['password'] := Password;

    for i := 0 to Params.Count - 1 do
      FDatabase.DBParams.Add(Params[i]);

{ Defined in IB unit:
  TTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc);

  Now, use the TraceLevel value to map to IB specific trace flags.  These assignments could be implemented in any way,
  but for maximum fun and flexibility, let's use binary arithmetic:-)
}

{
    // tracing is a bit different in FIBPlus

    FDatabase.TraceFlags := [];
    if (TraceLevel and $0001) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfMisc    ];
    if (TraceLevel and $0002) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfService ];
    if (TraceLevel and $0004) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfBlob    ];
    if (TraceLevel and $0008) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfTransact];
    if (TraceLevel and $0010) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfConnect ];
    if (TraceLevel and $0020) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfStmt    ];
    if (TraceLevel and $0040) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfError   ];
    if (TraceLevel and $0080) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfQFetch  ];
    if (TraceLevel and $0100) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfQExecute];
    if (TraceLevel and $0200) <> 0 then FDatabase.TraceFlags := FDatabase.TraceFlags + [tfQPrepare];
}
    FDatabase.Connected := true;

  except
    // ToDo: Must come up with a better solution that this:
    //       Try several times before raising an exception.
    //       Rather than calling 'Halt', just terminate this database connection,
    //       unless this is the first connection.
    on E: EFIBInterBaseError do
    begin
      // Invalid username / password error
      if E.IBErrorCode = 335544472 then
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistFIBP, DatabaseName, UserName, Password)
      else
      begin
        lMessage :=
          'Error attempting to connect to database.' + Cr +
          E.Message + Cr +
          'Error code: ' + IntToStr(E.IBErrorCode);
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistFIBP, DatabaseName, UserName, Password, lMessage)
      end;
    end
  else
    raise EtiOPFDBException.Create(cTIPersistFIBP, DatabaseName, UserName, Password)
  end;
end;

function TtiQueryFIBP.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).IsNull;
end;

// See borland communit article:
//   http://community.borland.com/article/0,1410,25259,00.html
// for more system table queries, or search google on 'interbase system tables'

procedure TtiDatabaseFIBP.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      { SQL Views are now also included }
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        '  FROM RDB$RELATIONS ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL)) ' +
//        '  AND (RDB$VIEW_SOURCE IS NULL) ' +
        'ORDER BY RDB$RELATION_NAME ';
      lQuery.Open;
      while not lQuery.EOF do
      begin
        lTable := TtiDBMetaDataTable.Create;
        lTable.Name := Trim(lQuery.FieldAsString['table_name']);
        lTable.ObjectState := posPK;
        lMetaData.Add(lTable);
        lQuery.Next;
      end;
      lQuery.DetachDatabase;
      lMetaData.ObjectState := posClean;
    finally
      Commit;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseFIBP.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableName : string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType : integer;
  lFieldLength : integer;
const
// Interbase field types
//   select * from rdb$types
//   where rdb$field_NAME = 'RDB$FIELD_TYPE'
//   ORDER BY RDB$TYPE

  cIBField_LONG       = 8;
  cIBField_DOUBLE     = 27;
  cIBField_TIMESTAMP  = 35;
  cIBField_DATE       = 12;
  cIBField_TIME       = 13;
  cIBField_VARYING    = 37;
  cIBField_BLOB       = 261;

  cIBField_SHORT      = 7;
  cIBField_QUAD       = 9;
  cIBField_FLOAT      = 10;
  cIBField_TEXT       = 14;
  cIBField_CSTRING    = 40;
  cIBField_BLOB_ID    = 45;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        '  select ' +
        '    r.rdb$field_name     as field_name ' +
        '    ,rdb$field_type      as field_type ' +
        '    ,rdb$field_sub_type  as field_sub_type ' +
        '    ,rdb$field_length    as field_length ' +
        '  from ' +
        '    rdb$relation_fields r ' +
        '    ,rdb$fields f ' +
        '  where ' +
        '      r.rdb$relation_name = ''' + lTableName + '''' +
        '  and f.rdb$field_name = r.rdb$field_source ';

      lQuery.Open;
      while not lQuery.EOF do
      begin
        lField := TtiDBMetaDataField.Create;
        lField.Name := Trim(lQuery.FieldAsString['field_name']);
        lFieldType := lQuery.FieldAsInteger['field_type'];
        lFieldLength := lQuery.FieldAsInteger['field_length'];

        lField.Width := 0;

        case lFieldType of
          cIBField_LONG      : lField.Kind := qfkInteger;
          cIBField_DOUBLE    : lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME      : lField.Kind := qfkDateTime;
          cIBField_VARYING,
          cIBField_TEXT      : begin
                                  lField.Kind := qfkString;
                                  lField.Width := lFieldLength;
                                end;
          cIBField_BLOB      : begin
                                  Assert(not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
                                  if lQuery.FieldAsInteger['field_sub_type'] = 0 then
                                    lField.Kind := qfkBinary
                                  else
                                  if lQuery.FieldAsInteger['field_sub_type'] = 1 then
                                    lField.Kind := qfkLongString
                                  else
                                    raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>');
                                end;
        else
          raise EtiOPFInternalException.Create('Invalid Interbase FieldType <' + IntToStr(lFieldType) + '>');
        end;
        lField.ObjectState := posClean;
        lTable.Add(lField);
        lQuery.Next;
      end;
    finally
      Commit;
    end;
    lQuery.DetachDatabase;
    lTable.ObjectState := posClean;
  finally
    lQuery.Free;
  end;
end;

function TtiDatabaseFIBP.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName : string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString: result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal(10, 5)';
    qfkFloat: result := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: if NativeDatabase.SQLDialect <> 1 then
        result := 'TIMESTAMP'
      else
        result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical   : result := 'Char(1) default ''F'' check(' + lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical   : result := 'VarChar(5) default ''FALSE'' check(' + lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary: result := 'Blob sub_type 0';
    qfkLongString: result := 'Blob sub_type 1';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

// ToDo: Shouldn't this be in the abstract?
procedure TtiQueryFIBP.AssignParams(const AParams, AWhere: TtiQueryParams);
begin
  if AParams = nil then
    Exit;
  Prepare;
  inherited;
end;


class procedure TtiDatabaseFIBP.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  LDatabase : TtiDatabaseFIBP;
  LPath: string;
begin
  LPath:= ExtractFilePath(ADatabaseName);
  if not DirectoryExists(LPath) then
    ForceDirectories(LPath);
  LDatabase := TtiDatabaseFIBP.Create;
  try
    LDatabase.FDatabase.DatabaseName := ADatabaseName;
    LDatabase.FDatabase.DBParams.Add('USER "' + AUserName + '"');
    LDatabase.FDatabase.DBParams.Add('PASSWORD "' + APassword + '"');
    LDatabase.FDatabase.CreateDatabase;
  finally
    LDatabase.Free;
  end;
end;

class procedure TtiDatabaseFIBP.DropDatabase(const ADatabaseName, AUserName,
  APassword: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;


class function TtiDatabaseFIBP.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
var
  lDatabase : TtiDatabaseFIBP;
begin
  lDatabase := TtiDatabaseFIBP.Create;
  try
    lDatabase.FDatabase.DatabaseName := ADatabaseName;
    lDatabase.FDatabase.DBParams.Values['user_name']:= AUserName;
    lDatabase.FDatabase.DBParams.Values['password']:= APassword;
    try
      lDatabase.FDatabase.Connected := true;
      result := true;
    except
      on e:exception do
        result := false;
    end;
    lDatabase.FDatabase.Connected := false;
  finally
    lDatabase.Free;
  end;
end;

function TtiQueryFIBP.HasNativeLogicalType: boolean;
begin
  result := false;
end;

function TtiDatabaseFIBP.Test: boolean;
begin
  result := Connected;
  // ToDo: Not ideal. Should select a record to confirm connection is OK
end;

function TtiDatabaseFIBP.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryFIBP;
end;

{ TtiPersistenceLayerIBX }

procedure TtiPersistenceLayerFIBP.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistFIBP;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'SYSDBA';
  APersistenceLayerDefaults.Password:= 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerFIBP.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseFIBP;
end;

function TtiPersistenceLayerFIBP.GetPersistenceLayerName: string;
begin
  result:= cTIPersistFIBP;
end;

function TtiPersistenceLayerFIBP.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryFIBP;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerFIBP);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistFIBP);

end.


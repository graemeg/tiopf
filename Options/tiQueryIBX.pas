unit tiQueryIBX;

{$I tiDefines.inc}

interface
uses
  tiQuery
  , Classes
  , IB
  , IBDatabase
  , IBSQL
  , tiDBConnectionPool
  , IBHeader
  , tiAutoMap
  , tiObject
 ;

// Turn this on if you have upgraded you IBX from the version that comes
// out of the box with Delphi. If you have not upgraded your IBX components,
// you will have to comment the $DEFINE below out.
// IBX can be upgraded from:
//   http://codecentral.borland.com/codecentral/ccweb.exe/author?authorid=102
{$IFDEF DELPHI6ORABOVE}
  {$DEFINE IBXx08ORABOVE}
{$ENDIF}

type

  TtiDatabaseIBX = class(TtiDatabaseSQL)
  private
    FDatabase: TIBDataBase;
    FIBTransaction: TIBTransaction;
  protected
    procedure SetConnected(AValue: boolean); override;
    function  GetConnected: boolean; override;
    function  FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    property        IBDatabase: TIBDatabase read FDatabase write FDatabase;
    procedure       StartTransaction; override;
    function        InTransaction: boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    procedure       ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;

  end;

  TtiQueryIBX = class(TtiQuerySQL)
  private
    FIBSQL: TIBSQL;
    FbActive: boolean;
    // If you build is falling over here, see the note around line 50 on IBX versions.
    {$IFDEF IBXx08ORABOVE}
      function IBFieldKindToTIFieldKind(AData : TSQLVAR): TtiQueryFieldKind;
    {$ELSE}
      function IBFieldKindToTIFieldKind(AData : PXSQLVAR): TtiQueryFieldKind;
    {$ENDIF}
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
    procedure   ExecSQL; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure   AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AValue : TStream); override;
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
   tiUtils
//  ,tiDialogs
  ,tiLog
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
{$IFDEF DELPHI6ORABOVE}
  ,Variants
{$ENDIF}
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryIBX
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiQueryIBX.Create;
begin
  inherited;
  FIBSQL := TIBSQL.Create(nil);
end;


destructor TtiQueryIBX.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;


procedure TtiQueryIBX.Close;
begin
  Active := false;
end;


procedure TtiQueryIBX.ExecSQL;
begin
  Prepare;
  FIBSQL.ExecQuery;
end;


function TtiQueryIBX.GetFieldAsBoolean(const AName: string): boolean;
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


function TtiQueryIBX.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsDateTime;
end;


function TtiQueryIBX.GetFieldAsFloat(const AName: string): extended;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsDouble;
end;


function TtiQueryIBX.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsInt64;
end;

function TtiQueryIBX.GetFieldAsString(const AName: string): string;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).AsString;
end;

function TtiQueryIBX.GetFieldAsStringByIndex(AIndex: Integer): string    ;
begin
  Result := FIBSQL.Fields[AIndex].AsString;
end;

function TtiQueryIBX.GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ;
begin
  Result := FIBSQL.Fields[AIndex].AsDouble;
end;

function TtiQueryIBX.GetFieldAsBooleanByIndex(AIndex: Integer): boolean ;
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

function TtiQueryIBX.GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ;
begin
  Result := FIBSQL.Fields[AIndex].AsInt64;
end;

function TtiQueryIBX.GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime;
begin
  Result := FIBSQL.Fields[AIndex].AsDateTime;
end;

function TtiQueryIBX.GetFieldIsNullByIndex(AIndex: Integer):Boolean      ;
begin
  Result := FIBSQL.Fields[AIndex].IsNull;
end;

function TtiQueryIBX.GetActive: boolean;
begin
  result := FbActive;
end;

function TtiQueryIBX.GetEOF: boolean;
begin
  result := FIBSQL.EOF;
end;

function TtiQueryIBX.GetParamAsBoolean(const AName: string): boolean;
var
  lValue: string;
begin
  lValue := FIBSQL.Params.ByName(UpperCase(AName)).AsString;
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
end;


function TtiQueryIBX.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FIBSQL.Params.ByName(UpperCase(AName)).AsDateTime;
end;


function TtiQueryIBX.GetParamAsFloat(const AName: string): extended;
begin
  result := FIBSQL.Params.ByName(UpperCase(AName)).AsDouble;
end;


function TtiQueryIBX.GetParamAsInteger(const AName: string): Int64;
begin
  result := FIBSQL.Params.ByName(UpperCase(AName)).AsInt64;
end;

function TtiQueryIBX.GetParamAsString(const AName: string): string;
begin
  result := FIBSQL.Params.ByName(UpperCase(AName)).AsString;
end;

function TtiQueryIBX.GetSQL: TStrings;
begin
  result := FIBSQL.SQL;
end;

procedure TtiQueryIBX.Next;
begin
  FIBSQL.Next;
end;

procedure TtiQueryIBX.Open;
begin
  Active := true;
end;


function TtiQueryIBX.ParamCount: integer;
begin
  result := FIBSQL.Params.Count;
end;


function TtiQueryIBX.ParamName(AIndex: integer): string;
begin
  result := FIBSQL.Params[AIndex].Name;
end;


procedure TtiQueryIBX.SetActive(const AValue: boolean);
begin
  Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
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


procedure TtiQueryIBX.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  Prepare;
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'T'
  else
    FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'F';
{$ELSE}
  if AValue then
    FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'TRUE'
  else
    FIBSQL.Params.ByName(UpperCase(AName)).AsString := 'FALSE';
{$ENDIF} // BOOLEAN_CHAR_1
end;


procedure TtiQueryIBX.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
  Prepare;
  FIBSQL.Params.ByName(UpperCase(AName)).AsDateTime := AValue;
end;


procedure TtiQueryIBX.SetParamAsFloat(const AName: string; const AValue: extended);
begin
  Prepare;
  FIBSQL.Params.ByName(UpperCase(AName)).AsDouble := AValue;
end;


procedure TtiQueryIBX.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  Prepare;
//  Log('Param as integer : '+psNAme,lsUserInfo);
//  if High(Integer) < AValue then
//  begin
//    Log('Int64',lsUserInfo);
//    FIBSQL.Params.ByName(UpperCase(AName)).AsInt64 := AValue;
//  end
//  else
//  begin
//    Log('Integer',lsUserInfo);
//    FIBSQL.Params.ByName(UpperCase(AName)).AsInteger :=  Integer(AValue);
    FIBSQL.Params.ByName(UpperCase(AName)).AsInt64 :=  AValue;
//  end;
end;

procedure TtiQueryIBX.SetParamAsString(const AName, AValue: string);
begin
  Prepare;
  FIBSQL.Params.ByName(UpperCase(AName)).AsString := AValue;
end;

procedure TtiQueryIBX.SetSQL(const AValue: TStrings);
begin
  FIBSQL.SQL.Assign(AValue);
end;

procedure TtiQueryIBX.Prepare;
begin
  if FIBSQL.Prepared then
    Exit; //==>
  FIBSQL.Prepare;
end;

procedure TtiQueryIBX.AssignParamFromStream(const AName: string; const AStream: TStream);
var
  LPos: integer;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  Prepare;
  LPos:= AStream.Position;
  FIBSQL.Params.ByName(UpperCase(AName)).LoadFromStream(AStream);
  AStream.Position:= LPos;
end;

procedure TtiQueryIBX.AssignParamToStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FIBSQL.Params.ByName(UpperCase(AName)).SaveToStream(AStream);
  AStream.Position := 0;
end;


procedure TtiQueryIBX.AssignFieldAsStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FIBSQL.FieldByName(UpperCase(AName)).SaveToStream(AStream);
  AStream.Position:= 0 ;
end;

procedure TtiQueryIBX.AssignFieldAsStreamByIndex(AIndex : integer; const AValue : TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  FIBSQL.Fields[AIndex].SaveToStream(AValue);
end;

procedure TtiQueryIBX.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  FIBSQL.Database := TtiDatabaseIBX(ADatabase).IBDatabase;
end;


procedure TtiQueryIBX.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Open then
    FIBSQL.Close;
  FIBSQL.Transaction := nil;
  FIBSQL.Database := nil;
end;

function TtiQueryIBX.FieldCount: integer;
begin
  if FIBSQL.EOF then
    result := 0
  else
    result := FIBSQL.Current.Count;
end;


function TtiQueryIBX.FieldName(AIndex: integer): string;
begin
  result := FIBSQL.Fields[AIndex].Name;
end;


procedure TtiQueryIBX.Reset;
begin
  Active := false;
  FIBSQL.SQL.Clear;
end;

function TtiQueryIBX.FieldIndex(const AName: string): integer;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).Index;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseIBX
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiDatabaseIBX.Create;
begin
  inherited Create;
  FDatabase := TIBDataBase.Create(nil);
  FDatabase.LoginPrompt := false;
  FDatabase.SQLDialect := 3; // IK added.
  FIBTransaction := TIBTransaction.Create(nil);
  FDatabase.DefaultTransaction := FIBTransaction;
  FIBTransaction.DefaultDatabase := FDatabase;
end;


destructor TtiDatabaseIBX.Destroy;
begin
  try
    FIBTransaction.Active := false;
    FDatabase.Connected := false;
    FDatabase.DefaultTransaction := nil;
    FIBTransaction.DefaultDatabase := nil;
    FIBTransaction.Free;
    FDatabase.Free;
  except
    on e: exception do
      LogError(e.message);
  end;
  inherited;
end;


procedure TtiDatabaseIBX.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FIBTransaction.Commit;
end;


function TtiDatabaseIBX.InTransaction: boolean;
begin
  result := FIBTransaction.InTransaction;
end;


procedure TtiDatabaseIBX.RollBack;
begin
  FIBTransaction.RollBack;
end;


procedure TtiDatabaseIBX.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  FIBTransaction.StartTransaction;
end;

// This code is cloned in TtiQueryBDEAbs - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery

function TtiQueryIBX.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  // See note at top of this file re IBX versions
  {$IFDEF IBXx08ORABOVE}
    result := IBFieldKindToTIFieldKind(FIBSQL.Fields[AIndex].SqlVar);
  {$ELSE}
    result := IBFieldKindToTIFieldKind(FIBSQL.Fields[AIndex].AsXSQLVAR);
  {$ENDIF}
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

// This code is cloned in TtiQueryBDEAbs - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery

{$IFDEF IBXx08ORABOVE}
function TtiQueryIBX.IBFieldKindToTIFieldKind(AData : TSQLVar): TtiQueryFieldKind;
begin
  case (AData.sqltype and (not 1))of
{$ELSE}
function TtiQueryIBX.IBFieldKindToTIFieldKind(AData : PXSQLVAR): TtiQueryFieldKind;
begin
  case (AData^.sqltype and (not 1))of
{$ENDIF}
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

function TtiQueryIBX.FieldSize(AIndex: integer): integer;
begin
  if FieldKind(AIndex) in [ qfkInteger, qfkFloat, qfkDateTime,
                            qfkLogical, qfkLongString ] then
    result := 0
  else
    result := FIBSQL.Fields[AIndex].Size;
end;

function TtiQueryIBX.GetParamIsNull(const AName: string): Boolean;
begin
  result := FIBSQL.Params.ByName(UpperCase(AName)).IsNull;
end;

procedure TtiQueryIBX.SetParamIsNull(const AName: string; const AValue: Boolean);
begin
  Prepare;
  FIBSQL.Params.ByName(UpperCase(AName)).IsNull := true;
end;

function TtiDatabaseIBX.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseIBX.SetConnected(AValue: boolean);
var
  lMessage : string;
begin

  try
    if (not AValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      FDatabase.Connected := false;
      Exit; //==>
    end;

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.Params.Values['user_name']:= UserName;
    FDatabase.Params.Values['password']:= Password;

{ Defined in IB unit:
  TTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc);

  Now, use the TraceLevel value to map to IB specific trace flags.  These assignments could be implemented in any way,
  but for maximum fun and flexibility, let's use binary arithmetic:-)
}
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

    FDatabase.Connected := true;

  except
    // ToDo: Must come up with a better solution that this:
    //       Try several times before raising an exception.
    //       Rather than calling 'Halt', just terminate this database connection,
    //       unless this is the first connection.
    on e: EIBError do
    begin
      // Invalid username / password error
      if (EIBError(E).IBErrorCode = 335544472) then
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistIBX, DatabaseName, UserName, Password)
      else
      begin
        lMessage :=
          'Error attempting to connect to database.' + Cr +
          e.Message + Cr +
          'Error code: ' + IntToStr(EIBError(E).IBErrorCode);
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistIBX, DatabaseName, UserName, Password, lMessage)
      end;
    end
  else
    raise EtiOPFDBException.Create(cTIPersistIBX, DatabaseName, UserName, Password)
  end;
end;

function TtiQueryIBX.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FIBSQL.FieldByName(UpperCase(AName)).IsNull;
end;

// See borland communit article:
//   http://community.borland.com/article/0,1410,25259,00.html
// for more system table queries, or search google on 'interbase system tables'

procedure TtiDatabaseIBX.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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

procedure TtiDatabaseIBX.ReadMetaDataFields(AData: TtiDBMetaDataTable);
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
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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

// This SQL was found somewhere on the web...
//      lQuery.SQLText :=
//        'select r.rdb$field_name as Column_Name,  ' +
//        '	t.rdb$type_name, ' +
//        '	f.rdb$field_length ' +
//        ' ' +
//        'from 	rdb$relation_fields r, rdb$types t, rdb$fields f  ' +
//        ' ' +
//        'where 	r.rdb$relation_name=''' + lTable.Name + ''' and  ' +
//        '	f.rdb$field_name=r.rdb$field_source and  ' +
//        '	t.rdb$field_name=''RDB$FIELD_TYPE'' and  ' +
//        '	f.rdb$field_type=t.rdb$type ';

end;

function TtiDatabaseIBX.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
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
    qfkDateTime: if IBDatabase.SQLDialect <> 1 then
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
procedure TtiQueryIBX.AssignParams(const AParams, AWhere: TtiQueryParams);
begin
  if AParams = nil then
    Exit;
  Prepare;
  inherited;
end;


{
Peter,

Yes, there is a more elegant way, but you may have to write a more code.
There is a white paper on the Borland site (I can't remember where - you
will have to search) which describes how to do an embedded install of
Interpose. It was written for IB5, but according to the newsgroups it is
equally applicable to both IB6 and Firebird. Part of the white paper is
instruction on how to detect if IB is installed (checking some registry
settings and testing for the existence of .

I don't know if you are using either IBX components or IBO components.
If you are using IBX components, and you have the latest version of the
component suite from code central, there are also components to detect
the presence of the server. There are also components to administer the
server. I suspect on of these components gives you the ability to test
for the existence and validity of a database, without having to actually
attempt to connect to the database.

I know I haven't answered you question in detail, but this may help to
point you in the right direction.

Faithfully,
Sean B. Durkin

}

{
>a) Is there Interbase/Firebird running at the given location;
>b) Can the GDB file be found at that location.

This routine should do both.  Assumes an "IBServerProperties" component is
on the form, so you should be safe to create one temporarily. Also, makes
reference to external names, so hope it makes sense
eg.
 const sLocalhost = 'Localhost'
and
 ExpandIBFileName returns full filename (server:path\filename.GDB)

Yell out if something isn't clear.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TfrmMainConfig.IBXTestConnectionOK: Boolean;
var
  saveCursor: TCursor;
begin
  result := False;
  StatusBar.SimpleCaption   := '';

  IBServerProperties.Params.Clear;
  IBServerProperties.Params.Append('user_name=' + uUser);
  IBServerProperties.Params.Append('password=' + uPwd);

  IBDatabase.Params.Clear;
  IBDatabase.Params.Append('user_name=' + uUser);
  IBDatabase.Params.Append('password=' + uPwd);

  IBServerProperties.ServerName  := IBXLocationServerEdit.Text;
  if SameText(IBServerProperties.ServerName, sLocalhost) then
    IBServerProperties.Protocol := Local
  else
    IBServerProperties.Protocol := TCP;
  IBServerProperties.Options := [Version];
  IBXTestResultLabel.Font.Color := clWindowText;
  IBXTestResultLabel.Caption := 'Connecting...';

  saveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    IBXTestAction.Enabled := False;
    try
      IBServerProperties.Active := True;
      IBServerProperties.FetchVersionInfo;
      IBXTestVersionLabel.Caption  :=
IBServerProperties.VersionInfo.ServerVersion;

      IBDatabase.DatabaseName   := ExpandIBFileName(
IBXLocationServerEdit.Text, IBXLocationDatabaseNameEdit.Text);
      IBDatabase.Connected      := True;
      IBDatabase.Connected      := False;

      IBXTestResultLabel.Font.Color := clGreen;
      IBXTestResultLabel.Caption   := 'OK';
      IBServerProperties.Active := False;
      result                    := True;
    except
      on e: Exception do begin
        IBXTestResultLabel.Font.Color := clRed;
        IBXTestResultLabel.Caption   := 'Failed';
        StatusBar.SimpleCaption   := 'Connection Error: ' + e.Message;
        MessageBox(0, PChar(e.Message), PChar('Connection Failed'),
MB_OK + MB_ICONWARNING);
        end;
      end;
  finally
    Screen.Cursor            := saveCursor;
    IBXTestAction.Enabled       := True;
    IBServerProperties.Active := False;
    IBDatabase.Connected     := False;
    end;
end;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ian Krigsman

Discovery Systems
www.discoverysystems.com.au
}



class procedure TtiDatabaseIBX.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  LDatabase : TtiDatabaseIBX;
  LPath: string;
begin
  LPath:= ExtractFilePath(ADatabaseName);
  if not DirectoryExists(LPath) then
    ForceDirectories(LPath);
  LDatabase := TtiDatabaseIBX.Create;
  try
    LDatabase.FDatabase.DatabaseName := ADatabaseName;
    LDatabase.FDatabase.Params.Add('USER "' + AUserName + '"');
    LDatabase.FDatabase.Params.Add('PASSWORD "' + APassword + '"');
    LDatabase.FDatabase.CreateDatabase;
  finally
    LDatabase.Free;
  end;
end;

// Test is an Interbase server is available. Will not test for the existance of a GDB
// file, just if the server is up. Will also return the server version.
// Problem is, that this routine takes an IP address and an interbase connection
// string comprises an IPAddress:PathName. So this will have to be parsed into
// its two parts. Some valid IB connection strings that would have to be paresd are:

//  C:\Data\MyData.gdb
//  \Data\MyData.gdb
//  MyData.gdb
//  ..\Data\MyData.gdb
//  Server:C:\Data\MyData.gdb
//  123.456.789:C:\Data\MyData.gdb
//  LocalHost:C:\Data\MyData.gdb
//  Rules: If there is only one ':', then assume it's a file name only.
//  If there are two ':', then assume a machine name.
{
function TtiDatabaseIBX.TestServer(const pServerIPAddress : string; var pServerVersion : string): boolean;
var
  lIBSP: TIBServerProperties;
begin
  lIBSP:= TIBServerProperties.Create(nil);
  try
    lIBSP.LoginPrompt := false;
    lIBSP.Params.Clear;
    lIBSP.Params.Append('user_name=' + uUser);
    lIBSP.Params.Append('password=' + uPwd);
    lIBSP.ServerName  := pServerIPAddress;
    if SameText(lIBSP.ServerName, 'LocalHost') then
      lIBSP.Protocol := Local
    else
      lIBSP.Protocol := TCP;
    lIBSP.Options := [Version];
    try
      try
        lIBSP.Active := True;
        lIBSP.FetchVersionInfo;
        pServerVersion := lIBSP.VersionInfo.ServerVersion;
        result := true;
        lIBSP.Active := False;
      except
        on e: Exception do
          result := false;
        end;
    finally
      lIBSP.Active := False;
    end;
  finally
    lIBSP.Free;
  end;
end;
}

class function TtiDatabaseIBX.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
var
  lDatabase : TtiDatabaseIBX;
begin
  lDatabase := TtiDatabaseIBX.Create;
  try
    lDatabase.FDatabase.DatabaseName := ADatabaseName;
    lDatabase.FDatabase.Params.Values['user_name']:= AUserName;
    lDatabase.FDatabase.Params.Values['password']:= APassword;
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

function TtiQueryIBX.HasNativeLogicalType: boolean;
begin
  result := false;
end;

function TtiDatabaseIBX.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');  
end;

function TtiDatabaseIBX.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryIBX;
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistIBX,
    TtiDBConnectionPoolDataAbs,
    TtiQueryIBX,
    TtiDatabaseIBX);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistIBX);

end.


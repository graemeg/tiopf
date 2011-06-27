unit tiQueryCrAbs;

interface
uses
   tiQuery
  ,Classes
  ,Windows
  ,DBAccess
  ,MSAccess
  ,OLEDBAccess
  ,DB
 ;

{$I tiDefines.inc}

const
  cErrorCrCoInitialize  = 'Attempt to access TtiDatabaseCrAbs in thread other than the one the ADOConnection was created in';
type

  TtiDatabaseCrAbs = class(TtiDatabaseSQL)
  private
    FMSConnection     : TMSConnection;
    FCurrentThreadID: DWord;
    FInTransaction: Boolean;
    function GetMSConnection: TMSConnection;
  protected
    function GetConnectionString: string; virtual; abstract;
    property  Connection : TMSConnection read GetMSConnection;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure SetupDBParams; virtual;
    function  FieldDataTypeToTIQueryFieldKind(pDataType: TFieldType): TtiQueryFieldKind;

  public
    constructor Create; override;
    destructor  Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;

    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    property    ConnectionString : string read GetConnectionString;

  end;

  TtiQueryCrSdac = class(TtiQuerySQL)
  private
    FMSQuery : TMSQuery;
  {:FSQL is used as a private placeholder for converting the WideString version (in D2006+). Ref GetSQL}
    FSQL     : TStringList;
    procedure DoOnChangeSQL(Sender: TObject);
  protected

    function  GetFieldAsString(const AName: string): string    ; override;
    function  GetFieldAsFloat(const AName: string): extended   ; override;
    function  GetFieldAsBoolean(const AName: string): boolean  ; override;
    function  GetFieldAsInteger(const AName: string): Int64    ; override;
    function  GetFieldAsDateTime(const AName: string):TDateTime; override;

    function  GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
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
    function  GetParamAsFloat(const AName: string): extended;override;
    function  GetParamAsInteger(const AName: string): Int64;override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string;const AValue: boolean);override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended);override;
    procedure SetParamAsInteger(const AName: string;const AValue: Int64);override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;

    function  GetParamIsNull(const AName: String): Boolean; override;
    procedure SetParamIsNull(const AName: String; const AValue: Boolean); override;
    function  GetFieldIsNull(const AName: string): Boolean; override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   Next   ; override;
    function    ExecSQL: integer; override;

    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;

    procedure   AssignParamToStream(  const AName : string;  const AValue : TStream); override;
    procedure   AssignParamFromStream(const AName : string;  const AValue : TStream); override;
    procedure   AssignFieldAsStream(  const AName : string;  const AValue : TStream); override;
    procedure   AssignFieldAsStreamByIndex(AIndex : integer; const AValue : TStream); override;

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


implementation
uses
  tiLog
  ,tiUtils
  ,TypInfo
  ,tiWin32
  ,tiExcept
  ,SysUtils
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
 ;

constructor TtiQueryCrSdac.Create;
begin
  inherited;
  tiWin32CoInitialize;
  FMSQuery := TMSQuery.Create(nil);

  FMSQuery.CursorType := ctDefaultResultSet;
  FMSQuery.Options.TrimFixedChar:= false;
  FSQL := TStringList.Create;
  FSQL.OnChange:= DoOnChangeSQL;
end;

destructor TtiQueryCrSdac.Destroy;
begin
  FMSQuery.Free;
  FSQL.Free;
  inherited;
end;

procedure TtiQueryCrSdac.Close;
begin
  Active := false;
end;

function TtiQueryCrSdac.ExecSQL: integer;
var
  ls : string;
begin
  try
    FMSQuery.Prepared:=true;
    FMSQuery.Execute;
    Result := -1;
  { TODO :
When implementing RowsAffected,
please return correct result
and put FSupportsRowsAffected := True; in TtiQueryXXX.Create;}
  except
    on e:exception do
    begin
      ls := e.Message;
      Log(ls);
      raise;
    end;
  end;
end;

function TtiQueryCrSdac.GetFieldAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FMSQuery.FieldByName(AName).Value;
  result := lValue = -1;
end;

function TtiQueryCrSdac.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FMSQuery.FieldByName(AName).AsDateTime;
end;

function TtiQueryCrSdac.GetFieldAsFloat(const AName: string): extended;
begin
  result := FMSQuery.FieldByName(AName).AsFloat;
end;

function TtiQueryCrSdac.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FMSQuery.FieldByName(AName).AsInteger;
end;

function TtiQueryCrSdac.GetFieldAsString(const AName: string): string;
begin
  result := FMSQuery.FieldByName(AName).AsString;
end;

function TtiQueryCrSdac.GetActive: boolean;
begin
  result := FMSQuery.Active;
end;

function TtiQueryCrSdac.GetEOF: boolean;
begin
  result := FMSQuery.EOF;
end;

function TtiQueryCrSdac.GetParamAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FMSQuery.Params.ParamByName(AName).Value;
  result := lValue = -1;
end;

function TtiQueryCrSdac.GetParamAsDateTime(const AName: string): TDateTime;
var
  lValue : string;
begin
  // ToDo: Should have some protection against different date time formats...
  lValue := FMSQuery.Params.ParamByName(AName).Value;
  result := StrToDateTime(lValue);
end;

function TtiQueryCrSdac.GetParamAsFloat(const AName: string): extended;
begin
  result := FMSQuery.Params.ParamByName(AName).Value;
end;

function TtiQueryCrSdac.GetParamAsInteger(const AName: string): Int64;
begin
  result := Longint(FMSQuery.Params.ParamByName(AName).Value);
end;

function TtiQueryCrSdac.GetParamAsString(const AName: string): string;
begin
  result := FMSQuery.Params.ParamByName(AName).Value;
end;

function TtiQueryCrSdac.GetSQL: TStrings;
begin
  FSQL.OnChange:= nil;
  try
    FSQL.Assign(FMSQuery.SQL);
  finally
    FSQL.OnChange:= DoOnChangeSQL;
  end;
  result := FSQL;
end;

procedure TtiQueryCrSdac.Next;
begin
  FMSQuery.Next;
end;

procedure TtiQueryCrSdac.Open;
begin
  FMSQuery.Prepared := False;
  Active := true;
end;

function TtiQueryCrSdac.ParamCount: integer;
begin
  result := FMSQuery.Params.Count;
end;

function TtiQueryCrSdac.ParamName(AIndex: integer): string;
begin
  result := FMSQuery.Params.Items[ AIndex ].Name;
end;

procedure TtiQueryCrSdac.SetActive(const AValue: boolean);
begin
  if AValue = FMSQuery.Active then
    Exit; //==>

  try
    FMSQuery.prepare;
    FMSQuery.Active := AValue;
  except
    on e:exception do
      raise Exception.Create(SQLAndParamsAsString + ' Message: ' + e.message);
  end;

end;

procedure TtiQueryCrSdac.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  if AValue then
    FMSQuery.Params.ParamByName(AName).Value := True
  else
    FMSQuery.Params.ParamByName(AName).Value := False;
end;

procedure TtiQueryCrSdac.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  FMSQuery.Params.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryCrSdac.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  FMSQuery.Params.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryCrSdac.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  FMSQuery.Params.ParamByName(AName).Value := Longint(AValue);
end;

procedure TtiQueryCrSdac.SetParamAsString(const AName, AValue: string);
begin
  FMSQuery.Params.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryCrSdac.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TtiQueryCrSdac.AssignParamToStream(const AName: string; const AValue: TStream);
var
  ls : string;
begin
  Assert(AValue <> nil, 'Stream not assigned');
  ls := FMSQuery.ParamByName(AName).Value;
  tiStringToStream(ls, AValue);
end;

procedure TtiQueryCrSdac.AssignParamFromStream(const AName: string; const AValue: TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  FMSQuery.Params.ParamByName(AName).LoadFromStream(AValue, ftBlob);
end;

procedure TtiQueryCrSdac.AssignFieldAsStream(const AName: string; const AValue: TStream);
begin
  // This does not look right, but it's the best I can do in the time available.
  // DUnit tests pass, but then at the time of writing, we don't have any
  // tests for 'real' binary data, just strings in a TStream.
  // Updated 2005-05-17 ipk  NB qfkBinary field type mapped to 'image' in MS SQL
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  (FMSQuery.FieldByName(AName) as TBlobField).SaveToStream(AValue);
end;

procedure TtiQueryCrSdac.AttachDatabase(ADatabase: TtiDatabase);
var
  lSQL : string;
begin
  // This mess is to work around the problem (that is only happening with ADO,
  // not other data access layers) of the query parameters not being accessable
  // if the database connection is set after the sql. This happens with the
  // SQLManager visitors. A better solution would be to remove the part of the
  // query factory that creates the query, and only have the query factory
  // assign the SQL. This would tidy the SQLVisitor framework up nicely as it
  // is quite a mess. The QueryFactory was a concept from the early days and
  // has now been replaced by the plugable persistence layers as packages.
  lSQL := FMSQuery.SQL.Text;
  FMSQuery.SQL.Clear;
  FMSQuery.Connection := (ADatabase as TtiDatabaseCrAbs).Connection;
  FMSQuery.SQL.Text := lSQL;
  Database := ADatabase;
end;

procedure TtiQueryCrSdac.DetachDatabase;
begin
  inherited DetachDatabase;
  if FMSQuery.Active then
    FMSQuery.Active := false;
  FMSQuery.Connection := nil;
end;

function TtiQueryCrSdac.FieldCount: integer;
begin
  result := FMSQuery.FieldCount;
end;

function TtiQueryCrSdac.FieldName(AIndex: integer): string;
begin
  result := FMSQuery.Fields[AIndex].FieldName;
end;

procedure TtiQueryCrSdac.Reset;
begin
  Active := false;
  FMSQuery.SQL.Clear;
  FMSQuery.Params.Clear;
end;

function TtiQueryCrSdac.FieldIndex(const AName: string): integer;
begin
  result := FMSQuery.FieldByName(AName).Index;
end;

constructor TtiDatabaseCrAbs.Create;
begin
  inherited Create;
  FCurrentThreadID:= 0;
  Connection;
end;

class procedure TtiDatabaseCrAbs.CreateDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseCrAbs.DatabaseExists(const ADatabaseName, AUserName,
  APassword: string; const AParams: string): boolean;
begin
  result := false;
  Assert(false, 'DatabaseExists not implemented in ' + ClassName);
end;

destructor TtiDatabaseCrAbs.Destroy;
begin
  FreeAndNil(FMSConnection);
  tiWin32CoUnInitialize;
  inherited;
end;

class procedure TtiDatabaseCrAbs.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

procedure TtiDatabaseCrAbs.Commit;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorCrCoInitialize);
  if not InTransaction then
    raise Exception.Create('Attempt to commit but not in a transaction.');
  FMSConnection.Commit;
  FInTransaction:= False;
end;

function TtiDatabaseCrAbs.InTransaction: boolean;
begin
  Result:= FInTransaction;
end;

procedure TtiDatabaseCrAbs.RollBack;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorCrCoInitialize);
  FMSConnection.Rollback;
  FInTransaction:= False;
end;

procedure TtiDatabaseCrAbs.StartTransaction;
begin
  if InTransaction then
    raise Exception.Create('Attempt to start a transaction but transaction already exists.');
  Connection.StartTransaction;
  FInTransaction:= True;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryCrSdac.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lDataType : TFieldType;
begin
  lDataType := FMSQuery.Fields[ AIndex ].DataType;

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
    ftBlob, ftGraphic, ftVarBytes :             result := qfkBinary  ;
    ftMemo, ftFmtMemo:                          result := qfkLongString;
    ftWideMemo :                                result := qfkLongString;
    else
      raise Exception.Create('Invalid FMSQuery.Fields[ AIndex ].DataType <' +
                      GetEnumName(TypeInfo(TFieldType), Ord(lDataType)));
    end;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

function TtiQueryCrSdac.FieldSize(AIndex: integer): integer;
begin
  case FieldKind(AIndex) of
    qfkString    : result := FMSQuery.FieldDefs[ AIndex ].Size;
    qfkLongString : result := 0;
    qfkInteger   : result := 0;
    qfkFloat     : result := 0;
    qfkDateTime  : result := 0;
    qfkBinary    : result := 0;
    qfkLogical   : result := 0;
  else
    raise Exception.Create('Invalid field type');
  end;
end;

function TtiQueryCrSdac.GetParamIsNull(const AName: String): Boolean;
begin
  result := VarIsNull(FMSQuery.Params.ParamByName(AName).Value);
end;

procedure TtiQueryCrSdac.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  if AValue then
    FMSQuery.Params.ParamByName(AName).Value := Null;
end;

function TtiDatabaseCrAbs.GetConnected: boolean;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorCrCoInitialize);
  Result := FMSConnection.Connected;
end;

procedure TtiDatabaseCrAbs.SetConnected(AValue: boolean);
var
  lsErrorMessage : string;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorCrCoInitialize);

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FMSConnection.Connected := false;
    Exit; //==>
  end;

  SetupDBParams;
  lsErrorMessage := '';
  try
    FMSConnection.Connected := true;
  except
    on e:exception do
    begin
      lsErrorMessage := e.message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:EDAError do
    begin
      lsErrorMessage :=
        'Error class: '   + EDAError(e).classname + Cr +
        'Error message: ' + EDAError(e).Message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
  end;
end;

procedure TtiDatabaseCrAbs.SetupDBParams;
var lParams: TStringList;
begin
  lParams := TStringList.Create;
  try
    lParams.Assign(Params);

    if lParams.Values['AUTHENTICATION'] <> '' then
    begin
      if lParams.Values['AUTHENTICATION'] = 'auServer' then
        Connection.Authentication := auServer
      else
        Connection.Authentication := auWindows;
    end
    else
    begin
      Connection.Authentication := auWindows;
    end;
    if lParams.Values['ISOLATIONLEVEL'] <> '' then
    begin
      if lParams.Values['ISOLATIONLEVEL'] = 'ilReadCommitted' then
        Connection.IsolationLevel := ilReadCommitted
      else if lParams.Values['ISOLATIONLEVEL'] = 'ilReadUnCommitted' then
        Connection.IsolationLevel := ilReadUnCommitted
      else if lParams.Values['ISOLATIONLEVEL'] = 'ilRepeatableRead' then
        Connection.IsolationLevel := ilRepeatableRead
      else if lParams.Values['ISOLATIONLEVEL'] = 'ilIsolated' then
        Connection.IsolationLevel := ilIsolated
      else if lParams.Values['ISOLATIONLEVEL'] = 'ilSnapshot' then
        Connection.IsolationLevel := ilSnapshot
      else
        Connection.IsolationLevel := ilReadCommitted;
    end
    else
      Connection.IsolationLevel := ilReadCommitted;
  finally
    lParams.free;
  end;
  Connection.LoginPrompt      := false;
  Connection.ConnectString := ConnectionString;
  Connection.Options.Provider := prSQL;
end;

function TtiQueryCrSdac.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FMSQuery.FieldByName(AName).IsNull;
end;

// This function is cloned in tiQueryBDEAbs - must move it to a common location, but without pulling DB.pas into any packages where it is not required
function TtiDatabaseCrAbs.FieldDataTypeToTIQueryFieldKind(pDataType : TFieldType): TtiQueryFieldKind;
begin
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

    case pDataType of
    ftString, ftWideString :                    result := qfkString  ;
    ftSmallint, ftInteger, ftWord, ftLargeint : result := qfkInteger ;
    ftBoolean :                                 result := qfkLogical ;
    ftFloat, ftCurrency, ftBCD :                result := qfkFloat   ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime;
    ftBlob, ftGraphic, ftVarBytes :             result := qfkBinary  ;
    ftMemo, ftFmtMemo:                          result := qfkLongString;
    ftWideMemo :                                result := qfkLongString;
    else
      raise Exception.Create('Invalid FQuery.Fields[ AIndex ].DataType <' +
                      GetEnumName(TypeInfo(TFieldType), Ord(pDataType)) +
                      '>');
    end;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;


function TtiQueryCrSdac.HasNativeLogicalType: boolean;
begin
  result := true;
end;

procedure TtiQueryCrSdac.AssignFieldAsStreamByIndex(AIndex: Integer;const AValue: TStream);
begin
  // This does not look right, but it's the best I can do in the time available.
  // DUnit tests pass, but then at the time of writing, we don't have any
  // tests for 'real' binary data, just strings in a TStream.
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  TBlobField(FMSQuery.Fields[AIndex]).SaveToStream(AValue);
end;

function TtiQueryCrSdac.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lValue : variant;
begin
  lValue := FMSQuery.Fields[AIndex].Value;
  result := lValue = -1;
end;

function TtiQueryCrSdac.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FMSQuery.Fields[AIndex].AsDateTime;
end;

function TtiQueryCrSdac.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FMSQuery.Fields[AIndex].AsFloat;
end;

function TtiQueryCrSdac.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FMSQuery.Fields[AIndex].AsInteger;
end;

function TtiQueryCrSdac.GetFieldAsStringByIndex(AIndex: Integer): string;
begin
  result := FMSQuery.Fields[AIndex].AsString;
end;

function TtiQueryCrSdac.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FMSQuery.Fields[AIndex].IsNull;
end;

procedure TtiQueryCrSdac.DoOnChangeSQL(Sender: TObject);
begin
  FMSQuery.SQL.Assign(Sender as TStringList);
end;

function TtiDatabaseCrAbs.GetMSConnection: TMSConnection;
var
  LCurrentThreadID: DWord;
  LConnOpen, LHasConnStr :Boolean;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  if LCurrentThreadID <> FCurrentThreadID then
  begin
    if Assigned(FMSConnection) then
    begin
      LHasConnStr := FMSConnection.ConnectString <> '';
      LConnOpen   := FMSConnection.Connected;
    end
    else
    begin
      LHasConnStr := False;
      LConnOpen   := False;
    end;
    FreeAndNil(FMSConnection);
    FCurrentThreadID:= LCurrentThreadID;
    tiWin32CoInitialize;
    FMSConnection := TMSConnection.Create(nil);
    FMSConnection.LoginPrompt := false;
    FInTransaction:= False;
{ NB Assign newly generated ConnectionString.
     Can't use pre-existing one because the 'password=xxx' param is not persisted by the ADODB getter.
     (This would have been OK with MS Access or Windows authentication (user NULL))
     Also, we can't just assign CS without this test since GetConnectionString will reference the database
     which may not be available yet }
    if LHasConnStr then
      FMSConnection.ConnectString := ConnectionString;
    FMSConnection.Connected := LConnOpen;
  end;
  Result:= FMSConnection;
end;

end.





unit tiQueryADOAbs;

interface
uses
   tiQuery
  ,Classes
  ,Windows
  ,ADODb
  ,tiQueryDataset
  ,DB
 ;

{$I tiDefines.inc}

const
  cDelphi5ADOErrorString = 'Either BOF or EOF is True, or the current record has been deleted. Requested operation requires a current record';
  cErrorADOCoInitialize  = 'Attempt to access TtiDatabaseADOAbs in thread other than the one the ADOConnection was created in';
type

  TtiDatabaseADOAbs = class(TtiDatabaseSQL)
  private
    FADOConnection     : TADOConnection;
    FCurrentThreadID: DWord;
    FInTransaction: Boolean;
    function GetADOConnection: TADOConnection;
  protected
    function GetConnectionString: string; virtual; abstract;
    property  Connection : TADOConnection read GetADOConnection;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure SetupDBParams; virtual;
    function  FieldDataTypeToTIQueryFieldKind(pDataType: TFieldType): TtiQueryFieldKind;

  public
    constructor Create; override;
    destructor  Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string); override;

    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    property    ConnectionString : string read GetConnectionString;

  end;

  // Don't use this class. Introduced here to gain access to PSGetParams.
  TADOQueryLocal = class(TADOQuery)
  public
    function PSGetParams: TParams; override;
  end;

  TtiQueryADO = class(TtiQueryDataset)
  private
    FADOQuery : TADOQueryLocal;
  {:FSQL is used as a private placeholder for converting the WideString version (in D2006+). Ref GetSQL}
    FSQL     : TStringList;
    procedure DoOnChangeSQL(Sender: TObject);
  protected

    function  GetFieldAsBoolean(const AName: string): boolean  ; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    procedure SetActive(const AValue: boolean); override;
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

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    function    ExecSQL: integer; override;

    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;

    procedure   AssignParamToStream(  const AName : string;  const AValue : TStream); override;
    procedure   AssignParamFromStream(const AName : string;  const AValue : TStream); override;

    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase;  override;
    procedure   Reset; override;

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

constructor TtiQueryADO.Create;
begin
  inherited;
  tiWin32CoInitialize;
  FADOQuery := TADOQueryLocal.Create(nil);
  FADOQuery.CursorType := ctOpenForwardOnly;
  FADOQuery.CursorLocation := clUseClient;
  FADOQuery.DisableControls;
  Dataset:=FADOQuery;
  Params:= FADOQuery.PSGetParams;
  FSQL := TStringList.Create;
  FSQL.OnChange:= DoOnChangeSQL;
  FSupportsRowsAffected := True;
end;

destructor TtiQueryADO.Destroy;
begin
  FADOQuery.Free;
  FSQL.Free;
  inherited;
end;

procedure TtiQueryADO.Close;
begin
  Active := false;
end;

function TtiQueryADO.ExecSQL: integer;
var
  ls : string;
begin
  try
    FADOQuery.Prepared:=true;
    Result := FADOQuery.ExecSQL;
  except
    on e:exception do
    begin
      ls := e.Message;
      Log(ls);
      raise;
    end;
  end;
end;

function TtiQueryADO.GetFieldAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FADOQuery.FieldByName(AName).Value;
  result := lValue = -1;
end;

function TtiQueryADO.GetParamAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FADOQuery.Parameters.ParamByName(AName).Value;
  result := lValue = -1;
end;

function TtiQueryADO.GetParamAsDateTime(const AName: string): TDateTime;
var
  lValue : string;
begin
  // ToDo: Should have some protection against different date time formats...
  lValue := FADOQuery.Parameters.ParamByName(AName).Value;
  result := StrToDateTime(lValue);
end;

function TtiQueryADO.GetParamAsFloat(const AName: string): extended;
begin
  result := FADOQuery.Parameters.ParamByName(AName).Value;
end;

function TtiQueryADO.GetParamAsInteger(const AName: string): Int64;
begin
  result := Longint(FADOQuery.Parameters.ParamByName(AName).Value);
end;

function TtiQueryADO.GetParamAsString(const AName: string): string;
begin
  result := FADOQuery.Parameters.ParamByName(AName).Value;
end;

function TtiQueryADO.GetSQL: TStrings;
begin
  FSQL.OnChange:= nil;
  try
    FSQL.Assign(FADOQuery.SQL);
  finally
    FSQL.OnChange:= DoOnChangeSQL;
  end;
  result := FSQL;
end;

procedure TtiQueryADO.Open;
begin
  FADOQuery.Prepared := False;
  Active := true;
end;

function TtiQueryADO.ParamCount: integer;
begin
  result := FADOQuery.Parameters.Count;
end;

function TtiQueryADO.ParamName(AIndex: integer): string;
begin
  result := FADOQuery.Parameters.Items[ AIndex ].Name;
end;

procedure TtiQueryADO.SetActive(const AValue: boolean);
begin
  if AValue = FADOQuery.Active then
    Exit; //==>

  try
    FADOQuery.Active := AValue;
  except
    on e:exception do
      raise Exception.Create(SQLAndParamsAsString + ' Message: ' + e.message);
  end;

end;

procedure TtiQueryADO.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  if AValue then
    FADOQuery.Parameters.ParamByName(AName).Value := True
  else
    FADOQuery.Parameters.ParamByName(AName).Value := False;
end;

procedure TtiQueryADO.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  FADOQuery.Parameters.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryADO.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  FADOQuery.Parameters.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryADO.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  FADOQuery.Parameters.ParamByName(AName).Value := Longint(AValue);
end;

procedure TtiQueryADO.SetParamAsString(const AName, AValue: string);
begin
  FADOQuery.Parameters.ParamByName(AName).Value := AValue;
end;

procedure TtiQueryADO.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TtiQueryADO.AssignParamToStream(const AName: string; const AValue: TStream);
var
  lBinData: OleVariant;
  lDataPtr: Pointer;
  lHigh, lLow, lLen: Integer;
  lParameter : TParameter;
begin
  Assert(AValue <> nil, 'Stream not assigned');
  lParameter := FADOQuery.Parameters.ParamByName(AName);
  lLow    := VarArrayLowBound(lParameter.Value, 1);
  lHigh   := VarArrayHighBound(lParameter.Value, 1);
  lLen    := lHigh - lLow + 1;
  lBinData := VarArrayCreate([0, lLen], varByte);
  lBinData := lParameter.Value;
  lDataPtr := VarArrayLock(lBinData);
  try
    AValue.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;
end;

procedure TtiQueryADO.AssignParamFromStream(const AName: string; const AValue: TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  FADOQuery.Parameters.ParamByName(AName).LoadFromStream(AValue, ftBlob);
end;


procedure TtiQueryADO.AttachDatabase(ADatabase: TtiDatabase);
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
  lSQL := FADOQuery.SQL.Text;
  FADOQuery.SQL.Clear;
  FADOQuery.Connection := (ADatabase as TtiDatabaseADOAbs).Connection;
  FADOQuery.SQL.Text := lSQL;
  Database := ADatabase;
end;

procedure TtiQueryADO.DetachDatabase;
begin
  inherited DetachDatabase;
  if FADOQuery.Active then
    FADOQuery.Active := false;
  FADOQuery.Connection := nil;
end;



procedure TtiQueryADO.Reset;
begin
  Active := false;
  FADOQuery.SQL.Clear;
  FADOQuery.Parameters.Clear;
end;

constructor TtiDatabaseADOAbs.Create;
begin
  inherited Create;
  FCurrentThreadID:= 0;
  Connection;
end;

destructor TtiDatabaseADOAbs.Destroy;
begin
  FreeAndNil(FADOConnection);
  tiWin32CoUnInitialize;
  inherited;
end;

procedure TtiDatabaseADOAbs.Commit;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorADOCoInitialize);
  if not InTransaction then
    raise Exception.Create('Attempt to commit but not in a transaction.');
  FADOConnection.CommitTrans;
  FInTransaction:= False;
end;

function TtiDatabaseADOAbs.InTransaction: boolean;
begin
  Result:= FInTransaction;
end;

procedure TtiDatabaseADOAbs.RollBack;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorADOCoInitialize);
  FADOConnection.RollbackTrans;
  FInTransaction:= False;
end;

procedure TtiDatabaseADOAbs.StartTransaction;
begin
  if InTransaction then
    raise Exception.Create('Attempt to start a transaction but transaction already exists.');
  Connection.BeginTrans;
  FInTransaction:= True;
end;


function TtiQueryADO.GetParamIsNull(const AName: String): Boolean;
begin
  result := VarIsNull(FADOQuery.Parameters.ParamByName(AName).Value);
end;

procedure TtiQueryADO.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  if AValue then
    FADOQuery.Parameters.ParamByName(AName).Value := Null;
end;

function TtiDatabaseADOAbs.GetConnected: boolean;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorADOCoInitialize);
  Result := FADOConnection.Connected;
end;

procedure TtiDatabaseADOAbs.SetConnected(AValue: boolean);
var
  lsErrorMessage : string;
begin
  Assert(FCurrentThreadID = GetCurrentThreadID, cErrorADOCoInitialize);

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FADOConnection.Connected := false;
    Exit; //==>
  end;

  SetupDBParams;
  lsErrorMessage := '';
  try
    FADOConnection.Connected := true;
  except
    on e:exception do
    begin
      lsErrorMessage := e.message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:EADOError do
    begin
      lsErrorMessage :=
        'Error class: '   + EADOError(e).classname + Cr +
        'Error message: ' + EADOError(e).Message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
  end;
end;

procedure TtiDatabaseADOAbs.SetupDBParams;
begin
  Connection.LoginPrompt      := false;
  Connection.IsolationLevel   := ilReadCommitted;
  Connection.ConnectionString := ConnectionString;
end;


// This function is cloned in tiQueryBDEAbs - must move it to a common location, but without pulling DB.pas into any packages where it is not required
function TtiDatabaseADOAbs.FieldDataTypeToTIQueryFieldKind(pDataType : TFieldType): TtiQueryFieldKind;
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
    ftFloat, ftCurrency, ftBCD, ftFMTBcd :      result := qfkFloat   ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime;
    ftBlob, ftGraphic, ftVarBytes :             result := qfkBinary  ;
    ftMemo, ftFmtMemo:                          result := qfkLongString;
    {$ifdef DELPHI10ORABOVE}
    ftWideMemo :                                result := qfkLongString;
    {$endif}
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


function TtiQueryADO.HasNativeLogicalType: boolean;
begin
  result := true;
end;

class procedure TtiDatabaseADOAbs.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class procedure TtiDatabaseADOAbs.DropDatabase(const ADatabaseName, AUserName,
  APassword: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseADOAbs.DatabaseExists(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  result := false;
  Assert(false, 'DatabaseExists not implemented in ' + ClassName);
end;


function TtiQueryADO.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lValue : variant;
begin
  lValue := FADOQuery.Fields[AIndex].Value;
  result := lValue = -1;
end;

procedure TtiQueryADO.DoOnChangeSQL(Sender: TObject);
begin
  FADOQuery.SQL.Assign(Sender as TStringList);
end;

function TtiDatabaseADOAbs.GetADOConnection: TADOConnection;
var
  LCurrentThreadID: DWord;
  LConnOpen, LHasConnStr :Boolean;
begin
  LCurrentThreadID:= GetCurrentThreadID;
  if LCurrentThreadID <> FCurrentThreadID then
  begin
    if Assigned(FADOConnection) then
    begin
      LHasConnStr := FADOConnection.ConnectionString <> '';
      LConnOpen   := FADOConnection.Connected;
    end
    else
    begin
      LHasConnStr := False;
      LConnOpen   := False;
    end;
    FreeAndNil(FADOConnection);
    FCurrentThreadID:= LCurrentThreadID;
    tiWin32CoInitialize;
    FADOConnection := TADOConnection.Create(nil);
    FADOConnection.LoginPrompt := false;
    FInTransaction:= False;
{ NB Assign newly generated ConnectionString.
     Can't use pre-existing one because the 'password=xxx' param is not persisted by the ADODB getter.
     (This would have been OK with MS Access or Windows authentication (user NULL))
     Also, we can't just assign CS without this test since GetConnectionString will reference the database
     which may not be available yet }
    if LHasConnStr then
      FADOConnection.ConnectionString := ConnectionString;
    FADOConnection.Connected := LConnOpen;
  end;
  Result:= FADOConnection;
end;

{ TADOQueryLocal }

function TADOQueryLocal.PSGetParams: TParams;
begin
  result:= inherited PSGetParams;
end;

end.


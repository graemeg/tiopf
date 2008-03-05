{$I tiDefines.inc}

unit tiQueryBDEAbs;

interface
uses
   tiQuery
  ,Classes
  ,DBTables
 ;

type

  TtiDatabaseBDEAbs = class(TtiDatabaseSQL)
  private
    FDatabase     : TDataBase;
    FSession      : TSession;
  protected
    property  Database : TDatabase read FDatabase write FDatabase;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure SetupDBParams; virtual; abstract;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;

  end;


  TtiQueryBDE = class(TtiQuerySQL)
  private
    FQuery : TQuery;
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


implementation
uses
  tiLog
  ,tiUtils
  ,tiExcept
  ,SysUtils
  ,DB
  ,TypInfo
 ;

var
  uSessionCount : integer;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryBDE
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryBDE.Create;
begin
  inherited;
  FQuery := TQuery.Create(nil);
end;

destructor TtiQueryBDE.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TtiQueryBDE.Close;
begin
  Active := false;
end;

procedure TtiQueryBDE.ExecSQL;
begin
  FQuery.ExecSQL;
end;

function TtiQueryBDE.GetFieldAsBoolean(const AName: string): boolean;
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

function TtiQueryBDE.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.FieldByName(AName).AsDateTime;
end;

function TtiQueryBDE.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQuery.FieldByName(AName).AsFloat;
end;

function TtiQueryBDE.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FQuery.FieldByName(AName).AsInteger;
end;

function TtiQueryBDE.GetFieldAsString(const AName: string): string;
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

function TtiQueryBDE.GetActive: boolean;
begin
  result := FQuery.Active;
end;

function TtiQueryBDE.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryBDE.GetParamAsBoolean(const AName: string): boolean;
begin
  result := FQuery.ParamByName(AName).AsBoolean;
end;

function TtiQueryBDE.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.ParamByName(AName).AsDateTime;
end;

function TtiQueryBDE.GetParamAsFloat(const AName: string): extended;
begin
  result := FQuery.ParamByName(AName).AsFloat;
end;

function TtiQueryBDE.GetParamAsInteger(const AName: string): Int64;
begin
  result := FQuery.ParamByName(AName).AsInteger;
end;

function TtiQueryBDE.GetParamAsString(const AName: string): string;
begin
  result := FQuery.ParamByName(AName).AsString;
end;

function TtiQueryBDE.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryBDE.Next;
begin
  FQuery.Next;
end;

procedure TtiQueryBDE.Open;
begin
  Active := true;
end;

function TtiQueryBDE.ParamCount: integer;
begin
  result := FQuery.ParamCount;
end;

function TtiQueryBDE.ParamName(AIndex: integer): string;
begin
  result := FQuery.Params.Items[ AIndex ].Name;
end;

procedure TtiQueryBDE.SetActive(const AValue: boolean);
begin
  FQuery.Active := AValue;
end;

procedure TtiQueryBDE.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsBoolean := AValue;
end;

procedure TtiQueryBDE.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsDateTime := AValue;
end;

procedure TtiQueryBDE.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsFloat := AValue;
end;

procedure TtiQueryBDE.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  FQuery.ParamByName(AName).AsInteger := AValue;
end;

procedure TtiQueryBDE.SetParamAsString(const AName, AValue: string);
var
  lParam : TParam;
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  lParam := FQuery.ParamByName(AName);
  if length(AValue) <= 255 then
    lParam.AsString := AValue
  else
    lParam.AsMemo := AValue;
end;

procedure TtiQueryBDE.SetSQL(const AValue: TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;

procedure TtiQueryBDE.AssignFieldAsStream(const AName: string; const AStream: TStream);
var
  lField : TField;
begin
  lField := FQuery.FieldByName(AName);
  Assert(lField is TBlobField, 'Field <' + AName + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AStream);
end;

procedure TtiQueryBDE.AttachDatabase(ADatabase: TtiDatabase);
begin
  FQuery.DatabaseName := TtiDatabaseBDEAbs(ADatabase).Database.DatabaseName;
  Database := ADatabase;
end;

procedure TtiQueryBDE.DetachDatabase;
begin
  inherited DetachDatabase;
end;

function TtiQueryBDE.FieldCount: integer;
begin
  result := FQuery.FieldCount;
end;

function TtiQueryBDE.FieldName(AIndex: integer): string;
begin
  result := FQuery.Fields[AIndex].FieldName;
end;

procedure TtiQueryBDE.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.Params.Clear;
end;

function TtiQueryBDE.FieldIndex(const AName: string): integer;
begin
  result := FQuery.FieldByName(AName).Index;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseBDEAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseBDEAbs.Create;
begin
  inherited Create;
  FDatabase := TDataBase.Create(nil);
  FDatabase.LoginPrompt := false;
  FSession      := TSession.Create(nil);
  FSession.AutoSessionName := true;
  FDatabase.SessionName := FSession.Name;
  // Must come up with a better way of doing this.
  FDatabase.DatabaseName := 'DB_' + FSession.SessionName;
  Inc(uSessionCount);
end;

destructor TtiDatabaseBDEAbs.Destroy;
begin
  FDatabase.Free;
  FSession.Free;
  Dec(uSessionCount);
  inherited;
end;

procedure TtiDatabaseBDEAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FDatabase.Commit;
end;

function TtiDatabaseBDEAbs.InTransaction: boolean;
begin
  result := FDatabase.InTransaction;
end;

procedure TtiDatabaseBDEAbs.RollBack;
begin
  FDatabase.RollBack;
end;

procedure TtiDatabaseBDEAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FDatabase.StartTransaction;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryBDE.FieldKind(AIndex: integer): TtiQueryFieldKind;
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

function TtiQueryBDE.FieldSize(AIndex: integer): integer;
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

function TtiQueryBDE.GetParamIsNull(const AName: String): Boolean;
begin
  result := FQuery.ParamByName(AName).IsNull;
end;

procedure TtiQueryBDE.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  FQuery.ParamByName(AName).Clear;
end;

function TtiDatabaseBDEAbs.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseBDEAbs.SetConnected(AValue: boolean);
var
  i : integer;
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
    // ToDo: Find a generic way of trapping an invalid password error, and
    //       re-raising this exception so the logon screen in activated again.
    //       But, the BDE raises a bunch of exception for the simplest error
    //       so this may be tricky.
    on e:EDBEngineError do
    begin
      lsErrorMessage := '';
      for i := 0 to EDBEngineError(e).ErrorCount-1 do
      begin
        if lsErrorMessage <> '' then lsErrorMessage := lsErrorMessage + CrLf(2);
        lsErrorMessage := lsErrorMessage +
                          'Error class: '   + EDBEngineError(e).classname + Cr +
                          'Error message: ' + EDBEngineError(e).Errors[i].Message + Cr +
                          'Error Code: ' + IntToStr(EDBEngineError(e).Errors[i].ErrorCode) + Cr +
                          'Native error code: ' + IntToStr(EDBEngineError(e).Errors[i].NativeError);
      end;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, e.message);
  end;
end;

function TtiQueryBDE.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FQuery.FieldByName(AName).IsNull;
end;

procedure TtiQueryBDE.AssignParamToStream(const AName: string; const AStream : TStream);
var
  ls : string;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  ls := FQuery.ParamByName(AName).Value;
  tiStringToStream(ls, AStream);
end;

procedure TtiQueryBDE.AssignParamFromStream(const AName: string; const AStream : TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  FQuery.ParamByName(AName).LoadFromStream(AStream, ftBlob);
end;

function TtiQueryBDE.HasNativeLogicalType: boolean;
begin
  result := true;
end;

procedure TtiQueryBDE.AssignFieldAsStreamByIndex(AIndex: Integer;const AValue: TStream);
var
  lField : TField;
begin
  lField := FQuery.Fields[AIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(AIndex) + '> not a TBlobField');
  (lField as TBlobField).SaveToStream(AValue);
end;

function TtiQueryBDE.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
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

function TtiQueryBDE.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ AIndex ].AsDateTime;
end;

function TtiQueryBDE.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FQuery.Fields[ AIndex ].AsFloat;
end;

function TtiQueryBDE.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FQuery.Fields[ AIndex ].AsInteger;
end;

function TtiQueryBDE.GetFieldAsStringByIndex(AIndex: Integer): string;
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

function TtiQueryBDE.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ AIndex ].IsNull;
end;

initialization
  uSessionCount := 0;

end.

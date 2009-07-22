{$I tiDefines.inc}

unit tiQueryDBEAbs;

interface
uses
   tiQuery
  ,Classes
  ,DBXpress
  ,SQLExpr
  ,tiClassToDBMap_BOM
  ,tiObject
  ,tiExcept
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseDBEAbs = class( TtiDatabaseSQL )
  private
    FSQLConnection : TSQLConnection;
    FTransaction   : TTransactionDesc;
  protected
    property  SQLConnection : TSQLConnection read FSQLConnection write FSQLConnection;
    property  Transaction : TTransactionDesc read FTransaction write FTransaction;
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
    procedure SetupDBParams ; virtual ; abstract ;

  public
    constructor create ; override ;
    destructor  Destroy ; override ;

    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;

  end ;


  // ---------------------------------------------------------------------------
  TtiQueryDBE = class( TtiQuerySQL )
  private
    FQuery : TSQLQuery ;
  protected

    function    GetFieldAsString(const psName: string): string   ; override ;
    function    GetFieldAsFloat(const psName: string): real      ; override ;
    function    GetFieldAsBoolean(const psName: string): boolean    ; override ;
    function    GetFieldAsInteger(const psName: string): Int64 ; override ;
    function    GetFieldAsDateTime(const psName: string):TDateTime ; override ;

    function    GetFieldAsStringByIndex(pIndex: Integer): string     ; override ;
    function    GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; override ;
    function    GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override ;
    function    GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override ;
    function    GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override ;
    function    GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override ;

    function    GetSQL: TStrings; override ;
    procedure   SetSQL(const Value: TStrings); override ;
    function    GetActive: boolean; override ;
    procedure   SetActive(const Value: boolean); override ;
    function    GetEOF: boolean; override ;
    function    GetParamAsString( const psName: string): string; override ;
    function    GetParamAsBoolean(const psName: string): boolean; override ;
    function    GetParamAsFloat(const psName: string): real;override ;
    function    GetParamAsInteger(const psName: string): Int64 ;override ;
    procedure   SetParamAsString( const psName, Value: string); override ;
    procedure   SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    procedure   SetParamAsFloat(const psName: string; const Value: real);override ;
    procedure   SetParamAsInteger(const psName: string;const Value: Int64);override ;
    function    GetParamAsDateTime(const psName: string): TDateTime ; override ;
    procedure   SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;

    function    GetParamIsNull( const psName: String): Boolean; override;
    procedure   SetParamIsNull( const psName: String; const Value: Boolean); override;
    function    GetFieldIsNull(const psName: string): Boolean; override ;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Open    ; override ;
    procedure   Close   ; override ;
    procedure   Next    ; override ;
    procedure   ExecSQL ; override ;

    function    ParamCount : integer ; override ;
    function    ParamName( pIndex : integer ) : string ; override ;

    procedure   AssignParamToStream( const pName : string ; const pStream : TStream ) ; override ;
    procedure   AssignParamFromStream( const pName : string ; const pStream : TStream ) ; override ;
    procedure   AssignFieldAsStream( const pName : string ; const pStream : TStream ) ; override ;
    procedure   AssignFieldAsStreamByIndex(      pIndex : integer ; const pValue : TStream ) ; override ;

    procedure   AttachDatabase( pDatabase : TtiDatabase ) ; override ;
    procedure   DetachDatabase ;  override ;
    procedure   Reset ; override ;

    function    FieldCount : integer ; override ;
    function    FieldName( pIndex : integer ) : string ; override ;
    function    FieldIndex( const psName : string ) : integer ; override ;
    function    FieldKind( pIndex : integer ) : TtiQueryFieldKind ; override ;
    function    FieldSize( pIndex : integer ) : integer ; override ;
    function    HasNativeLogicalType : boolean ; override ;

  end ;


implementation
uses
  SysUtils
  ,tiDBConnectionPool
  ,tiLog
  ,tiUtils
  ,FMTBcd
  ,DB
  ,TypInfo
  ,tiOPFManager
  ,tiDialogs
  ;

var
  uSessionCount : integer ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryBDE
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryDBE.Create;
begin
  inherited;
  FQuery := TSQLQuery.Create( nil ) ;
end;

// -----------------------------------------------------------------------------
destructor TtiQueryDBE.Destroy;
begin
  FQuery.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.Close;
begin
  Active := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.ExecSQL;
begin
  try
    FQuery.ExecSQL ;
  except
    on e:exception do
      raise EtiOPFInternalException.Create('ExecSQL, Classname : ' + ClassName + ', Params : ' + SQLAndParamsAsString) ;
  end;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetFieldAsBoolean(const psName: string): boolean;
var
  lsValue : string ;
begin
  lsValue := upperCase( FQuery.FieldByName( psName ).AsString ) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByName( psName ).AsDateTime ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetFieldAsFloat(const psName: string): real;
begin
  result := FQuery.FieldByName( psName ).AsFloat ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByName( psName ).AsInteger ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetFieldAsString(const psName: string): string;
var
  lField : TField ;
  lStream : TStringStream ;
begin
  lField := FQuery.FieldByName( psName ) ;
  if lField is TMemoField then
  begin
    lStream := TStringStream.Create('') ;
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0 ;
      result := lStream.DataString ;
    finally
      lStream.Free ;
    end ;
  end
  else
    result := lField.AsString ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetActive: boolean;
begin
  result := FQuery.Active ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetEOF: boolean;
begin
  result := FQuery.EOF ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetParamAsBoolean(const psName: string): boolean;
begin
  result := FQuery.ParamByName( psName ).AsBoolean ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.ParamByName( psName ).AsDateTime ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetParamAsFloat(const psName: string): real;
begin
  result := FQuery.ParamByName( psName ).AsFloat ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetParamAsInteger(const psName: string): Int64;
begin
  result := FQuery.ParamByName( psName ).AsInteger ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetParamAsString(const psName: string): string;
begin
  result := FQuery.ParamByName( psName ).AsString ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.GetSQL: TStrings;
begin
  result := FQuery.SQL ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.Next;
begin
  FQuery.Next ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.Open;
begin
  Active := true ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.ParamCount: integer;
begin
  result := FQuery.Params.Count ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.ParamName(pIndex: integer): string;
begin
  result := FQuery.Params.Items[ pIndex ].Name ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetActive(const Value: boolean);
begin
  try
    FQuery.Active := Value ;
  except
    on e:exception do
    begin
      raise EtiOPFInternalException.Create('ExecSQL, Classname : ' + ClassName + ', Params : ' + SQLAndParamsAsString) ;
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  if not FQuery.Prepared then
    FQuery.Prepared := True ;
  FQuery.ParamByName( psName ).AsBoolean := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  if not FQuery.Prepared then
    FQuery.Prepared := True ;
  FQuery.ParamByName( psName ).AsDateTime := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetParamAsFloat(const psName: string;
  const Value: real);
begin
  if not FQuery.Prepared then
    FQuery.Prepared := True ;
  FQuery.ParamByName( psName ).AsFloat := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  if not FQuery.Prepared then
    FQuery.Prepared := True ;
  FQuery.ParamByName( psName ).AsInteger := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetParamAsString(const psName, Value: string);
var
  lParam : TParam ;
begin
  if not FQuery.Prepared then
    FQuery.Prepared := True ;
  lParam := FQuery.ParamByName( psName ) ;
  if length( Value ) <= 255 then
    lParam.AsString := Value
  else
    lParam.AsMemo := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.AssignFieldAsStream(const pName: string; const pStream: TStream );
var
  lField : TField ;
begin
  lField := FQuery.FieldByName(pName);
  Assert(lField is TBlobField, 'Field <' + pName + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pStream);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.AttachDatabase(pDatabase: TtiDatabase);
begin
  FQuery.SQLConnection := TtiDatabaseDBEAbs( pDatabase ).SQLConnection;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.DetachDatabase;
begin
  try
    inherited DetachDatabase ;
  except
    on e:exception do
      raise EtiOPFInternalException.Create('DetachDatabase ' + e.Message);
  end ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.FieldCount: integer;
begin
  result := FQuery.FieldCount ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.FieldName(pIndex: integer): string;
begin
  result := FQuery.Fields[pIndex].FieldName ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDBE.Reset;
begin
  Active := false ;
  FQuery.SQL.Clear ;
  FQuery.Params.Clear ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.FieldIndex(const psName: string): integer;
begin
  result := FQuery.FieldByName( psName ).Index ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseDBEAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseDBEAbs.create;
begin
  inherited Create ;
  FSQLConnection := TSQLConnection.Create(nil);
  FSQLConnection.LoginPrompt := False;
  Inc(uSessionCount);
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseDBEAbs.Destroy;
begin
  FSQLConnection.Free;
  Dec(uSessionCount);
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseDBEAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FSQLConnection.Commit(FTransaction);
end;

// -----------------------------------------------------------------------------
function TtiDatabaseDBEAbs.InTransaction: boolean;
begin
  result := FSQLConnection.InTransaction ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseDBEAbs.RollBack;
begin
  FSQLConnection.RollBack(FTransaction);
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseDBEAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  FSQLConnection.StartTransaction(FTransaction);
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQueryDBE.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lDataType : TFieldType ;
begin
  lDataType := FQuery.Fields[ pIndex ].DataType ;

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
    ftString, ftWideString :                    result := qfkString   ;
    ftSmallint, ftInteger, ftWord, ftLargeint : result := qfkInteger  ;
    ftBoolean :                                 result := qfkLogical  ;
    ftFloat, ftCurrency, ftBCD :                result := qfkFloat    ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime ;
    ftBlob, ftGraphic :                         result := qfkBinary   ;
    ftMemo, ftFmtMemo :                         result := qfkLongString ;
    else
      result := qfkString ; // Just to shup up the compiler
      raise EtiOPFInternalException.Create( 'Invalid FADOQuery.Fields[ pIndex ].DataType <' +
                      GetEnumName( TypeInfo( TFieldType ), Ord( lDataType ))) ;
    end ;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

// -----------------------------------------------------------------------------
function TtiQueryDBE.FieldSize(pIndex: integer): integer;
begin
  case FieldKind( pIndex ) of
    qfkString     : result := FQuery.FieldDefs[ pIndex ].Size ;
    qfkLongString : result := 0 ;
    qfkInteger    : result := 0 ;
    qfkFloat      : result := 0 ;
    qfkDateTime   : result := 0 ;
    qfkBinary     : result := 0 ;
    qfkLogical    : result := 0 ;
  else
    result := -1 ;
    raise EtiOPFInternalException.Create( 'Invalid field type') ;
  end ;
end;

function TtiQueryDBE.GetParamIsNull(const psName: String): Boolean;
begin
  result := FQuery.ParamByName( psName ).IsNull ;
end;

procedure TtiQueryDBE.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  FQuery.ParamByName( psName ).Clear ;
end;

function TtiDatabaseDBEAbs.GetConnected: boolean;
begin
  Result := FSQLConnection.Connected ;
end;

procedure TtiDatabaseDBEAbs.SetConnected(pbValue: boolean);
var
  lsErrorMessage : string ;
begin

  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    FSQLConnection.Connected := false ;
    Exit ; //==>
  end ;

  SetupDBParams ;
  lsErrorMessage := '' ;
  try
    FSQLConnection.Connected := true ;
  except
    on e:EDatabaseError do
    begin
      lsErrorMessage := '' ;
{
      for i := 0 to EDatabaseError( e ).ErrorCount-1 do
      begin
        if lsErrorMessage <> '' then lsErrorMessage := lsErrorMessage + CrLf( 2 ) ;
        lsErrorMessage := lsErrorMessage +
                          'Error class: '   + EDBEngineError( e ).classname + Cr +
                          'Error message: ' + EDBEngineError( e ).Errors[i].Message + Cr +
                          'Error Code: ' + IntToStr( EDBEngineError( e ).Errors[i].ErrorCode ) + Cr +
                          'Native error code: ' + IntToStr( EDBEngineError( e ).Errors[i].NativeError ) ;
      end ;
}
        lsErrorMessage := lsErrorMessage +
                          'Error class: '   + EDatabaseError( e ).classname + Cr +
                          'Error message: ' + EDatabaseError( e ).Message + Cr ;
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, lsErrorMessage ) ;
    end ;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, e.message ) ;
  end ;
end;

function TtiQueryDBE.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldByName( psName ).IsNull ;
end;

//------------------------------------------------------------------------------
procedure TtiQueryDBE.AssignParamToStream(const pName: string; const pStream : TStream);
var
  ls : string ;
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  ls := FQuery.ParamByName(pName).Value ;
  tiStringToStream(ls, pStream);
end;

//------------------------------------------------------------------------------
procedure TtiQueryDBE.AssignParamFromStream(const pName: string; const pStream : TStream);
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  FQuery.ParamByName( pName ).LoadFromStream( pStream, ftBlob );
end;

function TtiQueryDBE.HasNativeLogicalType: boolean;
begin
  result := true ;
end;

procedure TtiQueryDBE.AssignFieldAsStreamByIndex(pIndex: Integer;const pValue: TStream);
var
  lField : TField ;
begin
  lField := FQuery.Fields[pIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(pIndex) + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pValue);
end;

function TtiQueryDBE.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
var
  lsValue : string ;
begin
  lsValue := upperCase( FQuery.Fields[ pIndex ].AsString ) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

function TtiQueryDBE.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ pIndex ].AsDateTime ;
end;

function TtiQueryDBE.GetFieldAsFloatByIndex(pIndex: Integer): real;
begin
  result := FQuery.Fields[ pIndex ].AsFloat ;
end;

function TtiQueryDBE.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FQuery.Fields[ pIndex ].AsInteger ;
end;

function TtiQueryDBE.GetFieldAsStringByIndex(pIndex: Integer): string;
var
  lField : TField ;
  lStream : TStringStream ;
begin
  lField := FQuery.Fields[pIndex] ;
  if lField is TMemoField then
  begin
    lStream := TStringStream.Create('') ;
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0 ;
      result := lStream.DataString ;
    finally
      lStream.Free ;
    end ;
  end
  else
    result := lField.AsString ;
end;

function TtiQueryDBE.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ pIndex ].IsNull ;
end;

initialization
  uSessionCount := 0 ;

end.



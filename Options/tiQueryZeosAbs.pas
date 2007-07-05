unit tiQueryZeosAbs;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  ,ZConnection
  ,ZDataset
  ,tiAutoMap
  ,tiObject
  ;

type

  TtiDatabaseZeosAbs = class( TtiDatabaseSQL )
  private
    FConnection    : TZConnection;
  protected
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
    procedure SetupDBParams ; virtual ; abstract ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;
    function    Test : boolean; override;

    property  Connection : TZConnection read FConnection write FConnection ;    
  end ;


  TtiQueryZeos = class( TtiQuerySQL )
  private
    FQuery : TZQuery ;
  protected

    function    GetFieldAsString(const psName: string): string   ; override ;
    function    GetFieldAsFloat(const psName: string): extended  ; override ;
    function    GetFieldAsBoolean(const psName: string): boolean    ; override ;
    function    GetFieldAsInteger(const psName: string): Int64 ; override ;
    function    GetFieldAsDateTime(const psName: string):TDateTime ; override ;

    function    GetFieldAsStringByIndex(pIndex: Integer): string     ; override ;
    function    GetFieldAsFloatByIndex(pIndex: Integer)   : extended ; override ;
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
    function    GetParamAsFloat(const psName: string): extended;override ;
    function    GetParamAsInteger(const psName: string): Int64 ;override ;
    procedure   SetParamAsString( const psName, Value: string); override ;
    procedure   SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    procedure   SetParamAsFloat(const psName: string; const Value: extended);override ;
    procedure   SetParamAsInteger(const psName: string;const Value: Int64);override ;
    function    GetParamAsDateTime(const psName: string): TDateTime ; override ;
    procedure   SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;
    function    GetParamAsTextBLOB(const psName: string): string; override ;
    procedure   SetParamAsTextBLOB(const psName, Value: string); override ;

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
   tiDBConnectionPool
  ,tiLog
  ,tiUtils
  ,tiOPFManager
  ,tiExcept
  ,SysUtils
  ,DB
  ,TypInfo
  ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryZeos
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryZeos.Create;
begin
  inherited;
  FQuery := TZQuery.Create( nil ) ;
end;

destructor TtiQueryZeos.Destroy;
begin
  FQuery.Free ;
  inherited;
end;

procedure TtiQueryZeos.Close;
begin
  Active := false ;
end;

procedure TtiQueryZeos.ExecSQL;
begin
  FQuery.ExecSQL ;
end;

function TtiQueryZeos.GetFieldAsBoolean(const psName: string): boolean;
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

function TtiQueryZeos.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByName( psName ).AsDateTime ;
end;

function TtiQueryZeos.GetFieldAsFloat(const psName: string): extended;
begin
  result := FQuery.FieldByName( psName ).AsFloat ;
end;

function TtiQueryZeos.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByName( psName ).AsInteger ;
end;

function TtiQueryZeos.GetFieldAsString(const psName: string): string;
begin
  result := FQuery.FieldByName(psName).AsString;
end;

function TtiQueryZeos.GetActive: boolean;
begin
  result := FQuery.Active ;
end;

function TtiQueryZeos.GetEOF: boolean;
begin
  result := FQuery.EOF ;
end;

function TtiQueryZeos.GetParamAsBoolean(const psName: string): boolean;
begin
  result := FQuery.ParamByName( psName ).AsBoolean ;
end;

function TtiQueryZeos.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.ParamByName( psName ).AsDateTime ;
end;

function TtiQueryZeos.GetParamAsFloat(const psName: string): extended;
begin
  result := FQuery.ParamByName( psName ).AsFloat ;
end;

function TtiQueryZeos.GetParamAsInteger(const psName: string): Int64;
begin
  result := FQuery.ParamByName( psName ).AsInteger ;
end;

function TtiQueryZeos.GetParamAsString(const psName: string): string;
begin
  result := FQuery.ParamByName(psName).AsString;
end;

function TtiQueryZeos.GetSQL: TStrings;
begin
  result := FQuery.SQL ;
end;

procedure TtiQueryZeos.Next;
begin
  FQuery.Next ;
end;

procedure TtiQueryZeos.Open;
begin
  Active := true ;
end;

function TtiQueryZeos.ParamCount: integer;
begin
  result := FQuery.Params.Count;
end;

function TtiQueryZeos.ParamName(pIndex: integer): string;
begin
  result := FQuery.Params.Items[ pIndex ].Name ;
end;

procedure TtiQueryZeos.SetActive(const Value: boolean);
begin
  FQuery.Active := Value ;
end;

procedure TtiQueryZeos.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  FQuery.ParamByName( psName ).AsBoolean := Value ;
end;

procedure TtiQueryZeos.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  FQuery.ParamByName( psName ).AsDateTime := Value ;
end;

procedure TtiQueryZeos.SetParamAsFloat(const psName: string;
  const Value: extended);
begin
  FQuery.ParamByName( psName ).AsFloat := Value ;
end;

procedure TtiQueryZeos.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  FQuery.ParamByName( psName ).AsInteger := Value ;
end;

procedure TtiQueryZeos.SetParamAsString(const psName, Value: string);
begin
  FQuery.ParamByName(UpperCase(psName)).AsString := Value;
end;

procedure TtiQueryZeos.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign( Value ) ;
end;

procedure TtiQueryZeos.AssignFieldAsStream(const pName: string; const pStream: TStream );
var
  lField : TField ;
begin
  lField := FQuery.FieldByName(pName);
  Assert(lField is TBlobField, 'Field <' + pName + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pStream);
end;

procedure TtiQueryZeos.AttachDatabase(pDatabase: TtiDatabase);
begin
  FQuery.Connection := TtiDatabaseZeosAbs( pDatabase ).Connection;
  Database := pDatabase ;
end;

procedure TtiQueryZeos.DetachDatabase;
begin
  inherited DetachDatabase ;
end;

function TtiQueryZeos.FieldCount: integer;
begin
  result := FQuery.FieldCount ;
end;

function TtiQueryZeos.FieldName(pIndex: integer): string;
begin
  result := FQuery.Fields[pIndex].FieldName ;
end;

procedure TtiQueryZeos.Reset;
begin
  Active := false ;
  FQuery.SQL.Clear ;
  FQuery.Params.Clear ;
end;

function TtiQueryZeos.FieldIndex(const psName: string): integer;
begin
  result := FQuery.FieldByName( psName ).Index ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseZeosAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseZeosAbs.Create;
begin
  inherited Create ;
  FConnection := TZConnection.Create( nil ) ;
  FConnection.LoginPrompt := false ;
end;

destructor TtiDatabaseZeosAbs.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TtiDatabaseZeosAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create( 'Attempt to commit but not in a transaction.');

  FConnection.Commit ;
end;

function TtiDatabaseZeosAbs.InTransaction: boolean;
begin
  result := FConnection.InTransaction ;
end;

procedure TtiDatabaseZeosAbs.RollBack;
begin
  FConnection.RollBack ;
end;

procedure TtiDatabaseZeosAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FConnection.StartTransaction ;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryZeos.FieldKind(pIndex: integer): TtiQueryFieldKind;
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
//    ftBoolean :                                 result := qfkLogical  ;
    ftFloat, ftCurrency, ftBCD :                result := qfkFloat    ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime ;
    ftBlob{, ftGraphic} :                       result := qfkBinary   ;
    ftMemo, ftFmtMemo :                         result := qfkLongString ;
    else
      raise EtiOPFInternalException.Create(
        'Invalid FQuery.Fields[ pIndex ].DataType <' +
                      GetEnumName( TypeInfo( TFieldType ), Ord( lDataType )) + '>') ;
    end ;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

function TtiQueryZeos.FieldSize(pIndex: integer): integer;
begin
  case FieldKind( pIndex ) of
    qfkString     : result := FQuery.FieldDefs[ pIndex ].Size ;
    qfkLongString : result := 0 ;
    qfkInteger    : result := 0 ;
    qfkFloat      : result := 0 ;
    qfkDateTime   : result := 0 ;
    qfkBinary     : result := 0 ;
 //   qfkLogical    : result := 0 ;
  else
    raise EtiOPFInternalException.Create( 'Invalid field type') ;
  end ;
end;

function TtiQueryZeos.GetParamIsNull(const psName: String): Boolean;
begin
  result := FQuery.ParamByName( psName ).IsNull ;
end;

procedure TtiQueryZeos.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  FQuery.ParamByName( psName ).Clear ;
end;

function TtiDatabaseZeosAbs.GetConnected: boolean;
begin
  Result := FConnection.Connected ;
end;

procedure TtiDatabaseZeosAbs.SetConnected(pbValue: boolean);
var
  i : integer ;
  lsErrorMessage : string ;
begin

  if ( not pbValue ) then
  begin
    Log( 'Disconnecting from %s', [DatabaseName] ) ;
    FConnection.Connected := false ;
    Exit ; //==>
  end ;

  SetupDBParams ;
  lsErrorMessage := '' ;
  try
    FConnection.Connected := true ;
  except
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, e.message ) ;
  end ;
end;

function TtiQueryZeos.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldByName( psName ).IsNull ;
end;

procedure TtiQueryZeos.AssignParamToStream(const pName: string; const pStream : TStream);
var
  ls : string ;
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  ls := FQuery.ParamByName(pName).Value ;
  tiStringToStream(ls, pStream);
end;

procedure TtiQueryZeos.AssignParamFromStream(const pName: string; const pStream : TStream);
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  FQuery.ParamByName( pName ).LoadFromStream( pStream, ftBlob );
end;

function TtiQueryZeos.HasNativeLogicalType: boolean;
begin
  result := false ;
end;

procedure TtiQueryZeos.AssignFieldAsStreamByIndex(pIndex: Integer;const pValue: TStream);
var
  lField : TField ;
begin
  lField := FQuery.Fields[pIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(pIndex) + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pValue);
end;

function TtiQueryZeos.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
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

function TtiQueryZeos.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ pIndex ].AsDateTime ;
end;

function TtiQueryZeos.GetFieldAsFloatByIndex(pIndex: Integer): extended;
begin
  result := FQuery.Fields[ pIndex ].AsFloat ;
end;

function TtiQueryZeos.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FQuery.Fields[ pIndex ].AsInteger ;
end;

function TtiQueryZeos.GetFieldAsStringByIndex(pIndex: Integer): string;
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

function TtiQueryZeos.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ pIndex ].IsNull ;
end;

function TtiDatabaseZeosAbs.Test: boolean;
begin
  result := false;
  Assert( false, 'Under construction' ) ;
end;

function TtiQueryZeos.GetParamAsTextBLOB(const psName: string): string;
begin
  result := FQuery.ParamByName(psName).AsMemo;
end;

procedure TtiQueryZeos.SetParamAsTextBLOB(const psName, Value: string);
begin
  FQuery.ParamByName(psName).AsMemo := Value;
end;

initialization


end.

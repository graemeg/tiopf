{$I tiDefines.inc}

{$DEFINE BOOLEAN_CHAR_1}

unit tiQueryIBO ;

interface
uses
   tiQuery
  ,Classes
  ,IB_Components
  ,tiDBConnectionPool
  ,IB_Header
  ,tiClassToDBMap_BOM
  ,tiObject
  ;

type

  TtiDatabaseIBO = class ( TtiDatabaseSQL )
  private
    FDatabase: TIB_Connection ;
    FIBTransaction: TIB_Transaction ;
    property IBDatabase: TIB_Connection read FDatabase write FDatabase ;
  protected
    function  FieldMetaDataToSQLCreate ( const pFieldMetaData: TtiDBMetaDataField) : string ; override ;
    function  GetConnected: Boolean ; override ;
    procedure SetConnected ( pbValue: Boolean ) ; override ;
  public
    constructor     create ; override ;
    destructor      Destroy ; override ;
    procedure       Commit ; override ;
    class procedure CreateDatabase ( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    class function  DatabaseExists ( const psDatabaseName, psUserName, psPassword: string ) : Boolean ; override ;
    function        InTransaction: Boolean ; override ;
    function        ReadGeneratorOID ( AGeneratorName: string ; AIncrement: integer = 1 ) :Integer ;
    procedure       ReadMetaDataFields ( pData: TtiDBMetaData ) ; override ;
    procedure       ReadMetaDataTables ( pData: TtiDBMetaDataTable ) ; override ;
    procedure       RollBack ; override ;
    procedure       StartTransaction ; override ;
    function        Test : boolean ; override ;
  end ;

  TtiQueryIBO = class ( TtiQuerySQL )
  private
    FbActive: Boolean ;
    FIBSQL: TIB_Cursor ;
    procedure CheckPrepared ;
    function IBFieldKindToTIFieldKind ( PSQLVAR: PXSQLVAR ) : TtiQueryFieldKind ;
  protected
    function GetActive: Boolean ; override ;
    function GetEOF: Boolean ; override ;
    function GetFieldAsBoolean ( const psName: string ) : Boolean ; override ;
    function GetFieldAsDateTime ( const psName: string ) : TDateTime ; override ;
    function GetFieldAsFloat ( const psName: string ) : extended ; override ;
    function GetFieldAsInteger ( const psName: string ) : Int64 ; override ;
    function GetFieldAsString ( const psName: string ) : string ; override ;
    function GetFieldAsVariant ( const psName: string ) : Variant ; override ;
    function GetFieldIsNull ( const psName: string ) : Boolean ; override ;
    function GetParamAsBoolean ( const psName: string ) : Boolean ; override ;
    function GetParamAsDateTime ( const psName: string ) : TDateTime ; override ;
    function GetParamAsFloat ( const psName: string ) : extended ; override ;
    function GetParamAsInteger ( const psName: string ) : Int64 ; override ;
    function GetParamAsString ( const psName: string ) : string ; override ;
    function GetParamAsTextBLOB ( const psName: string ) : string ; override ;
    function GetParamAsVariant ( const psName: string ) : Variant ; override ;
    function GetParamIsNull ( const psName: string ) : Boolean ; override ;
    function GetSQL: TStrings ; override ;
    procedure SetActive ( const Value: Boolean ) ; override ;
    procedure SetParamAsBoolean ( const psName: string ; const Value: Boolean ) ;
      override ;
    procedure SetParamAsDateTime ( const psName: string ; const Value:
      TDateTime ) ; override ;
    procedure SetParamAsFloat ( const psName: string ; const Value: extended ) ;
      override ;
    procedure SetParamAsInteger ( const psName: string ; const Value: Int64 ) ;
      override ;
    procedure SetParamAsString ( const psName: string ; const Value: string ) ;
      override ;
    procedure SetParamAsTextBLOB ( const psName: string ; const Value: string ) ;
      override ;
    procedure SetParamAsVariant ( const psName: string ; const Value: Variant ) ;
      override ;
    procedure SetParamIsNull ( const psName: string ; const Value: Boolean ) ;
      override ;
    procedure SetSQL ( const Value: TStrings ) ; override ;
  public
    constructor Create ; override ;
    destructor Destroy ; override ;
    procedure AssignFieldAsStream ( const pName: string ; const pVaule: TStream
      ) ; override ;
    procedure AssignParamFromStream ( const pName: string ; const pVaule:
      TStream ) ; override ;
    procedure AssignParams ( pParams: TtiQueryParams ; pWhere: TtiQueryParams =
      nil ) ; override ;
    procedure AssignParamToStream ( const pName: string ; const pVaule: TStream
      ) ; override ;
    procedure AttachDatabase ( pDatabase: TtiDatabase ) ; override ;
    procedure Close ; override ;
    procedure DetachDatabase ; override ;
    function ErrorMessage: string ; override ;
    procedure ExecSQL ; override ;
    function FieldCount: Integer ; override ;
    function FieldIndex ( const psName: string ) : Integer ; override ;
    function FieldKind ( pIndex: integer ) : TtiQueryFieldKind ; override ;
    function FieldName ( pIndex: integer ) : string ; override ;
    function FieldSize ( pIndex: integer ) : Integer ; override ;
    procedure Next ; override ;
    procedure Open ; override ;
    function ParamCount: Integer ; override ;
    function ParamName ( pIndex: integer ) : string ; override ;
    procedure Reset ; override ;
  end ;

implementation
uses
   tiUtils
  ,tiDialogs
  ,tiLog
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,IB_Session
  ,SysUtils
{$IFNDEF VER130}
  ,Variants
{$ENDIF}
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryIBX
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
********************************* TtiQueryIBO **********************************
}

constructor TtiQueryIBO.Create ;
begin
  inherited ;
  FIBSQL := TIB_Cursor.Create ( nil ) ;
end ;

destructor TtiQueryIBO.Destroy ;
begin
  FIBSQL.Free ;
  inherited ;
end ;

procedure TtiQueryIBO.AssignFieldAsStream ( const pName: string ; const pVaule:
  TStream ) ;
var
  st: TIB_BlobStream ;
begin
  Assert ( pVaule <> nil, 'Stream not assigned' ) ;
  pVaule.Position := 0 ;
  try
    st := FIBSQL.CreateBlobStream ( FIBSQL.ParamByName ( pName ) , bsmRead ) ;
    pVaule.Write ( st, st.Size ) ;
  finally
    st.Free ;
  end ;
end ;

procedure TtiQueryIBO.AssignParamFromStream ( const pName: string ; const pVaule
  : TStream ) ;
var
  st: TIB_BlobStream ;
begin
  Assert ( pVaule <> nil, 'Stream not assigned' ) ;
  pVaule.Position := 0 ;
  st := FIBSQL.CreateBlobStream ( FIBSQL.ParamByName ( pName ) , bsmWrite ) ;
  try
    st.CopyFrom ( pVaule, pVaule.Size ) ;
  finally
    st.Free ;
  end ;
  //  FIBSQL.ParamByName(pName).ReadFromStream(pStream);
end ;

procedure TtiQueryIBO.AssignParams ( pParams: TtiQueryParams ; pWhere:
  TtiQueryParams = nil ) ;
var
  i: Integer ;
begin
    // Only for IB5.5 support...
  //  if FIBSQL.Database.SQLDialect <> 1 then
  //    inherited
  //  else
  //  begin
  CheckPrepared ;
  if pParams = nil then
    Exit ;
  for i := 0 to pParams.Count - 1 do
    ParamAsVariant [ pParams.Items [ i ] .Name ] := pParams.Items [ i ] .Value ;
  if pWhere <> nil then
    for i := 0 to pWhere.Count - 1 do
      ParamAsVariant [ pWhere.Items [ i ] .Name ] := pWhere.Items [ i ] .Value ;
  //  end;
end ;

procedure TtiQueryIBO.AssignParamToStream ( const pName: string ; const pVaule:
  TStream ) ;
var
  st: TIB_BlobStream ;
begin
  Assert ( pVaule <> nil, 'Stream not assigned' ) ;
  pVaule.Position := 0 ;
  st := FIBSQL.CreateBlobStream ( FIBSQL.ParamByName ( pName ) , bsmRead ) ;
  try
    pVaule.CopyFrom ( st, st.Size ) ;
  finally
    st.Free ;
  end ;
  pVaule.Position := 0 ;
end ;

procedure TtiQueryIBO.AttachDatabase ( pDatabase: TtiDatabase ) ;
begin
  inherited AttachDatabase ( pDatabase ) ;
  FIBSQL.IB_Connection := TtiDatabaseIBO ( pDatabase ) .FDatabase ;
end ;

procedure TtiQueryIBO.CheckPrepared ;
begin
  if not FIBSQL.Prepared then FIBSQL.Prepare ;
end ;

procedure TtiQueryIBO.Close ;
begin
  Active := false ;
end ;

procedure TtiQueryIBO.DetachDatabase ;
begin
  inherited DetachDatabase ;
  if FIBSQL.Active then
    FIBSQL.Close ;
  FIBSQL.IB_Transaction := nil ;
  FIBSQL.IB_Connection := nil ;
end ;

function TtiQueryIBO.ErrorMessage: string ;
begin
  Result := ( inherited ErrorMessage ) + Cr ( 2 ) +
    'SQL:' + #13 + FIBSQL.SQLText + Cr ;
end ;

procedure TtiQueryIBO.ExecSQL ;
var
  lErrorMessage : string ;
begin
  try
    FIBSQL.ExecSQL ;
  except
    on e:exception do
    begin
      lErrorMessage :=
        'SQL: ' + FQuery.SQLText + Cr(2) +
        'Params: ' + FParams.AsString ;
      raise EtiOPFInternalException.Create( lErrorMessage + Cr(2) + e.Message) ;
    end;
  end;
end ;

function TtiQueryIBO.FieldCount: Integer ;
begin
  if FIBSQL.EOF then
    Result := 0
  else
    Result := FIBSQL.FieldCount ;
end ;

function TtiQueryIBO.FieldIndex ( const psName: string ) : Integer ;
begin
  CheckPrepared ;
  Result := FIBSQL.FieldByName ( psName ) .Index ;
end ;

function TtiQueryIBO.FieldKind ( pIndex: integer ) : TtiQueryFieldKind ;
var
  lValue: string ;
begin
  Result := IBFieldKindToTIFieldKind ( FIBSQL.Fields [ pIndex ] .PSQLVAR ) ;
  if ( Result = qfkString ) then
    begin
      lValue := FIBSQL.Fields [ pIndex ] .AsString ;
  //    if SameText(lValue, 'T') or
  //      SameText(lValue, 'F') or
  //      SameText(lValue, 'TRUE') or
  //      SameText(lValue, 'TRUE ') or
  //      SameText(lValue, 'FALSE') or
  //      SameText(lValue, '1') or
  //      SameText(lValue, '0') then
      // ToDo: How to detect a logical field in a SQL database
      //       where logicals are represented as VarChar or Char?
      //       In the IBX layer we are currently looking for a value of
      //       'TRUE' or 'FALSE', but this is not reliable as it will not
      //       detect a null. Also, other ways of representing booleans
      //       might be used like 'T', 'F', '0', '1', etc...
      //  Carlos  14/09/2003  added some values T and F for DUnit tests

      if SameText ( lValue, 'TRUE' ) or
        SameText ( lValue, 'TRUE ' ) or
        SameText ( lValue, 'T' ) or
        SameText ( lValue, 'F' ) or
        SameText ( lValue, 'FALSE' ) then
        Result := qfkLogical ;
    end ;
end ;

function TtiQueryIBO.FieldName ( pIndex: integer ) : string ;
begin
  Result := FIBSQL.Fields.Columns [ pIndex ] .BDEFieldName ;
end ;

function TtiQueryIBO.FieldSize ( pIndex: integer ) : Integer ;
begin
  CheckPrepared ;
  Result := FIBSQL.Fields [ pIndex ] .PSQLVAR.sqllen ;
end ;

function TtiQueryIBO.GetActive: Boolean ;
begin
  Result := FbActive ;
end ;

function TtiQueryIBO.GetEOF: Boolean ;
begin
  Result := FIBSQL.EOF ;
end ;

function TtiQueryIBO.GetFieldAsBoolean ( const psName: string ) : Boolean ;
var
  lsValue: string ;
begin
  lsValue := Trim ( upperCase ( FIBSQL.FieldByName ( psName ) .AsString ) ) ;
  Result := ( lsValue = 'T' ) or
    ( lsValue = 'TRUE' ) or
    ( lsValue = 'Y' ) or
    ( lsValue = 'YES' ) or
    ( lsValue = '1' ) ;
end ;

function TtiQueryIBO.GetFieldAsDateTime ( const psName: string ) : TDateTime ;
begin
  Result := FIBSQL.FieldByName ( psName ) .AsDateTime ;
end ;

function TtiQueryIBO.GetFieldAsFloat ( const psName: string ) : extended ;
begin
  Result := FIBSQL.FieldByName ( psName ) .AsFloat ;
end ;

function TtiQueryIBO.GetFieldAsInteger ( const psName: string ) : Int64 ;
begin
  Result := FIBSQL.FieldByName ( psName ) .AsInteger ;
end ;

function TtiQueryIBO.GetFieldAsString ( const psName: string ) : string ;
begin
  Result := FIBSQL.FieldByName ( psName ) .AsString ;
end ;

function TtiQueryIBO.GetFieldAsVariant ( const psName: string ) : Variant ;
var
  lFieldKind: TtiQueryFieldKind ;
begin
  lFieldKind := FieldKind ( FieldIndex ( psName ) ) ;
    // ToDo: This fixes the problem of AsString returning '(Blob)' for memos, but may
    //       break any attempt to read true binary data.
  case lFieldKind of
    qfkBinary: result := FIBSQL.FieldByName ( UpperCase ( psName ) ) .AsString
  else
    Result := FIBSQL.FieldByName ( UpperCase ( psName ) ) .Value ;
  end ;
end ;

function TtiQueryIBO.GetFieldIsNull ( const psName: string ) : Boolean ;
begin
  Result := FIBSQL.FieldByName ( psName ) .IsNull ;
end ;

function TtiQueryIBO.GetParamAsBoolean ( const psName: string ) : Boolean ;
var
  lValue: string ;
begin
  lValue := FIBSQL.ParamByName ( psName ) .AsString ;
{$IFDEF BOOLEAN_CHAR_1}
  Result := SameText ( lValue, 'T' ) ;
{$ELSE}
  Result := SameText ( lValue, 'TRUE' ) ;
{$ENDIF} // BOOLEAN_CHAR_1
end ;

function TtiQueryIBO.GetParamAsDateTime ( const psName: string ) : TDateTime ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsDateTime ;
end ;

function TtiQueryIBO.GetParamAsFloat ( const psName: string ) : extended ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsFloat ;
end ;

function TtiQueryIBO.GetParamAsInteger ( const psName: string ) : Int64 ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsInteger ;
end ;

function TtiQueryIBO.GetParamAsString ( const psName: string ) : string ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsString ;
end ;

function TtiQueryIBO.GetParamAsTextBLOB ( const psName: string ) : string ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsString ;
end ;

function TtiQueryIBO.GetParamAsVariant ( const psName: string ) : Variant ;
begin
  Result := FIBSQL.ParamByName ( psName ) .AsVariant ;
end ;

function TtiQueryIBO.GetParamIsNull ( const psName: string ) : Boolean ;
begin
  Result := FIBSQL.ParamByName ( psName ) .IsNull ;
end ;

function TtiQueryIBO.GetSQL: TStrings ;
begin
  Result := FIBSQL.SQL ;
end ;

function TtiQueryIBO.IBFieldKindToTIFieldKind ( PSQLVAR: PXSQLVAR ) :
  TtiQueryFieldKind ;
begin
    {:@ver Carlos - 12/06/2003  tipos nulos:  _}
  case PSQLVAR.sqltype of
    SQL_TEXT, SQL_TEXT_: result := qfkString ;
    SQL_VARYING, SQL_VARYING_: result := qfkString ;
    SQL_LONG, SQL_LONG_, SQL_SHORT, SQL_SHORT_, SQL_INT64, SQL_INT64_, SQL_QUAD, SQL_QUAD_: result := qfkInteger ;
    SQL_DOUBLE, SQL_DOUBLE_, SQL_FLOAT, SQL_FLOAT_, SQL_D_FLOAT, SQL_D_FLOAT_: result := qfkFloat ;
    SQL_BLOB, SQL_BLOB_: result := qfkBinary ;
    SQL_TIMESTAMP, SQL_TIMESTAMP_, SQL_TYPE_TIME, SQL_TYPE_TIME_, SQL_TYPE_DATE, SQL_TYPE_DATE_: result := qfkDateTime ;
      //SQL_ARRAY
      // What about these?
      //result := qfkLongString ;
  else
    Result := qfkString ; // Just to shup up the compiler
    raise EtiOPFInternalException.Create ( 'Invalid Interbase DataType <' +
      IntToStr ( PSQLVAR.sqltype ) + '>') ;
  end ;
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
end ;

procedure TtiQueryIBO.Next ;
begin
  FIBSQL.ApiNext ;
end ;

procedure TtiQueryIBO.Open ;
begin
  try
    FIBSQL.BeginBusy ( false ) ;
  except

  end ;
  Active := true ;
  FIBSQL.APIFirst ; // importante
end ;

function TtiQueryIBO.ParamCount: Integer ;
begin
  CheckPrepared ;
  Result := FIBSQL.Params.ColumnCount ;
end ;

function TtiQueryIBO.ParamName ( pIndex: integer ) : string ;
begin
  CheckPrepared ;
  Result := FIBSQL.Params.BySQLNo ( pIndex ) .FieldName ;
end ;

procedure TtiQueryIBO.Reset ;
begin
  Active := false ;
  FIBSQL.SQL.Clear ;
end ;

procedure TtiQueryIBO.SetActive ( const Value: Boolean ) ;
var
  lErrorMessage : string ;
begin
  if Value then
  begin
    try
      FIBSQL.ExecSQL ;
      FbActive := true ;
    except
      on e:exception do
      begin
        lErrorMessage :=
          'SQL: ' + FQuery.SQLText + Cr(2) +
          'Params: ' + FParams.AsString ;
        raise EtiOPFInternalException.Create(lErrorMessage + Cr(2) + 'Message: ' + e.message);
      end;
    end;
  end else
  begin
    FIBSQL.Close ;
    try
      FIBSQL.EndBusy ;
    except
    end ;
    FbActive := false ;
  end ;
end ;

procedure TtiQueryIBO.SetParamAsBoolean ( const psName: string ; const Value:
  Boolean ) ;
begin
{$IFDEF BOOLEAN_CHAR_1}
  if Value then
    FIBSQL.ParamByName ( psName ) .AsString := 'T'
  else
    FIBSQL.ParamByName ( psName ) .AsString := 'F' ;
{$ELSE}
  if Value then
    FIBSQL.ParamByName ( psName ) .AsString := 'TRUE'
  else
    FIBSQL.ParamByName ( psName ) .AsString := 'FALSE' ;
{$ENDIF} // BOOLEAN_CHAR_1
end ;

procedure TtiQueryIBO.SetParamAsDateTime ( const psName: string ; const Value:
  TDateTime ) ;
begin
  FIBSQL.ParamByName ( psName ) .AsDateTime := Value ;
end ;

procedure TtiQueryIBO.SetParamAsFloat ( const psName: string ; const Value:
  extended ) ;
begin
  FIBSQL.ParamByName ( psName ) .AsDouble := Value ;
end ;

procedure TtiQueryIBO.SetParamAsInteger ( const psName: string ; const Value:
  Int64 ) ;
begin
  //  Log('Param as integer : '+psNAme,lsUserInfo);
  //  if High(Integer) < Value then
  //  begin
  //    Log('Int64',lsUserInfo);
  //    FIBSQL.ParamByName(UpperCase(psName)).AsInt64 := Value;
  //  end
  //  else
  //  begin
  //    Log('Integer',lsUserInfo);
  CheckPrepared ;
  FIBSQL.ParamByName ( psName ) .AsInteger := Integer ( Value ) ;
  //  end;
end ;

procedure TtiQueryIBO.SetParamAsString ( const psName: string ; const Value:
  string ) ;
begin
  CheckPrepared ;
  FIBSQL.ParamByName ( psName ) .AsString := Value ;
end ;

procedure TtiQueryIBO.SetParamAsTextBLOB ( const psName: string ; const Value:
  string ) ;
begin
  CheckPrepared ;
  FIBSQL.ParamByName ( psName ) .AsString := Value ;
end ;

procedure TtiQueryIBO.SetParamAsVariant ( const psName: string ; const Value:
  Variant ) ;
var
  lFieldKind: TtiQueryFieldKind ;
  lValue: Variant ;
  lsValue: string ;
begin
  lValue := Value ;
    lFieldKind := IBFieldKindToTIFieldKind ( FIBSQL.ParamByName ( psName ) .PSQLVAR ) ;
    case lFieldKind of
      qfkDateTime: FIBSQL.ParamByName ( psName ) .AsDateTime := lValue ;
      qfkFloat: FIBSQL.ParamByName ( psName ) .AsDouble := lValue ;
      qfkInteger: FIBSQL.ParamByName ( psName ) .AsInteger := lValue ;
      qfkString:
        begin
          lsValue := VarToStr ( lValue ) ;
              // VarToStr on a boolean will return an integer so we must
              // check for booleans and convert to a string.
          if tiIsVariantOfType ( Value, varBoolean ) then
            begin
{$IFDEF BOOLEAN_CHAR_1}
              if Value then
                SetParamAsString ( psName, 'T' )
              else
                SetParamAsString ( psName, 'F' ) ;
{$ELSE}
              if Value then
                SetParamAsString ( psName, 'TRUE' )
              else
                SetParamAsString ( psName, 'FALSE' ) ;
{$ENDIF} // BOOLEAN_CHAR_1
            end
          else
            FIBSQL.ParamByName ( psName ) .AsString := lsValue ;
        end ;
    else
      FIBSQL.ParamByName ( psName ) .AsVariant := lValue ;
    end
  //    else
  //    begin
  //      case lFieldKind of
  //        qfkDateTime:
  //          begin
  //            lDateTime := FormatDateTime('yyyy-mm-dd hh:mm:ss', Value);
  //            FIBSQL.ParamByName(UpperCase(psName)).Value := lDateTime;
  //          end;
  //        qfkFloat:
  //          FIBSQL.ParamByName(UpperCase(psName)).Value := FloatToStR(Value);
  //      else
  //        FIBSQL.ParamByName(UpperCase(psName)).Value := Value;
  //      end;
  //    end;
end ;

procedure TtiQueryIBO.SetParamIsNull ( const psName: string ; const Value:
  Boolean ) ;
begin
  FIBSQL.ParamByName ( psName ) .IsNull := true ;
end ;

procedure TtiQueryIBO.SetSQL ( const Value: TStrings ) ;
begin
  FIBSQL.SQL.Assign ( Value ) ;
  FIBSQL.Prepare ;
end ;

{ Carlos  14/09/2003 checks if SQL is prepared, IBO uses dynamic fields and parameters }
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseIBO
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
******************************** TtiDatabaseIBO ********************************
}

constructor TtiDatabaseIBO.Create ;
begin
  inherited Create ;
  FDatabase := TIB_Connection.Create ( nil ) ;
  FDatabase.LoginPrompt := false ;
  FDatabase.SQLDialect := 1 ; // IK added.
  FIBTransaction := TIB_Transaction.Create ( nil ) ;
  FIBTransaction.Isolation := tiCommitted ;
  FDatabase.DefaultTransaction := FIBTransaction ;
  FIBTransaction.IB_Connection := FDatabase ;
end ;

destructor TtiDatabaseIBO.Destroy ;
begin
  try
  //   FIBTransaction.Active := false;
    FDatabase.Connected := false ;
    FDatabase.DefaultTransaction := nil ;
    FIBTransaction.IB_Connection := nil ;
    FIBTransaction.Free ;
    FDatabase.Free ;
  except
    on e: exception do
      LogError ( e.message ) ;
  end ;
  inherited ;
end ;

procedure TtiDatabaseIBO.Commit ;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.') ;
  FIBTransaction.CommitRetaining ;
end ;

class procedure TtiDatabaseIBO.CreateDatabase ( const psDatabaseName, psUserName,
  psPassword: string ) ;
var
  FDatabase: TIB_Connection ;
begin
  if DatabaseExists ( psDatabaseName, psUserName, psPassword ) then
    raise EtiDBAlreadyExists.Create ;
  FDatabase := TIB_Connection.Create ( nil ) ;
  try
    try
      FDatabase.Database := psDatabaseName ;
      FDatabase.Username := psUserName ;
      FDatabase.Password := psPassword ;
      FDatabase.CreateDatabase ;
    except
      on e: EIB_ISCERROR do
        begin
          tiAppError ( 'Error attempting to create database:' + CrLf ( 2 ) +
            psDatabaseName + CrLf ( 2 ) + e.Message + CrLf ( 2 ) +
            'Error code: ' + IntToStr ( EIB_ISCERROR ( E ) .ERRCODE ) ) ;
        end ;
    end ;
  finally
    FDatabase.Free ;
  end ;
end ;

class function TtiDatabaseIBO.DatabaseExists ( const psDatabaseName, psUserName,
  psPassword: string ) : Boolean ;
var
  FDatabase: TIB_Connection ;
begin
  Result := FileExists ( psDatabaseName ) ;
  if not Result then
    Exit ; //==>
  FDatabase := TIB_Connection.Create ( nil ) ;
  try
    try
      FDatabase.Database := psDatabaseName ;
      FDatabase.Username := psUserName ;
      FDatabase.Password := psPassword ;
      FDatabase.Connect ;
      Result := true ;
    except
      Result := false ;
    end ;
  finally
    FDatabase.Connected := false ;
    FDatabase.Free ;
  end ;
end ;

function TtiDatabaseIBO.FieldMetaDataToSQLCreate ( const pFieldMetaData:
  TtiDBMetaDataField ) : string ;
begin
  case pFieldMetaData.Kind of
    qfkString: result := 'VarChar( ' + IntToStr ( pFieldMetaData.Width ) + ' )' ;
    qfkInteger: result := 'Integer' ;
  //    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: result := 'DOUBLE PRECISION' ;
      // Just for new version of IB (6.x)
      // DATE holds only DATE without TIME...
    qfkDateTime:
      if IBDatabase.SQLDialect <> 1 then
        result := 'TIMESTAMP'
      else
        result := 'Date' ;
{$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: result := 'Char( 1 ) default ''F''' ;
{$ELSE}
    qfkLogical: result := 'VarChar( 5 ) default ''FALSE''' ;
{$ENDIF} // BOOLEAN_CHAR_1
    qfkBinary: result := 'Blob sub_type 2' ;
    qfkLongString: result := 'Blob sub_type 1' ;
  else
    raise EtiOPFInternalException.Create( 'Invalid FieldKind') ;
  end ;
    {
      gTIOPFManager.ExecSQL(
        'create table Test_Group ' +
        '  ( OID                 Integer not null, ' +
        '    Group_Str_Field     VarChar( 10 ), ' +
        '    Group_Int_Field     Integer, ' +
        '    Group_Float_Field   Decimal( 10, 5 ), ' +
        '    Primary Key       ( OID ) ' +
        '    )' ) ;
    }

    {
      gTIOPFManager.ExecSQL(
        'create table Test_Item ' +
        '  ( OID                Integer not null, ' +
        '    OID_Group          Integer, ' +
        '    Item_Str_Field     VarChar( 10 ), ' +
        '    Item_Int_Field     Integer, ' +
        '    Item_Float_Field   Decimal( 10, 5 ), ' +
        '    Item_Logical_Field VarChar( 5 ) default ''FALSE'', ' +
        '    Item_Date_Field    Date, ' +
        '    Item_Notes_Field   blob sub_type 1, ' +
        '    Primary Key ( OID ) ' +
        '    ) ') ;

      gTIOPFManager.ExecSQL(
        'create table Test_Bin ' +
        '  ( OID                Integer not null, ' +
        '    Item_Binary_Field  blob sub_type 2, ' +
        '    Primary Key ( OID ) ' +
        '    ) ' ) ;
    }

end ;

function TtiDatabaseIBO.GetConnected: Boolean ;
begin
  Result := FDatabase.Connected ;
end ;

function TtiDatabaseIBO.InTransaction: Boolean ;
begin
  result := FIBTransaction.InTransaction ;
end ;

function TtiDatabaseIBO.ReadGeneratorOID ( AGeneratorName: string ; AIncrement:
  integer = 1 ) : Integer ;
begin
  Result := IBDatabase.Gen_ID ( AGeneratorName, AIncrement ) ;
end ;

procedure TtiDatabaseIBO.ReadMetaDataFields ( pData: TtiDBMetaData ) ;
var
  lTableName: string ;
  lQuery: TtiQuery ;
  lTable: TtiDBMetaDataTable ;
  lField: TtiDBMetaDataField ;
  lFieldType: integer ;
  lFieldLength: integer ;

const
  // Interbase field types
  //   select * from rdb$types
  //   where rdb$field_NAME = 'RDB$FIELD_TYPE'
  //   ORDER BY RDB$TYPE

  cIBField_LONG = 8 ;
  cIBField_DOUBLE = 27 ;
  cIBField_TIMESTAMP = 35 ;
  cIBField_DATE = 12 ;
  cIBField_TIME = 13 ;
  cIBField_VARYING = 37 ;
  cIBField_BLOB = 261 ;

  cIBField_SHORT = 7 ;
  cIBField_QUAD = 9 ;
  cIBField_FLOAT = 10 ;
  cIBField_TEXT = 14 ;
  cIBField_CSTRING = 40 ;
  cIBField_BLOB_ID = 45 ;

begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lTableName := lTable.Name ;
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery ;
  try
    StartTransaction ;
    try
      lQuery.AttachDatabase ( Self ) ;
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
        '  and f.rdb$field_name = r.rdb$field_source ' ;

      lQuery.Open ;
      while not lQuery.EOF do
        begin
          lField := TtiDBMetaDataField.Create ;
          lField.Name := Trim ( lQuery.FieldAsString [ 'field_name' ] ) ;
          lFieldType := lQuery.FieldAsInteger [ 'field_type' ] ;
          lFieldLength := lQuery.FieldAsInteger [ 'field_length' ] ;

          lField.Width := 0 ;

          case lFieldType of
            cIBField_LONG: lField.Kind := qfkInteger ;
            cIBField_DOUBLE: lField.Kind := qfkFloat ;
            cIBField_TIMESTAMP,
              cIBField_DATE,
              cIBField_TIME: lField.Kind := qfkDateTime ;
            cIBField_VARYING,
              cIBField_TEXT: begin
                lField.Kind := qfkString ;
                lField.Width := lFieldLength ;
              end ;
            cIBField_BLOB: begin
                Assert ( not lQuery.FieldIsNull [ 'field_sub_type' ] , 'field_sub_type is null' ) ;
                if lQuery.FieldAsInteger [ 'field_sub_type' ] = 1 then
                  lField.Kind := qfkLongString
                else
                  raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr ( lQuery.FieldAsInteger [ 'field_sub_type' ] ) + '>') ;
              end ;
          else
            raise EtiOPFInternalException.Create( 'Invalid Interbase FieldType <' + IntToStr ( lFieldType ) + '>' ) ;
          end ;
          lField.ObjectState := posClean ;
          lTable.Add ( lField ) ;
          lQuery.Next ;
        end ;
    finally
      Commit ;
    end ;
    lQuery.DetachDatabase ;
    lTable.ObjectState := posClean ;
  finally
    lQuery.Free ;
  end ;

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

end ;

procedure TtiDatabaseIBO.ReadMetaDataTables ( pData: TtiDBMetaDataTable ) ;
var
  lQuery: TtiQuery ;
  lMetaData: TtiDBMetaData ;
  lTable: TtiDBMetaDataTable ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery ;
  try
    StartTransaction ;
    try
      lQuery.AttachDatabase ( Self ) ;
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        'FROM RDB$RELATIONS              ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR ' +
        '(RDB$SYSTEM_FLAG IS NULL)) AND  ' +
        '(RDB$VIEW_SOURCE IS NULL)       ' +
        'ORDER BY RDB$RELATION_NAME      ' ;
      lQuery.Open ;
      while not lQuery.EOF do
        begin
          lTable := TtiDBMetaDataTable.Create ;
          lTable.Name := Trim ( lQuery.FieldAsString [ 'table_name' ] ) ;
          lTable.ObjectState := posPK ;
          lMetaData.Add ( lTable ) ;
          lQuery.Next ;
        end ;
      lQuery.DetachDatabase ;
      lMetaData.ObjectState := posClean ;
    finally
      Commit ;
    end ;
  finally
    lQuery.Free ;
  end ;
end ;

procedure TtiDatabaseIBO.RollBack ;
begin
  FIBTransaction.RollbackRetaining ;
end ;

procedure TtiDatabaseIBO.SetConnected ( pbValue: Boolean ) ;
var
  lMessage : string ;
begin

  try
    if ( not pbValue ) then
      begin
        FDatabase.Connected := false ;
        Exit ; //==>
      end ;
    FDatabase.Params.Clear ;
    FDatabase.DatabaseName := Self.DatabaseName ;
    FDatabase.Username := UserName ;
    FDatabase.Password := Password ;
    FDatabase.Params.Add ( 'user_name=' + UserName ) ;
    FDatabase.Params.Add ( 'password=' + Password ) ;
    FDatabase.Connected := true ;

  except
    // ToDo: Must come up with a better solution that this:
    //       Try several times before raising an exception.
    //       Rather than calling 'Halt', just terminate this database connection,
    //       unless this is the first connection.
    on e: EIB_ISCERROR do
      begin
      // Invalid username / password error
        if ( EIB_ISCERROR ( E ) .ERRCODE = 335544472 ) then
          raise
        else
          begin
            lMessage := 'Error attempting to connect to database.' + CrLf ( 2 ) +
              e.Message + CrLf ( 2 ) +
              'Error code: ' + IntToStr ( EIB_ISCERROR ( E ) .ERRCODE ) ;
            tiAppError ( lMessage ) ;
            LogError( lMessage ) ;
            LogError( 'About to call HALT from ' + ClassName + '.SetConnected');
            Halt ;
          end ;
      end
    else
      raise ;
  end ;
end ;

procedure TtiDatabaseIBO.StartTransaction ;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create( 'Attempt to start a transaction but transaction already exists.');
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction ;
end ;

function TtiDatabaseIBO.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

initialization

  gTIOPFManager.PersistenceLayers.RegisterPersistenceLayer (
    cTIPersistIBO,
    TtiDBConnectionPoolDataAbs,
    TtiQueryIBO,
    TtiDatabaseIBO ) ;

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.UnRegisterPersistenceLayer ( cTIPersistIBO ) ;

end.


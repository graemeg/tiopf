{$I tiDefines.inc}

{$DEFINE BOOLEAN_CHAR_1}

unit tiQueryIBO;

interface
uses
   tiQuery
  ,Classes
  ,IB_Components
  ,tiDBConnectionPool
  ,IB_Header
  ,tiAutoMap
  ,tiObject
  ,tiPersistenceLayers
 ;

type

  TtiPersistenceLayerIBO = class(TtiPersistenceLayer)
  protected
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetPersistenceLayerName: string; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseIBO = class (TtiDatabaseSQL)
  private
    FDatabase: TIB_Connection;
    FIBTransaction: TIB_Transaction;
    property IBDatabase: TIB_Connection read FDatabase write FDatabase;
  protected
    function  FieldMetaDataToSQLCreate (const AFieldMetaData: TtiDBMetaDataField): string; override;
    function  GetConnected: Boolean; override;
    procedure SetConnected (AValue: Boolean); override;
  public
    constructor     create; override;
    destructor      Destroy; override;
    procedure       Commit; override;
    class procedure CreateDatabase (const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class function  DatabaseExists (const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): Boolean; override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    function        InTransaction: Boolean; override;
    function        ReadGeneratorOID (AGeneratorName: string; AIncrement: integer = 1) :Integer;
    procedure       ReadMetaDataFields (AData: TtiDBMetaData); override;
    procedure       ReadMetaDataTables (AData: TtiDBMetaDataTable); override;
    procedure       RollBack; override;
    procedure       StartTransaction; override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryIBO = class (TtiQuerySQL)
  private
    FbActive: Boolean;
    FIBSQL: TIB_Cursor;
    procedure CheckPrepared;
    function IBFieldKindToTIFieldKind (PSQLVAR: PXSQLVAR): TtiQueryFieldKind;
  protected
    function GetActive: Boolean; override;
    function GetEOF: Boolean; override;
    function GetFieldAsBoolean (const AName: string): Boolean; override;
    function GetFieldAsDateTime (const AName: string): TDateTime; override;
    function GetFieldAsFloat (const AName: string): extended; override;
    function GetFieldAsInteger (const AName: string): Int64; override;
    function GetFieldAsString (const AName: string): string; override;
    function GetFieldAsVariant (const AName: string): Variant; override;
    function GetFieldIsNull (const AName: string): Boolean; override;
    function GetParamAsBoolean (const AName: string): Boolean; override;
    function GetParamAsDateTime (const AName: string): TDateTime; override;
    function GetParamAsFloat (const AName: string): extended; override;
    function GetParamAsInteger (const AName: string): Int64; override;
    function GetParamAsString (const AName: string): string; override;
    function GetParamAsTextBLOB (const AName: string): string; override;
    function GetParamAsVariant (const AName: string): Variant; override;
    function GetParamIsNull (const AName: string): Boolean; override;
    function GetSQL: TStrings; override;
    procedure SetActive (const AValue: Boolean); override;
    procedure SetParamAsBoolean (const AName: string; const AValue: Boolean);
      override;
    procedure SetParamAsDateTime (const AName: string; const AValue:
      TDateTime); override;
    procedure SetParamAsFloat (const AName: string; const AValue: extended);
      override;
    procedure SetParamAsInteger (const AName: string; const AValue: Int64);
      override;
    procedure SetParamAsString (const AName: string; const AValue: string);
      override;
    procedure SetParamAsTextBLOB (const AName: string; const AValue: string);
      override;
    procedure SetParamAsVariant (const AName: string; const AValue: Variant);
      override;
    procedure SetParamIsNull (const AName: string; const AValue: Boolean);
      override;
    procedure SetSQL (const AValue: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignFieldAsStream (const AName: string; const AValue: TStream
     ); override;
    procedure AssignParamFromStream (const AName: string; const AValue:
      TStream); override;
    procedure AssignParams (AParams: TtiQueryParams; AWhere: TtiQueryParams =
      nil); override;
    procedure AssignParamToStream (const AName: string; const AValue: TStream
     ); override;
    procedure AttachDatabase (ADatabase: TtiDatabase); override;
    procedure Close; override;
    procedure DetachDatabase; override;
    function ErrorMessage: string; override;
    procedure ExecSQL; override;
    function FieldCount: Integer; override;
    function FieldIndex (const AName: string): Integer; override;
    function FieldKind (AIndex: integer): TtiQueryFieldKind; override;
    function FieldName (AIndex: integer): string; override;
    function FieldSize (AIndex: integer): Integer; override;
    procedure Next; override;
    procedure Open; override;
    function ParamCount: Integer; override;
    function ParamName (AIndex: integer): string; override;
    procedure Reset; override;
  end;

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

constructor TtiQueryIBO.Create;
begin
  inherited;
  FIBSQL := TIB_Cursor.Create (nil);
end;

destructor TtiQueryIBO.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;

procedure TtiQueryIBO.AssignFieldAsStream (const AName: string; const AValue:
  TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  try
    st := FIBSQL.CreateBlobStream (FIBSQL.ParamByName (AName) , bsmRead);
    AValue.Write (st, st.Size);
  finally
    st.Free;
  end;
end;

procedure TtiQueryIBO.AssignParamFromStream (const AName: string; const AValue
 : TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  st := FIBSQL.CreateBlobStream (FIBSQL.ParamByName (AName) , bsmWrite);
  try
    st.CopyFrom (AValue, AValue.Size);
  finally
    st.Free;
  end;
  //  FIBSQL.ParamByName(AName).ReadFromStream(AStream);
end;

procedure TtiQueryIBO.AssignParams (AParams: TtiQueryParams; AWhere:
  TtiQueryParams = nil);
var
  i: Integer;
begin
    // Only for IB5.5 support...
  //  if FIBSQL.Database.SQLDialect <> 1 then
  //    inherited
  //  else
  //  begin
  CheckPrepared;
  if AParams = nil then
    Exit;
  for i := 0 to AParams.Count - 1 do
    ParamAsVariant [ AParams.Items [ i ] .Name ]:= AParams.Items [ i ] .AValue;
  if AWhere <> nil then
    for i := 0 to AWhere.Count - 1 do
      ParamAsVariant [ AWhere.Items [ i ] .Name ]:= AWhere.Items [ i ] .AValue;
  //  end;
end;

procedure TtiQueryIBO.AssignParamToStream (const AName: string; const AValue:
  TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  st := FIBSQL.CreateBlobStream (FIBSQL.ParamByName (AName) , bsmRead);
  try
    AValue.CopyFrom (st, st.Size);
  finally
    st.Free;
  end;
  AValue.Position := 0;
end;

procedure TtiQueryIBO.AttachDatabase (ADatabase: TtiDatabase);
begin
  inherited AttachDatabase (ADatabase);
  FIBSQL.IB_Connection := TtiDatabaseIBO (ADatabase) .FDatabase;
end;

procedure TtiQueryIBO.CheckPrepared;
begin
  if not FIBSQL.Prepared then FIBSQL.Prepare;
end;

procedure TtiQueryIBO.Close;
begin
  Active := false;
end;

procedure TtiQueryIBO.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Active then
    FIBSQL.Close;
  FIBSQL.IB_Transaction := nil;
  FIBSQL.IB_Connection := nil;
end;

function TtiQueryIBO.ErrorMessage: string;
begin
  Result := (inherited ErrorMessage) + Cr (2) +
    'SQL:' + #13 + FIBSQL.SQLText + Cr;
end;

procedure TtiQueryIBO.ExecSQL;
var
  lErrorMessage : string;
begin
  try
    FIBSQL.ExecSQL;
  except
    on e:exception do
    begin
      lErrorMessage :=
        'SQL: ' + FQuery.SQLText + Cr(2) +
        'Params: ' + FParams.AsString;
      raise EtiOPFInternalException.Create(lErrorMessage + Cr(2) + e.Message);
    end;
  end;
end;

function TtiQueryIBO.FieldCount: Integer;
begin
  if FIBSQL.EOF then
    Result := 0
  else
    Result := FIBSQL.FieldCount;
end;

function TtiQueryIBO.FieldIndex (const AName: string): Integer;
begin
  CheckPrepared;
  Result := FIBSQL.FieldByName (AName) .Index;
end;

function TtiQueryIBO.FieldKind (AIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  Result := IBFieldKindToTIFieldKind (FIBSQL.Fields [ AIndex ] .PSQLVAR);
  if (Result = qfkString) then
    begin
      lValue := FIBSQL.Fields [ AIndex ] .AsString;
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

      if SameText (lValue, 'TRUE') or
        SameText (lValue, 'TRUE ') or
        SameText (lValue, 'T') or
        SameText (lValue, 'F') or
        SameText (lValue, 'FALSE') then
        Result := qfkLogical;
    end;
end;

function TtiQueryIBO.FieldName (AIndex: integer): string;
begin
  Result := FIBSQL.Fields.Columns [ AIndex ] .BDEFieldName;
end;

function TtiQueryIBO.FieldSize (AIndex: integer): Integer;
begin
  CheckPrepared;
  Result := FIBSQL.Fields [ AIndex ] .PSQLVAR.sqllen;
end;

function TtiQueryIBO.GetActive: Boolean;
begin
  Result := FbActive;
end;

function TtiQueryIBO.GetEOF: Boolean;
begin
  Result := FIBSQL.EOF;
end;

function TtiQueryIBO.GetFieldAsBoolean (const AName: string): Boolean;
var
  lsValue: string;
begin
  lsValue := Trim (upperCase (FIBSQL.FieldByName (AName) .AsString));
  Result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQueryIBO.GetFieldAsDateTime (const AName: string): TDateTime;
begin
  Result := FIBSQL.FieldByName (AName) .AsDateTime;
end;

function TtiQueryIBO.GetFieldAsFloat (const AName: string): extended;
begin
  Result := FIBSQL.FieldByName (AName) .AsFloat;
end;

function TtiQueryIBO.GetFieldAsInteger (const AName: string): Int64;
begin
  Result := FIBSQL.FieldByName (AName) .AsInteger;
end;

function TtiQueryIBO.GetFieldAsString (const AName: string): string;
begin
  Result := FIBSQL.FieldByName (AName) .AsString;
end;

function TtiQueryIBO.GetFieldAsVariant (const AName: string): Variant;
var
  lFieldKind: TtiQueryFieldKind;
begin
  lFieldKind := FieldKind (FieldIndex (AName));
    // ToDo: This fixes the problem of AsString returning '(Blob)' for memos, but may
    //       break any attempt to read true binary data.
  case lFieldKind of
    qfkBinary: result := FIBSQL.FieldByName (UpperCase (AName)) .AsString
  else
    Result := FIBSQL.FieldByName (UpperCase (AName)) .AValue;
  end;
end;

function TtiQueryIBO.GetFieldIsNull (const AName: string): Boolean;
begin
  Result := FIBSQL.FieldByName (AName) .IsNull;
end;

function TtiQueryIBO.GetParamAsBoolean (const AName: string): Boolean;
var
  lValue: string;
begin
  lValue := FIBSQL.ParamByName (AName) .AsString;
{$IFDEF BOOLEAN_CHAR_1}
  Result := SameText (lValue, 'T');
{$ELSE}
  Result := SameText (lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
end;

function TtiQueryIBO.GetParamAsDateTime (const AName: string): TDateTime;
begin
  Result := FIBSQL.ParamByName (AName) .AsDateTime;
end;

function TtiQueryIBO.GetParamAsFloat (const AName: string): extended;
begin
  Result := FIBSQL.ParamByName (AName) .AsFloat;
end;

function TtiQueryIBO.GetParamAsInteger (const AName: string): Int64;
begin
  Result := FIBSQL.ParamByName (AName) .AsInteger;
end;

function TtiQueryIBO.GetParamAsString (const AName: string): string;
begin
  Result := FIBSQL.ParamByName (AName) .AsString;
end;

function TtiQueryIBO.GetParamAsTextBLOB (const AName: string): string;
begin
  Result := FIBSQL.ParamByName (AName) .AsString;
end;

function TtiQueryIBO.GetParamAsVariant (const AName: string): Variant;
begin
  Result := FIBSQL.ParamByName (AName) .AsVariant;
end;

function TtiQueryIBO.GetParamIsNull (const AName: string): Boolean;
begin
  Result := FIBSQL.ParamByName (AName) .IsNull;
end;

function TtiQueryIBO.GetSQL: TStrings;
begin
  Result := FIBSQL.SQL;
end;

function TtiQueryIBO.IBFieldKindToTIFieldKind (PSQLVAR: PXSQLVAR):
  TtiQueryFieldKind;
begin
    {:@ver Carlos - 12/06/2003  tipos nulos:  _}
  case PSQLVAR.sqltype of
    SQL_TEXT, SQL_TEXT_: result := qfkString;
    SQL_VARYING, SQL_VARYING_: result := qfkString;
    SQL_LONG, SQL_LONG_, SQL_SHORT, SQL_SHORT_, SQL_INT64, SQL_INT64_, SQL_QUAD, SQL_QUAD_: result := qfkInteger;
    SQL_DOUBLE, SQL_DOUBLE_, SQL_FLOAT, SQL_FLOAT_, SQL_D_FLOAT, SQL_D_FLOAT_: result := qfkFloat;
    SQL_BLOB, SQL_BLOB_: result := qfkBinary;
    SQL_TIMESTAMP, SQL_TIMESTAMP_, SQL_TYPE_TIME, SQL_TYPE_TIME_, SQL_TYPE_DATE, SQL_TYPE_DATE_: result := qfkDateTime;
      //SQL_ARRAY
      // What about these?
      //result := qfkLongString;
  else
    Result := qfkString; // Just to shup up the compiler
    raise EtiOPFInternalException.Create ('Invalid Interbase DataType <' +
      IntToStr (PSQLVAR.sqltype) + '>');
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
end;

procedure TtiQueryIBO.Next;
begin
  FIBSQL.ApiNext;
end;

procedure TtiQueryIBO.Open;
begin
  try
    FIBSQL.BeginBusy (false);
  except

  end;
  Active := true;
  FIBSQL.APIFirst; // importante
end;

function TtiQueryIBO.ParamCount: Integer;
begin
  CheckPrepared;
  Result := FIBSQL.Params.ColumnCount;
end;

function TtiQueryIBO.ParamName (AIndex: integer): string;
begin
  CheckPrepared;
  Result := FIBSQL.Params.BySQLNo (AIndex) .FieldName;
end;

procedure TtiQueryIBO.Reset;
begin
  Active := false;
  FIBSQL.SQL.Clear;
end;

procedure TtiQueryIBO.SetActive (const AValue: Boolean);
var
  lErrorMessage : string;
begin
  if AValue then
  begin
    try
      FIBSQL.ExecSQL;
      FbActive := true;
    except
      on e:exception do
      begin
        lErrorMessage :=
          'SQL: ' + FQuery.SQLText + Cr(2) +
          'Params: ' + FParams.AsString;
        raise EtiOPFInternalException.Create(lErrorMessage + Cr(2) + 'Message: ' + e.message);
      end;
    end;
  end else
  begin
    FIBSQL.Close;
    try
      FIBSQL.EndBusy;
    except
    end;
    FbActive := false;
  end;
end;

procedure TtiQueryIBO.SetParamAsBoolean (const AName: string; const AValue:
  Boolean);
begin
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FIBSQL.ParamByName (AName) .AsString := 'T'
  else
    FIBSQL.ParamByName (AName) .AsString := 'F';
{$ELSE}
  if AValue then
    FIBSQL.ParamByName (AName) .AsString := 'TRUE'
  else
    FIBSQL.ParamByName (AName) .AsString := 'FALSE';
{$ENDIF} // BOOLEAN_CHAR_1
end;

procedure TtiQueryIBO.SetParamAsDateTime (const AName: string; const AValue:
  TDateTime);
begin
  FIBSQL.ParamByName (AName) .AsDateTime := AValue;
end;

procedure TtiQueryIBO.SetParamAsFloat (const AName: string; const AValue:
  extended);
begin
  FIBSQL.ParamByName (AName) .AsDouble := AValue;
end;

procedure TtiQueryIBO.SetParamAsInteger (const AName: string; const AValue:
  Int64);
begin
  //  Log('Param as integer : '+psNAme,lsUserInfo);
  //  if High(Integer) < AValue then
  //  begin
  //    Log('Int64',lsUserInfo);
  //    FIBSQL.ParamByName(UpperCase(AName)).AsInt64 := AValue;
  //  end
  //  else
  //  begin
  //    Log('Integer',lsUserInfo);
  CheckPrepared;
  FIBSQL.ParamByName (AName) .AsInteger := Integer (AValue);
  //  end;
end;

procedure TtiQueryIBO.SetParamAsString (const AName: string; const AValue:
  string);
begin
  CheckPrepared;
  FIBSQL.ParamByName (AName) .AsString := AValue;
end;

procedure TtiQueryIBO.SetParamAsTextBLOB (const AName: string; const AValue:
  string);
begin
  CheckPrepared;
  FIBSQL.ParamByName (AName) .AsString := AValue;
end;

procedure TtiQueryIBO.SetParamAsVariant (const AName: string; const AValue:
  Variant);
var
  lFieldKind: TtiQueryFieldKind;
  lValue: Variant;
  lsValue: string;
begin
  lValue := AValue;
    lFieldKind := IBFieldKindToTIFieldKind (FIBSQL.ParamByName (AName) .PSQLVAR);
    case lFieldKind of
      qfkDateTime: FIBSQL.ParamByName (AName) .AsDateTime := lValue;
      qfkFloat: FIBSQL.ParamByName (AName) .AsDouble := lValue;
      qfkInteger: FIBSQL.ParamByName (AName) .AsInteger := lValue;
      qfkString:
        begin
          lsValue := VarToStr (lValue);
              // VarToStr on a boolean will return an integer so we must
              // check for booleans and convert to a string.
          if tiIsVariantOfType (AValue, varBoolean) then
            begin
{$IFDEF BOOLEAN_CHAR_1}
              if AValue then
                SetParamAsString (AName, 'T')
              else
                SetParamAsString (AName, 'F');
{$ELSE}
              if AValue then
                SetParamAsString (AName, 'TRUE')
              else
                SetParamAsString (AName, 'FALSE');
{$ENDIF} // BOOLEAN_CHAR_1
            end
          else
            FIBSQL.ParamByName (AName) .AsString := lsValue;
        end;
    else
      FIBSQL.ParamByName (AName) .AsVariant := lValue;
    end                   gui/appstart.pp
  //    else
  //    begin
  //      case lFieldKind of
  //        qfkDateTime:
  //          begin
  //            lDateTime := FormatDateTime('yyyy-mm-dd hh:mm:ss', AValue);
  //            FIBSQL.ParamByName(UpperCase(AName)).AValue := lDateTime;
  //          end;
  //        qfkFloat:
  //          FIBSQL.ParamByName(UpperCase(AName)).AValue := FloatToStR(AValue);
  //      else
  //        FIBSQL.ParamByName(UpperCase(AName)).AValue := AValue;
  //      end;
  //    end;
end;

procedure TtiQueryIBO.SetParamIsNull (const AName: string; const AValue:
  Boolean);
begin
  FIBSQL.ParamByName (AName) .IsNull := true;
end;

procedure TtiQueryIBO.SetSQL (const AValue: TStrings);
begin
  FIBSQL.SQL.Assign (AValue);
  FIBSQL.Prepare;
end;

{ Carlos  14/09/2003 checks if SQL is prepared, IBO uses dynamic fields and parameters }
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseIBO
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
******************************** TtiDatabaseIBO ********************************
}

constructor TtiDatabaseIBO.Create;
begin
  inherited Create;
  FDatabase := TIB_Connection.Create (nil);
  FDatabase.LoginPrompt := false;
  FDatabase.SQLDialect := 1; // IK added.
  FIBTransaction := TIB_Transaction.Create (nil);
  FIBTransaction.Isolation := tiCommitted;
  FDatabase.DefaultTransaction := FIBTransaction;
  FIBTransaction.IB_Connection := FDatabase;
end;

destructor TtiDatabaseIBO.Destroy;
begin
  try
  //   FIBTransaction.Active := false;
    FDatabase.Connected := false;
    FDatabase.DefaultTransaction := nil;
    FIBTransaction.IB_Connection := nil;
    FIBTransaction.Free;
    FDatabase.Free;
  except
    on e: exception do
      LogError (e.message);
  end;
  inherited;
end;

procedure TtiDatabaseIBO.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FIBTransaction.CommitRetaining;
end;

class procedure TtiDatabaseIBO.CreateDatabase (const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
var
  FDatabase: TIB_Connection;
begin
  if DatabaseExists (ADatabaseName, AUserName, APassword) then
    raise EtiDBAlreadyExists.Create;
  FDatabase := TIB_Connection.Create (nil);
  try
    try
      FDatabase.Database := ADatabaseName;
      FDatabase.Username := AUserName;
      FDatabase.Password := APassword;
      FDatabase.CreateDatabase;
    except
      on e: EIB_ISCERROR do
        begin
          tiAppError ('Error attempting to create database:' + tiLineEnd (2) +
            ADatabaseName + tiLineEnd (2) + e.Message + tiLineEnd (2) +
            'Error code: ' + IntToStr (EIB_ISCERROR (E) .ERRCODE));
        end;
    end;
  finally
    FDatabase.Free;
  end;
end;

class function TtiDatabaseIBO.DatabaseExists (const ADatabaseName, AUserName,
  APassword: string; const AParams: string): Boolean;
var
  FDatabase: TIB_Connection;
begin
  Result := FileExists (ADatabaseName);
  if not Result then
    Exit; //==>
  FDatabase := TIB_Connection.Create (nil);
  try
    try
      FDatabase.Database := ADatabaseName;
      FDatabase.Username := AUserName;
      FDatabase.Password := APassword;
      FDatabase.Connect;
      Result := true;
    except
      Result := false;
    end;
  finally
    FDatabase.Connected := false;
    FDatabase.Free;
  end;
end;

class procedure TtiDatabaseIBO.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

function TtiDatabaseIBO.FieldMetaDataToSQLCreate (const AFieldMetaData:
  TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString: result := 'VarChar(' + IntToStr (AFieldMetaData.Width) + ')';
    qfkInteger: result := 'Integer';
  //    qfkFloat: result := 'Decimal(10, 5)';
    qfkFloat: result := 'DOUBLE PRECISION';
      // Just for new version of IB (6.x)
      // DATE holds only DATE without TIME...
    qfkDateTime:
      if IBDatabase.SQLDialect <> 1 then
        result := 'TIMESTAMP'
      else
        result := 'Date';
{$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: result := 'Char(1) default ''F''';
{$ELSE}
    qfkLogical: result := 'VarChar(5) default ''FALSE''';
{$ENDIF} // BOOLEAN_CHAR_1
    qfkBinary: result := 'Blob sub_type 2';
    qfkLongString: result := 'Blob sub_type 1';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
    {
      GTIOPFManager.ExecSQL(
        'create table Test_Group ' +
        '  (OID                 Integer not null, ' +
        '    Group_Str_Field     VarChar(10), ' +
        '    Group_Int_Field     Integer, ' +
        '    Group_Float_Field   Decimal(10, 5), ' +
        '    Primary Key       (OID) ' +
        '   )');
    }

    {
      GTIOPFManager.ExecSQL(
        'create table Test_Item ' +
        '  (OID                Integer not null, ' +
        '    OID_Group          Integer, ' +
        '    Item_Str_Field     VarChar(10), ' +
        '    Item_Int_Field     Integer, ' +
        '    Item_Float_Field   Decimal(10, 5), ' +
        '    Item_Logical_Field VarChar(5) default ''FALSE'', ' +
        '    Item_Date_Field    Date, ' +
        '    Item_Notes_Field   blob sub_type 1, ' +
        '    Primary Key (OID) ' +
        '   ) ');

      GTIOPFManager.ExecSQL(
        'create table Test_Bin ' +
        '  (OID                Integer not null, ' +
        '    Item_Binary_Field  blob sub_type 2, ' +
        '    Primary Key (OID) ' +
        '   ) ');
    }

end;

function TtiDatabaseIBO.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TtiDatabaseIBO.InTransaction: Boolean;
begin
  result := FIBTransaction.InTransaction;
end;

function TtiDatabaseIBO.ReadGeneratorOID (AGeneratorName: string; AIncrement:
  integer = 1): Integer;
begin
  Result := IBDatabase.Gen_ID (AGeneratorName, AIncrement);
end;

procedure TtiDatabaseIBO.ReadMetaDataFields (AData: TtiDBMetaData);
var
  lTableName: string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType: integer;
  lFieldLength: integer;

const
  // Interbase field types
  //   select * from rdb$types
  //   where rdb$field_NAME = 'RDB$FIELD_TYPE'
  //   ORDER BY RDB$TYPE

  cIBField_LONG = 8;
  cIBField_DOUBLE = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_DATE = 12;
  cIBField_TIME = 13;
  cIBField_VARYING = 37;
  cIBField_BLOB = 261;

  cIBField_SHORT = 7;
  cIBField_QUAD = 9;
  cIBField_FLOAT = 10;
  cIBField_TEXT = 14;
  cIBField_CSTRING = 40;
  cIBField_BLOB_ID = 45;

begin
  lTable := (AData as TtiDBMetaDataTable);
  lTableName := lTable.Name;
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery;
  try
    StartTransaction;
    try
      lQuery.AttachDatabase (Self);
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
          lField.Name := Trim (lQuery.FieldAsString [ 'field_name' ]);
          lFieldType := lQuery.FieldAsInteger [ 'field_type' ];
          lFieldLength := lQuery.FieldAsInteger [ 'field_length' ];

          lField.Width := 0;

          case lFieldType of
            cIBField_SHORT,
            cIBField_LONG: lField.Kind := qfkInteger;
            cIBField_DOUBLE: lField.Kind := qfkFloat;
            cIBField_TIMESTAMP,
              cIBField_DATE,
              cIBField_TIME: lField.Kind := qfkDateTime;
            cIBField_VARYING,
              cIBField_TEXT: begin
                lField.Kind := qfkString;
                lField.Width := lFieldLength;
              end;
            cIBField_BLOB: begin
                Assert (not lQuery.FieldIsNull [ 'field_sub_type' ] , 'field_sub_type is null');
                if lQuery.FieldAsInteger [ 'field_sub_type' ] = 1 then
                  lField.Kind := qfkLongString
                else
                  raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr (lQuery.FieldAsInteger [ 'field_sub_type' ]) + '>');
              end;
          else
            raise EtiOPFInternalException.Create('Invalid Interbase FieldType <' + IntToStr (lFieldType) + '>');
          end;
          lField.ObjectState := posClean;
          lTable.Add (lField);
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

procedure TtiDatabaseIBO.ReadMetaDataTables (AData: TtiDBMetaDataTable);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery;
  try
    StartTransaction;
    try
      lQuery.AttachDatabase (Self);
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
          lTable.Name := Trim (lQuery.FieldAsString [ 'table_name' ]);
          lTable.ObjectState := posPK;
          lMetaData.Add (lTable);
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

procedure TtiDatabaseIBO.RollBack;
begin
  FIBTransaction.RollbackRetaining;
end;

procedure TtiDatabaseIBO.SetConnected (AValue: Boolean);
var
  lMessage : string;
begin

  try
    if (not AValue) then
      begin
        FDatabase.Connected := false;
        Exit; //==>
      end;
    FDatabase.Params.Clear;
    FDatabase.DatabaseName := Self.DatabaseName;
    FDatabase.Username := UserName;
    FDatabase.Password := Password;
    FDatabase.Params.Add ('user_name=' + UserName);
    FDatabase.Params.Add ('password=' + Password);
    FDatabase.Connected := true;

  except
    // ToDo: Must come up with a better solution that this:
    //       Try several times before raising an exception.
    //       Rather than calling 'Halt', just terminate this database connection,
    //       unless this is the first connection.
    on e: EIB_ISCERROR do
      begin
      // Invalid username / password error
        if (EIB_ISCERROR (E) .ERRCODE = 335544472) then
          raise
        else
          begin
            lMessage := 'Error attempting to connect to database.' + tiLineEnd (2) +
              e.Message + tiLineEnd (2) +
              'Error code: ' + IntToStr (EIB_ISCERROR (E) .ERRCODE);
            tiAppError (lMessage);
            LogError(lMessage);
            LogError('About to call HALT from ' + ClassName + '.SetConnected');
            Halt;
          end;
      end
    else
      raise;
  end;
end;

procedure TtiDatabaseIBO.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  if not FIBTransaction.InTransaction then
    FIBTransaction.StartTransaction;
end;

function TtiDatabaseIBO.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseIBO.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryIBO;
end;

{ TtiPersistenceLayerIBO }

procedure TtiPersistenceLayerIBO.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistIBO;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'SYSDBA';
  APersistenceLayerDefaults.Password:= 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerIBO.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseIBO;
end;

function TtiPersistenceLayerIBO.GetPersistenceLayerName: string;
begin
  result:= cTIPersistIBO;
end;

function TtiPersistenceLayerIBO.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryIBO;
end;

initialization

  GTIOPFManager.PersistenceLayers.RegisterPersistenceLayer(
    TtiPersistenceLayerIBO);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.UnRegisterPersistenceLayer (cTIPersistIBO);

end.


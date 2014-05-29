{$I tiDefines.inc}

{$DEFINE BOOLEAN_CHAR_1}

unit tiQueryIBO;

interface
uses
   tiPersistenceLayers
  ,tiQuery
  ,Classes
  ,IB_Components
  ,tiDBConnectionPool
  ,IB_Header
  ,tiAutoMap
  ,tiObject
 ;

type

  TtiPersistenceLayerIBO = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
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
    procedure SetConnected (AValue: Boolean); override;
    function  GetConnected: Boolean; override;
    function  FieldMetaDataToSQLCreate (const AFieldMetaData: TtiDBMetaDataField): string; override;
  public
    constructor     Create; override;
    destructor      Destroy; override;
    class function  DatabaseExists (const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): Boolean; override;
    class procedure CreateDatabase (const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    function        ReadGeneratorOID (AGeneratorName: string; AIncrement: integer = 1) :Integer;
    procedure       StartTransaction; override;
    function        InTransaction: Boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    procedure       ReadMetaDataTables (AData: TtiDBMetaData); override;
    procedure       ReadMetaDataFields (AData: TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryIBO = class (TtiQuerySQL)
  private
    FIBSQL: TIB_Cursor;
    FbActive: Boolean;
    function IBFieldKindToTIFieldKind (PSQLVAR: PXSQLVAR): TtiQueryFieldKind;
  protected
    function  GetFieldAsBoolean(const AName: string): boolean; override;
    function  GetFieldAsDateTime(const AName: string): TDateTime; override;
    function  GetFieldAsFloat(const AName: string): extended; override;
    function  GetFieldAsInteger(const AName: string): Int64; override;
    function  GetFieldAsString(const AName: string): string; override;
//    function  GetFieldAsVariant (const AName: string): Variant; // override;
    function  GetFieldIsNull(const AName: string): Boolean; override;

    procedure Prepare; override;
    function  GetActive: boolean; override;
    function  GetEOF: boolean; override;
    function  GetRecordCount: Integer; override;
    function  GetRowsAffected: Integer; override;
    function  GetSQL: TStrings; override;
    procedure SetActive(const AValue: boolean); override;
    procedure SetSQL(const AValue: TStrings); override;

    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    function  GetParamAsFloat(const AName: string): extended; override;
    function  GetParamAsInteger(const AName: string): Int64; override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsTextBLOB (const AName: string): string; override;
    function  GetParamIsNull(const AName: string): Boolean; override;

    procedure SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: Int64); override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsTextBLOB (const AName: string; const AValue: string); override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;

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
 
//    procedure   AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;
    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;
 
    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;
//    function    ErrorMessage: string; override;
  end;

implementation
uses
   tiUtils
  ,tiDialogs
  ,tiLog
  ,TypInfo
  ,SysUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,IB_Session
{$IFNDEF VER130}
  ,Variants
{$ENDIF}
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryIBO
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiQueryIBO.Create;
begin
  inherited;
  FIBSQL := TIB_Cursor.Create (nil);
  FSupportsRowsAffected := True;
end;

destructor TtiQueryIBO.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;

procedure TtiQueryIBO.Close;
begin
  Active := false;
end;

function TtiQueryIBO.ExecSQL: integer;
//var
//  lErrorMessage : string;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Prepare;
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
//  try
    FIBSQL.ExecSQL;
    Result := FIBSQL.RowsAffected;
//  except
//    on e:exception do
//    begin
//      lErrorMessage :=
//        'SQL: ' + FQuery.SQLText + Cr(2) +
//        'Params: ' + FParams.AsString;
//      raise EtiOPFInternalException.Create(lErrorMessage + Cr(2) + e.Message);
//    end;
//  end;
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

//++IPK removed 2012-05-16
//function TtiQueryIBO.GetFieldAsVariant (const AName: string): Variant;
//var
//  lFieldKind: TtiQueryFieldKind;
//begin
//  lFieldKind := FieldKind (FieldIndex (AName));
//    // ToDo: This fixes the problem of AsString returning '(Blob)' for memos, but may
//    //       break any attempt to read true binary data.
//  case lFieldKind of
//    qfkBinary: result := FIBSQL.FieldByName (UpperCase (AName)) .AsString
//  else
//    Result := FIBSQL.FieldByName (UpperCase (AName)) .AValue;
//  end;
//end;


function TtiQueryIBO.GetActive: Boolean;
begin
  Result := FbActive;
end;

function TtiQueryIBO.GetRecordCount: Integer;
begin
  Result := FIBSQL.RecordCount;
end;

function TtiQueryIBO.GetRowsAffected: Integer;
begin
  Result := FIBSQL.RowsAffected;
end;

function TtiQueryIBO.GetEOF: Boolean;
begin
  Result := FIBSQL.EOF;
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

function TtiQueryIBO.GetSQL: TStrings;
begin
  Result := FIBSQL.SQL;
end;

procedure TtiQueryIBO.Next;
begin
  FIBSQL.ApiNext;
end;

procedure TtiQueryIBO.Open;
begin
  Log(ClassName + ': ' + tiNormalizeStr(self.SQLText), lsSQL);
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  try
    FIBSQL.BeginBusy (false);
  except

  end;
  Active := true;
  FIBSQL.APIFirst; // importante
end;

function TtiQueryIBO.ParamCount: Integer;
begin
  Prepare;
  Result := FIBSQL.Params.ColumnCount;
end;

function TtiQueryIBO.ParamName (AIndex: integer): string;
begin
  Prepare;
  Result := FIBSQL.Params.BySQLNo (AIndex) .FieldName;
end;

procedure TtiQueryIBO.SetActive (const AValue: Boolean);
//var
//  lErrorMessage : string;
begin
  Assert(Database.TestValid(TtiDatabase), CTIErrorInvalidObject);
  if AValue then
  begin
//    try
      FIBSQL.ExecSQL;
      FbActive := true;
//    except
//      on e:exception do
//      begin
//        lErrorMessage :=
//          'SQL: ' + FQuery.SQLText + Cr(2) +
//          'Params: ' + FParams.AsString;
//        raise EtiOPFInternalException.Create(lErrorMessage + Cr(2) + 'Message: ' + e.message);
//      end;
//    end;
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

procedure TtiQueryIBO.SetParamAsBoolean (const AName: string; const AValue: boolean);
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

procedure TtiQueryIBO.SetParamAsDateTime (const AName: string; const AValue: TDateTime);
begin
  FIBSQL.ParamByName (AName) .AsDateTime := AValue;
end;

procedure TtiQueryIBO.SetParamAsFloat (const AName: string; const AValue: extended);
begin
  FIBSQL.ParamByName (AName) .AsDouble := AValue;
end;

procedure TtiQueryIBO.SetParamAsInteger (const AName: string; const AValue: Int64);
begin
//  Prepare;
  //  Log('Param as integer : '+psNAme,lsUserInfo);
  //  if High(Integer) < AValue then
  //  begin
  //    Log('Int64',lsUserInfo);
  //    FIBSQL.ParamByName(UpperCase(AName)).AsInt64 := AValue;
  //  end
  //  else
  //  begin
  //    Log('Integer',lsUserInfo);
  FIBSQL.ParamByName (AName) .AsInteger := Integer (AValue);
  //  end;
end;

procedure TtiQueryIBO.SetParamAsString (const AName: string; const AValue: string);
begin
//  Prepare;
  FIBSQL.ParamByName (AName) .AsString := AValue;
end;

procedure TtiQueryIBO.SetSQL (const AValue: TStrings);
begin
  FIBSQL.SQL.Assign (AValue);
  FIBSQL.Prepare;
end;

procedure TtiQueryIBO.Prepare;
begin
  if not FIBSQL.Prepared then
    FIBSQL.Prepare;
end;

procedure TtiQueryIBO.AssignParamFromStream (const AName: string; const AStream : TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  st := FIBSQL.CreateBlobStream (FIBSQL.ParamByName (AName) , bsmWrite);
  try
    st.CopyFrom (AStream, AStream.Size);
  finally
    st.Free;
  end;
  //  FIBSQL.ParamByName(AName).ReadFromStream(AStream);
end;

procedure TtiQueryIBO.AssignParamToStream (const AName: string; const AStream: TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  st := FIBSQL.CreateBlobStream (FIBSQL.ParamByName (AName) , bsmRead);
  try
    AStream.CopyFrom (st, st.Size);
  finally
    st.Free;
  end;
  AStream.Position := 0;
end;

procedure TtiQueryIBO.AssignFieldAsStream (const AName: string; const AStream: TStream);
var
  st: TIB_BlobStream;
begin
  Assert (AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  try
    st := FIBSQL.CreateBlobStream (FIBSQL.FieldByName (AName) , bsmRead);
    AStream.Write (st, st.Size);
  finally
    st.Free;
  end;
end;

procedure TtiQueryIBO.SetParamAsTextBLOB (const AName: string; const AValue: string);
begin
//  Prepare;
  FIBSQL.ParamByName (AName) .AsString := AValue;
end;

//    procedure TtiQueryIBO.SetParamAsVariant (const AName: string; const AValue:
//      Variant);
//    var
//      lFieldKind: TtiQueryFieldKind;
//      lValue: Variant;
//      lsValue: string;
//    begin
//      lValue := AValue;
//        lFieldKind := IBFieldKindToTIFieldKind (FIBSQL.ParamByName (AName) .PSQLVAR);
//        case lFieldKind of
//          qfkDateTime: FIBSQL.ParamByName (AName) .AsDateTime := lValue;
//          qfkFloat: FIBSQL.ParamByName (AName) .AsDouble := lValue;
//          qfkInteger: FIBSQL.ParamByName (AName) .AsInteger := lValue;
//          qfkString:
//            begin
//              lsValue := VarToStr (lValue);
//                  // VarToStr on a boolean will return an integer so we must
//                  // check for booleans and convert to a string.
//              if tiIsVariantOfType (AValue, varBoolean) then
//                begin
//    {$IFDEF BOOLEAN_CHAR_1}
//                  if AValue then
//                    SetParamAsString (AName, 'T')
//                  else
//                    SetParamAsString (AName, 'F');
//    {$ELSE}
//                  if AValue then
//                    SetParamAsString (AName, 'TRUE')
//                  else
//                    SetParamAsString (AName, 'FALSE');
//    {$ENDIF} // BOOLEAN_CHAR_1
//                end
//              else
//                FIBSQL.ParamByName (AName) .AsString := lsValue;
//            end;
//        else
//          FIBSQL.ParamByName (AName) .AsVariant := lValue;
//        end                   gui/appstart.pp
//      //    else
//      //    begin
//      //      case lFieldKind of
//      //        qfkDateTime:
//      //          begin
//      //            lDateTime := FormatDateTime('yyyy-mm-dd hh:mm:ss', AValue);
//      //            FIBSQL.ParamByName(UpperCase(AName)).AValue := lDateTime;
//      //          end;
//      //        qfkFloat:
//      //          FIBSQL.ParamByName(UpperCase(AName)).AValue := FloatToStR(AValue);
//      //      else
//      //        FIBSQL.ParamByName(UpperCase(AName)).AValue := AValue;
//      //      end;
//      //    end;
//    end;

function TtiQueryIBO.GetParamAsTextBLOB (const AName: string): string;
begin
  Result := FIBSQL.ParamByName (AName) .AsString;
end;

//    function TtiQueryIBO.GetParamAsVariant (const AName: string): Variant;
//    begin
//      Result := FIBSQL.ParamByName (AName) .AsVariant;
//    end;

procedure TtiQueryIBO.AttachDatabase (ADatabase: TtiDatabase);
begin
  inherited AttachDatabase (ADatabase);
  FIBSQL.IB_Connection := TtiDatabaseIBO (ADatabase) .FDatabase;
end;

procedure TtiQueryIBO.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Active then
    FIBSQL.Close;
  FIBSQL.IB_Transaction := nil;
  FIBSQL.IB_Connection := nil;
end;

//    function TtiQueryIBO.ErrorMessage: string;
//    begin
//      Result := (inherited ErrorMessage) + Cr (2) +
//        'SQL:' + #13 + FIBSQL.SQLText + Cr;
//    end;

function TtiQueryIBO.FieldCount: Integer;
begin
  if FIBSQL.EOF then
    result := 0
  else
    result := FIBSQL.FieldCount;
end;

function TtiQueryIBO.FieldName (AIndex: integer): string;
begin
  result := FIBSQL.Fields.Columns [ AIndex ] .BDEFieldName;
end;

procedure TtiQueryIBO.Reset;
begin
  Active := false;
  FIBSQL.SQL.Clear;
end;

function TtiQueryIBO.FieldIndex (const AName: string): Integer;
begin
  Prepare;
  result := FIBSQL.FieldByName (AName) .Index;
end;

function TtiQueryIBO.FieldKind (AIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  Result := IBFieldKindToTIFieldKind (FIBSQL.Fields [ AIndex ] .PSQLVAR);
  if (result = qfkString) then
  begin
    lValue := FIBSQL.Fields[AIndex].AsString;
    // ToDo: How to detect a logical field in a SQL database
    //       where logicals are represented as VarChar or Char?
    //       In the IBX layer we are currently looking for a value of
    //       'TRUE' or 'FALSE', but this is not reliable as it will not
    //       detect a null. Also, other ways of representing booleans
    //       might be used like 'T', 'F', '0', '1', etc...
    if SameText (lValue, 'TRUE') or
      SameText (lValue, 'TRUE ') or
      SameText (lValue, 'T') or
      SameText (lValue, 'F') or
      SameText (lValue, 'FALSE') then
      result := qfkLogical;
  end;
end;


function TtiQueryIBO.IBFieldKindToTIFieldKind (PSQLVAR: PXSQLVAR): TtiQueryFieldKind;
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

function TtiQueryIBO.FieldSize (AIndex: integer): Integer;
begin
  Prepare;
  Result := FIBSQL.Fields [ AIndex ] .PSQLVAR.sqllen;
end;

function TtiQueryIBO.GetParamIsNull (const AName: string): Boolean;
begin
  Result := FIBSQL.ParamByName (AName) .IsNull;
end;

procedure TtiQueryIBO.SetParamIsNull (const AName: string; const AValue: Boolean);
begin
  FIBSQL.ParamByName (AName) .IsNull := true;
end;

function TtiQueryIBO.GetFieldIsNull (const AName: string): Boolean;
begin
  Result := FIBSQL.FieldByName (AName) .IsNull;
end;

//    procedure TtiQueryIBO.AssignParams (AParams: TtiQueryParams; AWhere: TtiQueryParams = nil);
//    var
//      i: Integer;
//    begin
//        // Only for IB5.5 support...
//      //  if FIBSQL.Database.SQLDialect <> 1 then
//      //    inherited
//      //  else
//      //  begin
//      Prepare;
//      if AParams = nil then
//        Exit;
//      for i := 0 to AParams.Count - 1 do
//        ParamAsVariant [ AParams.Items [ i ] .Name ]:= AParams.Items [ i ] .AValue;
//      if AWhere <> nil then
//        for i := 0 to AWhere.Count - 1 do
//          ParamAsVariant [ AWhere.Items [ i ] .Name ]:= AWhere.Items [ i ] .AValue;
//    end;

function TtiQueryIBO.HasNativeLogicalType: boolean;
begin
  result := false;
end;

{ Carlos  14/09/2003 checks if SQL is prepared, IBO uses dynamic fields and parameters }
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseIBO
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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
  Log(ClassName + ': [Commit Trans]', lsSQL);
  FIBTransaction.CommitRetaining;
end;

function TtiDatabaseIBO.InTransaction: Boolean;
begin
  result := FIBTransaction.InTransaction;
end;

procedure TtiDatabaseIBO.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  FIBTransaction.RollbackRetaining;
end;

procedure TtiDatabaseIBO.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
//  if not FIBTransaction.InTransaction then //++ IPK 2012-05-07 Commented out (already tested)
  Log(ClassName + ': [Start Trans]', lsSQL);
  FIBTransaction.StartTransaction;
end;

function TtiDatabaseIBO.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;


procedure TtiDatabaseIBO.SetConnected (AValue: boolean);
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

procedure TtiDatabaseIBO.ReadMetaDataTables (AData: TtiDBMetaData);
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

procedure TtiDatabaseIBO.ReadMetaDataFields (AData: TtiDBMetaDataTable);
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

function TtiDatabaseIBO.FieldMetaDataToSQLCreate (const AFieldMetaData: TtiDBMetaDataField): string;
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

 
class procedure TtiDatabaseIBO.CreateDatabase (const ADatabaseName, AUserName, APassword: string; const AParams: string);
var
  FDatabase: TIB_Connection;
begin
  if DatabaseExists (ADatabaseName, AUserName, APassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistIBO, ADatabaseName, AUserName, APassword);
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

class procedure TtiDatabaseIBO.DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseIBO.DatabaseExists (const ADatabaseName, AUserName, APassword: string; const AParams: string): Boolean;
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


function TtiDatabaseIBO.ReadGeneratorOID (AGeneratorName: string; AIncrement:
  integer = 1): Integer;
begin
  Result := IBDatabase.Gen_ID (AGeneratorName, AIncrement);
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

procedure TtiPersistenceLayerIBO.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
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

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(TtiPersistenceLayerIBO);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistIBO);

end.


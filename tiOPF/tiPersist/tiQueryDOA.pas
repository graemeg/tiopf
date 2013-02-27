{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
                                         
  Please submit changes to tiOPF@techinsite.com.au

  Purpose:
    Use the Adapter Pattern [GoF 139] to wrapper the DOA query
    component to allow a standard interface to be presented to the
    application for all data access APIs.

  Classes:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryDOA;

interface
uses
   tiQuery
  ,Classes
  ,Oracle
  ,tiDBConnectionPool
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  {$IFDEF DELPHI6}
  ,Variants
  {$ENDIF}
  ,tiObjAbs
  ,Contnrs
  ;

type

  TtiDBConnectionPoolDataDOA = Class( TtiDBConnectionPoolDataAbs )
  private
    procedure AddEntryToTNSNames(const psDBSid: string); overload ;
    procedure AddEntryToTNSNames(const psFileName, psDBSid, psHost : string); overload ;
    function  CheckTNSNamesForSid(const psDBSid: string): boolean;
  public
    procedure InitDBConnectionPool ; override ;
  end ;

  TtiDatabaseDOA = class( TtiDatabaseSQL )
  private
    FOracleSession: TOracleSession;
    FInTransaction : boolean ;
    property OracleSession : TOracleSession read FOracleSession write FOracleSession ;
  protected
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
    function  FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    constructor     create ; override ;
    destructor      destroy ; override ;
    class procedure CreateDatabase(const psDatabaseName, psUserName,psPassword: string); override ;
    class function  DatabaseExists(const psDatabaseName, psUserName, psPassword: string): boolean; override ;
    procedure       StartTransaction ; override ;
    function        InTransaction : boolean ; override ;
    procedure       Commit ; override ;
    procedure       RollBack ; override ;
    procedure       ReadMetaDataTables( pData : TPersistent ) ; override ;
    procedure       ReadMetaDataFields( pData : TPersistent ) ; override ;
    function        Test : boolean ; override ;
  end ;

  TDOABinParamItem = class( TtiObjAbs )
  private
    FBuffer: Pointer;
    FParamName: string;
    FSize: integer;
  public
    destructor destroy ; override ;
    property   ParamName : string read FParamName write FParamName ;
    property   Buffer    : Pointer read FBuffer write FBuffer ;
    property   Size      : integer read FSize write FSize ;
    procedure  LoadFromStream(const pStream : TStream) ;
    procedure  SaveToStream(const pStream : TStream);
  end;

  // ---------------------------------------------------------------------------
  TtiQueryDOA = class( TtiQuerySQL )
  private
    FQuery : TOracleQuery ;
    FbActive : boolean ;
    FslVariables : TStringList ;
    FDOABinParamList : TObjectList ;
    procedure DeclareVariable( const psName : string ; piOracleType : integer ) ;
    function  OracleErrorMessage : string ;
    function  FindCreateBinParam(const pName: string): TDOABinParamItem;
  protected
    function  GetSQL: TStrings; override ;
    procedure SetSQL(const Value: TStrings); override ;
    function  GetActive: boolean; override ;
    procedure SetActive(const Value: boolean); override ;
    function  GetEOF: boolean; override ;

    function  GetFieldAsString(const psName: string): string      ; override ;
    function  GetFieldAsFloat(const psName: string): real         ; override ;
    function  GetFieldAsBoolean(const psName: string): boolean    ; override ;
    function  GetFieldAsInteger(const psName: string): Int64      ; override ;
    function  GetFieldAsDateTime(const psName: string):TDateTime  ; override ;
    function  GetFieldIsNull(const psName: string): Boolean       ; override ;

    function  GetFieldAsStringByIndex(pIndex: Integer)  : string   ; override ;
    function  GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; override ;
    function  GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override ;
    function  GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override ;
    function  GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override ;
    function  GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override ;

    function  GetParamAsString( const psName: string): string; override ;
    procedure SetParamAsString( const psName, Value: string); override ;
    function  GetParamAsBoolean(const psName: string): boolean; override ;
    procedure SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    function  GetParamAsFloat(const psName: string): real;override ;
    procedure SetParamAsFloat(const psName: string; const Value: real);override ;
    function  GetParamAsInteger(const psName: string): Int64;override ;
    procedure SetParamAsInteger(const psName: string;const Value: Int64);override ;
    function  GetParamAsDateTime(const psName: string): TDateTime ; override ;
    procedure SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;

    procedure SetParamAsMacro( const psName: string;
                               const Value: string); override ;

    function  GetParamIsNull( const psName: String): Boolean; override;
    procedure SetParamIsNull( const psName: String; const Value: Boolean); override;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Open    ; override ;
    procedure   Close   ; override ;
    procedure   Next    ; override ;
    procedure   ExecSQL ; override ;

    function    ParamCount : integer ; override ;
    function    ParamName( pIndex : integer ) : string ; override ;

    function    FieldCount : integer ; override ;
    function    FieldName( pIndex : integer ) : string ; override ;
    function    FieldIndex( const psName : string ) : integer ; override ;
    function    FieldKind( pIndex : integer ) : TtiQueryFieldKind ; override ;
    function    FieldSize( pIndex : integer ) : integer ; override ;
    function    HasNativeLogicalType : boolean ; override ;

    procedure   AssignParamFromStream( const pName  : string  ; const pValue  : TStream ) ; override ;
    procedure   AssignParamToStream(   const pName  : string  ; const pValue  : TStream ) ; override ;
    procedure   AssignFieldAsStream(   const pName  : string  ; const pValue  : TStream ) ; override ;
    procedure   AssignFieldAsStreamByIndex(  pIndex : integer ; const pValue  : TStream ) ; override ;

    procedure   AttachDatabase( pDatabase : TtiDatabase ) ; override ;
    procedure   DetachDatabase ; override ;
    procedure   Reset ; override ;

  end ;


implementation
uses
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  SysUtils
  ,tiLog
  ,tiUtils
  ,Forms
  ,OracleCI
  ,tiPersist
  ,tiDialogs
  ,cTIPersist
  ,Windows
  ,Controls
  ;

const
  cSavePoint = 'DOA_Save_Point' ;
  
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryDOA
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryDOA.Create;
begin
  inherited;
  FQuery       := TOracleQuery.Create( nil ) ;
  FslVariables := TStringList.Create ;
  FbActive     := false ;
end;

// -----------------------------------------------------------------------------
destructor TtiQueryDOA.Destroy;
begin
  FQuery.ClearVariables ;
  FQuery.Free ;
  FslVariables.Free ;
  FDOABinParamList.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.Close;
begin
  Active := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.ExecSQL;
begin
  try
    FQuery.Execute ;
  except
    on e:exception do
    begin
      Database.ErrorInLastCall := true ;
      tiFmtException( e, OracleErrorMessage, ClassName, 'ExecSQL' ) ;
    end ;
  end;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetFieldAsBoolean(const psName: string): boolean;
var
  lsValue : string ;
begin
  lsValue := upperCase( FQuery.Field( psName )) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldAsDate( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetFieldAsFloat(const psName: string): real;
begin
  result := FQuery.FieldAsFloat( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetFieldAsInteger(const psName: string): Int64;
var
  lr : double ;
begin
  // Delphi real types
  // Real48	2.9 x 10^–39 .. 1.7 x 10^38	11–12	6
  // Single	1.5 x 10^–45 .. 3.4 x 10^38	7–8	4
  // Double	5.0 x 10^–324 .. 1.7 x 10^308	15–16	8
  // Extended	3.6 x 10^–4951 .. 1.1 x 10^4932	19–20	10
  // An OID is Number( 15 ) so we must read it as a double, then convert to an integer.
  lr := FQuery.Field( psName ) ;
  result := Trunc( lr ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetFieldAsString(const psName: string): string;
begin
  result := FQuery.Field( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetActive: boolean;
begin
  result := FbActive ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetEOF: boolean;
begin
  result := FQuery.EOF ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetParamAsBoolean(const psName: string): boolean;
var
  ls : string ;
begin
  ls := FQuery.GetVariable( psName ) ;
  result :=
    SameText( ls, 'TRUE' ) or
    SameText( ls, 'T' ) or
    SameText( ls, 'YES' ) or
    SameText( ls, 'Y' ) or
    ( ls = '1' ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.GetVariable( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetParamAsFloat(const psName: string): real;
begin
  result := FQuery.GetVariable( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetParamAsInteger(const psName: string): Int64;
var
  lr : real ;
begin
  lr := FQuery.GetVariable( psName ) ;
  result := trunc( lr ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetParamAsString(const psName: string): string;
begin
  result := FQuery.GetVariable( psName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.GetSQL: TStrings;
begin
  result := FQuery.SQL ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.Next;
begin
  try
    FQuery.Next ;
  except
    on e:exception do
    begin
      Database.ErrorInLastCall := true ;
      raise ;
    end ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.Open;
begin
  Active := true ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.ParamCount: integer;
begin
  result := FQuery.VariableCount ;
end;

// -----------------------------------------------------------------------------
function TtiQueryDOA.ParamName(pIndex: integer): string;
var
  ls : string ;
begin
  ls := FQuery.VariableName( pIndex ) ;
  result := tiStrTran( ls, ':', '' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetActive(const Value: boolean);
begin
  if Value then
  begin
    try
      FQuery.Execute ;
      FbActive := true ;
    except
      on e:exception do
        tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
    end;
  end else
  begin
    FQuery.Close ;
    FbActive := false ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetParamAsBoolean(const psName: string;
  const Value: boolean);
begin
  DeclareVariable( psName, otString ) ;
  if Value then
    FQuery.SetVariable( psName, 'T' )
  else
    FQuery.SetVariable( psName, 'F' )
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  DeclareVariable( psName, otDate ) ;
  FQuery.SetVariable( psName, DateTimeToStr(Value));

  // This was causing conversion error in StrToDateTime when called in Oracle.pas
  //FQuery.SetVariable( psName, FormatDateTime( 'dd/mm/yyyy hh:nn:ss', Value )) ;

  // Not sure why this was commented out. Think it might have been because of
  // problems converting TDateTime (float)
  //FQuery.SetVariable( psName, Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetParamAsFloat(const psName: string;
  const Value: real);
begin
  DeclareVariable( psName, otFloat ) ;
  FQuery.SetVariable( psName, Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetParamAsInteger(const psName: string;
  const Value: Int64 );
var
  lr : real ;
begin
  DeclareVariable( psName, otFloat ) ;
  lr := Value ;
  FQuery.SetVariable( psName, lr ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetParamAsString(const psName, Value: string);
var
  lpc : PChar ;
begin
  // ToDo: Better to read the data type from the metadata, rather than guess based on length
  if Length( Value ) < 255 then
  begin
    DeclareVariable( psName, otString ) ;
    FQuery.SetVariable( psName, Value ) ;
  end else
  begin
    DeclareVariable( psName, otLong ) ;
    lpc := PChar( Value ) ;
    FQuery.SetLongVariable( psName, lpc, length( lpc )) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.SetSQL(const Value: TStrings);
begin
  FQuery.DeleteVariables ;
  FslVariables.Clear ;
  FQuery.SQL.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
{
function TtiQueryDOA.GetFieldAsStream(const psName: string): TStream;
const
  cmiBuffer = 12000 ;
var
  liOffset : integer ;
  liLen    : integer ;
  lBuffer  : array [0..cmiBuffer-1] of byte ;
begin

  // If the stream does not exist, then create it
//  if FStream = nil then
//    FStream := TMemoryStream.Create
//  else
//    FStream.Clear ;

  if FStream <> nil then
    FStream.Free ;
  FStream := TMemoryStream.Create ;

  liOffset := 0 ;
  repeat
    liLen := FQuery.GetLongField( psName,
                                  @lBuffer, liOffset, cmiBuffer ) ;
    FStream.Write( lBuffer, liLen ) ;
    inc( liOffset, cmiBuffer ) ;
  until liLen < cmiBuffer ;

  result := FStream ;

end ;
}

// -----------------------------------------------------------------------------
procedure TtiQueryDOA.AssignFieldAsStream(const pName: string; const pValue: TStream );
const
  cmiBuffer = 12000 ;
var
  liOffset : integer ;
  liLen    : integer ;
  lBuffer  : array [0..cmiBuffer-1] of byte ;
begin
  Assert( pValue <> nil, 'Stream not assigned' ) ;
  pValue.Size := 0 ;
  liOffset := 0 ;
  repeat
    liLen := FQuery.GetLongField( pName,
                                  @lBuffer, liOffset, cmiBuffer ) ;
    pValue.Write( lBuffer, liLen ) ;
    inc( liOffset, cmiBuffer ) ;
  until liLen < cmiBuffer ;
  pValue.Position := 0 ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseDOA
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabaseDOA.Commit;
begin
  try
    Assert( FInTransaction, 'Attempt to call commit when a transaction is not open' ) ;
    try
      // mmm what happens if a commit fails. Should we roll back?
      FOracleSession.Commit ;
    finally
      FInTransaction := false ;
    end ;
  except
    on e:exception do
    begin
      ErrorInLastCall := true ;
      raise ;
    end ;
  end ;
end;

constructor TtiDatabaseDOA.create;
begin
  inherited;
  FOracleSession := TOracleSession.Create( nil ) ;
//  FOracleSession.UseOCI80 := false ;
  FOracleSession.Preferences.UseOCI7 := true ;
  FInTransaction := false ;
end;

destructor TtiDatabaseDOA.destroy;
begin
  // Swallow any exceptions as we might be destroying a DBConnection from
  // the pool after Oracle has gone down. An exception will be raised by
  // DOA in this case.
  try
    FOracleSession.Connected := false ;
  except end ;
  FOracleSession.Free ;
  inherited;
end;

procedure TtiQueryDOA.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase( pDatabase ) ;
  FQuery.Session := TtiDatabaseDOA( pDatabase ).OracleSession ;
end;

procedure TtiQueryDOA.DetachDatabase;
begin
  inherited DetachDatabase ;
  FQuery.Session := nil ;
end;

procedure TtiDatabaseDOA.StartTransaction;
begin
  try
    Assert( not FInTransaction, 'Attempt to start a transaction when one is already open' ) ;
  //  FOracleSession.SavePoint( cSavePoint ) ;
    FInTransaction := true ;
  except
    on e:exception do
    begin
      ErrorInLastCall := true ;
      raise;
    end ;
  end ;
end;

function TtiDatabaseDOA.InTransaction: boolean;
begin
  try
    result := FInTransaction ;
  //  result := FOracleSession.InTransaction ;
  except
    on e:exception do
    begin
      ErrorInLastCall := true ;
      raise ;
    end ;
  end ;
end;

procedure TtiDatabaseDOA.RollBack;
begin
  if not FInTransaction then
    Exit ; //==>
  try
    // FOracleSession.RollBackToSavePoint( cSavePoint ) ;
    // The DBConnection might be broken, so a rollback will raise an
    // exception. Assume the DB will rollback if the connection has been
    // broken.
    try
      FOracleSession.Rollback ;
    except
      on e:exception do
        ErrorInLastCall := true ;
    end ;
  finally
    FInTransaction := false ;
  end ;
end;

function TtiQueryDOA.FieldCount: integer;
begin
  result := FQuery.FieldCount ;
end;

function TtiQueryDOA.FieldName(pIndex: integer): string;
begin
  result := FQuery.FieldName( pIndex ) ;
end;

// Reset the query, same as calling SQL.Clear and DeleteVariables
// -----------------------------------------------------------------------------
procedure TtiQueryDOA.Reset;
begin
  Active := false ;
  FQuery.SQL.Clear ;
  FQuery.DeleteVariables ;
  FslVariables.Clear ;
end;

function TtiQueryDOA.FieldIndex(const psName: string): integer;
begin
  result := FQuery.FieldIndex( psName ) ;
end;

procedure TtiQueryDOA.SetParamAsMacro(const psName, Value: string);
begin
  SQLText :=
    tiCIStrTran( SQLText,
                 cgtiQueryMacroChr + psName,
                 Value ) ;
end;

function  TtiQueryDOA.GetParamIsNull( const psName: String): Boolean;
var
  AValue: Variant;
begin
  AValue := FQuery.GetVariable( psName);
  Result := VarIsNull(AValue) or VarIsEmpty(AValue);
end;

procedure TtiQueryDOA.SetParamIsNull( const psName: String; const Value: Boolean);
begin
  if Value then
    FQuery.SetVariable(psName, null)
  else
    tiFmtException('Parameter <' + psName + '> cannot be explicitly set to non-null',
                   ClassName,
                   'SetParamIsNull');
end;

function TtiQueryDOA.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lPrecision : integer ;
  lScale : integer ;
begin
  case FQuery.FieldType( pIndex ) of
  otString  : result := qfkString     ;
  otInteger : result := qfkInteger    ;
  otFloat   : result := qfkFloat      ;
  otDate    : result := qfkDateTime   ;
  otLong    : result := qfkLongString ;
  otLongRaw : result := qfkBinary     ;
  else
    result := Low( TtiQueryFieldKind ) ;
    tiFmtException( 'Invalid oracle field type <' +
                    IntToStr(FQuery.FieldType(pIndex)) + '>',
                    ClassName,
                    'FieldType' ) ;
  end ;

  if (result = qfkFloat) then
  begin
    lPrecision := FQuery.FieldPrecision(pIndex) ;
    lScale     := FQuery.FieldScale(pIndex) ;
    if ( lPrecision <> 0 ) and ( lScale = 0 ) then
      result := qfkInteger ;
  end ;

end;

function TtiQueryDOA.FieldSize(pIndex: integer): integer;
//var
//  lValue : string ;
begin
  case FieldKind( pIndex ) of
    qfkString     : begin
                      result := FQuery.FieldSize( pIndex ) ;
                      //  Can not detect field size for a logical as it will be stored as a string
                      //  qfkLogical    : result := 0 ;
                      // ToDo: What it it's a logical, but there is no data
                      //       returned. Can't check the field width then
(*
                      if (result = 1) or (result = 5) then
                      begin
                        lValue := FieldAsString[FieldName(pIndex)] ;
                        {$IFDEF BOOLEAN_CHAR_1}
                           if ( result = 1 ) and
                              ( SameText( lValue, 'T' ) or SameText( lValue, 'F' )) then
                             result := 0 ;
                        {$ELSE}
                           if ( result = 5 ) and
                              ( SameText( lValue, 'TRUE' ) or SameText( lValue, 'FALSE' )) then
                             result := 0 ;
                        {$ENDIF}
                      end ;
*)                      
                    end ;
    qfkLongString : result := 0 ;
    qfkInteger    : result := 0 ;
    qfkFloat      : result := 0 ;
    qfkDateTime   : result := 0 ;
    qfkBinary     : result := 0 ;
  else
    result := -1 ;
    tiFmtException( 'Invalid oracle field type <%s>',
                    [FQuery.FieldType(pIndex)],
                    ClassName,
                    'FieldType' ) ;
  end ;
end;

procedure TtiQueryDOA.DeclareVariable(const psName: string; piOracleType: integer);
var
  lsName : string ;
begin
  lsName := UpperCase( psName ) ;
  // Not sure if this check before declaring a parameter is necessary. Test later.
  if FslVariables.IndexOf( lsName ) = -1 then
  begin
    FslVariables.Add( lsName ) ;
    FQuery.DeclareVariable( psName, piOracleType ) ;
  end ;
//  FQuery.DeclareVariable( psName, otFloat ) ;
//  FQuery.DeclareVariable( psName, otString ) ;
//  FQuery.DeclareVariable( psName, otDate ) ;
//  FQuery.DeclareVariable( psName, otLong ) ;
//  FQuery.DeclareVariable( psName, otLongRaw ) ;

end;

procedure TtiDatabaseDOA.SetConnected(pbValue: boolean);
var
  FOldCursor : TCursor ;
begin

  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    FOracleSession.Connected := false ;
    FInTransaction := false ;
    Exit ; //==>
  end ;

  FOracleSession.LogonDatabase := DatabaseName ;
  FOracleSession.LogonUserName := UserName ;
  FOracleSession.LogonPassword := Password ;

  try
    FOldCursor := FOracleSession.Cursor ;
    if GetCurrentThreadId <> MainThreadID then
      FOracleSession.Cursor := Screen.Cursor;
    try
      FOracleSession.Connected     := true ;
    finally
      FOracleSession.Cursor := FOldCursor ;
    end;
  except
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create( ctiPersistDOA, DatabaseName, UserName, Password, e.message ) ;
  end ;

end;

function TtiDatabaseDOA.GetConnected: boolean;
begin
  result := FOracleSession.Connected ;
end;

function TtiQueryDOA.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldIsNull( FieldIndex( psName )) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDBConnectionPoolDataDOA
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBConnectionPoolDataDOA.InitDBConnectionPool ;
var
  lsDBSid : string ;
begin
  // Commented our because of bug running DSA system

  lsDBSid := DBConnectionPool.DBConnectParams.DatabaseName ;
  if CheckTNSNamesForSid( lsDBSid ) then
    Exit ; //==>


  // If you get here, then the DBSid was not found in TNSNames, so it must be
  // added
  AddEntryToTNSNames( lsDBSid ) ;

  // Confirm we where able to add the database to TNSNames
  if not CheckTNSNamesForSid( lsDBSid ) then
    tiFmtException( 'TNSNames.ora does not contain an entry for %s',
                    [lsDBSid],
                    ClassName,
                    'InitDBConnectionPool' ) ;

end;

function TtiDBConnectionPoolDataDOA.CheckTNSNamesForSid( const psDBSid : string ) : boolean ;
var
  i : integer ;
begin
  BuildOracleAliasList ;
  result := false ;
  for i := 0 to OracleAliasList.Count - 1 do
    if SameText( psDBSid, OracleAliasList.Strings[i] ) then
    begin
      result := true ;
      Break ; //==>
    end ;
end ;

procedure TtiDBConnectionPoolDataDOA.AddEntryToTNSNames( const psDBSid : string ) ;
var
  lsl : TStringList ;
  lsStartDir : string ;
  i : integer ;
begin
  lsStartDir := tiExtractDirToLevel( OCIDLL, 1 ) ;
  lsl := TStringList.Create ;
  try
    tiFilesToStringList( lsStartDir,
                         'TNSNames.Ora',
                         lsl,
                         true ) ;
    for i := 0 to lsl.Count - 1 do
      AddEntryToTNSNames( lsl.Strings[i],
                          psDBSid,
                          DBConnectionPool.DBConnectParams.HostName
                         ) ;

  finally
    lsl.Free ;
  end ;

end ;

procedure TtiDBConnectionPoolDataDOA.AddEntryToTNSNames(const psFileName, psDBSid, psHost : string);
var
  lStream : TFileStream ;
  ls : string ;
  lb : PChar ;
begin
  ls :=
    CrLf +
    '%s.world ='                     + CrLf +
    '  (DESCRIPTION ='               + CrLf +
    '    (ADDRESS_LIST ='            + CrLf +
    '        (ADDRESS ='             + CrLf +
    '          (PROTOCOL = TCP)'     + CrLf +
    '          (Host = %s)'          + CrLf +
    '          (Port = 1521)'        + CrLf +
    '        )'                      + CrLf +
    '    )'                          + CrLf +
    '    (CONNECT_DATA = (SID = %s)' + CrLf +
    '    )'                          + CrLf +
    '  )'                            + CrLf ;
  ls :=  Format( ls, [psDBSid, psHost, psDBSid ]) ;
  lb := PChar( ls ) ;
  lStream := TFileStream.Create( psFileName, fmOpenReadWrite or fmShareDenyNone	 )  ;
  try
    lStream.Position := lStream.Size ;
    lStream.WriteBuffer( lb^, Length( ls )) ;
  finally
    lStream.Free ;
  end ;
end ;

procedure TtiDatabaseDOA.ReadMetaDataTables(pData: TPersistent);
var
  lQuery : TtiQuery ;
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lQuery := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    lQuery.AttachDatabase( Self ) ;
    lQuery.SQLText :=
    'select ' +
    '   table_name ' +
    'from ' +
// ZL Change - was all_tables
// In all tables unique key is TABLE_NAME+OWNER. We are not using OWNER so
// we have to change to USER_TABLES and than uk is only TABLE_NAME
    '  user_tables ' +
    'where ' +
    '    not table_name like ''RM$%'' ' +
    'and not table_name like ''RM_%'' ' +
    'and not table_name like ''SDD_%'' ' +
    'and not table_name like ''SDW_%'' ' +
    'and not table_name like ''CDI_%'' ' +
    'and not table_name like ''CK_%'' ' +
    'and not table_name like ''O_%'' ' +
    'and not table_name like ''COPY_RULE%'' ' +
// ZL Change - in user_tables there is no such column
//    'and owner <> ''SYS'' ' +
    ' ' +
    'order by ' +

    '  table_name ' ;

{
      'select  ' +
      '   table_name ' +
      'from ' +
      '  user_tables ' +
      'where ' +
      '    not table_name like ''RM$%'' ' +
      'and not table_name like ''RM_%'' ' +
      'and not table_name like ''SDD_%'' ' +
      'and not table_name like ''SDW_%'' ' +
      'and not table_name like ''CDI_%'' ' +
      'and not table_name like ''CK_%'' ' +
      'and not table_name like ''O_%'' ' +
      'and not table_name like ''COPY_RULE%'' ' +
      'order by ' +
      '  table_name ' ;
}
    lQuery.Open ;
    while not lQuery.EOF do
    begin
      lTable := TtiDBMetaDataTable.Create ;
      lTable.Name := lQuery.FieldAsString[ 'table_name' ] ;
      lTable.ObjectState := posPK ;
      lMetaData.Add( lTable ) ;
      lQuery.Next ;
    end ;
    lQuery.DetachDatabase ;
    lMetaData.ObjectState := posClean ;
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabaseDOA.ReadMetaDataFields(pData: TPersistent);
var
  lQuery : TtiQuery ;
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lQuery := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    lQuery.AttachDatabase( Self ) ;
    lQuery.SQLText :=
      'select ' +
      '     COLUMN_NAME ' +
      '    ,DATA_TYPE            ' +
      '    ,DATA_LENGTH ' +
      '    ,DATA_PRECISION       ' +
      '    ,NULLABLE ' +
      'from ' +
// ZL Change - was all_tab_columns - but than we can see all columns
// of all tables and we should use also OWNER
// (unique key is OWNER+TABLE_NAME+COLUMN_NAME) in user_Tab_columns unique key
// is only TABLE_NAME+COLUMN_NAME
      '   user_tab_columns ' +
      'where ' +
      '   table_name = ''' + lTable.Name + '''' {+
      'order by ' +
      '    column_name ' +
      ' ' } ;
    lQuery.Open ;
    while not lQuery.EOF do
    begin
      lField := TtiDBMetaDataField.Create ;
      lField.Name := lQuery.FieldAsString[ 'COLUMN_NAME' ] ;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
      lQuery.Next ;
    end ;
    lQuery.DetachDatabase ;
    lTable.ObjectState := posClean ;
  finally
    lQuery.Free ;
  end ;
end;

function TtiQueryDOA.FindCreateBinParam(const pName: string): TDOABinParamItem;
var
  i : integer ;
begin
  result := nil ;
  if FDOABinParamList = nil then
    FDOABinParamList := TObjectList.Create(True);
  for i := 0 to FDOABinParamList.Count - 1 do
    if SameText( TDOABinParamItem(FDOABinParamList.Items[i]).ParamName, pName) then
    begin
      result := TDOABinParamItem(FDOABinParamList.Items[i]);
      Break ; //==>
    end ;
  if result = nil then
  begin
    result := TDOABinParamItem.Create;
    result.ParamName := pName ;
    FDOABinParamList.Add(result);
  end;
end;

procedure TtiQueryDOA.AssignParamFromStream(const pName: string; const pValue: TStream);
var
  lBinParam : TDOABinParamItem ;
begin
  lBinParam := FindCreateBinParam(pName);
  lBinParam.LoadFromStream(pValue);
  DeclareVariable( pName, otLongRaw ) ;
  FQuery.SetLongVariable( pName, lBinParam.Buffer, pValue.Size ) ;
end;

procedure TtiQueryDOA.AssignParamToStream(const pName: string; const pValue: TStream);
var
  lBinParam : TDOABinParamItem ;
begin
  lBinParam := FindCreateBinParam(pName);
  lBinParam.SaveToStream(pValue);
end;

function TtiDatabaseDOA.FieldMetaDataToSQLCreate( const pFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName : string ;
begin
  lFieldName := pFieldMetaData.Name ;
  case pFieldMetaData.Kind of
    qfkString     : result := 'VarChar2( ' + IntToStr( pFieldMetaData.Width ) + ')' ;
// Change by ZL
// was Number(12,0) - Number(12) looks cleaner and is used by Oracle it self
    qfkInteger    : result := 'Number( 12 )' ;
//Change by ZL
// was Numeric(11,5) - better Number than we can cover whole Real range.
// this is equal to Number(*) and means Maximal precision and scale.
// Numeric(11,5) is subset og Number so there is back-compability
    qfkFloat      : result := 'Number' ;
    qfkDateTime   : result := 'Date' ;
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical    : result := 'Char( 1 ) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))' ;
    {$ELSE}
    qfkLogical    : result := 'VarChar( 5 ) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ' ;    {$ENDIF}
    qfkBinary     : result := 'Long Raw' ;
    qfkLongString : result := 'Long' ;
  else
    tiFmtException( 'Invalid FieldKind', ClassName, 'FieldMetaDataToSQLCreate' ) ;
  end ;
end;

class procedure TtiDatabaseDOA.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
begin
  Assert( false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDOA.DatabaseExists(const psDatabaseName,psUserName, psPassword: string):boolean;
begin
  result := false ;
  Assert( false, 'DatabaseExists not implemented in ' + ClassName);
end;

function TtiQueryDOA.HasNativeLogicalType: boolean;
begin
  result := false ;
end;

function TtiQueryDOA.OracleErrorMessage: string;
var
  lRow : integer ;
  lCol  : integer ;
  lArrow : string ;
  i : integer ;
  ls : string ;
begin
  ls     := SQLAndParamsAsString;
  lRow   := FQuery.ErrorLine ;
  lCol   := FQuery.ErrorPosition ;

  if ( lRow <> 0 ) and ( lCol <> 0 ) then
  begin
    result := '' ;
    lArrow := tiReplicate( '-', lCol ) + '---^';
    for i := 1 to tiNumToken( ls, Cr ) do
    begin
      if i = lRow+2 then
        result := result + Cr + lArrow ;
      result := result + Cr + tiToken( ls, Cr, i ) ;
    end;
  end else
    result := ls ;

  result :=
    result + Cr( 2 ) +
    'Error in row: ' + IntToStr( lRow ) + Cr +
    'Error in col: ' + IntToStr( lCol ) ;

end;

{ TDOABinParamItem }

destructor TDOABinParamItem.destroy;
begin
  System.FreeMem(FBuffer);
  inherited;
end;

procedure TDOABinParamItem.LoadFromStream(const pStream: TStream);
begin
  if FSize <> 0 then
    FreeMem(FBuffer);
  pStream.Position := 0 ;
  FSize := pStream.Size ;
  GetMem(FBuffer, FSize);
  pStream.Read( FBuffer^, FSize ) ;
end;

procedure TDOABinParamItem.SaveToStream(const pStream: TStream);
begin
  pStream.Size := 0 ;
  pStream.Write(FBuffer^,FSize);
  pStream.Position := 0;
end;

function TtiDatabaseDOA.Test: boolean;
var
  lQuery : TtiQuery ;
begin
  result := false ;
  try
    lQuery := CreateTIQuery;
    try
      lQuery.AttachDatabase( Self ) ;
      lQuery.SQLText := 'select null from dual' ;
      lQuery.Open ;
      while ( not lQuery.EOF ) and
            ( not result ) do
        result := true ;
      lQuery.DetachDatabase ;
    finally
      lQuery.Free ;
    end ;
  except
    on e:exception do
      result := false ;
  end ;
  if not result then
    ErrorInLastCall := true ;
end;

function TtiQueryDOA.GetFieldAsStringByIndex(pIndex: Integer): string;
begin
  result := FQuery.Field(pIndex);
end;

function TtiQueryDOA.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
var
  lsValue : string ;
begin
  lsValue := upperCase( FQuery.Field( pIndex )) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

function TtiQueryDOA.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQuery.FieldAsDate( pIndex ) ;
end;

function TtiQueryDOA.GetFieldAsFloatByIndex(pIndex: Integer): real;
begin
  result := FQuery.FieldAsFloat( pIndex ) ;
end;

function TtiQueryDOA.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
var
  lr : double ;
begin
  // Delphi real types
  // Real48	2.9 x 10^–39 .. 1.7 x 10^38	11–12	6
  // Single	1.5 x 10^–45 .. 3.4 x 10^38	7–8	4
  // Double	5.0 x 10^–324 .. 1.7 x 10^308	15–16	8
  // Extended	3.6 x 10^–4951 .. 1.1 x 10^4932	19–20	10
  // An OID is Number( 15 ) so we must read it as a double, then convert to an integer.
  lr := FQuery.Field( pIndex ) ;
  result := Trunc( lr ) ;
end;

function TtiQueryDOA.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQuery.FieldIsNull( pIndex ) ;
end;

procedure TtiQueryDOA.AssignFieldAsStreamByIndex(pIndex: Integer;const pValue: TStream);
const
  cmiBuffer = 12000 ;
var
  liOffset : integer ;
  liLen    : integer ;
  lBuffer  : array [0..cmiBuffer-1] of byte ;
begin
  Assert( pValue <> nil, 'Stream not assigned' ) ;
  pValue.Size := 0 ;
  liOffset := 0 ;
  repeat
    liLen := FQuery.GetLongField( pIndex,
                                  @lBuffer, liOffset, cmiBuffer ) ;
    pValue.Write( lBuffer, liLen ) ;
    inc( liOffset, cmiBuffer ) ;
  until liLen < cmiBuffer ;
  pValue.Position := 0 ;
end;

initialization
  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistDOA,
              TtiDBConnectionPoolDataAbs,
              TtiQueryDOA,
              TtiDatabaseDOA ) ;

finalization
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistDOA ) ;

end.



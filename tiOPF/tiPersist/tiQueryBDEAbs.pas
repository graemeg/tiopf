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
    Use the Adapter Pattern [GoF 139] to wrapper the TQuery
    component to allow a standard interface to be presented to the
    application for all data access APIs.


  Classes:
    TtiQueryBDE

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryBDEAbs;

interface
uses
   tiQuery
  ,Classes
  ,DBTables
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseBDEAbs = class( TtiDatabaseSQL )
  private
    FDatabase      : TDataBase;
    FSession       : TSession ;
  protected
    property  Database : TDatabase read FDatabase write FDatabase ;
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
  TtiQueryBDE = class( TtiQuerySQL )
  private
    FQuery : TQuery ;
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
  ,DB
  ,TypInfo
  ,tiPersist
  ,tiDialogs
  ;

var
  uSessionCount : integer ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryBDE
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryBDE.Create;
begin
  inherited;
  FQuery := TQuery.Create( nil ) ;
end;

// -----------------------------------------------------------------------------
destructor TtiQueryBDE.Destroy;
begin
  FQuery.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.Close;
begin
  Active := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.ExecSQL;
begin
  try
    FQuery.ExecSQL ;
  except
    on e:exception do
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
  end;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetFieldAsBoolean(const psName: string): boolean;
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
function TtiQueryBDE.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByName( psName ).AsDateTime ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetFieldAsFloat(const psName: string): real;
begin
  result := FQuery.FieldByName( psName ).AsFloat ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByName( psName ).AsInteger ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetFieldAsString(const psName: string): string;
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
function TtiQueryBDE.GetActive: boolean;
begin
  result := FQuery.Active ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetEOF: boolean;
begin
  result := FQuery.EOF ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetParamAsBoolean(const psName: string): boolean;
begin
  result := FQuery.ParamByName( psName ).AsBoolean ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.ParamByName( psName ).AsDateTime ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetParamAsFloat(const psName: string): real;
begin
  result := FQuery.ParamByName( psName ).AsFloat ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetParamAsInteger(const psName: string): Int64;
begin
  result := FQuery.ParamByName( psName ).AsInteger ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetParamAsString(const psName: string): string;
begin
  result := FQuery.ParamByName( psName ).AsString ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.GetSQL: TStrings;
begin
  result := FQuery.SQL ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.Next;
begin
  FQuery.Next ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.Open;
begin
  Active := true ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.ParamCount: integer;
begin
  result := FQuery.ParamCount ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.ParamName(pIndex: integer): string;
begin
  result := FQuery.Params.Items[ pIndex ].Name ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetActive(const Value: boolean);
begin
  try
    FQuery.Active := Value ;
  except
    on e:exception do
    begin
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  if not FQuery.Prepared then
    FQuery.Prepare ;
  FQuery.ParamByName( psName ).AsBoolean := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  if not FQuery.Prepared then
    FQuery.Prepare ;
  FQuery.ParamByName( psName ).AsDateTime := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetParamAsFloat(const psName: string;
  const Value: real);
begin
  if not FQuery.Prepared then
    FQuery.Prepare ;
  FQuery.ParamByName( psName ).AsFloat := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  if not FQuery.Prepared then
    FQuery.Prepare ;
  FQuery.ParamByName( psName ).AsInteger := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetParamAsString(const psName, Value: string);
var
  lParam : TParam ;
begin
  if not FQuery.Prepared then
    FQuery.Prepare ;
  lParam := FQuery.ParamByName( psName ) ;
  if length( Value ) <= 255 then
    lParam.AsString := Value
  else
    lParam.AsMemo := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.AssignFieldAsStream(const pName: string; const pStream: TStream );
var
  lField : TField ;
begin
  lField := FQuery.FieldByName(pName);
  Assert(lField is TBlobField, 'Field <' + pName + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pStream);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.AttachDatabase(pDatabase: TtiDatabase);
begin
  FQuery.DatabaseName := TtiDatabaseBDEAbs( pDatabase ).Database.DatabaseName ;
  Database := pDatabase ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.DetachDatabase;
begin
  try
    inherited DetachDatabase ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DetachDatabase' ) ;
  end ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.FieldCount: integer;
begin
  result := FQuery.FieldCount ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.FieldName(pIndex: integer): string;
begin
  result := FQuery.Fields[pIndex].FieldName ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryBDE.Reset;
begin
  Active := false ;
  FQuery.SQL.Clear ;
  FQuery.Params.Clear ;
end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.FieldIndex(const psName: string): integer;
begin
  result := FQuery.FieldByName( psName ).Index ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseBDEAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseBDEAbs.create;
begin
  inherited Create ;
  FDatabase := TDataBase.Create( nil ) ;
  FDatabase.LoginPrompt := false ;
  FSession       := TSession.Create( nil ) ;
  FSession.AutoSessionName := true ;
  FDatabase.SessionName := FSession.Name ;
  // Must come up with a better way of doing this.
  FDatabase.DatabaseName := 'DB_' + FSession.SessionName ;
  Inc(uSessionCount);
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseBDEAbs.Destroy;
begin
  FDatabase.Free;
  FSession.Free ;
  Dec(uSessionCount);
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseBDEAbs.Commit;
begin
  if not InTransaction then
    tiFmtException( 'Attempt to commit but not in a transaction.',
                    ClassName,
                    'Commit' ) ;
  FDatabase.Commit ;
end;

// -----------------------------------------------------------------------------
function TtiDatabaseBDEAbs.InTransaction: boolean;
begin
  result := FDatabase.InTransaction ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseBDEAbs.RollBack;
begin
  FDatabase.RollBack ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseBDEAbs.StartTransaction;
begin
  if InTransaction then
    tiFmtException( 'Attempt to start a transaction but transaction already exists.',
                    ClassName,
                    'StartTransaction' ) ;
  FDatabase.StartTransaction ;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQueryBDE.FieldKind(pIndex: integer): TtiQueryFieldKind;
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
      tiFmtException( 'Invalid FQuery.Fields[ pIndex ].DataType <' +
                      GetEnumName( TypeInfo( TFieldType ), Ord( lDataType )) +
                      '>', ClassName, 'FieldKind' ) ;
    end ;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

// -----------------------------------------------------------------------------
function TtiQueryBDE.FieldSize(pIndex: integer): integer;
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
    tiFmtException( 'Invalid field type', ClassName, 'FieldType' ) ;
  end ;
end;

function TtiQueryBDE.GetParamIsNull(const psName: String): Boolean;
begin
  result := FQuery.ParamByName( psName ).IsNull ;
end;

procedure TtiQueryBDE.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  FQuery.ParamByName( psName ).Clear ;
end;

function TtiDatabaseBDEAbs.GetConnected: boolean;
begin
  Result := FDatabase.Connected ;
end;

procedure TtiDatabaseBDEAbs.SetConnected(pbValue: boolean);
var
  i : integer ;
  lsErrorMessage : string ;
begin

  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    FDatabase.Connected := false ;
    FSession.Active := false ;
    Exit ; //==>
  end ;

  SetupDBParams ;
  lsErrorMessage := '' ;
  try
    FDatabase.Connected := true ;
    FSession.Active := true ;
  except
    // ToDo: Find a generic way of trapping an invalid password error, and
    //       re-raising this exception so the logon screen in activated again.
    //       But, the BDE raises a bunch of exception for the simplest error
    //       so this may be tricky.
    on e:EDBEngineError do
    begin
      lsErrorMessage := '' ;
      for i := 0 to EDBEngineError( e ).ErrorCount-1 do
      begin
        if lsErrorMessage <> '' then lsErrorMessage := lsErrorMessage + CrLf( 2 ) ;
        lsErrorMessage := lsErrorMessage +
                          'Error class: '   + EDBEngineError( e ).classname + Cr +
                          'Error message: ' + EDBEngineError( e ).Errors[i].Message + Cr +
                          'Error Code: ' + IntToStr( EDBEngineError( e ).Errors[i].ErrorCode ) + Cr +
                          'Native error code: ' + IntToStr( EDBEngineError( e ).Errors[i].NativeError ) ;
      end ;
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, lsErrorMessage ) ;
    end ;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, e.message ) ;
  end ;
end;

function TtiQueryBDE.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldByName( psName ).IsNull ;
end;

//------------------------------------------------------------------------------
procedure TtiQueryBDE.AssignParamToStream(const pName: string; const pStream : TStream);
var
  ls : string ;
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  ls := FQuery.ParamByName(pName).Value ;
  tiStringToStream(ls, pStream);
end;

//------------------------------------------------------------------------------
procedure TtiQueryBDE.AssignParamFromStream(const pName: string; const pStream : TStream);
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  FQuery.ParamByName( pName ).LoadFromStream( pStream, ftBlob );
end;

function TtiQueryBDE.HasNativeLogicalType: boolean;
begin
  result := true ;
end;

procedure TtiQueryBDE.AssignFieldAsStreamByIndex(pIndex: Integer;const pValue: TStream);
var
  lField : TField ;
begin
  lField := FQuery.Fields[pIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(pIndex) + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pValue);
end;

function TtiQueryBDE.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
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

function TtiQueryBDE.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ pIndex ].AsDateTime ;
end;

function TtiQueryBDE.GetFieldAsFloatByIndex(pIndex: Integer): real;
begin
  result := FQuery.Fields[ pIndex ].AsFloat ;
end;

function TtiQueryBDE.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FQuery.Fields[ pIndex ].AsInteger ;
end;

function TtiQueryBDE.GetFieldAsStringByIndex(pIndex: Integer): string;
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

function TtiQueryBDE.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ pIndex ].IsNull ;
end;

initialization
  uSessionCount := 0 ;

end.



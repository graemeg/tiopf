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

  Revision history:
    Aug 2001, Piero Bonanno, Implement

  Purpose:
    Use the Adapter Pattern [GoF 139] to wrapper the TQuery
    component to allow a standard interface to be presented to the
    application for all data access APIs.


  Classes:
    TtiQueryADO

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryADOAbs;

interface
uses
   tiQuery
  ,Classes
  ,ADODb
  ,DB
  ;

const
  cDelphi5ADOErrorString = 'Either BOF or EOF is True, or the current record has been deleted. Requested operation requires a current record' ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseADOAbs = class( TtiDatabaseSQL )
  private
    FADOConnection      : TADOConnection;
  protected
    property  Connection : TADOConnection read FADOConnection write FADOConnection ;
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
    procedure SetupDBParams ; virtual ; abstract ;
    function  FieldDataTypeToTIQueryFieldKind(pDataType: TFieldType): TtiQueryFieldKind;

  public
    constructor create ; override ;
    destructor  Destroy ; override ;

    class function  DatabaseExists( const pDatabaseName, pUserName, pPassword : string ) : boolean ; override ;
    class procedure CreateDatabase( const pDatabaseName, pUserName, pPassword : string ) ; override ;

    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;

  end ;

  // ---------------------------------------------------------------------------
  TtiQueryADO = class( TtiQuerySQL )
  private
    FADOQuery : TADOQuery ;
  protected

    function  GetFieldAsString(const psName: string): string     ; override ;
    function  GetFieldAsFloat(const psName: string): real        ; override ;
    function  GetFieldAsBoolean(const psName: string): boolean   ; override ;
    function  GetFieldAsInteger(const psName: string): Int64     ; override ;
    function  GetFieldAsDateTime(const psName: string):TDateTime ; override ;

    function  GetFieldAsStringByIndex(pIndex: Integer): string     ; override ;
    function  GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; override ;
    function  GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override ;
    function  GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override ;
    function  GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override ;
    function  GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override ;

    function  GetSQL: TStrings; override ;
    procedure SetSQL(const Value: TStrings); override ;
    function  GetActive: boolean; override ;
    procedure SetActive(const Value: boolean); override ;
    function  GetEOF: boolean; override ;
    function  GetParamAsString( const psName: string): string; override ;
    function  GetParamAsBoolean(const psName: string): boolean; override ;
    function  GetParamAsFloat(const psName: string): real;override ;
    function  GetParamAsInteger(const psName: string): Int64 ;override ;
    procedure SetParamAsString( const psName, Value: string); override ;
    procedure SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    procedure SetParamAsFloat(const psName: string; const Value: real);override ;
    procedure SetParamAsInteger(const psName: string;const Value: Int64);override ;
    function  GetParamAsDateTime(const psName: string): TDateTime ; override ;
    procedure SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;

    function  GetParamIsNull( const psName: String): Boolean; override;
    procedure SetParamIsNull( const psName: String; const Value: Boolean); override;
    function  GetFieldIsNull(const psName: string): Boolean; override ;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Open    ; override ;
    procedure   Close   ; override ;
    procedure   Next    ; override ;
    procedure   ExecSQL ; override ;

    function    ParamCount : integer ; override ;
    function    ParamName( pIndex : integer ) : string ; override ;

    procedure   AssignParamToStream(   const pName : string ;  const pValue : TStream ) ; override ;
    procedure   AssignParamFromStream( const pName : string ;  const pValue : TStream ) ; override ;
    procedure   AssignFieldAsStream(   const pName : string ;  const pValue : TStream ) ; override ;
    procedure   AssignFieldAsStreamByIndex( pIndex : integer ; const pValue : TStream ) ; override ;

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
  ,TypInfo
  ,tiWin32
  ,tiDialogs
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
  ,tiRJMime
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryADO
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryADO.Create;
begin
  inherited;
  tiWin32CoInitialize ;
  FADOQuery := TADOQuery.Create( nil ) ;
  FADOQuery.CursorType := ctOpenForwardOnly ;
  FADOQuery.CursorLocation := clUseServer ;
end;

// -----------------------------------------------------------------------------
destructor TtiQueryADO.Destroy;
begin
  FADOQuery.Free ;
//  tiCoUnInitialize ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.Close;
begin
  Active := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.ExecSQL;
var
  ls : string ;
begin
  try
    FADOQuery.Prepared:=true;
    FADOQuery.ExecSQL ;
  except
    on e:exception do
    begin
      ls := e.Message ;
      Log(ls);
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
    end ;
  end;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetFieldAsBoolean(const psName: string): boolean;
var
  lValue : variant ;
begin
  lValue := FADOQuery.FieldByName( psName ).Value ;
  result := lValue = -1 ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FADOQuery.FieldByName( psName ).AsDateTime ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetFieldAsFloat(const psName: string): real;
begin
  result := FADOQuery.FieldByName( psName ).AsFloat ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FADOQuery.FieldByName( psName ).AsInteger ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetFieldAsString(const psName: string): string;
begin
  result := FADOQuery.FieldByName( psName ).AsString ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetActive: boolean;
begin
  result := FADOQuery.Active ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetEOF: boolean;
begin
  result := FADOQuery.EOF ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetParamAsBoolean(const psName: string): boolean;
var
  lValue : variant ;
begin
  lValue := FADOQuery.Parameters.ParamByName( psName ).Value ;
  result := lValue = -1 ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetParamAsDateTime(const psName: string): TDateTime;
var
  lValue : string ;
begin
  // ToDo: Should have some protection against different date time formats...
  lValue := FADOQuery.Parameters.ParamByName( psName ).Value ;
  result := StrToDateTime( lValue ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetParamAsFloat(const psName: string): real;
begin
  result := FADOQuery.Parameters.ParamByName( psName ).Value ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetParamAsInteger(const psName: string): Int64;
begin
  result := Longint(FADOQuery.Parameters.ParamByName( psName ).Value) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.GetParamAsString(const psName: string): string;
begin
  result := FADOQuery.Parameters.ParamByName( psName ).Value ;
end;

function TtiQueryADO.GetSQL: TStrings;
begin
  result := FADOQuery.SQL ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.Next;
begin
  FADOQuery.Next ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.Open;
begin
  FADOQuery.Prepared := False;
  Active := true ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.ParamCount: integer;
begin
  result := FADOQuery.Parameters.Count ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.ParamName(pIndex: integer): string;
begin
  result := FADOQuery.Parameters.Items[ pIndex ].Name ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetActive(const Value: boolean);
begin
  if Value = FADOQuery.Active then
    Exit ; //==>

  // This try finally is to swallow an exception that has been fixed by
  // applying the D5 ADO update package. Un comment if you are getting the
  // error "Either BOF or EOF is True, or the current record has been deleted.
  // Requested operation requires a current record" or better still,
  // apply the update, available here:
  //   http://info.borland.com/devsupport/delphi/mdac26.html  
  //try
  //  FADOQuery.Active := Value ;
  //except
  //  on e:exception do
  //  begin
  //    {$IFDEF DELPHI5}
  //    if not SameText( e.Message, cDelphi5ADOErrorString ) then
  //    {$ENDIF}
  //      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' )
  //end ;

  try
    FADOQuery.Active := Value ;
  except
    on e:exception do
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' )
  end;

end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  if Value then
    FADOQuery.Parameters.ParamByName( psName ).Value := True
  else
    FADOQuery.Parameters.ParamByName( psName ).Value := False ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  FADOQuery.Parameters.ParamByName( psName ).Value := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetParamAsFloat(const psName: string;
  const Value: real);
begin
  FADOQuery.Parameters.ParamByName( psName ).Value := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  FADOQuery.Parameters.ParamByName( psName ).Value := Longint(Value) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.SetParamAsString(const psName, Value: string);
begin
  FADOQuery.Parameters.ParamByName( psName ).Value := Value ;
end;

procedure TtiQueryADO.SetSQL(const Value: TStrings);
begin
  FADOQuery.SQL.Assign( Value ) ;
//  FADOQuery.Prepared:=true;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.AssignParamToStream(const pName: string; const pValue: TStream );
var
  lBinData: OleVariant;
  lDataPtr: Pointer;
  lHigh, lLow, lLen: Integer;
  lParameter : TParameter ;
begin
  Assert(pValue <> nil, 'Stream not assigned');
  lParameter := FADOQuery.Parameters.ParamByName(pName);
  lLow     := VarArrayLowBound(lParameter.Value, 1);
  lHigh    := VarArrayHighBound(lParameter.Value, 1);
  lLen     := lHigh - lLow + 1 ;
  lBinData := VarArrayCreate([0, lLen], varByte);
  lBinData := lParameter.Value ;
  lDataPtr := VarArrayLock(lBinData);
  try
    pValue.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.AssignParamFromStream(const pName: string; const pValue: TStream );
begin
  Assert( pValue <> nil, 'Stream not assigned' ) ;
  FADOQuery.Parameters.ParamByName( pName ).LoadFromStream( pValue, ftBlob );
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.AssignFieldAsStream(const pName: string; const pValue: TStream );
//var
//  ls : string ;
//  lField : TField ;
begin
  // This does not look right, but it's the best I can do in the time available.
  // DUnit tests pass, but then at the time of writing, we don't have any
  // tests for 'real' binary data, just strings in a TStream.
  // Updated 2005-05-17 ipk  NB qfkBinary field type mapped to 'image' in MS SQL   
  Assert(pValue <> nil, 'Stream not assigned');
  pValue.Position := 0 ;
  (FADOQuery.FieldByName(pName) as TBlobField).SaveToStream(pValue);
//  LogFmt('%s.AssignFieldAsStream Field is <%s> Size:%d',[ClassName, FADOQuery.FieldByName(pName).ClassName, pValue.Size],lsDebug );
//  lField := FADOQuery.FieldByName(pName);
//  ls       := lField.Value;
//  tiStringToStream(ls,pStream);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.AttachDatabase(pDatabase: TtiDatabase);
var
  lSQL : string ;
begin
  // This mess is to work around the problem (that is only happening with ADO,
  // not other data access layers) of the query parameters not being accessable
  // if the database connection is set after the sql. This happens with the
  // SQLManager visitors. A better solution would be to remove the part of the
  // query factory that creates the query, and only have the query factory
  // assign the SQL. This would tidy the SQLVisitor framework up nicely as it
  // is quite a mess. The QueryFactory was a concept from the early days and
  // has now been replaced by the plugable persistence layers as packages.
  lSQL := FADOQuery.SQL.Text ;
  FADOQuery.SQL.Clear ;
  FADOQuery.Connection := TtiDatabaseADOAbs( pDatabase ).Connection ;
  FADOQuery.SQL.Text := lSQL ;
  Database := pDatabase ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.DetachDatabase;
begin
  inherited DetachDatabase ;
  if FADOQuery.Active then
    FADOQuery.Active := false ;
  FADOQuery.Connection := nil ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.FieldCount: integer;
begin
  result := FADOQuery.FieldCount ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.FieldName(pIndex: integer): string;
begin
  result := FADOQuery.Fields[pIndex].FieldName ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryADO.Reset;
begin
  Active := false ;
  FADOQuery.SQL.Clear ;
  FADOQuery.Parameters.Clear ;
end;

// -----------------------------------------------------------------------------
function TtiQueryADO.FieldIndex(const psName: string): integer;
begin
  result := FADOQuery.FieldByName( psName ).Index ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseADOAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseADOAbs.create;
begin
  inherited Create ;
  tiWin32CoInitialize ;
  FADOConnection := TADOConnection.Create( nil ) ;
  FADOConnection.LoginPrompt := false ;
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseADOAbs.Destroy;
begin
  FADOConnection.Free;
  tiWin32CoUnInitialize ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseADOAbs.Commit;
begin
  if not InTransaction then
    tiFmtException( 'Attempt to commit but not in a transaction.',
                    ClassName,
                    'Commit' ) ;
  FADOConnection.CommitTrans ;
//  tiCoUnInitialize ;
end;

// -----------------------------------------------------------------------------
function TtiDatabaseADOAbs.InTransaction: boolean;
begin
  result := FADOConnection.InTransaction ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseADOAbs.RollBack;
begin
  FADOConnection.RollbackTrans ;
//  tiCoUnInitialize ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseADOAbs.StartTransaction;
begin
  if InTransaction then
    tiFmtException( 'Attempt to start a transaction but transaction already exists.',
                    ClassName,
                    'StartTransaction' ) ;
  tiWin32CoInitialize ;
  FADOConnection.BeginTrans ;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQueryADO.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lDataType : TFieldType ;
begin
  lDataType := FADOQuery.Fields[ pIndex ].DataType ;

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
    ftBlob, ftGraphic, ftVarBytes :             result := qfkBinary   ;
    ftMemo, ftFmtMemo :                         result := qfkLongString ;
    else
      result := qfkString ; // Just to shup up the compiler
      tiFmtException( 'Invalid FADOQuery.Fields[ pIndex ].DataType <' +
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
function TtiQueryADO.FieldSize(pIndex: integer): integer;
begin
  case FieldKind( pIndex ) of
    qfkString     : result := FADOQuery.FieldDefs[ pIndex ].Size ;
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

function TtiQueryADO.GetParamIsNull(const psName: String): Boolean;
begin
  result := VarIsNull( FADOQuery.Parameters.ParamByName( psName ).Value ) ;
end;

procedure TtiQueryADO.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  if Value then
    FADOQuery.Parameters.ParamByName( psName ).Value := Null ;
end;

function TtiDatabaseADOAbs.GetConnected: boolean;
begin
  Result := FADOConnection.Connected ;
end;

procedure TtiDatabaseADOAbs.SetConnected(pbValue: boolean);
var
  lsErrorMessage : string ;
begin

  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    FADOConnection.Connected := false ;
    Exit ; //==>
  end ;

  SetupDBParams ;
  lsErrorMessage := '' ;
  try
    FADOConnection.Connected := true ;
  except
    on e:exception do
    begin
      lsErrorMessage := e.message ;
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, lsErrorMessage );
    end ;
    on e:EADOError do
    begin
      lsErrorMessage :=
        'Error class: '   + EADOError( e ).classname + Cr +
        'Error message: ' + EADOError( e ).Message;
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, lsErrorMessage );
    end ;
  end ;
end;

function TtiQueryADO.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FADOQuery.FieldByName( psName ).IsNull ;
end;

// This function is cloned in tiQueryBDEAbs - must move it to a common location, but without pulling DB.pas into any packages where it is not required
function TtiDatabaseADOAbs.FieldDataTypeToTIQueryFieldKind( pDataType : TFieldType ) : TtiQueryFieldKind ;
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
    ftString, ftWideString :                    result := qfkString   ;
    ftSmallint, ftInteger, ftWord, ftLargeint : result := qfkInteger  ;
    ftBoolean :                                 result := qfkLogical  ;
    ftFloat, ftCurrency, ftBCD :                result := qfkFloat    ;
    ftDate, ftTime, ftDateTime :                result := qfkDateTime ;
    ftBlob, ftGraphic, ftVarBytes :             result := qfkBinary   ;
    ftMemo, ftFmtMemo :                         result := qfkLongString ;
    else
      result := qfkString ; // Just to shup up the compiler
      tiFmtException( 'Invalid FQuery.Fields[ pIndex ].DataType <' +
                      GetEnumName( TypeInfo( TFieldType ), Ord( pDataType )) +
                      '>', ClassName, 'FieldKind' ) ;
    end ;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;


function TtiQueryADO.HasNativeLogicalType: boolean;
begin
  result := true ;
end;

class procedure TtiDatabaseADOAbs.CreateDatabase(const pDatabaseName, pUserName, pPassword: string);
begin
  Assert( false, 'Not implemented in ' + ClassName ) ;
end;

class function TtiDatabaseADOAbs.DatabaseExists(const pDatabaseName,
  pUserName, pPassword: string): boolean;
begin
  result := false ;
  Assert( false, 'Not implemented in ' + ClassName ) ;
end;

procedure TtiQueryADO.AssignFieldAsStreamByIndex(pIndex: Integer;const pValue: TStream);
//var
//  ls : string ;
//  lField : TField ;
begin
  // This does not look right, but it's the best I can do in the time available.
  // DUnit tests pass, but then at the time of writing, we don't have any
  // tests for 'real' binary data, just strings in a TStream.
  Assert(pValue <> nil, 'Stream not assigned');
  pValue.Position := 0 ;
  TBlobField(FADOQuery.Fields[pIndex]).SaveToStream(pValue);
//  pValue.Size := 0 ;
//  lField := FADOQuery.Fields[pIndex];
//  ls       := lField.Value;
//  tiStringToStream(ls,pValue);
end;

function TtiQueryADO.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
var
  lValue : variant ;
begin
  lValue := FADOQuery.Fields[pIndex].Value ;
  result := lValue = -1 ;
end;

function TtiQueryADO.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FADOQuery.Fields[pIndex].AsDateTime ;
end;

function TtiQueryADO.GetFieldAsFloatByIndex(pIndex: Integer): real;
begin
  result := FADOQuery.Fields[pIndex].AsFloat ;
end;

function TtiQueryADO.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FADOQuery.Fields[pIndex].AsInteger ;
end;

function TtiQueryADO.GetFieldAsStringByIndex(pIndex: Integer): string;
begin
  result := FADOQuery.Fields[pIndex].AsString ;
end;

function TtiQueryADO.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FADOQuery.Fields[pIndex].IsNull ;
end;

end.




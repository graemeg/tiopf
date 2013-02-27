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

unit tiQuerySQLiteAbs;

interface
uses
  tiQuery
  ,Classes
  ,SQLite3
  ,DBTAbles
  ,DB
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseSQLiteAbs = class(TtiDatabaseSQL)
  private
    FSession: TSession;
    FInTransAction : Boolean;
    FDatabase: TtiSQLite3DB;
  protected
    function GetConnected: Boolean;override;
    procedure SetConnected(pbValue : boolean);override;
    procedure SetupDBParams; virtual;abstract;
  public
    constructor create;override;
    destructor Destroy; override;
    procedure Commit;override;
    procedure RollBack;override;
    procedure StartTransaction;override;
    function  InTransaction:Boolean;override;
    property Database: TtiSQLite3DB read FDatabase write FDatabase;
  end;

  TtiQuerySQLiteAbs = class(TtiQuerySQL)
  private
    FQuery: TtiSQLite3Query;
  protected
    function GetActive: Boolean; override;
    function GetEOF: Boolean; override;
    function GetFieldAsBoolean(const psName: string): Boolean; override;
    function GetFieldAsBooleanByIndex(pIndex: Integer): Boolean; override;
    function GetFieldAsDateTime(const psName: string): TDateTime; override;
    function GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime; override;
    function GetFieldAsFloat(const psName: string): Real; override;
    function GetFieldAsFloatByIndex(pIndex: Integer): Real; override;
    function GetFieldAsInteger(const psName: string): Int64; override;
    function GetFieldAsIntegerByIndex(pIndex: Integer): Int64; override;
    function GetFieldAsString(const psName: string): string; override;
    function GetFieldAsStringByIndex(pIndex: Integer): string; override;
    function GetFieldIsNull(const psName: string): Boolean; override;
    function GetFieldIsNullByIndex(pIndex: Integer): Boolean; override;
    function GetParamAsBoolean(const psName: string): Boolean; override;
    function GetParamAsDateTime(const psName: string): TDateTime; override;
    function GetParamAsFloat(const psName: string): Real; override;
    function GetParamAsInteger(const psName: string): Int64; override;
    function GetParamAsString(const psName: string): string; override;
    function GetParamIsNull(const psName: String): Boolean; override;
    function GetSQL: TStrings; override;
    procedure SetActive(const Value: boolean); override;
    procedure SetParamAsBoolean(const psName: string;const Value: boolean);
            override;
    procedure SetParamAsDateTime(const psName :string ; const Value: TDateTime);
            override;
    procedure SetParamAsFloat(const psName: string; const Value: real);
            override;
    procedure SetParamAsInteger(const psName: string;const Value: Int64);
            override;
    procedure SetParamAsString(const psName, Value: string); override;
    procedure SetParamIsNull(const psName: String; const Value: Boolean);
            override;
    procedure SetSQL(const Value: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignFieldAsStream(const pName : string ; const pStream :
            TStream); override;
    procedure AssignFieldAsStreamByIndex(pIndex : integer ; const pValue :
            TStream); override;
    procedure AssignParamFromStream(const pName : string ; const pStream :
            TStream); override;
    procedure AssignParamToStream(const pName : string ; const pStream :
            TStream); override;
    procedure AttachDatabase(pDatabase : TtiDatabase); override;
    procedure Close; override;
    procedure DetachDatabase; override;
    procedure ExecSQL; override;
    function FieldCount: Integer; override;
    function FieldIndex(const psName : string): Integer; override;
    function FieldKind(pIndex : integer): TtiQueryFieldKind; override;
    function FieldName(pIndex : integer): string; override;
    function FieldSize(pIndex : integer): Integer; override;
    function HasNativeLogicalType: Boolean; override;
    procedure Next; override;
    procedure Open; override;
    function ParamCount: Integer; override;
    function ParamName(pIndex : integer): string; override;
    procedure Reset; override;
  end;


implementation
uses
  SysUtils ,tiDBConnectionPool
  ,tiLog
  ,tiUtils
  ,TypInfo
  ,tiPersist
  ,tiDialogs
  ;

var
  uSessionCount : integer ;



{
***************************** TtiDatabaseSQLiteAbs *****************************
}
constructor TtiDatabaseSQLiteAbs.create;
begin
  inherited Create ;
  FDatabase := TtiSQLite3DB.Create ;
  FDataBase.MustExist := False;
  FSession       := TSession.Create( nil ) ;
  FSession.AutoSessionName := true ;
  FDatabase.SessionName := FSession.Name ;
  // Must come up with a better way of doing this.
  FDatabase.DatabaseName := 'DB_' + FSession.SessionName ;
  Inc(uSessionCount);
  FInTransAction := False;
end;

destructor TtiDatabaseSQLiteAbs.Destroy;
begin
  FDatabase.Free;
  FSession.Free ;
  Dec(uSessionCount);
  inherited;
end;

procedure TtiDatabaseSQLiteAbs.Commit;
begin
  if not InTransaction then
    tiFmtException( 'Attempt to commit but not in a transaction.',
                    ClassName,
                    'Commit' ) ;
  FDatabase.Commit ;
  FInTransAction := False;
end;

function TtiDatabaseSQLiteAbs.GetConnected: Boolean;
begin
  Result := FDatabase.Connected ;
end;

procedure TtiDatabaseSQLiteAbs.RollBack;
begin
  FDatabase.RollBack ;
  FInTransAction := False;
end;

procedure TtiDatabaseSQLiteAbs.SetConnected(pbValue : boolean);
var
  i: Integer;
  lsErrorMessage: string;
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

procedure TtiDatabaseSQLiteAbs.StartTransaction;
begin
  if InTransaction then
    tiFmtException( 'Attempt to start a transaction but transaction already exists.',
                    ClassName,
                    'StartTransaction' ) ;
  FDatabase.StartTransaction ;
  FInTransAction := True;
end;

function  TtiDatabaseSQLiteAbs.InTransaction:Boolean;
begin
  Result := FInTransAction;
end;

{
******************************** TtiQuerySQLiteAbs ********************************
}
constructor TtiQuerySQLiteAbs.Create;
begin
  inherited;
  FQuery := TtiSQLite3Query.Create;
//  FQuery.Connection :=
end;

destructor TtiQuerySQLiteAbs.Destroy;
begin
  FQuery.Free ;
  inherited;
end;

procedure TtiQuerySQLiteAbs.AssignFieldAsStream(const pName : string ; const
        pStream : TStream);
var
  lField: TField;
begin
  lField := FQuery.FieldByName(pName);
  Assert(lField is TBlobField, 'Field <' + pName + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pStream);
end;

procedure TtiQuerySQLiteAbs.AssignFieldAsStreamByIndex(pIndex : integer ; const
        pValue : TStream);
var
  lField: TField;
begin
  lField := FQuery.Fields[pIndex];
  Assert(lField is TBlobField, 'Field index <' + IntToStr(pIndex) + '> not a TBlobField' ) ;
  ( lField as TBlobField ).SaveToStream(pValue);
end;

procedure TtiQuerySQLiteAbs.AssignParamFromStream(const pName : string ; const
        pStream : TStream);
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  FQuery.ParamByName( pName ).LoadFromStream( pStream, ftBlob );
end;

procedure TtiQuerySQLiteAbs.AssignParamToStream(const pName : string ; const
        pStream : TStream);
var
  ls: string;
begin
  Assert( pStream <> nil, 'Stream not assigned' ) ;
  ls := FQuery.ParamByName(pName).Value ;
  tiStringToStream(ls, pStream);
end;

procedure TtiQuerySQLiteAbs.AttachDatabase(pDatabase : TtiDatabase);
begin
  FQuery.DatabaseName := TtiDatabaseSQLiteAbs( pDatabase ).Database.DatabaseName ;
  FQuery.Connection :=  TtiDatabaseSQLiteAbs( pDatabase ).Database;
  Database := pDatabase ;
//  TtiDatabaseSQLiteAbs( pDatabase ).Database.DefaultDir := ExtractFilePath(TtiDatabaseSQLiteAbs( pDatabase ).Database.DatabaseName);
end;

procedure TtiQuerySQLiteAbs.Close;
begin
  FQuery.Close;
  Active := false ;
end;

procedure TtiQuerySQLiteAbs.DetachDatabase;
begin
  try
    inherited DetachDatabase ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DetachDatabase' ) ;
  end ;
end;

procedure TtiQuerySQLiteAbs.ExecSQL;
begin
  try
    {if QueryType=qtSelect then }FQuery.ExecSQL;
    //else FQuery.Connection.SQLite3_ExecSQL(FQuery.SQL.Text);
  except
    on e:exception do
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
  end;
end;

function TtiQuerySQLiteAbs.FieldCount: Integer;
begin
  result := FQuery.FieldCount ;
end;

function TtiQuerySQLiteAbs.FieldIndex(const psName : string): Integer;
begin
  result := FQuery.FieldByName( psName ).Index ;
end;

function TtiQuerySQLiteAbs.FieldKind(pIndex : integer): TtiQueryFieldKind;
var
  lDataType: TFieldType;
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

function TtiQuerySQLiteAbs.FieldName(pIndex : integer): string;
begin
  result := FQuery.Fields[pIndex].FieldName ;
end;

function TtiQuerySQLiteAbs.FieldSize(pIndex : integer): Integer;
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

function TtiQuerySQLiteAbs.GetActive: Boolean;
begin
  result := FQuery.Active ;
end;

function TtiQuerySQLiteAbs.GetEOF: Boolean;
begin
  result := FQuery.EOF ;
end;

function TtiQuerySQLiteAbs.GetFieldAsBoolean(const psName: string): Boolean;
var
  lsValue: string;
begin
  lsValue := upperCase( FQuery.FieldByName( psName ).AsString ) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

function TtiQuerySQLiteAbs.GetFieldAsBooleanByIndex(pIndex: Integer): Boolean;
var
  lsValue: string;
begin
  lsValue := upperCase( FQuery.Fields[ pIndex ].AsString ) ;
  result := ( lsValue = 'T'    ) or
            ( lsValue = 'TRUE' ) or
            ( lsValue = 'Y'    ) or
            ( lsValue = 'YES'  ) or
            ( lsValue = '1'    ) ;
end;

function TtiQuerySQLiteAbs.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByName( psName ).AsDateTime ;
end;

function TtiQuerySQLiteAbs.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  result := FQuery.Fields[ pIndex ].AsDateTime ;
end;

function TtiQuerySQLiteAbs.GetFieldAsFloat(const psName: string): Real;
begin
  result := FQuery.FieldByName( psName ).AsFloat ;
end;

function TtiQuerySQLiteAbs.GetFieldAsFloatByIndex(pIndex: Integer): Real;
begin
  result := FQuery.Fields[ pIndex ].AsFloat ;
end;

function TtiQuerySQLiteAbs.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByName( psName ).AsInteger ;
end;

function TtiQuerySQLiteAbs.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  result := FQuery.Fields[ pIndex ].AsInteger ;
end;

function TtiQuerySQLiteAbs.GetFieldAsString(const psName: string): string;
var
  lField: TField;
  lStream: TStringStream;
begin
  lField := FQuery.FieldByName( psName ) ;
  if lField.IsNull then result := ''
  else begin
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
end;

function TtiQuerySQLiteAbs.GetFieldAsStringByIndex(pIndex: Integer): string;
var
  lField: TField;
  lStream: TStringStream;
begin
  lField := FQuery.Fields[pIndex] ;
  if lField.IsNull then result := ''
  else begin
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
end;

function TtiQuerySQLiteAbs.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldByName( psName ).IsNull ;
end;

function TtiQuerySQLiteAbs.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  result := FQuery.Fields[ pIndex ].IsNull ;
end;

function TtiQuerySQLiteAbs.GetParamAsBoolean(const psName: string): Boolean;
begin
  result := FQuery.ParamByName( psName ).AsBoolean ;
end;

function TtiQuerySQLiteAbs.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.ParamByName( psName ).AsDateTime ;
end;

function TtiQuerySQLiteAbs.GetParamAsFloat(const psName: string): Real;
begin
  result := FQuery.ParamByName( psName ).AsFloat ;
end;

function TtiQuerySQLiteAbs.GetParamAsInteger(const psName: string): Int64;
begin
  result := FQuery.ParamByName( psName ).AsInteger ;
end;

function TtiQuerySQLiteAbs.GetParamAsString(const psName: string): string;
begin
  result := FQuery.ParamByName( psName ).AsString ;
end;

function TtiQuerySQLiteAbs.GetParamIsNull(const psName: String): Boolean;
begin
  result := FQuery.ParamByName( psName ).IsNull ;
end;

function TtiQuerySQLiteAbs.GetSQL: TStrings;
begin
  result := FQuery.SQL ;
end;

function TtiQuerySQLiteAbs.HasNativeLogicalType: Boolean;
begin
  result := true ;
end;

procedure TtiQuerySQLiteAbs.Next;
begin
  FQuery.Next ;
end;

procedure TtiQuerySQLiteAbs.Open;
begin
  Active := true ;
  
end;

function TtiQuerySQLiteAbs.ParamCount: Integer;
begin
  result := FQuery.ParamCount ;
end;

function TtiQuerySQLiteAbs.ParamName(pIndex : integer): string;
begin
  result := FQuery.Params.Items[ pIndex ].Name ;
end;

procedure TtiQuerySQLiteAbs.Reset;
begin
  Active := false ;
  FQuery.SQL.Clear ;
  FQuery.Params.Clear ;
end;

procedure TtiQuerySQLiteAbs.SetActive(const Value: boolean);
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

procedure TtiQuerySQLiteAbs.SetParamAsBoolean(const psName: string;const Value:
        boolean);
begin
  FQuery.ParamByName( psName ).AsBoolean := Value ;
end;

procedure TtiQuerySQLiteAbs.SetParamAsDateTime(const psName :string ; const Value:
        TDateTime);
begin
  FQuery.ParamByName( psName ).AsDateTime := Value ;
end;

procedure TtiQuerySQLiteAbs.SetParamAsFloat(const psName: string; const Value:
        real);
begin
  FQuery.ParamByName( psName ).AsFloat := Value ;
end;

procedure TtiQuerySQLiteAbs.SetParamAsInteger(const psName: string;const Value:
        Int64);
begin
  FQuery.ParamByName( psName ).AsInteger := Value ;
end;

procedure TtiQuerySQLiteAbs.SetParamAsString(const psName, Value: string);
var
  lParam: TParam;
begin
  lParam := FQuery.ParamByName( psName ) ;
  if length( Value ) <= 255 then
    lParam.AsString := Value
  else
    lParam.AsMemo := Value ;
end;

procedure TtiQuerySQLiteAbs.SetParamIsNull(const psName: String; const Value:
        Boolean);
begin
  FQuery.ParamByName( psName ).Clear ;
end;

procedure TtiQuerySQLiteAbs.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign( Value ) ;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseBDEAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
  uSessionCount := 0 ;

end.



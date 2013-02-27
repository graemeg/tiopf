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
    To extend the tiOPF with the addition of a NexusDb persistence
    layer.

  Classes:
    TtiDatabaseNx1
    TtiQueryNx1

  History:
    02 Feb 2004:  Released. Uses TnxWinsockTransport. Transport and
                  RemoteServerEngine components are singletons.

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryNx1;

interface
uses
  tiQuery
  , Classes
  , Db
  , nxDb
  , nxllTransport
  , nxtwWinsockTransport
  , nxreRemoteServerEngine
  , nxllComponent
  , nxsdTypes
  , tiClassToDBMap_BOM
  , tiPtnVisPerObj
{$IFNDEF VER130}
  , Variants
{$ENDIF}
  ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseNx1 = class(TtiDatabaseSQL)
  private
    FDatabase: TnxDatabase;
    FSession: TnxSession;
    property Database: TnxDatabase read FDatabase write FDatabase;
    procedure SetLoginParams;
  protected
    function FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField
      ): string; override;
    function GetConnected: Boolean; override;
    function NxFieldDataTypeToTIQueryFieldKind(aDataType: TnxFieldType):
      TtiQueryFieldKind;
    procedure SetConnected(pbValue: boolean); override;
  public
    constructor create; override;
    destructor Destroy; override;
    procedure Commit; override;
    class procedure CreateDatabase(const psDatabaseName, psUserName,
      psPassword: string); override;
    class function DatabaseExists(const psDatabaseName, psUserName,
      psPassword: string): boolean; override;
    function InTransaction: Boolean; override;
    procedure ReadMetaDataFields(pData: TPersistent); override;
    procedure ReadMetaDataTables(pData: TPersistent); override;
    procedure RollBack; override;
    procedure StartTransaction; override;
  end;

  // ---------------------------------------------------------------------------
  TtiQueryNx1 = class(TtiQuerySQL)
  private
    FQuery: TnxQuery;
    procedure Prepare;
  protected
    function GetActive: Boolean; override;
    function GetEOF: Boolean; override;
    function GetFieldAsBoolean(const psName: string): Boolean; override;
    function GetFieldAsDateTime(const psName: string): TDateTime; override;
    function GetFieldAsFloat(const psName: string): Real; override;
    function GetFieldAsInteger(const psName: string): Int64; override;
    function GetFieldAsString(const psName: string): string; override;
    function GetFieldAsVariant(const psName: string): Variant; override;
    function GetFieldIsNull(const psName: string): Boolean; override;
    function GetParamAsBoolean(const psName: string): Boolean; override;
    function GetParamAsDateTime(const psName: string): TDateTime; override;
    function GetParamAsFloat(const psName: string): Real; override;
    function GetParamAsInteger(const psName: string): Int64; override;
    function GetParamAsString(const psName: string): string; override;
    function GetParamAsTextBLOB(const psName: string): string; override;
    function GetParamAsVariant(const psName: string): Variant; override;
    function GetParamIsNull(const psName: string): Boolean; override;
    function GetSQL: TStrings; override;
    procedure SetActive(const Value: boolean); override;
    procedure SetParamAsBoolean(const psName: string; const Value: boolean);
      override;
    procedure SetParamAsDateTime(const psName: string; const Value: TDateTime);
      override;
    procedure SetParamAsFloat(const psName: string; const Value: real);
      override;
    procedure SetParamAsInteger(const psName: string; const Value: Int64);
      override;
    procedure SetParamAsString(const psName, Value: string); override;
    procedure SetParamAsTextBLOB(const psName, Value: string); override;
    procedure SetParamAsVariant(const psName: string; const Value: Variant);
      override;
    procedure SetParamIsNull(const psName: string; const Value: Boolean);
      override;
    procedure SetSQL(const Value: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignFieldAsStream(const pName: string; const pStream:
      TStream); override;
    procedure AssignParamFromStream(const pName: string; const pStream:
      TStream); override;
    procedure AssignParamToStream(const pName: string; const pStream:
      TStream); override;
    //procedure AssignParams(pParams, pWhere: TtiQueryParams); override;
    procedure AttachDatabase(pDatabase: TtiDatabase); override;
    procedure Close; override;
    procedure DetachDatabase; override;
    procedure ExecSQL; override;
    function FieldCount: Integer; override;
    function FieldIndex(const psName: string): Integer; override;
    function FieldKind(pIndex: integer): TtiQueryFieldKind; override;
    function FieldName(pIndex: integer): string; override;
    function FieldSize(pIndex: integer): Integer; override;
    function HasNativeLogicalType: boolean; override;
    procedure Next; override;
    procedure Open; override;
    function ParamCount: Integer; override;
    function ParamName(pIndex: integer): string; override;
    function ParamsAsString: string; override;
    procedure Reset; override;
  end;

function gNxServerEngine: TnxRemoteServerEngine;

implementation
uses
  SysUtils
  , tiUtils
  , tiDialogs
  , tiLog
  , TypInfo
  , tiPersist
  , cTIPersist
  , tiDBConnectionPool
  , nxsdDataDictionary
  , tiCommandLineParams
  , tiRegINI
  ;

var
  uNxServerEngine: TnxRemoteServerEngine;
  uNxTransport: TnxWinsockTransport;

function CreateNxRemoteServerEngine: TnxRemoteServerEngine;
var
  lNxServerEngine: TnxRemoteServerEngine;
begin
  lNxServerEngine := nil;;

  try
    uNxTransport := TnxWinsockTransport.Create(nil);
    { Use 'dummy' param to force read of command line
      config file if available. }
    uNxTransport.ServerName := gTIPerMgr.ReadConfigValue('dummy', 'ServerName');
    { If ServerName not available try using local computer name. }
    if uNxTransport.ServerName = '' then
      uNxTransport.ServerName := tiGetComputerName;

    uNxTransport.Timeout := 10000;
    uNxTransport.Enabled := True;

    lNxServerEngine := TnxRemoteServerEngine.Create(nil);
    lNxServerEngine.Transport := uNxTransport;
    lNxServerEngine.Active := True;
  except
    on e: EnxTransportException do begin
      tiAppError('Database transport error!' + CrLf +
              'Cannot connect to ServerName: "' +
              uNxTransport.ServerName + '".' + CrLf(2) + e.Message);
      Halt;
    end;
    on e: EnxRemoteServerEngineException do begin
      tiAppError('Remote Server Engine error!' + CrLf +
              'Cannot activate Remote Server Engine.' +
        CrLf(2) + e.Message);
      Halt;
    end;
  else
    raise;
  end;

  result := lNxServerEngine;
end;

function gNxServerEngine: TnxRemoteServerEngine;
begin
  if uNxServerEngine = nil then
    uNxServerEngine := CreateNxRemoteServerEngine;

  result := uNxServerEngine;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryNx1
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
********************************* TtiQueryNx1 **********************************
}

constructor TtiQueryNx1.Create;
begin
  inherited;
  FQuery := TnxQuery.Create(nil);
end;

destructor TtiQueryNx1.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TtiQueryNx1.AssignFieldAsStream(const pName: string; const pStream
  : TStream);
begin
  Assert(false, 'Not implemented in ' + ClassName);
  (*Assert( pStream <> nil, 'Stream not assigned' ) ;
  pStream.Position := 0;
  FQuery.FieldByName( pName ).SaveToStream( pStream );*)
end;

procedure TtiQueryNx1.AssignParamFromStream(const pName: string; const
  pStream: TStream);
begin
  //Assert( false, 'Not implemented in ' + ClassName ) ;
  Assert(pStream <> nil, 'Stream not assigned');
  Prepare ;
  //pStream.Position := 0 ;
  FQuery.ParamByName(pName).LoadFromStream(pStream, ftBlob);
end;

procedure TtiQueryNx1.AssignParamToStream(const pName: string; const pStream
  : TStream);

{var
  lPointer : Pointer ;
  lSize    : integer ;
  //lBlobStrm: TffBlobStream;}

begin
  Assert(false, 'Not implemented in ' + ClassName);
  {Assert( pStream <> nil, 'Stream not assigned' ) ;
  FQuery.ParamByName( pName ).SaveToStream( pStream );
  pStream.Position := 0 ;}
  {Assert( pStream <> nil, 'Stream not assigned' ) ;
  lSize := FQuery.ParamByName( pName ).GetDataSize ;
  //tiShowMessage( IntToStr(lSize) ) ;
  GetMem( lPointer, lSize ) ;
  try
    FQuery.ParamByName( pName ).GetData( lPointer ) ;
    pStream.WriteBuffer( lPointer, lSize ) ;
    pStream.Position := 0 ;
    //tiShowMessage( 'GetDataSize: ' + IntToStr(lSize) +
    //              '   Stream Size: ' +
    //              IntToStr(pStream.Size)) ;
    //tiShowMessage( TStringStream( pStream ).DataString ) ;
  finally
    FreeMem( lPointer ) ;
  end ;}
end;

(*procedure TtiQueryNx1.AssignParams(pParams, pWhere: TtiQueryParams);
var
  i: integer;
begin
  if pParams = nil then
    Exit;
  FQuery.Prepare ;
  for i := 0 to pParams.Count - 1 do
    ParamAsVariant[pParams.Items[i].Name] := pParams.Items[i].Value;
  if pWhere <> nil then
    for i := 0 to pWhere.Count - 1 do
      ParamAsVariant[pWhere.Items[i].Name] := pWhere.Items[i].Value;
end;*)

procedure TtiQueryNx1.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
  FQuery.Database := TtiDatabaseNx1(pDatabase).Database;
end;

procedure TtiQueryNx1.Close;
begin
  Active := false;
end;

procedure TtiQueryNx1.DetachDatabase;
begin
  try
    inherited DetachDatabase;
  except
    on e: exception do
      tiFmtException(e, ClassName, 'DetachDatabase');
  end;
end;

procedure TtiQueryNx1.ExecSQL;
begin
  try
    Prepare ;
    FQuery.ExecSQL;
  except
    on e:exception do
      tiFmtException( e, SQLAndParamsAsString, ClassName, 'ExecSQL' ) ;
  end;
end;

function TtiQueryNx1.FieldCount: Integer;
begin
  result := FQuery.FieldCount;
end;

function TtiQueryNx1.FieldIndex(const psName: string): Integer;
begin
  result := FQuery.FieldByName(psName).Index;
end;

function TtiQueryNx1.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lDataType: TFieldType;
begin
  lDataType := FQuery.Fields[pIndex].DataType;

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
    ftString, ftWideString: result := qfkString;
    ftSmallint, ftInteger, ftWord, ftLargeint: result := qfkInteger;
    ftBoolean: result := qfkLogical;
    ftFloat, ftCurrency, ftBCD: result := qfkFloat;
    ftDate, ftTime, ftDateTime: result := qfkDateTime;
    ftBlob, ftGraphic: result := qfkBinary;
    ftMemo, ftFmtMemo: result := qfkLongString;
  else
    result := qfkString; // Just to shut up the compiler
    tiFmtException('Invalid FQuery.Fields[ pIndex ].DataType <' +
      GetEnumName(TypeInfo(TFieldType), Ord(lDataType)) +
      '>', ClassName, 'FieldKind');
  end;
  //    ftUnknown,
  //    ftBytes, ftVarBytes, ftAutoInc,
  //    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
  //    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
  //    ftVariant, ftInterface, ftIDispatch, ftGuid
end;

function TtiQueryNx1.FieldName(pIndex: integer): string;
begin
  result := FQuery.Fields[pIndex].FieldName;
end;

function TtiQueryNx1.FieldSize(pIndex: integer): Integer;
begin
  result := FQuery.FieldDefs[pIndex].Size;
end;

function TtiQueryNx1.GetActive: Boolean;
begin
  result := FQuery.Active;
end;

function TtiQueryNx1.GetEOF: Boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryNx1.GetFieldAsBoolean(const psName: string): Boolean;
begin
  result := FQuery.FieldByName(psName).AsBoolean;
end;

function TtiQueryNx1.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByName(psName).AsDateTime;
end;

function TtiQueryNx1.GetFieldAsFloat(const psName: string): Real;
begin
  result := FQuery.FieldByName(psName).AsFloat;
end;

function TtiQueryNx1.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByName(psName).AsInteger;
end;

function TtiQueryNx1.GetFieldAsString(const psName: string): string;
begin
  result := FQuery.FieldByName(psName).AsString;
end;

function TtiQueryNx1.GetFieldAsVariant(const psName: string): Variant;
begin
  result := FQuery.FieldByName(psName).Value;
end;

function TtiQueryNx1.GetFieldIsNull(const psName: string): Boolean;
begin
  result := FQuery.FieldByName(psName).IsNull;
end;

function TtiQueryNx1.GetParamAsBoolean(const psName: string): Boolean;
begin
  result := FQuery.ParamByName(psName).AsBoolean;
end;

function TtiQueryNx1.GetParamAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.ParamByName(psName).AsDateTime;
end;

function TtiQueryNx1.GetParamAsFloat(const psName: string): Real;
begin
  result := FQuery.ParamByName(psName).AsFloat;
end;

function TtiQueryNx1.GetParamAsInteger(const psName: string): Int64;
begin
  result := FQuery.ParamByName(psName).AsInteger;
end;

function TtiQueryNx1.GetParamAsString(const psName: string): string;
begin
  result := FQuery.ParamByName(psName).AsString;
end;

function TtiQueryNx1.GetParamAsTextBLOB(const psName: string): string;
begin
  result := FQuery.ParamByName(psName).AsString;
end;

function TtiQueryNx1.GetParamAsVariant(const psName: string): Variant;
begin
  result := FQuery.ParamByName(psName).Value;
end;

function TtiQueryNx1.GetParamIsNull(const psName: string): Boolean;
begin
  result := FQuery.ParamByName(psName).IsNull;
end;

function TtiQueryNx1.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryNx1.Next;
begin
  FQuery.Next;
end;

procedure TtiQueryNx1.Open;
begin
  Active := true;
end;

function TtiQueryNx1.ParamCount: Integer;
begin
  result := FQuery.ParamCount;
end;

function TtiQueryNx1.ParamName(pIndex: integer): string;
begin
  result := FQuery.Params.Items[pIndex].Name;
end;

function TtiQueryNx1.ParamsAsString: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to ParamCount - 1 do begin
    result := tiAddTrailingValue(result, CrLf);
    result := result +
      ParamName(i) + ' := ' +
      ParamAsString[ParamName(i)];
  end;
end;

procedure TtiQueryNx1.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.Params.Clear;
end;

procedure TtiQueryNx1.SetActive(const Value: boolean);
begin
  FQuery.ActiveRunTime := Value;
end;

procedure TtiQueryNx1.SetParamAsBoolean(const psName: string; const Value:
  boolean);
begin
  Prepare ;
  FQuery.ParamByName(psName).AsBoolean := Value;
end;

procedure TtiQueryNx1.SetParamAsDateTime(const psName: string; const Value:
  TDateTime);
begin
  Prepare ;
  FQuery.ParamByName(psName).AsDateTime := Value;
end;

procedure TtiQueryNx1.SetParamAsFloat(const psName: string; const Value: real);
begin
  Prepare ;
  FQuery.ParamByName(psName).AsFloat := Value;
end;

procedure TtiQueryNx1.SetParamAsInteger(const psName: string; const Value:
  Int64);
begin
  Prepare ;
  FQuery.ParamByName(psName).AsInteger := Value;
end;

procedure TtiQueryNx1.SetParamAsString(const psName, Value: string);
begin
  Prepare ;
  if Length(Value) > 255 then
    FQuery.ParamByName(psName).AsMemo := Value
  else
    FQuery.ParamByName(psName).AsString := Value;
end;

procedure TtiQueryNx1.SetParamAsTextBLOB(const psName, Value: string);
begin
  Prepare ;
  FQuery.ParamByName(psName).AsMemo := Value;
end;

procedure TtiQueryNx1.SetParamAsVariant(const psName: string; const Value:
  Variant);
begin
  Prepare ;
  FQuery.ParamByName(psName).Value := Value;
end;

procedure TtiQueryNx1.SetParamIsNull(const psName: string; const Value:
  Boolean);
begin
  Prepare ;
  FQuery.ParamByName(psName).Clear;
end;

procedure TtiQueryNx1.Prepare;
begin
  if FQuery.Prepared then
    Exit ; //==>
  try
    FQuery.SQL.Text := UpperCase( FQuery.SQL.Text );
    FQuery.Prepare;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'Prepare' ) ;
  end ;
end;

procedure TtiQueryNx1.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign(Value);
end;

function TtiQueryNx1.HasNativeLogicalType: boolean;
begin
  result := true;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseNx1
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
******************************** TtiDatabaseNx1 ********************************
}

constructor TtiDatabaseNx1.create;
begin
  inherited Create;

  FSession := TnxSession.Create(nil);
  FSession.ServerEngine := gNxServerEngine;
  FSession.Timeout := 10000;
  FSession.Active := True;

  FDatabase := TnxDatabase.Create(nil);
  FDatabase.Session := FSession;
end;

destructor TtiDatabaseNx1.Destroy;
begin
  try
    if Assigned(FDatabase) and FDatabase.Connected and InTransaction then
      Commit;
    FDatabase.Connected := false;
    FSession.CloseInactiveFolders;
    FSession.CloseInactiveTables;
    FDatabase.Free;
    FSession.Free;
  except
    on e: exception do
      LogError(e.message);
  end;
  inherited;
end;

class procedure TtiDatabaseNx1.CreateDatabase(const psDatabaseName, psUserName,
  psPassword: string);
var
  lDir: string;
begin
  lDir := ExpandFileName(psDatabaseName);
  if DatabaseExists(lDir, psUserName, psPassword) then
    raise EtiDBAlreadyExists.Create;
  if not ForceDirectories(lDir) then
    raise EtiDBFailedCreatingDatabase.Create;
end;

class function TtiDatabaseNx1.DatabaseExists(const psDatabaseName, psUserName,
  psPassword: string): boolean;
var
  lDir: string;
begin
  lDir := ExpandFileName(psDatabaseName);
  result := DirectoryExists(lDir);
end;

procedure TtiDatabaseNx1.Commit;
begin
  if not InTransaction then
    tiFmtException('Attempt to commit but not in a transaction.',
      ClassName,
      'Commit');
  FDatabase.Commit;
end;

function TtiDatabaseNx1.FieldMetaDataToSQLCreate(const pFieldMetaData:
  TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString: result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: result := 'Integer';
    qfkFloat: result := 'Real';
    qfkDateTime: result := 'DateTime';
    qfkLogical: result := 'Boolean';
    qfkBinary: result := 'Blob';
    qfkLongString: result := 'Text';
  else
    tiFmtException('Invalid FieldKind', ClassName, 'FieldMetaDataToSQLCreate');
  end;
end;

function TtiDatabaseNx1.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TtiDatabaseNx1.InTransaction: Boolean;
begin
  result := FDatabase.InTransaction;
end;

function TtiDatabaseNx1.NxFieldDataTypeToTIQueryFieldKind(aDataType:
  TnxFieldType): TtiQueryFieldKind;
begin
  case aDataType of
    nxtShortString, nxtNullString, nxtWideString:  result := qfkString;
    nxtInt8, nxtInt16, nxtInt32, nxtInt64,
      nxtByte, nxtWord16, nxtWord32, nxtAutoInc:   result := qfkInteger;
    nxtBoolean:                                    result := qfkLogical;
    nxtSingle, nxtDouble, nxtCurrency,
      nxtExtended:                                 result := qfkFloat;
    nxtDate, nxtTime, nxtDateTime:                 result := qfkDateTime;
    nxtBlob, nxtBlobGraphic, nxtByteArray:         result := qfkBinary;
    nxtBlobMemo:                                   result := qfkLongString;
  else
    result := qfkString; // Just to shut up the compiler
    tiFmtException('Invalid FQuery.Fields[ pIndex ].DataType <' +
      GetEnumName(TypeInfo(TnxFieldType), Ord(aDataType)) +
      '>', 'no class', 'FieldKind');
  end;
end;

procedure TtiDatabaseNx1.ReadMetaDataFields(pData: TPersistent);
var
  i: Integer;
  nxTable: TnxTable;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  ldict: TnxDataDictionary;
begin
  lTable := (pData as TtiDBMetaDataTable);
  nxTable := TnxTable.Create(nil);
  try
    nxTable.Database := FDatabase;
    nxTable.TableName := lTable.Name;
    nxTable.Open;
    ldict := nxTable.Dictionary;
    for i := 0 to ldict.FieldCount - 1 do begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := ldict.FieldDescriptor[i].Name;
      lField.Kind := NxFieldDataTypeToTIQueryFieldKind(
        ldict.FieldDescriptor[i].fdType);
      if lField.Kind = qfkString then
        lField.Width := nxTable.FieldDefs[i].Size
      else
        lField.Width := 0;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
    nxTable.Close;
  finally
    nxTable.Free;
  end;
end;

procedure TtiDatabaseNx1.ReadMetaDataTables(pData: TPersistent);
var
  i: Integer;
  aList: TStringList;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (pData as TtiDBMetaData);
  aList := TStringList.Create;
  try
    FDatabase.GetTableNames(aList);
    for i := 0 to aList.Count - 1 do begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := Trim(aList[i]);
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
    end;
    lMetaData.ObjectState := posClean;
  finally
    aList.Free;
  end; // try/finally
end;

procedure TtiDatabaseNx1.RollBack;
begin
  FDatabase.RollBack;
end;

procedure TtiDatabaseNx1.SetConnected(pbValue: boolean);
begin

  if (not pbValue) then begin
    FDatabase.Connected := False;
    Exit; //==>
  end;

  if FDatabase.Connected then begin
    FDatabase.Connected := False;
  end; { if }
  
  if gTIPerMgr.ReadConfigValue('dummy', 'UseAliasPath') = '0' then
    FDatabase.AliasName := DatabaseName
  else
    FDatabase.AliasPath := DatabaseName;

  try
    SetLoginParams;
    FDatabase.Connected := True;
  except
    on e: Exception do begin
      tiAppError('Error attempting to connect to database.' + CrLf(2) +
        e.Message);
      Halt;
    end;
  end;
end;

procedure TtiDatabaseNx1.SetLoginParams;
begin
  if (UserName <> '') and not SameText(UserName, 'null') then
    FSession.UserName := UserName;

  if (Password <> '') and not SameText(Password, 'null') then
    FSession.Password := Password;
end;

procedure TtiDatabaseNx1.StartTransaction;
begin
  if InTransaction then
    tiFmtException(
      'Attempt to start a transaction but transaction already exists.',
      ClassName,
      'StartTransaction');
  FDatabase.StartTransaction;
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

initialization
  gtiPerMgr.RegPerLayers.RegisterPersistenceLayer(
    cTIPersistNx1,
    TtiDBConnectionPoolDataAbs,
    TtiQueryNx1,
    TtiDatabaseNx1);

finalization
  gtiPerMgr.RegPerLayers.UnRegisterPersistenceLayer(cTIPersistNx1);
  uNxServerEngine.Free;
  uNxTransport.Free;

end.


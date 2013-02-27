{
    This file is part of the tiOPF project.

    See the file license.txt, included in this distribution,
    for details about redistributing tiOPF.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A NexusDB persistence layer for tiOPF.
      Introduced on 2011-06-13 by Dmitry Konnov(konnov_d@mail.ru)
}
unit tiQueryNexusDB;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiQuery
  ,tiObject
  ,tiOPFManager
  ,tiQueryDataset
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ,tiConstants
  ,DB
  ,Windows
  ,nxdb
  ,nxllComponent
  ,nxsdServerEngine
  ,nxsrServerEngine
  ,nxseAutoComponent
  ,nxsrSqlEngineBase
  ,nxsqlEngine;

type

  TtiPersistenceLayerNexusDB = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseNexusDB = class(TtiDatabaseSQL)
  private
    FnxDatabase: TnxDatabase;
    FnxSesion: TnxSession;
    FCurrentThreadID: DWord;
    FInTransaction: Boolean;
  protected
    function GetConnectionString: string; virtual;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure SetupDBParams; virtual;
    function  FieldDataTypeToTIQueryFieldKind(pDataType: TFieldType): TtiQueryFieldKind;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    TIQueryClass: TtiQueryClass; override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    procedure   ExecSQL(const pSQL: string; AParams: TtiQueryParams = nil); override;
    property    ConnectionString : string read GetConnectionString;
  end;


  TtiQueryNexusDB = class(TtiQueryDataset)
  private
    FnxQuery: TnxQuery;
    FSQL     : TStringList;
    procedure DoOnChangeSQL(Sender: TObject);
    procedure SetParamAsVariant(const AName: string;const AValue: Variant);
  protected
    function  GetFieldAsBoolean(const AName: string): boolean  ; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    procedure SetActive(const AValue: boolean); override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsFloat(const AName: string): extended;override;
    function  GetParamAsInteger(const AName: string): Int64;override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: boolean);override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended);override;
    procedure SetParamAsInteger(const AName: string; const AValue: Int64);override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;
    function  GetParamIsNull(const AName: String): Boolean; override;
    procedure SetParamIsNull(const AName: String; const AValue: Boolean); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    function    ExecSQL: integer; override;
    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;
    procedure   AssignParamToStream(  const AName : string; const AValue : TStream); override;
    procedure   AssignParamFromStream(const AName : string; const AValue : TStream); override;
    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase;  override;
    procedure   Reset; override;
    function    HasNativeLogicalType : boolean; override;
  end;


implementation

uses
  tiLog
  ,tiUtils
  ,TypInfo
  ,tiWin32
  ,tiExcept
  ,SysUtils
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
 ;

var
  GnxServerEngine: TnxServerEngine;
  GnxSQLEngine: TnxSQLEngine;

constructor TtiQueryNexusDB.Create;
begin
  inherited;
  FnxQuery:= TnxQuery.Create(Nil);
  FnxQuery.DisableControls;
  Dataset:= FnxQuery;
  Params:= FnxQuery.Params;
  FSQL:= TStringList.Create;
  FSQL.OnChange:= DoOnChangeSQL;
  FSupportsRowsAffected:= True;
end;

destructor TtiQueryNexusDB.Destroy;
begin
  FnxQuery.Free;
  FSQL.Free;
  inherited;
end;

procedure TtiQueryNexusDB.Close;
begin
  Active:= False;
end;

function TtiQueryNexusDB.ExecSQL: integer;
var
  ls: string;
begin
  try
    FnxQuery.Prepared:= True;
    FnxQuery.ExecSQL;
    Result:= FnxQuery.RowsAffected;
  except
    on e:exception do
    begin
      ls := e.Message;
      Log(ls);
      raise;
    end;
  end;
end;

function TtiQueryNexusDB.GetFieldAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FnxQuery.FieldByName(AName).Value;
  Result := lValue = -1;
end;

function TtiQueryNexusDB.GetParamAsBoolean(const AName: string): boolean;
var
  lValue : variant;
begin
  lValue := FnxQuery.Params.ParamByName(AName).Value;
  Result := lValue = -1;
end;

function TtiQueryNexusDB.GetParamAsDateTime(const AName: string): TDateTime;
var
  lValue : string;
begin
  lValue := FnxQuery.Params.ParamByName(AName).Value;
  Result := StrToDateTime(lValue);
end;

function TtiQueryNexusDB.GetParamAsFloat(const AName: string): extended;
begin
  Result := FnxQuery.Params.ParamByName(AName).Value;
end;

function TtiQueryNexusDB.GetParamAsInteger(const AName: string): Int64;
begin
  Result := Longint(FnxQuery.Params.ParamByName(AName).Value);
end;

function TtiQueryNexusDB.GetParamAsString(const AName: string): string;
begin
  Result := FnxQuery.Params.ParamByName(AName).Value;
end;

function TtiQueryNexusDB.GetSQL: TStrings;
begin
  FSQL.OnChange:= nil;
  try
    FSQL.Assign(FnxQuery.SQL);
  finally
    FSQL.OnChange:= DoOnChangeSQL;
  end;
  Result:= FSQL;
end;

procedure TtiQueryNexusDB.Open;
begin
  FnxQuery.Prepared := False;
  Active:= true;
end;

function TtiQueryNexusDB.ParamCount: integer;
begin
  Result:= FnxQuery.Params.Count;
end;

function TtiQueryNexusDB.ParamName(AIndex: integer): string;
begin
  Result:= FnxQuery.Params.Items[AIndex].Name;
end;

procedure TtiQueryNexusDB.SetActive(const AValue: boolean);
begin
  if AValue = FnxQuery.Active then
    Exit; //==>
  try
    FnxQuery.Active := AValue;
  except
    on e:exception do
      raise Exception.Create(SQLAndParamsAsString + ' Message: ' + e.message);
  end;

end;

procedure TtiQueryNexusDB.SetParamAsBoolean(const AName: string;
  const AValue: boolean);
var
  i: Integer;
begin
  for i := 0 to FnxQuery.Params.Count - 1 do
    if SameText(AName, FnxQuery.Params.Items[i].Name) then
    begin
      if AValue then
        FnxQuery.Params.Items[i].Value := True
      else
        FnxQuery.Params.Items[i].Value := False;
    end;
end;

procedure TtiQueryNexusDB.SetParamAsDateTime(const AName : string;
  const AValue: TDateTime);
begin
  SetParamAsVariant(AName, AValue);
end;

procedure TtiQueryNexusDB.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  SetParamAsVariant(AName, AValue);
end;

procedure TtiQueryNexusDB.SetParamAsInteger(const AName: string;
  const AValue: Int64);
var
  i: Integer;
begin
  for i := 0 to FnxQuery.Params.Count - 1 do
    if SameText(AName, FnxQuery.Params.Items[i].Name) then
      FnxQuery.Params.Items[i].Value := Longint(AValue);
end;

procedure TtiQueryNexusDB.SetParamAsString(const AName, AValue: string);
begin
  SetParamAsVariant(AName, AValue);
end;

procedure TtiQueryNexusDB.SetParamAsVariant(const AName: string;
  const AValue: Variant);
var
  i: Integer;
begin
  for i := 0 to FnxQuery.Params.Count - 1 do
    if SameText(AName, FnxQuery.Params.Items[i].Name) then
      FnxQuery.Params.Items[i].Value := AValue;
end;

procedure TtiQueryNexusDB.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TtiQueryNexusDB.AssignParamToStream(const AName: string;
  const AValue: TStream);
var
  lBinData: OleVariant;
  lDataPtr: Pointer;
  lHigh, lLow, lLen: Integer;
  lParameter : TParam;
begin
  Assert(AValue <> nil, 'Stream not assigned');
  lParameter := FnxQuery.Params.ParamByName(AName);
  lLow    := VarArrayLowBound(lParameter.Value, 1);
  lHigh   := VarArrayHighBound(lParameter.Value, 1);
  lLen    := lHigh - lLow + 1;
  lBinData := VarArrayCreate([0, lLen], varByte);
  lBinData := lParameter.Value;
  lDataPtr := VarArrayLock(lBinData);
  try
    AValue.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;
end;

procedure TtiQueryNexusDB.AssignParamFromStream(const AName: string;
  const AValue: TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  FnxQuery.Params.ParamByName(AName).LoadFromStream(AValue, ftBlob);
end;

procedure TtiQueryNexusDB.AttachDatabase(ADatabase: TtiDatabase);
var
  lSQL : string;
begin
  lSQL := FnxQuery.SQL.Text;
  FnxQuery.SQL.Clear;
  FnxQuery.Database:= (ADatabase as TtiDatabaseNexusDB).FnxDatabase;
  FnxQuery.Session:= (ADatabase as TtiDatabaseNexusDB).FnxSesion;
  FnxQuery.SQL.Text:= lSQL;
  Database:= ADatabase;
end;

procedure TtiQueryNexusDB.DetachDatabase;
begin
  inherited DetachDatabase;
  if FnxQuery.Active then
    FnxQuery.Active := false;
  FnxQuery.Database := nil;
end;

procedure TtiQueryNexusDB.Reset;
begin
  Active := false;
  FnxQuery.SQL.Clear;
  FnxQuery.Params.Clear;
end;

constructor TtiDatabaseNexusDB.Create;
begin
  inherited Create;
  FCurrentThreadID:= 0;
  FnxDatabase:= TnxDatabase.Create(Nil);
  FnxSesion:= TnxSession.Create(Nil);
  FnxSesion.ServerEngine:= GnxServerEngine;
  FnxDatabase.Session:= FnxSesion;
end;

destructor TtiDatabaseNexusDB.Destroy;
begin
  FreeAndNil(FnxSesion);
  FreeAndNil(FnxDatabase);
  inherited;
end;

procedure TtiDatabaseNexusDB.Commit;
begin
  if not InTransaction then
    raise Exception.Create('Attempt to commit but not in a transaction.');
  FnxDatabase.Commit;
  FInTransaction:= False;
end;

function TtiDatabaseNexusDB.InTransaction: boolean;
begin
  Result:= FInTransaction;
end;

procedure TtiDatabaseNexusDB.RollBack;
begin
  FnxDatabase.Rollback;
  FInTransaction:= False;
end;

procedure TtiDatabaseNexusDB.StartTransaction;
begin
  if InTransaction then
    raise Exception.Create(
    'Attempt to start a transaction but transaction already exists.');
  FnxDatabase.StartTransaction;
  FInTransaction:= True;
end;

function TtiDatabaseNexusDB.TIQueryClass: TtiQueryClass;
begin
  Result:= TtiQueryNexusDB;
end;

procedure TtiDatabaseNexusDB.ReadMetaDataTables(AData : TtiDBMetaData);
var
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
  lsl: TStringList;
  i: integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    FnxDatabase.GetTableNames(lsl);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := lsl.Strings[i];
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lMetaData.ObjectState := posClean;
    end;
  finally
    lsl.Free;
  end;
end;

procedure TtiDatabaseNexusDB.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lDelphiTable: TnxTable;
  i: integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TnxTable.Create(Nil);
  try
    lDelphiTable.Database:= FnxDatabase;
    lDelphiTable.Session:= FnxSesion;
    lDelphiTable.TableName := lTable.Name;
    lDelphiTable.Active:= True;
    lDelphiTable.FieldDefs.Update;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := lDelphiTable.FieldDefs[i].Name;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
  finally
    FreeAndNil(lDelphiTable);
  end;
end;

class procedure TtiDatabaseNexusDB.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string; const AParams: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class procedure TtiDatabaseNexusDB.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseNexusDB.DatabaseExists(const ADatabaseName,
  AUserName, APassword: string; const AParams: string): boolean;
begin
  Result := false;
  Assert(false, 'DatabaseExists not implemented in ' + ClassName);
end;

procedure TtiDatabaseNexusDB.ExecSQL(const pSQL: string; AParams:
  TtiQueryParams = nil);
var
  lQuery : TtiQuery;
  lHadToStartTransaction : boolean;
  lMessage : string;
begin
  lQuery := CreateAndAttachTIQuery;
  try
    lQuery.SQLText := pSQL;
    //NexusDB requires not in Transaction if lQuery.QueryType = qtDDL
    lHadToStartTransaction := not InTransaction
      and (lQuery.QueryType <> qtDDL);
    if lHadToStartTransaction then
      StartTransaction;
    lQuery.AssignParams(AParams);
    try
      lQuery.ExecSQL;
      if lHadToStartTransaction then
        Commit;
    except
      on e:exception do
      begin
        lMessage := e.message;
        if (lHadToStartTransaction {and InTransaction}) then
        begin
          try
            RollBack;
          except
            on e:exception do
              lMessage := lMessage + tiLineEnd +
                'Error rolling transaction after SQL failed:'
                  + tiLineEnd + e.message;
          end;
        end;
        raise EtiOPFProgrammerException.Create(lMessage);
      end;
    end;
  finally
    lQuery.Free;
  end;
end;

function TtiQueryNexusDB.GetParamIsNull(const AName: String): Boolean;
begin
  Result := VarIsNull(FnxQuery.Params.ParamByName(AName).Value);
end;

procedure TtiQueryNexusDB.SetParamIsNull(const AName: String;
  const AValue: Boolean);
begin
  if AValue then
    SetParamAsVariant(AName, Null);
end;

function TtiDatabaseNexusDB.GetConnected: boolean;
begin
  Result:= FnxDatabase.Connected;
end;

function TtiDatabaseNexusDB.GetConnectionString: string;
begin
  Result:= DatabaseName;
end;

procedure TtiDatabaseNexusDB.SetConnected(AValue: boolean);
var
  lsErrorMessage : string;
begin
  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FnxDatabase.Connected := False;
    Exit; //==>
  end;
  SetupDBParams;
  lsErrorMessage := '';
  try
    FnxDatabase.Connected := true;
  except
    on e:exception do
    begin
      lsErrorMessage := e.message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName,
      UserName, Password, lsErrorMessage);
    end;
    on e:Exception do
    begin
      lsErrorMessage :=
        'Error message: ' + e.Message;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName,
      UserName, Password, lsErrorMessage);
    end;
  end;
end;

procedure TtiDatabaseNexusDB.SetupDBParams;
begin
  FnxDatabase.AliasPath:= ConnectionString;
end;

// This function is cloned in tiQueryBDEAbs - must move it to a common location,
//but without pulling DB.pas into any packages where it is not required
function TtiDatabaseNexusDB.FieldDataTypeToTIQueryFieldKind(
  pDataType : TFieldType): TtiQueryFieldKind;
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
    ftString, ftWideString :                    Result := qfkString  ;
    ftSmallint, ftInteger, ftWord, ftLargeint : Result := qfkInteger ;
    ftBoolean :                                 Result := qfkLogical ;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd :      Result := qfkFloat   ;
    ftDate, ftTime, ftDateTime :                Result := qfkDateTime;
    ftBlob, ftGraphic, ftVarBytes :             Result := qfkBinary  ;
    ftMemo, ftFmtMemo:                          Result := qfkLongString;
    {$ifdef DELPHI10ORABOVE}
    ftWideMemo :                                Result := qfkLongString;
    {$endif}
    else
      raise Exception.Create('Invalid FQuery.Fields[ AIndex ].DataType <' +
                      GetEnumName(TypeInfo(TFieldType), Ord(pDataType)) + '>');
    end;
//    ftUnknown,
//    ftBytes, ftVarBytes, ftAutoInc,
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
//    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
//    ftVariant, ftInterface, ftIDispatch, ftGuid

end;

function TtiQueryNexusDB.HasNativeLogicalType: boolean;
begin
  Result := true;
end;

function TtiQueryNexusDB.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lValue : variant;
begin
  lValue := FnxQuery.Fields[AIndex].Value;
  Result := lValue = -1;
end;

procedure TtiQueryNexusDB.DoOnChangeSQL(Sender: TObject);
begin
  FnxQuery.SQL.Assign(Sender as TStringList);
end;

procedure TtiPersistenceLayerNexusDB.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistADOSQLServer;
  APersistenceLayerDefaults.DatabaseName:=
    CDefaultDatabaseDirectory + CDefaultDatabaseName + '.XXX';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'XXX';
  APersistenceLayerDefaults.Password:= 'XXX';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerNexusDB.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseNexusDB;
end;

function TtiPersistenceLayerNexusDB.GetPersistenceLayerName: string;
begin
  result:= cTIPersistNexusDB;
end;

function TtiPersistenceLayerNexusDB.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryNexusDB;
end;


initialization
  GnxSQLEngine:= TnxSQLEngine.Create(Nil);
  GnxServerEngine:= TnxServerEngine.Create(Nil);
  GnxServerEngine.SqlEngine:= GnxSQLEngine;
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(TtiPersistenceLayerNexusDB);

finalization
  FreeAndNil(GnxServerEngine);
  FreeAndNil(GnxSQLEngine);
  if not tiOPFManager.ShuttingDown then
   GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistNexusDB);

end.

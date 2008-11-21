unit tiFlashFilerAbs;

{$I tiDefines.inc}

interface

uses
   tiQuery
  ,Classes
  ,tiPersistenceLayers
  ,ffsrintm
  ,ffSrEng
  ,ffDB
  ;


type
  TtiDatabaseFlashFilerAbs = class(TtiDatabaseSQL)
  private
    FClient: TffClient;
    FDatabase: TffDataBase;
    FSession: TffSession;
    FEngine: TffIntermediateServerEngine;
  protected
//    procedure SetConnected(AValue: boolean); override;
    function  GetConnected: boolean; override;
    function  FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    procedure SetFFEngine(const AffServer: TffIntermediateServerEngine);
    procedure InternalConnect; virtual;
    procedure InternalDisconnect; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;

    procedure       DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure       CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure       StartTransaction; override;
    function        InTransaction: boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    procedure       ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;

    property        FFClient: TffClient read FClient write FClient;
    property        FFDatabase: TffDatabase read FDatabase write FDatabase;
    property        FFEngine: TffIntermediateServerEngine read FEngine
                      write FEngine;
    property        FFSession: TffSession read FSession write FSession; 
  end;

  
  TtiQueryFlashFiler = class(TtiQuerySQL)
  private
    FffQuery: TffQuery;
    procedure Prepare;
  protected
    function  GetFieldAsString(const AName: string): string; override;
    function  GetFieldAsFloat(const AName: string): extended; override;
    function  GetFieldAsBoolean(const AName: string): boolean; override;
    function  GetFieldAsInteger(const AName: string): Int64; override;
    function  GetFieldAsDateTime(const AName: string): TDateTime; override;

    function  GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function  GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function  GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function  GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function  GetEOF: boolean; override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsFloat(const AName: string): extended; override;
    function  GetParamAsInteger(const AName: string): Int64; override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: Int64); override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;

    function  GetParamIsNull(const AName: string): Boolean; override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;
    function  GetFieldIsNull(const AName: string): Boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    procedure   ExecSQL; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure   AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AStream : TStream); override;
    procedure   AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;
  end;




implementation

uses
   tiUtils
  ,tiLog
  ,TypInfo
  ,tiOPFManager
  ,tiObject
  ,tiConstants
  ,tiExcept
  ,SysUtils, DB
{$IFDEF DELPHI6ORABOVE}
  ,Variants
{$ENDIF}
 ;


 { TtiQueryFlashFiler }

procedure TtiQueryFlashFiler.AssignFieldAsStream(const AName: string;
  const AStream: TStream);
begin
end;

procedure TtiQueryFlashFiler.AssignFieldAsStreamByIndex(AIndex: integer;
  const AStream: TStream);
begin

end;

procedure TtiQueryFlashFiler.AssignParamFromStream(const AName: string;
  const AStream: TStream);
//var
//  LPos: integer;
begin
//  Assert(AStream <> nil, 'Stream not assigned');
//  AStream.Position := 0;
//  Prepare;
//  LPos:= AStream.Position;
//  FffQuery.Params.ParamByName(UpperCase(AName)).LoadFromStream(AStream);
//  AStream.Position:= LPos;
end;

procedure TtiQueryFlashFiler.AssignParams(const AParams,
  AWhere: TtiQueryParams);
begin
  if AParams = nil then
    Exit;
  Prepare;
  inherited;
end;

procedure TtiQueryFlashFiler.AssignParamToStream(const AName: string;
  const AStream: TStream);
begin

end;

procedure TtiQueryFlashFiler.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);

  FffQuery.DatabaseName :=
    TtiDatabaseFlashFilerAbs(ADatabase).FFDatabase.DatabaseName;
end;

procedure TtiQueryFlashFiler.Close;
begin
  Active := false;
end;

constructor TtiQueryFlashFiler.Create;
begin
  inherited;

  FffQuery := TffQuery.Create(nil);
end;

destructor TtiQueryFlashFiler.Destroy;
begin
  FffQuery.Free;

  inherited;
end;

procedure TtiQueryFlashFiler.DetachDatabase;
begin
  inherited DetachDatabase;

  if FffQuery.Active then
    FffQuery.Close;

  FffQuery.SessionName := '';
  FffQuery.DatabaseName := '';
end;

procedure TtiQueryFlashFiler.ExecSQL;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Prepare;
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);

  FffQuery.ExecSQL;
end;

function TtiQueryFlashFiler.FieldCount: integer;
begin
  result := FffQuery.FieldCount;
end;

function TtiQueryFlashFiler.FieldIndex(const AName: string): integer;
begin
  result := FffQuery.FieldByName(AName).Index;
end;

function TtiQueryFlashFiler.FieldKind(AIndex: integer): TtiQueryFieldKind;
begin
  Case FffQuery.Fields[AIndex].DataType of
    ftString:   result := qfkString;
    ftInteger,
    ftSmallInt,
    ftLargeInt,
    ftWord:     result := qfkInteger;
    ftFloat,
    ftCurrency: result := qfkFloat;
    ftBoolean:  result := qfkLogical;
    ftBCD: ;
    ftDate,
    ftTime,
    ftTimeStamp,
    ftDateTime: result := qfkDateTime;
    ftBytes: ;
    ftVarBytes: ;
    ftAutoInc: ;
    ftBlob:     result := qfkBinary;
    ftMemo,
    ftWideMemo,
    ftFmtMemo:  result := qfkLongString;
    ftGraphic: ;
    ftParadoxOle: ;
    ftDBaseOle: ;
    ftTypedBinary: ;
    ftCursor: ;
    ftFixedChar: ;
    ftWideString: ;
    ftADT: ;
    ftArray: ;
    ftReference: ;
    ftDataSet: ;
    ftOraBlob: ;
    ftOraClob: ;
    ftVariant: ;
    ftInterface: ;
    ftIDispatch: ;
    ftGuid: ;
    ftFMTBcd: ;
    ftFixedWideChar: ;
  else
    raise EtiOPFInternalException.Create('Invalid Flash Filer field type');  
  End;
end;

function TtiQueryFlashFiler.FieldName(AIndex: integer): string;
begin
  result := FffQuery.Fields[AIndex].Name;
end;

function TtiQueryFlashFiler.FieldSize(AIndex: integer): integer;
begin
  result := FffQuery.Fields[AIndex].Size;
end;

function TtiQueryFlashFiler.GetActive: boolean;
begin
  result := FffQuery.Active;
end;

function TtiQueryFlashFiler.GetEOF: boolean;
begin
  result := FffQuery.Eof;
end;

function TtiQueryFlashFiler.GetFieldAsBoolean(const AName: string): boolean;
begin
  result := FffQuery.FieldByName(AName).AsBoolean;
end;

function TtiQueryFlashFiler.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
begin
  result := FffQuery.Fields[AIndex].AsBoolean;
end;

function TtiQueryFlashFiler.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FffQuery.FieldByName(AName).AsDateTime;
end;

function TtiQueryFlashFiler.GetFieldAsDateTimeByIndex(
  AIndex: Integer): TDateTime;
begin
  result := FffQuery.Fields[AIndex].AsDateTime;
end;

function TtiQueryFlashFiler.GetFieldAsFloat(const AName: string): extended;
begin
  result := FffQuery.FieldByName(AName).AsFloat;
end;

function TtiQueryFlashFiler.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  result := FffQuery.Fields[AIndex].AsFloat;
end;

function TtiQueryFlashFiler.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FffQuery.FieldByName(AName).AsInteger;
end;

function TtiQueryFlashFiler.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  result := FffQuery.Fields[AIndex].AsInteger;
end;

function TtiQueryFlashFiler.GetFieldAsString(const AName: string): string;
begin
  result := FffQuery.FieldByName(AName).AsString;
end;

function TtiQueryFlashFiler.GetFieldAsStringByIndex(AIndex: Integer): string;
begin
  result := FffQuery.Fields[AIndex].AsString;
end;

function TtiQueryFlashFiler.GetFieldIsNull(const AName: string): Boolean;
begin
  result := FffQuery.FieldByName(AName).IsNull;
end;

function TtiQueryFlashFiler.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  result := FffQuery.Fields[AIndex].IsNull;
end;

function TtiQueryFlashFiler.GetParamAsBoolean(const AName: string): boolean;
begin
  result := FffQuery.ParamByName(AName).AsBoolean;
end;

function TtiQueryFlashFiler.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := FffQuery.ParamByName(AName).AsDateTime;
end;

function TtiQueryFlashFiler.GetParamAsFloat(const AName: string): extended;
begin
  result := FffQuery.ParamByName(AName).AsFloat;
end;

function TtiQueryFlashFiler.GetParamAsInteger(const AName: string): Int64;
begin
  result := FffQuery.ParamByName(AName).AsInteger;
end;

function TtiQueryFlashFiler.GetParamAsString(const AName: string): string;
begin
  result := FffQuery.ParamByName(AName).AsString;
end;

function TtiQueryFlashFiler.GetParamIsNull(const AName: string): Boolean;
begin
  result := FffQuery.ParamByName(AName).IsNull;
end;

function TtiQueryFlashFiler.GetSQL: TStrings;
begin
  result := FffQuery.SQL;
end;

function TtiQueryFlashFiler.HasNativeLogicalType: boolean;
begin
  result := true;
end;

procedure TtiQueryFlashFiler.Next;
begin
  FffQuery.Next;
end;

procedure TtiQueryFlashFiler.Open;
begin
  Log(ClassName + ': ' + tiNormalizeStr(self.SQLText), lsSQL);
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);

  Active := true;
end;

function TtiQueryFlashFiler.ParamCount: integer;
begin
  result := FffQuery.ParamCount;
end;

function TtiQueryFlashFiler.ParamName(AIndex: integer): string;
begin
  result := FffQuery.Params[AIndex].Name;
end;

procedure TtiQueryFlashFiler.Prepare;
begin
  if FffQuery.Prepared then
    Exit; //==>

  FffQuery.Prepare;
end;

procedure TtiQueryFlashFiler.Reset;
begin
  Active := false;
  FffQuery.SQL.Clear;
end;

procedure TtiQueryFlashFiler.SetActive(const AValue: boolean);
begin
  FffQuery.Active := AValue;
end;

procedure TtiQueryFlashFiler.SetParamAsBoolean(const AName: string;
  const AValue: boolean);
begin
  Prepare;
  FffQuery.Params.ParamValues[AName] := AValue;
end;

procedure TtiQueryFlashFiler.SetParamAsDateTime(const AName: string;
  const AValue: TDateTime);
begin
  Prepare;
  FffQuery.Params.ParamValues[AName] := AValue;
end;

procedure TtiQueryFlashFiler.SetParamAsFloat(const AName: string;
  const AValue: extended);
begin
  Prepare;
  FffQuery.Params.ParamValues[AName] := AValue;
end;

procedure TtiQueryFlashFiler.SetParamAsInteger(const AName: string;
  const AValue: Int64);
begin
  Prepare;
  FffQuery.Params.ParamValues[AName] := AValue;
end;

procedure TtiQueryFlashFiler.SetParamAsString(const AName, AValue: string);
begin
  Prepare;
  FffQuery.Params.ParamValues[AName] := AValue;
end;

procedure TtiQueryFlashFiler.SetParamIsNull(const AName: string;
  const AValue: Boolean);
begin
  Prepare;
  FffQuery.ParamByName(AName).Clear;
end;

procedure TtiQueryFlashFiler.SetSQL(const AValue: TStrings);
begin
  FffQuery.SQL.Assign(AValue);
end;

{ TtiDatabaseFlashFiler }

procedure TtiDatabaseFlashFilerAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to commit but not in a transaction.');
  Log(ClassName + ': [Commit Trans]', lsSQL);
  FDatabase.Commit;
end;

constructor TtiDatabaseFlashFilerAbs.Create;
begin
  inherited;

  FEngine := nil;

  FClient := TffClient.Create(nil);
  FClient.AutoClientName := True;

  FSession := TffSession.Create(Nil);
  FSession.AutoSessionName := True;

  FDatabase := TffDatabase.Create(Nil);
  FDatabase.AutoDatabaseName := True;
end;

class procedure TtiDatabaseFlashFilerAbs.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string);
begin
  Assert(false, 'Not yet implemented');
end;

procedure TtiDatabaseFlashFilerAbs.CreateTable(
  const ATableMetaData: TtiDBMetaDataTable);
begin
  Assert(false, 'Not yet implemented');
end;

class function TtiDatabaseFlashFilerAbs.DatabaseExists(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  result := false;
  Assert(false, 'Not yet implemented');
end;

destructor TtiDatabaseFlashFilerAbs.Destroy;
begin
  FDatabase.Free;
  FSession.Free;
  FClient.Free;

  inherited;
end;

procedure TtiDatabaseFlashFilerAbs.DropTable(
  const ATableMetaData: TtiDBMetaDataTable);
begin
  Assert(false, 'Not yet implemented');
end;

function TtiDatabaseFlashFilerAbs.FieldMetaDataToSQLCreate(
  const AFieldMetaData: TtiDBMetaDataField): string;
begin
  result := '';
end;

function TtiDatabaseFlashFilerAbs.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseFlashFilerAbs.InternalConnect;
begin
  FClient.Open;
  FSession.ClientName := FClient.ClientName;
  FSession.Open;
  FDatabase.SessionName := FSession.SessionName;  
  FDatabase.Open;
end;

procedure TtiDatabaseFlashFilerAbs.InternalDisconnect;
begin
  FClient.Close;
  FSession.Close;
  FDatabase.Close;
end;

function TtiDatabaseFlashFilerAbs.InTransaction: boolean;
begin
  result := FDatabase.InTransaction;
end;

procedure TtiDatabaseFlashFilerAbs.ReadMetaDataFields(AData: TtiDBMetaDataTable);
begin
  Assert(false, 'Not implemented');
end;

procedure TtiDatabaseFlashFilerAbs.ReadMetaDataTables(AData: TtiDBMetaData);
begin
  Assert(false, 'Not implemented');
end;

procedure TtiDatabaseFlashFilerAbs.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  FDatabase.RollBack;
end;

procedure TtiDatabaseFlashFilerAbs.SetFFEngine(
  const AffServer: TffIntermediateServerEngine);
begin
  FEngine := AffServer;
end;

procedure TtiDatabaseFlashFilerAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  Log(ClassName + ': [Start Trans]', lsSQL);
  FDatabase.StartTransaction;
end;

function TtiDatabaseFlashFilerAbs.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseFlashFilerAbs.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryFlashFiler;
end;



end.
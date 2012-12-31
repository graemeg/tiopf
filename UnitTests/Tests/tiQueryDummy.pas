unit tiQueryDummy;

{$I tiDefines.inc}

interface
uses
  tiPersistenceLayers,
  tiQuery,
  Classes;

type

  TtiPersistenceLayerDummy = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
    const CTIPersistDummy = 'Dummy';
  end;

  TtiDatabaseDummy = class(TtiDatabaseSQL)
  private
    FIsConnected : boolean;
    FTransactionActive: boolean;
  public
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    procedure       StartTransaction; override;
    function        InTransaction: boolean; override;
    procedure       Commit; override;
    procedure       RollBack; override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
    procedure       SetConnected(AValue: boolean); override;
    function        GetConnected: boolean; override;
  end;

  TtiQueryDummy = class(TtiQuerySQL)
  private
    FActive : boolean;
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
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    function    ExecSQL: integer; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure   AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AStream : TStream); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

  end;

implementation
uses
  tiUtils,
  tiLog,
  TypInfo,
  SysUtils,
  Variants,
  tiOPFManager,
  tiObject,
  tiConstants,
  tiExcept;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryIBX
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiQueryDummy.Create;
begin
  inherited;
  FActive := false;
end;

procedure TtiQueryDummy.Close;
begin
  FActive := false;
end;


function TtiQueryDummy.ExecSQL: integer;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  Result := -1;
end;


function TtiQueryDummy.GetFieldAsBoolean(const AName: string): boolean;
begin
  result := true;
end;


function TtiQueryDummy.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := 1.23;
end;


function TtiQueryDummy.GetFieldAsFloat(const AName: string): extended;
begin
  result := 12.34;
end;


function TtiQueryDummy.GetFieldAsInteger(const AName: string): Int64;
begin
  result := 12345;
end;

function TtiQueryDummy.GetFieldAsString(const AName: string): string;
begin
  result := '123456';
end;

function TtiQueryDummy.GetFieldAsStringByIndex(AIndex: Integer): string    ;
begin
  Result := '1234567';
end;

function TtiQueryDummy.GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ;
begin
  Result := 123.45678;
end;

function TtiQueryDummy.GetFieldAsBooleanByIndex(AIndex: Integer): boolean ;
begin
  result := true
end;

function TtiQueryDummy.GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ;
begin
  Result := 123456789;
end;

function TtiQueryDummy.GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime;
begin
  Result := 9.87;
end;

function TtiQueryDummy.GetFieldIsNullByIndex(AIndex: Integer):Boolean      ;
begin
  Result := True;
end;

function TtiQueryDummy.GetActive: boolean;
begin
  result := FActive;
end;

function TtiQueryDummy.GetEOF: boolean;
begin
  result := False;
end;

function TtiQueryDummy.GetParamAsBoolean(const AName: string): boolean;
begin
  result := true;
end;


function TtiQueryDummy.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := 9.876;
end;


function TtiQueryDummy.GetParamAsFloat(const AName: string): extended;
begin
  result := 98.765;
end;


function TtiQueryDummy.GetParamAsInteger(const AName: string): Int64;
begin
  result := 987654;
end;

function TtiQueryDummy.GetParamAsString(const AName: string): string;
begin
  result := '9876543';
end;

function TtiQueryDummy.GetSQL: TStrings;

begin
  result := TStringList.Create;
  result.Add('aa');
  result.Add('bb');
end;

procedure TtiQueryDummy.Next;
begin

end;

procedure TtiQueryDummy.Open;
begin
  FActive := true;
end;


function TtiQueryDummy.ParamCount: integer;
begin
  result := 98765432;
end;


function TtiQueryDummy.ParamName(AIndex: integer): string;
begin
  result := '987654321'
end;


procedure TtiQueryDummy.SetActive(const AValue: boolean);
begin
  FActive := true;
end;


procedure TtiQueryDummy.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
end;


procedure TtiQueryDummy.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
end;


procedure TtiQueryDummy.SetParamAsFloat(const AName: string; const AValue: extended);
begin
end;


procedure TtiQueryDummy.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
end;

procedure TtiQueryDummy.SetParamAsString(const AName, AValue: string);
begin
end;

procedure TtiQueryDummy.SetSQL(const AValue: TStrings);
begin
end;

procedure TtiQueryDummy.AssignParamFromStream(const AName: string; const AStream: TStream);
begin
end;

procedure TtiQueryDummy.AssignParamToStream(const AName: string; const AStream: TStream);
begin
end;


procedure TtiQueryDummy.AssignFieldAsStream(const AName: string; const AStream: TStream);
begin
end;

procedure TtiQueryDummy.AssignFieldAsStreamByIndex(AIndex : integer; const AStream : TStream);
begin
end;

procedure TtiQueryDummy.AttachDatabase(ADatabase: TtiDatabase);
begin
end;


procedure TtiQueryDummy.DetachDatabase;
begin
  inherited DetachDatabase;
end;

function TtiQueryDummy.FieldCount: integer;
begin
  result := 1;
end;


function TtiQueryDummy.FieldName(AIndex: integer): string;
begin
  result := '11';
end;


procedure TtiQueryDummy.Reset;
begin
  FActive := false;
end;

function TtiQueryDummy.FieldIndex(const AName: string): integer;
begin
  result := 22;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseDummy
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabaseDummy.Commit;
begin
   FTransactionActive := False;
end;


function TtiDatabaseDummy.InTransaction: boolean;
begin
  result := FTransactionActive;
end;


procedure TtiDatabaseDummy.RollBack;
begin
  FTransactionActive := False;
end;


procedure TtiDatabaseDummy.StartTransaction;
begin
  FTransactionActive := true;
end;


function TtiQueryDummy.FieldSize(AIndex: integer): integer;
begin
  result := 987123;
end;

function TtiQueryDummy.GetParamIsNull(const AName: string): Boolean;
begin
  result := false;
end;

procedure TtiQueryDummy.SetParamIsNull(const AName: string; const AValue: Boolean);
begin

end;

function TtiDatabaseDummy.GetConnected: boolean;
begin
  Result := FIsConnected
end;

procedure TtiDatabaseDummy.SetConnected(AValue: boolean);
begin
   FIsConnected := AValue;
end;

function TtiQueryDummy.GetFieldIsNull(const AName: string): Boolean;
begin
  result := true;
end;



class procedure TtiDatabaseDummy.CreateDatabase(const ADatabaseName,AUserName, APassword: string; const AParams: string);
begin

end;

class procedure TtiDatabaseDummy.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDummy.DatabaseExists(const ADatabaseName,AUserName, APassword: string; const AParams: string): boolean;
begin
   result := true;
end;

function TtiQueryDummy.HasNativeLogicalType: boolean;
begin
  result := false;
end;

function TtiDatabaseDummy.Test: boolean;
begin
  result := FIsConnected;
end;

function TtiDatabaseDummy.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDummy;
end;

{ TtiPersistenceLayerDummy }

procedure TtiPersistenceLayerDummy.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistDummy;
  APersistenceLayerDefaults.DatabaseName:= CTIPersistDummy;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.Username:= CTIPersistDummy;
  APersistenceLayerDefaults.Password:= CTIPersistDummy;
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerDummy.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseDummy;
end;

function TtiPersistenceLayerDummy.GetPersistenceLayerName: string;
begin
  result:= CTIPersistDummy;
end;

function TtiPersistenceLayerDummy.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryDummy;
end;

end.



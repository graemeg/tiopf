unit tiQueryZeosAbs;

{$I tiDefines.inc}

interface

uses
   tiQuery
  ,tiQueryDataset
  ,Classes
  ,ZConnection
  ,ZDataset
  ;

type

  TtiDatabaseZeosAbs = class( TtiDatabaseSQL )
  private
    FConnection    : TZConnection;
  protected
    procedure SetConnected( pbValue : boolean ) ; override;
    function  GetConnected : boolean ; override ;
    procedure SetupDBParams ; virtual ; abstract ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;
    function    Test : boolean; override;
    function    TIQueryClass: TtiQueryClass; override;
    property    Connection : TZConnection read FConnection write FConnection ;
  end;


  TtiQueryZeos = class( TtiQueryDataset )
  private
    FQuery : TZQuery ;
    procedure Prepare;
  protected
    procedure   CheckPrepared; override;
    function    GetSQL: TStrings; override ;
    procedure   SetSQL(const Value: TStrings); override ;
    procedure   SetActive(const Value: boolean); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    function    ExecSQL: integer; override;
    procedure   AttachDatabase( pDatabase : TtiDatabase ) ; override ;
    procedure   DetachDatabase ;  override ;
    procedure   Reset ; override ;
    function    HasNativeLogicalType : boolean ; override ;
  end;


implementation

uses
  tiConstants
  ,tiLog
  ,tiUtils
  ,tiExcept
  ,SysUtils
  ,DB
  ,TypInfo
  ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryZeos
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryZeos.Create;
begin
  inherited Create;
  FQuery := TZQuery.Create( nil ) ;
  Dataset := FQuery;
  Params  := FQuery.Params;
  FSupportsRowsAffected := True;
end;

destructor TtiQueryZeos.Destroy;
begin
  Params := nil;
  Dataset := nil;
  FQuery.Free ;
  inherited Destroy;
end;

function TtiQueryZeos.ExecSQL: integer;
begin
  Prepare;
  LogParams;
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

procedure TtiQueryZeos.Prepare;
begin
  if FQuery.Prepared then
    Exit; // ==>
  FQuery.Prepare;
end;

procedure TtiQueryZeos.CheckPrepared;
begin
  inherited CheckPrepared;
  Prepare;
end;

function TtiQueryZeos.GetSQL: TStrings;
begin
  Result := FQuery.SQL ;
end;

procedure TtiQueryZeos.SetActive(const Value: boolean);
begin
  Assert(Database.TestValid(TtiDatabase), 'Database is not valid');
  if Value then
  begin
    FQuery.Open;
  end
  else
  begin
    FQuery.Close;
  end;
end;

procedure TtiQueryZeos.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign( Value ) ;
end;

procedure TtiQueryZeos.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
  if (pDatabase is TtiDatabaseZeosAbs) then
  begin
    FQuery.Connection := TtiDatabaseZeosAbs( pDatabase ).Connection;
  end;
end;

procedure TtiQueryZeos.DetachDatabase;
begin
  inherited DetachDatabase;
  if FQuery.Active then
     FQuery.Close;
  FQuery.Connection := Nil;
end;

procedure TtiQueryZeos.Reset;
begin
  Active := False;
  FQuery.SQL.Clear;
  //FQuery.Params.Clear;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseZeosAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseZeosAbs.Create;
begin
  inherited Create ;
  FConnection := TZConnection.Create( nil ) ;
  FConnection.LoginPrompt := false ;
end;

destructor TtiDatabaseZeosAbs.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TtiDatabaseZeosAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create( 'Attempt to commit but not in a transaction.');

  FConnection.Commit ;
end;

function TtiDatabaseZeosAbs.InTransaction: boolean;
begin
  result := FConnection.InTransaction ;
end;

procedure TtiDatabaseZeosAbs.RollBack;
begin
  FConnection.RollBack ;
end;

procedure TtiDatabaseZeosAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FConnection.StartTransaction ;
end;

function TtiDatabaseZeosAbs.GetConnected: boolean;
begin
  Result := FConnection.Connected ;
end;

procedure TtiDatabaseZeosAbs.SetConnected(pbValue: boolean);
var
  lsErrorMessage : string ;
begin

  if ( not pbValue ) then
  begin
    Log( 'Disconnecting from %s', [DatabaseName] ) ;
    FConnection.Connected := false ;
    Exit ; //==>
  end ;

  SetupDBParams ;
  lsErrorMessage := '' ;
  try
    FConnection.Connected := true ;
  except
    on e: EDatabaseError do
    begin
      lsErrorMessage := 'Error attempting to connect to database.' + Cr + e.Message;
      raise EtiOPFDBExceptionUserNamePassword.Create(
        cTIPersistZeosFB, DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create( 'Unknown', DatabaseName, UserName, Password, e.message ) ;
  end ;
end;

function TtiQueryZeos.HasNativeLogicalType: boolean;
begin
  result := false ;
end;

function TtiDatabaseZeosAbs.Test: boolean;
begin
  Result := Connected;
end;

function TtiDatabaseZeosAbs.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryZeos;
end;

end.

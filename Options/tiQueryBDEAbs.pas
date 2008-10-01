unit tiQueryBDEAbs;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  ,DBTables
  ,tiQueryDataset
 ;

type

  TtiDatabaseBDEAbs = class(TtiDatabaseSQL)
  private
    FDatabase     : TDataBase;
    FSession      : TSession;
  protected
    property  Database : TDatabase read FDatabase write FDatabase;
    procedure SetConnected(AValue : boolean); override;
    function  GetConnected : boolean; override;
    procedure SetupDBParams; virtual; abstract;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;

  end;

  TtiQueryBDE = class(TtiQueryDataset)
  private
    FQuery : TQuery;
  protected
    procedure CheckPrepared; override;

    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    procedure   SetActive(const AValue: boolean); override;
    procedure   SetParamAsString(const AName, AValue: string); override;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   ExecSQL; override;

    procedure   AssignParamToStream(const AName : string; const AStream : TStream); override;
    procedure   AttachDatabase(ADatabase : TtiDatabase); override;
    procedure   DetachDatabase;  override;
    procedure   Reset; override;

    function    HasNativeLogicalType : boolean; override;

  end;


implementation
uses
  tiLog
  ,tiUtils
  ,tiExcept
  ,SysUtils
  ,DB
  ,TypInfo
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryBDE
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryBDE.Create;
begin
  inherited;
  FQuery := TQuery.Create(nil);
  Dataset:=FQuery;
  Params:=FQuery.Params;
end;

destructor TtiQueryBDE.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TtiQueryBDE.Close;
begin
  Active := false;
end;

procedure TtiQueryBDE.ExecSQL;
begin
  FQuery.ExecSQL;
end;


procedure TtiQueryBDE.Open;
begin
  Active := true;
end;

procedure TtiQueryBDE.SetActive(const AValue: boolean);
begin
  FQuery.Active := AValue;
end;

procedure TtiQueryBDE.CheckPrepared;
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
end;

procedure TtiQueryBDE.SetParamAsString(const AName, AValue: string);
var
  lParam : TParam;
begin
  if not FQuery.Prepared then
    FQuery.Prepare;
  lParam := FQuery.ParamByName(AName);
  if length(AValue) <= 255 then
    lParam.AsString := AValue
  else
    lParam.AsMemo := AValue;
end;

procedure TtiQueryBDE.SetSQL(const AValue: TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;

Function TtiQueryBDE.GetSQL: TStrings;
begin
  Result:=FQuery.SQL;
end;

procedure TtiQueryBDE.AttachDatabase(ADatabase: TtiDatabase);
begin
  FQuery.DatabaseName := TtiDatabaseBDEAbs(ADatabase).Database.DatabaseName;
  Database := ADatabase;
end;

procedure TtiQueryBDE.DetachDatabase;
begin
  inherited DetachDatabase;
end;

procedure TtiQueryBDE.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
  FQuery.Params.Clear;
end;

procedure TtiQueryBDE.AssignParamToStream(const AName: string; const AStream : TStream);
var
  ls : string;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  ls := FQuery.ParamByName(AName).Value;
  tiStringToStream(ls, AStream);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseBDEAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseBDEAbs.Create;
begin
  inherited Create;
  FDatabase := TDataBase.Create(nil);
  FDatabase.LoginPrompt := false;
  FSession      := TSession.Create(nil);
  FSession.AutoSessionName := true;
  FDatabase.SessionName := FSession.Name;
  // Must come up with a better way of doing this.
  FDatabase.DatabaseName := 'DB_' + FSession.SessionName;
end;

destructor TtiDatabaseBDEAbs.Destroy;
begin
  FDatabase.Free;
  FSession.Free;
  inherited;
end;

procedure TtiDatabaseBDEAbs.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FDatabase.Commit;
end;

function TtiDatabaseBDEAbs.InTransaction: boolean;
begin
  result := FDatabase.InTransaction;
end;

procedure TtiDatabaseBDEAbs.RollBack;
begin
  FDatabase.RollBack;
end;

procedure TtiDatabaseBDEAbs.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');
  FDatabase.StartTransaction;
end;


function TtiDatabaseBDEAbs.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseBDEAbs.SetConnected(AValue: boolean);
var
  i : integer;
  lsErrorMessage : string;
begin

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    FDatabase.Connected := false;
    FSession.Active := false;
    Exit; //==>
  end;

  SetupDBParams;
  lsErrorMessage := '';
  try
    FDatabase.Connected := true;
    FSession.Active := true;
  except
    // ToDo: Find a generic way of trapping an invalid password error, and
    //       re-raising this exception so the logon screen in activated again.
    //       But, the BDE raises a bunch of exception for the simplest error
    //       so this may be tricky.
    on e:EDBEngineError do
    begin
      lsErrorMessage := '';
      for i := 0 to EDBEngineError(e).ErrorCount-1 do
      begin
        if lsErrorMessage <> '' then lsErrorMessage := lsErrorMessage + CrLf(2);
        lsErrorMessage := lsErrorMessage +
                          'Error class: '   + EDBEngineError(e).classname + Cr +
                          'Error message: ' + EDBEngineError(e).Errors[i].Message + Cr +
                          'Error Code: ' + IntToStr(EDBEngineError(e).Errors[i].ErrorCode) + Cr +
                          'Native error code: ' + IntToStr(EDBEngineError(e).Errors[i].NativeError);
      end;
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, lsErrorMessage);
    end;
    on e:exception do
      raise EtiOPFDBExceptionCanNotConnect.Create('Unknown', DatabaseName, UserName, Password, e.message);
  end;
end;


function TtiQueryBDE.HasNativeLogicalType: boolean;
begin
  result := true;
end;

end.

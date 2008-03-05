unit tiStructuredCSVWriter;

interface
uses
  tiBaseObject,
  tiQuery,
  tiStreams
  ;

type

  TtiStructuredCSVWriter = class(TtiBaseObject)
  private
    FStream: TtiPreSizedStream;
    FDatabase: TtiDatabase;
    FQuery: TtiQuery;
    function GetAsString: string;
  protected
    property    Query: TtiQuery read FQuery;
    property    Stream: TtiPreSizedStream read FStream;
    property    AsString: string read GetAsString;
    procedure   LockDatabaseConnection; virtual;
    procedure   UnLockDatabaseConnection; virtual;
    procedure   Write(const AValue: string);
    procedure   WriteLn(const AValue: string);
    procedure   WriteI(const ADataBlockName: string);
    procedure   WriteD(const ADataBlockName: string);

    function    DoExecute: string; virtual; abstract;
    function    DatabaseAlias: string; virtual; abstract;

  public
    constructor Create;
    destructor  Destroy; override;
    class function Execute: string;
  end;

  TtiStructuredCSVWriterSingleQuery = class(TtiStructuredCSVWriter)
  protected
    function    DoExecute: string; override;
    procedure   SetupQuery; virtual; abstract;
    procedure   WriteIRow; virtual; abstract;
    procedure   WriteDRow; virtual; abstract;

  end;

implementation
uses
  tiOPFManager,
  tiConstants
  ;

{ TtiStructuredCSVWriterSingleQuery }

constructor TtiStructuredCSVWriter.Create;
begin
  inherited;
  FQuery:= GTIOPFManager.DefaultPerLayer.QueryClass.Create;
  FStream:= TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
end;

destructor TtiStructuredCSVWriter.Destroy;
begin
  FStream.Free;
  FQuery.Free;
  inherited;
end;

function TtiStructuredCSVWriterSingleQuery.DoExecute: string;
begin
  WriteIRow;
  LockDatabaseConnection;
  try
    SetupQuery;
    FQuery.ExecSQL;
    while not FQuery.EOF do
    begin
      WriteDRow;
      FQuery.Next;
    end;
    Result:= Stream.AsString;
  finally
    UnlockDatabaseConnection;
  end;
end;

class function TtiStructuredCSVWriter.Execute: string;
var
  L: TtiStructuredCSVWriter;
begin
  L:= Create;
  try
    Result:= L.DoExecute;
  finally
    L.Free;
  end;
end;

function TtiStructuredCSVWriter.GetAsString: string;
begin
  result:= FStream.AsString;
end;

procedure TtiStructuredCSVWriter.LockDatabaseConnection;
begin
  FDatabase:= GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(DatabaseAlias);
  FQuery.AttachDatabase(FDatabase);
  FDatabase.StartTransaction;
end;

procedure TtiStructuredCSVWriter.UnLockDatabaseConnection;
begin
  FDatabase.Commit;
  GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(DatabaseAlias, FDatabase);
end;

procedure TtiStructuredCSVWriter.Write(const AValue: string);
begin
  FStream.Write(AValue);
end;

procedure TtiStructuredCSVWriter.WriteD(const ADataBlockName: string);
begin
  Assert(ADataBlockName <> '', 'ADataBlockName not assigned');
  Write(CStructCSVPrefixD + ',' + ADataBlockName);
end;

procedure TtiStructuredCSVWriter.WriteI(const ADataBlockName: string);
begin
  Assert(ADataBlockName <> '', 'ADataBlockName not assigned');
  Write(CStructCSVPrefixI + ',' + ADataBlockName);
end;

procedure TtiStructuredCSVWriter.WriteLn(const AValue: string);
begin
  FStream.WriteLn(AValue);
end;

end.


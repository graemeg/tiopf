unit tiStructuredCSVWriterQuery;

interface

uses
  tiStructuredCSVWriter,
  tiQuery;

type

  TtiStructuredCSVWriterQueryList = class(TtiStructuredCSVWriter)
  private
    FDatabase: TtiDatabase;
    FQuery:    TtiQuery;
  protected
    property Query: TtiQuery read FQuery;
    procedure LockDatabaseConnection; virtual;
    procedure UnLockDatabaseConnection; virtual;
    function DatabaseAlias: string; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function Execute: string;
  end;

  TtiStructuredCSVWriterSingleQuery = class(TtiStructuredCSVWriterQueryList)
  protected
    procedure DoExecute; override;
    procedure SetupQuery; virtual; abstract;
    procedure WriteIRow; virtual; abstract;
    procedure WriteDRow; virtual; abstract;
  end;

  TtiStructuredCSVWriterRowMethod = procedure of object;

  TtiStructuredCSVWriterMultipleQuery = class(TtiStructuredCSVWriterQueryList)
  protected
    procedure DoExecute; override;
    procedure WriteSection(
        const ADataBlockName: string;
        const AFieldNames: array of string;
        const ASelectSQL: string;
        const AWriteRowMethod: TtiStructuredCSVWriterRowMethod);
    procedure WriteAll; virtual; abstract;
  end;

implementation

uses
  tiOPFManager,
  tiConstants;

{ TtiStructuredCSVWriterQueryList }

constructor TtiStructuredCSVWriterQueryList.Create;
begin
  inherited;
  FQuery := GTIOPFManager.DefaultPerLayer.QueryClass.Create;
end;

destructor TtiStructuredCSVWriterQueryList.Destroy;
begin
  FQuery.Free;
  inherited;
end;

class function TtiStructuredCSVWriterQueryList.Execute: string;
var
  L: TtiStructuredCSVWriterQueryList;
begin
  L := Create;
  try
    L.DoExecute;
    Result:= L.AsString;
  finally
    L.Free;
  end;
end;

procedure TtiStructuredCSVWriterQueryList.LockDatabaseConnection;
begin
  FDatabase := GTIOPFManager.DefaultPerLayer.DBConnectionPools.Lock(DatabaseAlias);
  FQuery.AttachDatabase(FDatabase);
  FDatabase.StartTransaction;
end;

procedure TtiStructuredCSVWriterQueryList.UnLockDatabaseConnection;
begin
  FDatabase.Commit;
  GTIOPFManager.DefaultPerLayer.DBConnectionPools.UnLock(DatabaseAlias, FDatabase);
end;

{ TtiStructuredCSVWriterSingleQuery }

procedure TtiStructuredCSVWriterSingleQuery.DoExecute;
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
  finally
    UnlockDatabaseConnection;
  end;
end;

{ TtiStructuredCSVWriterMultipleQuery }

procedure TtiStructuredCSVWriterMultipleQuery.DoExecute;
begin
  LockDatabaseConnection;
  try
    WriteAll;
  finally
    UnlockDatabaseConnection;
  end;
end;

procedure TtiStructuredCSVWriterMultipleQuery.WriteSection(
  const ADataBlockName: string;
  const AFieldNames: array of string;
  const ASelectSQL: string;
  const AWriteRowMethod: TtiStructuredCSVWriterRowMethod);
begin
  WriteI(ADataBlockName, AFieldNames);

  Query.SQLText := ASelectSQL;
  Query.Open;
  try
    while not Query.EOF do
    begin
      AWriteRowMethod;
      Query.Next;
    end;
  finally
    Query.Close;
  end;
end;

end.


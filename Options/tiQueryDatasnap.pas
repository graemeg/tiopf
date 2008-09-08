unit tiQueryDatasnap;

{$I tiDefines.inc}

interface

uses
  SysUtils,
  Classes,
  DBClient,
  tiObject,
  tiQuery,
  tiQueryDataset;

type
  TtiDatabaseDataSnap = class(TtiDatabaseSQL)
  private
    FProviderName: string;
  protected
    // Set up remote server from databasename and
    function GetRemoteServer: TCustomRemoteServer; virtual; abstract;
  public
    procedure ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    property RemoteServer: TCustomRemoteServer read GetRemoteServer;
    property ProviderName: string read FProviderName write FProviderName;
  end;


  TtiQueryDatasnap = class(TtiQueryDataset)
  private
    FQuery: TClientDataset;
    FSQL: TStrings;
    procedure DoSQLChange(Sender: TObject);
    function GetProviderName: string;
    procedure SetProviderName(const Value: string);
  protected
    procedure CheckPrepared; override;
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    procedure SetActive(const AValue: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    procedure ExecSQL; override;

    procedure AttachDatabase(ADatabase: TtiDatabase); override;
    procedure DetachDatabase; override;
    procedure Reset; override;
    function HasNativeLogicalType: Boolean; override;
    property ProviderName: string read GetProviderName write SetProviderName;
  end;


implementation

uses
  variants;


{ TtiQueryDatasnap }

procedure TtiQueryDatasnap.AttachDatabase(ADatabase: TtiDatabase);
var
  DB: TtiDatabaseDataSnap;
begin
  DB := (ADatabase as TtiDatabaseDataSnap);
  FQuery.RemoteServer := DB.RemoteServer;
  if (DB.ProviderName <> '') then
    FQuery.ProviderName := DB.ProviderName;
end;

procedure TtiQueryDatasnap.CheckPrepared;
begin
  // Always prepared
end;

procedure TtiQueryDatasnap.Close;
begin
  FQuery.Active := False;
end;

constructor TtiQueryDatasnap.Create;
begin
  inherited;
  FQuery  := TClientDataset.Create(nil);
  Dataset := FQuery;
  Params  := FQuery.Params;
  FSQL    := TStringList.Create;
  TStringList(FSQL).OnChange := DoSQLChange;
end;

destructor TtiQueryDatasnap.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FQuery);
  inherited;
end;

procedure TtiQueryDatasnap.DetachDatabase;
begin
  FQuery.Active       := False;
  FQuery.RemoteServer := nil;
end;

procedure TtiQueryDatasnap.DoSQLChange(Sender: TObject);
var
  S: string;
begin
  FQuery.CommandText := FSQL.Text;
  S := FQuery.CommandText;
  UniqueString(S);
  if (S <> '') then
    FQuery.Params.ParseSQL(S, True)
  else
    FQuery.Params.Clear;
end;

procedure TtiQueryDatasnap.ExecSQL;
begin
  FQuery.Execute;
end;

function TtiQueryDatasnap.GetProviderName: string;
begin
  Result := FQuery.ProviderName;
end;

function TtiQueryDatasnap.GetSQL: TStrings;
begin
  Result := FSQL;
end;

function TtiQueryDatasnap.HasNativeLogicalType: Boolean;
begin
  Result := True;
end;

procedure TtiQueryDatasnap.Open;
begin
  Active := True;
end;

procedure TtiQueryDatasnap.Reset;
begin
  FSQL.Clear; // Will reset commantext.
end;

procedure TtiQueryDatasnap.SetActive(const AValue: Boolean);
begin
  FQuery.Active := Avalue;
end;

procedure TtiQueryDatasnap.SetProviderName(const Value: string);
begin
  FQuery.ProviderName := Value;
end;

procedure TtiQueryDatasnap.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;


{ TtiDatabaseDataSnap }

type
  TMyClientDataSet = class(TClientDataset)
  public
    procedure OpenCursor(InfoQuery: Boolean); override;
  end;

procedure TtiDatabaseDataSnap.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  CDS: TMyClientDataset;
  i: integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  CDS    := TMyClientDataset.Create(nil);
  try
    CDS.RemoteServer := Self.RemoteServer;
    CDS.ProviderName := lTable.Name;
    CDS.OpenCursor(True);
    CDS.FieldDefs.Update;
    for I := 0 to CDS.FieldDefs.Count - 1 do
    begin
      lField      := TtiDBMetaDataField.Create;
      lField.Name := CDS.FieldDefs[i].Name;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
  finally
    CDS.Free;
  end;
  inherited;
end;

procedure TtiDatabaseDataSnap.ReadMetaDataTables(AData: TtiDBMetaData);
var
  S: string;
  List: olevariant;
  I: integer;
  lTable: TtiDBMetaDataTable;
begin
  List := Remoteserver.GetServer.AS_GetProviderNames;
  if VarIsArray(List) and (VarArrayDimCount(List) = 1) then
    for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
    begin
      S           := List[i];
      lTable      := TtiDBMetaDataTable.Create;
      lTable.Name := S;
      lTable.ObjectState := posPK;
      AData.Add(lTable);
      AData.ObjectState := posClean;
    end;
end;


{ TMyClientDataSet }

procedure TMyClientDataSet.OpenCursor(InfoQuery: Boolean);
begin
  inherited OpenCursor(InfoQuery);
end;

end.


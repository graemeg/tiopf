unit tiDataPump_BOM;

interface
uses
  tiObjAbs
  ,Classes
  ,tiRegPerLayer
  ,tiQuery
  ,tiPerObjOIDGUID
  ;

type

  TtiDataPumpUpdateProgressEvent = procedure( const pMessage : string ) of object ;

  TtiDataPump = class( TtiObjAbs )
  private
    FSourcePerLayerName: string;
    FTargetPerLayerName: string;
    FSourceDatabaseName: string;
    FTargetUserName: string;
    FSourceUserName: string;
    FTargetDatabaseName: string;
    FSourcePassword: string;
    FTargetPassword: string;
    FSourceDatabase : TtiDatabase ;
    FTargetDatabase : TtiDatabase ;

    FTables: TStrings;

    FSourcePerLayer: TtiRegPerLayer;
    FTargetPerLayer: TtiRegPerLayer;

    FSourceDB : TtiDatabase ;
    FTargetDB : TtiDatabase ;
    FOnUpdateProgress: TtiDataPumpUpdateProgressEvent;

    procedure   CheckTableStructures ;
    procedure   CheckTableStructure(const pTableName: string);

    procedure   CopyTableStructures ;
    procedure   CopyTableStructure(const pTableName: string);

    procedure   CopyTablesData ;
    procedure   CopyTableData(const pTableName: string);

    procedure   UpdateProgress(const pMessage : string);

  public
    constructor create ;
    destructor  destroy ; override ;
    property    SourcePerLayerName : string read FSourcePerLayerName write FSourcePerLayerName;
    property    SourceDatabaseName : string read FSourceDatabaseName write FSourceDatabaseName;
    property    SourceUserName     : string read FSourceUserName     write FSourceUserName;
    property    SourcePassword     : string read FSourcePassword     write FSourcePassword;

    property    TargetPerLayerName : string read FTargetPerLayerName write FTargetPerLayerName;
    property    TargetDatabaseName : string read FTargetDatabaseName write FTargetDatabaseName;
    property    TargetUserName     : string read FTargetUserName     write FTargetUserName;
    property    TargetPassword     : string read FTargetPassword     write FTargetPassword;

    property    OnUpdateProgress   : TtiDataPumpUpdateProgressEvent read FOnUpdateProgress Write FOnUpdateProgress;

    property    Tables : TStrings read FTables ;

    procedure   Execute ;

  end ;

implementation
uses
  tiPersist
  ,SysUtils
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ;

{ TtiDataPump }

procedure TtiDataPump.CheckTableStructure(const pTableName: string);
begin

end;

procedure TtiDataPump.CheckTableStructures;
var
  i : integer ;
begin
  for i := FTables.Count-1 downto 0 do
    if Trim(FTables.Strings[i]) <> '' then
      CheckTableStructure(Trim(FTables.Strings[i]));
end;

procedure TtiDataPump.CopyTableData(const pTableName: string);
var
  lQueryFrom : TtiQuery ;
  lQueryTo   : TtiQuery ;
  i : integer ;
  lParams : TtiQueryParams;
  lStream : TMemoryStream;
  lCount : Integer ;
begin
  lQueryFrom := FSourcePerLayer.tiQueryClass.Create;
  try
    lQueryFrom.AttachDatabase(FSourceDatabase);
    lQueryTo := FTargetPerLayer.tiQueryClass.Create;
    try
      lQueryTo.AttachDatabase(FTargetDatabase);
      lQueryFrom.SelectRow(pTableName, nil);
      lCount := 1 ;
      while not lQueryFrom.eof do
      begin
        lParams := TtiQueryParams.Create;
        try
          for i := 0 to lQueryFrom.FieldCount - 1 do
          begin
            case lQueryFrom.FieldKind(i) of
            qfkString,
            qfkLongString : lParams.SetValueAsString(  lQueryFrom.FieldName(i), lQueryFrom.FieldAsString[lQueryFrom.FieldName(i)]);
            qfkInteger    : lParams.SetValueAsInteger( lQueryFrom.FieldName(i), lQueryFrom.FieldAsInteger[lQueryFrom.FieldName(i)]);
            qfkFloat      : lParams.SetValueAsFloat(   lQueryFrom.FieldName(i), lQueryFrom.FieldAsFloat[lQueryFrom.FieldName(i)]);
            qfkDateTime   : lParams.SetValueAsDateTime(lQueryFrom.FieldName(i), lQueryFrom.FieldAsDateTime[lQueryFrom.FieldName(i)]);
            qfkLogical    : lParams.SetValueAsBoolean( lQueryFrom.FieldName(i), lQueryFrom.FieldAsBoolean[lQueryFrom.FieldName(i)]);
            qfkBinary     : begin
                              lStream := TMemoryStream.Create;
                              try
                                lQueryFrom.AssignFieldAsStream(lQueryFrom.FieldName(i),lStream);
                                lParams.SetValueAsStream(lQueryFrom.FieldName(i),lStream);
                              finally
                                lStream.Free;
                              end;
                            end ;
            else
              tiFmtException('Invalid field kind',ClassName, 'CopyTableData');
            end;
          end ;
          lQueryTo.InsertRow(pTableName,lParams);
          UpdateProgress( 'Record ' + IntToStr(lCount) + ' inserted into ' + pTableName ) ;
          Inc(lCount);
        finally
          lParams.Free;
        end;

        lQueryFrom.Next;
      end;
    finally
      lQueryTo.Free;
    end;
  finally
    lQueryFrom.Free;
  end;
end;

procedure TtiDataPump.CopyTablesData;
var
  i : integer ;
begin
  FSourceDatabase.StartTransaction;
  FTargetDatabase.StartTransaction;
  for i := 0 to FTables.Count-1 do
    if Trim(FTables.Strings[i]) <> '' then
      CopyTableData(Trim(FTables.Strings[i]));
  FTargetDatabase.Commit;
  FSourceDatabase.RollBack;
end;

procedure TtiDataPump.CopyTableStructure(const pTableName: string);
var
  lMDT : TtiDBMetaDataTable;
begin
  lMDT := TtiDBMetaDataTable.Create;
  try
    lMDT.Name := pTableName;
    FSourceDatabase.ReadMetaDataFields(lMDT);
    FTargetDatabase.CreateTable(lMDT);
    UpdateProgress( 'Table ' + pTableName + ' created' ) ;
  finally
    lMDT.Free;
  end;
end;

procedure TtiDataPump.CopyTableStructures;
var
  i : integer ;
begin
  for i := 0 to FTables.Count-1 do
    if Trim(FTables.Strings[i]) <> '' then
      CopyTableStructure(Trim(FTables.Strings[i]));
end;

constructor TtiDataPump.create;
begin
  inherited;
  FTables := TStringList.Create;
end;

destructor TtiDataPump.destroy;
begin
  FTables.Free;
  inherited;
end;

procedure TtiDataPump.Execute;
begin
  Assert(FSourcePerLayerName<>'', 'SourcePerLayerName = ""');
  Assert(FTargetPerLayerName<>'', 'TargetPerLayerName = ""');
  Assert(FSourceDatabaseName<>'', 'SourceDatabaseName = ""');
  Assert(FTargetDatabaseName<>'', 'TargetDatabaseName = ""');

deletefile(FTargetDatabaseName);

  gTIPerMgr.LoadPersistenceLayer(FSourcePerLayerName);
  try
    gTIPerMgr.LoadPersistenceLayer(FTargetPerLayerName);
    try
      FSourcePerLayer:= gTIPerMgr.RegPerLayers.FindByPerLayerName(FSourcePerLayerName);
      Assert(FSourcePerLayer<>nil, 'FSourcePerLayer = nil');
      FTargetPerLayer:= gTIPerMgr.RegPerLayers.FindByPerLayerName(FTargetPerLayerName);
      Assert(FTargetPerLayer<>nil, 'FTargetPerLayer = nil');

//      if not FSourcePerLayer.tiDatabaseClass.DatabaseExists(FSourceDatabaseName,FSourceUserName,FSourcePassword) then
//        raise exception.create('Database <'+FSourceDatabaseName+'> not found.');
      FSourcePerLayer.DBConnectionPools.Connect(FSourceDatabaseName,FSourceUserName,FSourcePassword,'');

      if not FTargetPerLayer.tiDatabaseClass.DatabaseExists(FTargetDatabaseName,FTargetUserName,FTargetPassword) then
        FTargetPerLayer.tiDatabaseClass.CreateDatabase(FTargetDatabaseName,FTargetUserName,FTargetPassword);
      if not FTargetPerLayer.tiDatabaseClass.DatabaseExists(FTargetDatabaseName,FTargetUserName,FTargetPassword) then
        raise exception.create('Unable to create target database <'+FTargetDatabaseName+'>');

      FTargetPerLayer.DBConnectionPools.Connect(FTargetDatabaseName,FTargetUserName,FTargetPassword, '');

      FSourceDatabase := FSourcePerLayer.DefaultDBConnectionPool.Lock.Database;
      FTargetDatabase := FTargetPerLayer.DefaultDBConnectionPool.Lock.Database;

      CheckTableStructures;
      CopyTableStructures;
      CopyTablesData;
      UpdateProgress( 'Done' ) ;

    finally
      gTIPerMgr.UnLoadPersistenceLayer(FTargetPerLayerName);
    end ;
  finally
    gTIPerMgr.UnLoadPersistenceLayer(FSourcePerLayerName);
  end;
end;

procedure TtiDataPump.UpdateProgress(const pMessage: string);
begin
  if Assigned(FOnUpdateProgress) then
    FOnUpdateProgress(pMessage);
end;

end.

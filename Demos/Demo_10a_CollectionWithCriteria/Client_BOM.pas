unit Client_BOM;

interface
uses
  tiObject
  ,tiFilteredObjectList
  ,tiAutoMap
 ;

type
  TClient = class;
  TClients = class;

  TClients = class(TtiFilteredObjectList)
  public
    procedure Read; override;
    procedure Save; override;
    // ToDo: Remove Criteria and HasCriteria and access them through ItiFiltered
    function HasCriteria: boolean;
    property Criteria;
  end;

  // The following attribute will automatically register the class for auto
  // mapping. We disable this in this demo as the user can choose the
  // persistence mechanism.
  //[TAutoMap] // Class automatically registered for auto mapping
  [TAutoMapTable('Client')] // Auto mapping table
  TClient = class(TtiObject)
  private
    FClientID: string;
    FClientName: string;
  public
    constructor CreateNew(const pDatabaseName: string = ''; const pPersistenceLayerName: string = ''); override;
  published
    [TAutoMapColumn('OID', [pktDB])] // Auto mapping column
    property OID;
    [TAutoMapColumn('Client_Name')] // Auto mapping column
    property ClientName: string read FClientName write FClientName;
    [TAutoMapColumn('Client_ID')] // Auto mapping column
    property ClientID: string read FClientID write FClientID;
  end;

implementation
uses
   tiOPFManager
  ,SysUtils
  ,Windows
 ;

{ TClient }

constructor TClient.CreateNew(const pDatabaseName: string = ''; const pPersistenceLayerName: string = '');
begin
  inherited;
  // Set some default values for the demo
  ClientID:= IntToStr(GetTickCount);

  ClientName:= Char(GetTickCount mod 26 + ord('A')) + DateTimeToStr(Now);
end;

{ TClients }

function TClients.HasCriteria: boolean;
begin
  result:= inherited HasCriteria;
end;

procedure TClients.Read;
begin
  inherited;
end;

procedure TClients.Save;
begin
  inherited;
end;

end.


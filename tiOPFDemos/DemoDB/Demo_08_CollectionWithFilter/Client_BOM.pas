unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ;

type

  TClient = class ;
  TClients = class ;

  TClientName = String[200];
  TClientID   = String[9];

  TClients = class( TPerObjList ) ;

  TClient = class( TPerObjAbs )
  private
    FClientID: TClientID;
    FClientName: TClientName;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    ClientID   : TClientID read FClientID write FClientID ;
  end ;

  TClientsByName = class( TPerObjList )
  private
    FClientName: TClientName;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
  end ;

procedure RegisterMappings ;

implementation
uses
  tiPersist
  ,tiClassToDBMap_BOM
  ;

procedure RegisterMappings ;
begin
  //                                          Class,   Table,    Property,     Column,       Special Info
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'         );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'           );
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end ;

end.


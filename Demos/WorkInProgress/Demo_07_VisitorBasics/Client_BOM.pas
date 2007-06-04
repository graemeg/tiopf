unit Client_BOM;

interface
uses
  tiObject
  ,tiOID
  ,tiOIDGUID
  , tiVisitor
  ;

type

  TClient = class ;
  TClients = class ;

  TClientName = String[200];
  TClientID   = String[9];

  TClients = class( TtiObjectList ) ;

  TClient = class( TtiObject )
  private
    FClientID: TClientID;
    FClientName: TClientName;
  published
    property    ClientName : TClientName read FClientName write FClientName ;
    property    ClientID   : TClientID read FClientID write FClientID ;
  end ;

  TClientVisitor = class( TtiVisitor )
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
  end ;

procedure RegisterMappings ;

implementation
uses
   tiOPFManager
  ,tiClassToDBMap_BOM
  ,tiConstants
  ,tiGUIUtils
  ;

procedure RegisterMappings ;
begin
  //                                          Class,   Table,    Property,     Column,       Special Info
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'         );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'           );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end ;

{ TClientVisitor }

function TClientVisitor.AcceptVisitor: boolean;
begin
  // Put the code to check if this visitor should act on this object in here.
  Result := Visited is TClient ;
  // Remove this line and the visitor will touch the TClients object
  // as well as it's owned TClient objects.
end;

procedure TClientVisitor.Execute(const pVisited: TtiVisited);
begin
  inherited;
  if not AcceptVisitor then
    Exit ;
  tiShowPerObjAbs(Visited as TtiObject);
end;

end.


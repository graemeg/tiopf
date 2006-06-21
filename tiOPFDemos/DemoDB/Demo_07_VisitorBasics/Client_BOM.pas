unit Client_BOM;

interface
uses
  tiPtnVisPerObj
  ,tiPtnVis
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

  TClientVisitor = class( TVisitorAbs )
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

procedure RegisterMappings ;

implementation
uses
  tiPersist
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj_Cli
  ;

procedure RegisterMappings ;
begin
  //                                          Class,   Table,    Property,     Column,       Special Info
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB] );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'         );
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'           );
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end ;

{ TClientVisitor }

function TClientVisitor.AcceptVisitor: boolean;
begin
  // Put the code to check if this visitor should act on this object in here.
  Result := Visited is TClient ;
  // Remove this line and the visitor will touch the TClients object
  // as well as it's owned TClient objects.
end;

procedure TClientVisitor.Execute(const pVisited: TVisitedAbs);
begin
  inherited;
  if not AcceptVisitor then
    Exit ;
  tiShowPerObjAbs(Visited as TPerObjAbs);
end;

end.


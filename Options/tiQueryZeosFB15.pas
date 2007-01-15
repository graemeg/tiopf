unit tiQueryZeosFB15;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiQueryZeosIBFB
  ;

type
  TtiDatabaseZeosFB15 = class(TtiDatabaseZeosIBFB)
  public
    constructor Create; override;
  end;


implementation

uses
  tiOPFManager
  ,tiDBConnectionPool
  ,tiClassToDBMap_BOM
  ,tiObject
  ,tiConstants
  ,SysUtils
  ,tiQueryZeosAbs
  ;

{ TtiDatabaseZeosFB10 }

constructor TtiDatabaseZeosFB15.Create;
begin
  inherited;

  Connection.Protocol := 'firebird-1.5';
end;


initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistZeosFB15,
    TtiDBConnectionPoolDataAbs,
    TtiQueryZeos,
    TtiDatabaseZeosFB15);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB15);


end.

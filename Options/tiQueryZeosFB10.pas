unit tiQueryZeosFB10;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiQueryZeosIBFB
  ;

type
  TtiDatabaseZeosFB10 = class(TtiDatabaseZeosIBFB)
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

constructor TtiDatabaseZeosFB10.Create;
begin
  inherited;

  Connection.Protocol := 'firebird-1.0';
end;


initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistZeosFB10,
    TtiDBConnectionPoolDataAbs,
    TtiQueryZeos,
    TtiDatabaseZeosFB10);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB10);


end.

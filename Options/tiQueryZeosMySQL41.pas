unit tiQueryZeosMySQL41;

interface

uses
    tiQuery
  , tiQueryZeosMySQL;

type
  TtiDatabaseZeosMySQL41 = class(TtiDatabaseZeosMySQL)
  public
    constructor Create; override;
  end;

implementation

uses
  tiOPFManager
  ,tiDBConnectionPool
  ,tiAutoMap
  ,tiObject
  ,tiConstants
  ,SysUtils
  ,tiQueryZeosAbs
  ;

{ TtiDatabaseZeosFB10 }

constructor TtiDatabaseZeosMySQL41.Create;
begin
  inherited;
  Connection.Protocol := 'mysql-4.1';
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistZeosMySQL41,
    TtiDBConnectionPoolDataAbs,
    TtiQueryZeos,
    TtiDatabaseZeosMySQL41);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL41);


end.

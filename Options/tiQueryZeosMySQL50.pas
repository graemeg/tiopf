unit tiQueryZeosMySQL50;

interface

uses
    tiQuery
  , tiQueryZeosMySQL;

type
  TtiDatabaseZeosMySQL50 = class(TtiDatabaseZeosMySQL)
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

constructor TtiDatabaseZeosMySQL50.Create;
begin
  inherited;
  Connection.Protocol := 'mysql-5';
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistZeosMySQL50,
    TtiDBConnectionPoolDataAbs,
    TtiQueryZeos,
    TtiDatabaseZeosMySQL50);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL50);


end.

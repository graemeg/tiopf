unit tiQueryZeosMySQL41;

interface

uses
   tiQuery
  ,tiQueryZeosMySQL
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ;

type

  TtiPersistenceLayerZeosMySQL41 = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  end;

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

{ TtiPersistenceLayerZeosMySQL41 }

function TtiPersistenceLayerZeosMySQL41.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosMySQL41;
end;

function TtiPersistenceLayerZeosMySQL41.GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
begin
  result:= TtiDBConnectionPoolDataAbs;
end;

function TtiPersistenceLayerZeosMySQL41.GetPersistenceLayerName: string;
begin
  result:= cTIPersistZeosMySQL41;
end;

function TtiPersistenceLayerZeosMySQL41.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryZeos;
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosMySQL41);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL41);


end.

unit tiQueryZeosMySQL50;

interface

uses
   tiQuery
  ,tiQueryZeosMySQL
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ;

type

  TtiPersistenceLayerZeosMySQL50 = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  end;

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

{ TtiPersistenceLayerZeosMySQL50 }

function TtiPersistenceLayerZeosMySQL50.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosMySQL50;
end;

function TtiPersistenceLayerZeosMySQL50.GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
begin
  result:= TtiDBConnectionPoolDataAbs;
end;

function TtiPersistenceLayerZeosMySQL50.GetPersistenceLayerName: string;
begin
  result:= cTIPersistZeosMySQL50;
end;

function TtiPersistenceLayerZeosMySQL50.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryZeos;
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosMySQL50);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL50);

end.

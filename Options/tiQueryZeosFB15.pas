unit tiQueryZeosFB15;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiQueryZeosIBFB
  ,tiPersistenceLayers
  ,tiDBConnectionPool
 ;

type

  TtiPersistenceLayerZeosFB15 = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  end;

type
  TtiDatabaseZeosFB15 = class(TtiDatabaseZeosIBFB)
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

constructor TtiDatabaseZeosFB15.Create;
begin
  inherited;

  Connection.Protocol := 'firebird-1.5';
end;


{ TtiPersistenceLayerZeosFB15 }

function TtiPersistenceLayerZeosFB15.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosFB15;
end;

function TtiPersistenceLayerZeosFB15.GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
begin
  result:= TtiDBConnectionPoolDataAbs;
end;

function TtiPersistenceLayerZeosFB15.GetPersistenceLayerName: string;
begin
  result:= cTIPersistZeosFB15;
end;

function TtiPersistenceLayerZeosFB15.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryZeos;
end;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosFB15);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB15);


end.

unit tiQueryZeosFB10;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiQueryZeosIBFB
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ;

type

  TtiPersistenceLayerZeosFB10 = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseZeosFB10 = class(TtiDatabaseZeosIBFB)
  public
    constructor Create; override;
  end;


implementation

uses
   tiOPFManager
  ,tiAutoMap
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


{ TtiPersistenceLayerZeosFB10 }

procedure TtiPersistenceLayerZeosFB10.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistZeosFB10;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.Username:= 'SYSDBA';
  APersistenceLayerDefaults.Password:= 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerZeosFB10.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosFB10;
end;

function TtiPersistenceLayerZeosFB10.GetPersistenceLayerName: string;
begin
  result:= cTIPersistZeosFB10;
end;

function TtiPersistenceLayerZeosFB10.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryZeos;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosFB10);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB10);


end.

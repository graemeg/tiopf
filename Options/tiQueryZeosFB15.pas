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
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

type
  TtiDatabaseZeosFB15 = class(TtiDatabaseZeosIBFB)
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

constructor TtiDatabaseZeosFB15.Create;
begin
  inherited;

  Connection.Protocol := 'firebird-1.5';
end;


{ TtiPersistenceLayerZeosFB15 }

procedure TtiPersistenceLayerZeosFB15.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistZeosFB15;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.Username:= 'SYSDBA';
  APersistenceLayerDefaults.Password:= 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerZeosFB15.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosFB15;
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

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosFB15);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB15);


end.

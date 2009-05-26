unit tiQueryZeosMySQL41;

{$I tiDefines.inc}

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
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseZeosMySQL41 = class(TtiDatabaseZeosMySQL)
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

constructor TtiDatabaseZeosMySQL41.Create;
begin
  inherited;
  Connection.Protocol := 'mysql-4.1';
end;

{ TtiPersistenceLayerZeosMySQL41 }

procedure TtiPersistenceLayerZeosMySQL41.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistZeosMySQL41;;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.Username:= 'root';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerZeosMySQL41.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosMySQL41;
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

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosMySQL41);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL41);


end.

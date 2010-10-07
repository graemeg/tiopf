unit tiQueryZeosMySQL50;

{$I tiDefines.inc}

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
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseZeosMySQL50 = class(TtiDatabaseZeosMySQL)
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

constructor TtiDatabaseZeosMySQL50.Create;
begin
  inherited;
  Connection.Protocol := 'mysql-5';
end;

{ TtiPersistenceLayerZeosMySQL50 }

procedure TtiPersistenceLayerZeosMySQL50.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistZeosMySQL50;;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= False;
  APersistenceLayerDefaults.Username:= 'root';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerZeosMySQL50.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseZeosMySQL50;
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

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosMySQL50);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosMySQL50);

end.

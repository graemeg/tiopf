unit tiQueryEmbeddedFlashFiler;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  ,tiPersistenceLayers
  ,ffSrEng
  ,ffDB
  ,ffSQLEng
  ,tiFlashFilerAbs
  ;


type

  TtiPersistenceLayerEmbeddedFlashFiler = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseEmbeddedFlashFiler = class(TtiDatabaseFlashFilerAbs)
  private
//    FServerEngine: TffServerEngine;
  protected
    procedure SetConnected(AValue: boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;



implementation

uses
   tiUtils
  ,tiLog
  ,TypInfo
  ,tiOPFManager
  ,tiObject
  ,tiConstants
  ,tiExcept
  ,SysUtils, DB
  ,Variants
 ;



{ TtiPersistenceLayerFlashFiler }

procedure TtiPersistenceLayerEmbeddedFlashFiler.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, cTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistEmbeddedFF;
  APersistenceLayerDefaults.DatabaseName:= cDefaultDatabaseDirectory;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= '';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerEmbeddedFlashFiler.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseEmbeddedFlashFiler;
end;

function TtiPersistenceLayerEmbeddedFlashFiler.GetPersistenceLayerName: string;
begin
  result := cTIPersistEmbeddedFF;
end;

function TtiPersistenceLayerEmbeddedFlashFiler.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryFlashFiler;
end;




{ TtiDatabaseEmbeddedFlashFiler }

constructor TtiDatabaseEmbeddedFlashFiler.Create;
begin
  inherited;

//  FServerEngine := TffServerEngine.Create(nil);
  SetFFEngine(TffServerEngine.Create(nil));

  FFClient.ServerEngine := FFEngine;
end;

destructor TtiDatabaseEmbeddedFlashFiler.Destroy;
begin
//  FServerEngine.Free;

  inherited;
end;

procedure TtiDatabaseEmbeddedFlashFiler.SetConnected(AValue: boolean);
begin
  try
    if (not AValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      InternalDisconnect;
      Exit; //==>
    end;

    FFSession.SetLoginParameters(UserName, Password);
    FFDatabase.AliasName := DatabaseName;

    InternalConnect;
  except
    raise EtiOPFDBException.Create(cTIPersistEmbeddedFF, DatabaseName,
      UserName, Password)
  end;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerEmbeddedFlashFiler);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(
      cTIPersistEmbeddedFF);



end.

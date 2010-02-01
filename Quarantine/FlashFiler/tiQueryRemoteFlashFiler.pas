unit tiQueryRemoteFlashFiler;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  ,tiPersistenceLayers
  ,ffclreng
  ,ffDB
  ,tiFlashFilerAbs
  ;


type

  TtiPersistenceLayerRemoteFlashFiler = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseRemoteFlashFiler = class(TtiDatabaseFlashFilerAbs)
  private
//    FRemoteServerEngine: TffRemoteServerEngine;
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
{$IFDEF DELPHI6ORABOVE}
  ,Variants
{$ENDIF}
 ;



{ TtiPersistenceLayerFlashFiler }

procedure TtiPersistenceLayerRemoteFlashFiler.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, cTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistRemoteFF;
  APersistenceLayerDefaults.DatabaseName:= cDefaultDatabaseDirectory;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= '';
  APersistenceLayerDefaults.Password:= '';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerRemoteFlashFiler.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseRemoteFlashFiler;
end;

function TtiPersistenceLayerRemoteFlashFiler.GetPersistenceLayerName: string;
begin
  result := cTIPersistRemoteFF;
end;

function TtiPersistenceLayerRemoteFlashFiler.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryFlashFiler;
end;




{ TtiDatabaseEmbeddedFlashFiler }

constructor TtiDatabaseRemoteFlashFiler.Create;
begin
  inherited;

//  FRemoteServerEngine := TffRemoteServerEngine.Create(nil);
  SetFFEngine(TffRemoteServerEngine.Create(nil));

  FFClient.ServerEngine := FFEngine;
end;

destructor TtiDatabaseRemoteFlashFiler.Destroy;
begin
  //FRemoteServerEngine.Free;

  inherited;
end;

procedure TtiDatabaseRemoteFlashFiler.SetConnected(AValue: boolean);
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
    raise EtiOPFDBException.Create(cTIPersistRemoteFF, DatabaseName,
      UserName, Password)
  end;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerRemoteFlashFiler);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(
      cTIPersistRemoteFF);



end.

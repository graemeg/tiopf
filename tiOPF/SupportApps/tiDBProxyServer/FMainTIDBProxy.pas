unit FMainTIDBProxy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiHyperlink, StdCtrls, tiDBProxyServerIndyHTTP, ExtCtrls, tiRegINI ;

type
  TFormMainTIDBProxyServer = class(TForm)
    MemoLog: TMemo;
    tmrStartUp: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function  GetDBConnectionAsString:string;
    procedure tmrStartUpTimer(Sender: TObject);
  private
    FtiDBProxy : TtiDBProxyServerIndyHTTP ;
    FPerLayerName : string ;
    FDatabaseName : string ;
    FUsername     : string ;
    FPassword     : string ;
    FINI          : TtiINIFile ;
  public
  end;

var
  FormMainTIDBProxyServer: TFormMainTIDBProxyServer;

{$R *.DFM}

implementation

uses
  tiPersist
  ,tiQuery
  ,tiDialogs
  ,tiPerObjOIDGUID
  ,tiUtils
  ,tiQueryRemote_Svr
  ,cTIPersist
  ,tiQueryRemote
  ,tiLog
  ,tiXML
  ,tiObjAbs
  ,tiDBConnectionPool
  ,cTIDBProxyServerConstants
  ;

procedure TFormMainTIDBProxyServer.FormCreate(Sender: TObject);
var
  lStaticPageLocation  : string ;
  lTimeOut      : integer ;
begin
  FINI          := TtiINIFile.Create( tiAddTrailingSlash(tiGetEXEPath) + 'tiDBProxyServer.ini') ;

  CreateMutex(nil, False, cTIDBProxyServerMutex );
  gLog.RegisterGUILog ;
  FINI.ReadFormState(Self);

  // Move all this to an object as it's shared between two aps
  Caption := FINI.ReadString('System', 'ApplicationTitle', cRemoteServerMainFormCaption ) ;
  Application.Title := Caption ;
  FPerLayerName        := FINI.ReadString('DatabaseConnection', 'PerLayerName', cTIPersistIBX ) ;
  FDatabaseName        := FINI.ReadString('DatabaseConnection', 'DatabaseName', ExpandFileName('..\Data\Demo.gdb')) ;
  FUserName            := FINI.ReadString('DatabaseConnection', 'UserName', 'SYSDBA' ) ;
  FPassword            := FINI.ReadString('DatabaseConnection', 'Password', 'masterkey' ) ;
  lStaticPageLocation  := FINI.ReadString('System',     'StaticPageLocation', tiAddTrailingSlash( tiGetEXEPath ) + 'StaticPages' ) ;
  lTimeOut             := FINI.ReadInteger('DatabaseConnection', 'TimeOut', 1 ) ;

  gTIPerMgr.TerminateOnFailedDBConnection := false ;
  gTIPerMgr.LoadPersistenceLayer( FPerLayerName ) ;
  gTIPerMgr.DefaultPerLayerName := FPerLayerName ;

  FtiDBProxy := TtiDBProxyServerIndyHTTP.Create ;
  FtiDBProxy.StaticPageLocation := lStaticPageLocation ;
  gTIPerMgr.LoadDatabaseLayer( FPerLayerName, FDatabaseName, FUserName, FPassword ) ;

  gStatefulDBConnectionPool.TimeOut := lTimeOut ;
  memoLog.Lines.Text := GetDBConnectionAsString ;
  tmrStartup.Enabled := true ;

end;

procedure TFormMainTIDBProxyServer.FormDestroy(Sender: TObject);
begin
  FtiDBProxy.Free;
  FINI.WriteFormState(Self);
  FINI.Free;
end;

function TFormMainTIDBProxyServer.GetDBConnectionAsString:string;
begin
  result :=
    gTIPerMgr.DefaultDBConnectionPool.DetailsAsString + Cr(2) +
    'Transaction timeout:     ' + FloatToStr( gStatefulDBConnectionPool.TimeOut ) + ' min' {+ Cr +} ;
end;

procedure TFormMainTIDBProxyServer.tmrStartUpTimer(Sender: TObject);
begin
  tmrStartup.Enabled := false ;
end;

end.

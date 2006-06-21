unit FMainTIDBProxyServiceController;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JclSvcCtrl, StdCtrls, tiRegINI ;

type
  TForm1 = class(TForm)
    btnStartStop: TButton;
    lblIsRunning: TLabel;
    btnInstallUnInstall: TButton;
    btnClose: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure btnInstallUnInstallClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    FINI                : TtiINIFile ;
    FServiceDisplayName : string ;
    function  IsInstalled : boolean ;
    function  IsRunning : boolean ;
    procedure SetCaptions;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


implementation
uses
  cTIPersist
  ,tiUtils
  ,tiDialogs
  ,cTIDBProxyServerConstants
  {$IFDEF madexcept} ,madexcept {$ENDIF}
  ;

const
  cBtnCaptionStartService = 'Start service' ;
  cBtnCaptionStopService  = 'Stop service' ;
  cBtnCaptionInstallService = 'Install service' ;
  cBtnCaptionUnInstallService = 'Uninstall service' ;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateMutex(nil, False, cTIDBProxyServerMutex );
  FINI          := TtiINIFile.Create( tiAddTrailingSlash(tiGetEXEPath) + 'tiDBProxyServer.ini') ;
  FServiceDisplayName := FINI.ReadString('System', 'ApplicationTitle',
                             cRemoteServerMainFormCaption ) ;
  Caption := FServiceDisplayName + ' Controller' ;
  Application.Title := Caption ;
  SetCaptions;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FINI.Free;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
//  FServiceManager.StartService ;
end;

procedure TForm1.SetCaptions;
begin
  if not IsInstalled then
  begin
    lblIsRunning.Caption := 'The ' + FServiceDisplayName + ' IS NOT installed ' +
                            'into Windows Service Manager. Click <Install Service> ' +
                            'to install now.';
    btnStartStop.Caption := cBtnCaptionStartService ;
    btnStartStop.Enabled := false ;
    btnInstallUnInstall.Caption := cBtnCaptionInstallService;
    btnInstallUnInstall.Enabled := true ;
    btnInstallUnInstall.Hint := 'Install into Windows Service Manager' ;
    Exit ; //==>
  end ;

  // The service is installed.
  if IsRunning then
  begin
    lblIsRunning.Caption := 'The ' + FServiceDisplayName + ' IS running.' + Cr(2) +
                            'Click <Stop Service> to stop the service.';
    btnStartStop.Caption := cBtnCaptionStopService ;
    btnStartStop.Enabled := true ;
    btnStartStop.Hint := 'Stop the service';
    btnInstallUnInstall.Caption := cBtnCaptionUnInstallService;
    btnInstallUnInstall.Enabled := false ;
  end else
  begin
    lblIsRunning.Caption := 'The ' + FServiceDisplayName + ' IS NOT running.' + Cr(2) +
                            'Click <Start Service> to start the service.';
    btnStartStop.Caption := cBtnCaptionStartService ;
    btnStartStop.Enabled := true ;
    btnInstallUnInstall.Caption := cBtnCaptionUnInstallService;
    btnInstallUnInstall.Enabled := true ;
    btnStartStop.Hint := 'Start the service';
    btnInstallUnInstall.Hint := 'Uninstall from Windows Service Manager' ;
  end ;

end;

procedure TForm1.btnStartStopClick(Sender: TObject);
var
  lService : TJclNtService ;
  lServiceManager : TJclSCManager ;
begin
  if not IsInstalled then
    Exit ; //==>
  lServiceManager     := TJclSCManager.Create ;
  try
    lServiceManager.Refresh(true);
    lServiceManager.FindService(cTIDBProxyServiceName, lService);
    Screen.Cursor := crHourGlass ;
    try
      if lService.ServiceState <> ssRunning then
        lService.Start
      else
        lService.Stop;
      Sleep(2000);  
    finally
      screen.cursor := crDefault ;
    end ;
  finally
    lServiceManager.Free;
  end;
  SetCaptions;
end;

procedure TForm1.btnInstallUnInstallClick(Sender: TObject);
var
  lFileName : string ;
begin
  lFileName := tiAddTrailingSlash( tiGetEXEPath ) + 'tiDBProxyService.exe' ;
  Screen.Cursor := crHourGlass ;
  try
    if IsInstalled then
    begin
      if tiAppConfirmation( 'This will uninstall the service from Windows' + Cr +
                            'ServiceManager. It will not delete the service' + Cr +
                            'from your computer.' + Cr(2) +
                            'Are you sure you want to continue?' ) then
        tiShellExecute(lFileName, '/UNINSTALL');
        Sleep( 3000 ) ;
    end else
    begin
      tiShellExecute(lFileName, '/INSTALL');
      Sleep( 3000 ) ;
    end;
  finally
    Screen.Cursor := crDefault ;
  end ;
  SetCaptions;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close ;
end;

function TForm1.IsInstalled: boolean;
var
  lService : TJclNtService ;
  lServiceManager : TJclSCManager ;
begin
  lServiceManager := TJclSCManager.Create ;
  try
    lServiceManager.Refresh(true);
    lServiceManager.FindService(cTIDBProxyServiceName, lService);
    result := lService <> nil ;
  finally
    lServiceManager.Free;
  end ;
end;

function TForm1.IsRunning: boolean;
var
  lService : TJclNtService ;
  lServiceManager : TJclSCManager ;
begin
  lServiceManager := TJclSCManager.Create;
  try
    lServiceManager.Refresh(true);
    lServiceManager.FindService(cTIDBProxyServiceName, lService);
    if lService <> nil then
      result := lService.ServiceState = ssRunning
    else
      result := false ;
  finally
    lServiceManager.Free;
  end;
end;

end.

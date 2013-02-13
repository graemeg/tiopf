unit main;

interface

procedure Register;

implementation

uses
   ToolsAPI
  ,XP_OTAWizards  // TXP_OTAWizard
  ,ExtCtrls       // TTimer
  ,Classes        // TNotifyEvent
  ,IniFiles       // TIniFile
  ,SysUtils       // StrToIntDef
  ,WatchFile      // ReadWatchFile
  ,XP_OTAUtils    // GetCurrentProject, GetProjectAbsoluteSearchPaths, OpenFileInIDE...
  ,Forms          // Application
  ,Windows
  ;

const
  cPluginAuthor = 'Paul Spain';
  cPluginName = 'DUnitTestInspector';

type

  TDUnitTestInspector = class(TXP_OTAWizard)
  private
    FTimer: TTimer;
    FDestroyed: boolean;
    function InitialiseTimer(var ATimer: TTimer;
      const ATimerEvent: TNotifyEvent;
      const AFrequencyMSec: integer = cWatchFileInspectionFrequencyMSec): boolean;
    procedure TimerEvent(Sender: TObject);
    function BuildSearchPaths(const ASearchPaths: TStrings): boolean;

  protected
    function GetAuthor: string; override;
    function GetName: string; override;
    procedure Destroyed; override;


  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure Register;
begin
  ToolsAPI.RegisterPackageWizard(TDUnitTestInspector.Create);
end;

procedure SwitchToThisWindow(hwnd: HWND; fUnknown: BOOL); stdcall;
  external user32 name 'SwitchToThisWindow';

{ TDUnitTestInspector }

function TDUnitTestInspector.BuildSearchPaths(const ASearchPaths: TStrings): boolean;
var
  LActiveProject: IOTAProject;
  LSearchPaths: TStrings;
begin
  Result := false;
  LSearchPaths := TStringList.Create;

  try

    if XP_OTAUtils.GetCurrentProject(LActiveProject)
      and XP_OTAUtils.GetProjectAbsoluteSearchPaths(LActiveProject, LSearchPaths) then
    begin
      ASearchPaths.AddStrings(LSearchPaths);
      LSearchPaths.Clear;

      if XP_OTAUtils.GetIDEDelphiLibraryPath(LSearchPaths) then
      begin
        ASearchPaths.AddStrings(LSearchPaths);
        Result := true;
      end;

    end;

  finally
    LSearchPaths.Free;
  end;
end;

constructor TDUnitTestInspector.Create;
begin
  inherited Create;
  InitialiseTimer(FTimer, TimerEvent);
end;

destructor TDUnitTestInspector.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TDUnitTestInspector.Destroyed;
begin
  inherited;
  // TODO: not being called by IDE
  FDestroyed := true;
end;

function TDUnitTestInspector.GetAuthor: string;
begin
  Result := cPluginAuthor;
end;

function TDUnitTestInspector.GetName: string;
begin
  Result := cPluginName;
end;

function TDUnitTestInspector.InitialiseTimer(var ATimer: TTimer;
  const ATimerEvent: TNotifyEvent; const AFrequencyMSec: integer): boolean;
begin
  Result := false;

  if not Assigned(ATimer) then
  begin
    ATimer := TTimer.Create(nil);
    ATimer.Enabled := false;
    ATimer.OnTimer := ATimerEvent;
    ATimer.Interval := AFrequencyMSec;
    ATimer.Enabled := true;
    Result := true;
  end;
end;

procedure TDUnitTestInspector.TimerEvent(Sender: TObject);
var
  LFilesToOpen: TStrings;
  LSearchPaths: TStrings;
  LFileName: string;
  LFilePath: string;
  LLineNumber: integer;
  i,j: integer;
const
  cAltTab = true;
begin
  LFilesToOpen := nil;
  LSearchPaths := nil;

  if FDestroyed then
    exit;

  try
    LFilesToOpen := TStringList.Create;

    if WatchFile.ReadWatchFile(LFilesToOpen) then
    begin
      LSearchPaths := TStringList.Create;
      BuildSearchPaths(LSearchPaths);

      for i := 0 to LFilesToOpen.Count - 1 do
      begin
        LFileName := LFilesToOpen.Names[i];
        LLineNumber := StrToIntDef(LFilesToOpen.ValueFromIndex[i], 1);

        for  j := 0 to LSearchPaths.Count - 1 do
        begin
          LFilePath := LSearchPaths[j] + LFileName;

          if FileExists(LFilePath) then
          begin
            XP_OTAUtils.OpenFileInIDE(LFilePath, LLineNumber);
            Break;
          end;

        end;

        // Application.BringToFront; // doesn't work for minimised app
        // Bring app to foreground (even if minimised)
        SwitchToThisWindow(Application.MainForm.Handle, cAltTab);
      end;

    end;

  finally
    LSearchPaths.Free;
    LFilesToOpen.Free;
  end;

end;

end.

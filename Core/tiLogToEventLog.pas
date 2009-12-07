unit tiLogToEventLog;

{$I tiDefines.inc}

interface
uses
  tiLog
 ,Classes
 ;

type

  {: Log to Windows Event Application Log }
  TtiLogToEventLog = class(TtiLogToAbs)
  private
    FList: TThreadList;
  protected
    function  AcceptEvent(const ADateTime : string;
                           const AMessage : string;
                           ASeverity : TtiLogSeverity): boolean; override;
    procedure WriteToOutput; override;
  public
    procedure   Log(const ADateTime : string;
                     const AThreadID : string;
                     const AMessage : string;
                     ASeverity : TtiLogSeverity); override;
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation
uses
  tiUtils;

{ TtiLogToEventLog }

function TtiLogToEventLog.AcceptEvent(const ADateTime, AMessage: string;
  ASeverity: TtiLogSeverity): boolean;
begin
  result := (ASeverity = lsError);
end;

constructor TtiLogToEventLog.Create;
begin
  inherited;
  FList := TThreadList.Create;
end;

destructor TtiLogToEventLog.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TtiLogToEventLog.Log(const ADateTime, AThreadID, AMessage: string;
  ASeverity: TtiLogSeverity);
var
  lLogEvent: TtiLogEvent;
begin
  if not AcceptEvent(ADateTime, AMessage, ASeverity) then
    Exit; //==>
  lLogEvent := TtiLogEvent.Create;
  lLogEvent.DateTime  := ADateTime;
  lLogEvent.LogMessage := AMessage;
  lLogEvent.Severity  := ASeverity;
  lLogEvent.ThreadID  := AThreadID;
  FList.LockList.Add(lLogEvent);
  FList.UnlockList;
  WriteToOutput;
end;

procedure TtiLogToEventLog.WriteToOutput;
var
  LList: TList;
  i: integer;
  LLogType: string;
  LLogSource: string;
begin
  LList := FList.LockList;
 try
   for i := 0 to LList.Count - 1 do
   begin
     LLogType := TtiLogEvent(LList[i]).SeverityAsGUIString;
     LLogSource := tiExtractFileNameOnly(tiGetModuleFileName);
     tiShellExecute('eventcreate.exe', '/T ' + LLogType + ' /SO "' + LLogSource + '" /ID 1 /L APPLICATION /D "' + TtiLogEvent(LList[i]).LogMessage + '"');
     TtiLogEvent(LList[i]).Free;
   end;
   LList.Clear;
 finally
   FList.UnlockList;
 end;

end;

end.




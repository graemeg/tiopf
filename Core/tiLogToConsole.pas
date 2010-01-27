unit tiLogToConsole;

{$I tiDefines.inc}

interface
uses
  tiLog
 ,Classes
 ;

type

  {: Log to console (stdout) }
  TtiLogToConsole = class(TtiLogToAbs)
  private
    FList: TThreadList;
  protected
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
  Windows;

{ TtiLogToConsole }

constructor TtiLogToConsole.Create;
begin
  inherited;
  FList := TThreadList.Create;
end;

destructor TtiLogToConsole.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TtiLogToConsole.Log(const ADateTime, AThreadID, AMessage: string;
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

procedure TtiLogToConsole.WriteToOutput;
var
  LList: TList;
  i: integer;
begin
  LList := FList.LockList;

 try

   for i := 0 to LList.Count - 1 do
   begin
     Writeln(TtiLogEvent(LList[i]).AsLeftPaddedString);
     TtiLogEvent(LList[i]).Free;
   end;

   LList.Clear;
 finally
   FList.UnlockList;
 end;

end;

end.

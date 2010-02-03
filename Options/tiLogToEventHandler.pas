unit tiLogToEventHandler;

{$I tiDefines.inc}

interface

uses
   tiLog
  ;

type

  // Not to be confused with TtiLogEvent!
  TtiWriteLogEvent = procedure(ASender: TObject; const ALogEvent: TtiLogEvent) of object;

  // Log to an event handler. Call the assigned OnLog event handler each time
  // a log call is accepted.
  // NOTE: Do NOT register this logger using the class type like this:
  //       RegisterLog(TtiLogToEventHandler). Use one of the following instead:
  //       RegisterLog(TtiLogToEventHandler.Create(DoOnLog)), or
  //       RegisterLog(ASomeLogToEventHandlerInstance)
  TtiLogToEventHandler = class(TtiLogToCacheAbs)
  private
    FOnLog: TtiWriteLogEvent;
  protected
    procedure WriteToOutput; override;
  public
    constructor Create(
        const AOnLog: TtiWriteLogEvent;
        const ASynchronized: Boolean = True); reintroduce; virtual;
    property  OnLog: TtiWriteLogEvent read FOnLog write FOnLog;
  end;


implementation

uses
   SysUtils
  ;

{ TtiLogToEventHandler }

constructor TtiLogToEventHandler.Create(
    const AOnLog: TtiWriteLogEvent;
    const ASynchronized: Boolean);
begin
  if ASynchronized then
    inherited CreateSynchronized
  else
    inherited Create;
  FOnLog := AOnLog;
  ThrdLog.Start;
end;

procedure TtiLogToEventHandler.WriteToOutput;
var
  I: Integer;
begin
  if ThrdLog.Terminated or (not Assigned(FOnLog)) then
    Exit; //==>

  inherited WriteToOutput;

  for I := 0 to Pred(ListWorking.Count) do
  begin
    if ThrdLog.Terminated then
      Break; //==>
    FOnLog(Self, ListWorking.Items[I]);
  end;

  ListWorking.Clear;
end;

end.

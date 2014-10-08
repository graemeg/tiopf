unit tiLogToDebugString;

interface

uses
  SysUtils,
  Classes,
  tiLog;


type
  TtiLogToDebugString = class(TtiLogToAbs)
  private
    FList: TThreadList;
    procedure WriteToDebugString(const AMessage: string);
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
  Windows,
  tiUtils;


{ TtiLogToToDebugString }

constructor TtiLogToDebugString.Create;
begin
  inherited;

  FList := TThreadList.Create;
end;

destructor TtiLogToDebugString.Destroy;
begin
  FList.Free;

  inherited;
end;

procedure TtiLogToDebugString.Log(const ADateTime, AThreadID,
  AMessage: string; ASeverity: TtiLogSeverity);
var
  lLogEvent: TtiLogEvent;
begin
  lLogEvent := TtiLogEvent.Create;

  lLogEvent.DateTime  := ADateTime;
  lLogEvent.LogMessage := AMessage;
  lLogEvent.Severity  := ASeverity;
  lLogEvent.ThreadID  := AThreadID;

  FList.LockList.Add(lLogEvent);
  FList.UnlockList;

  WriteToOutput;
end;

procedure TtiLogToDebugString.WriteToDebugString(const AMessage: string);
var
  i: integer;
  LLine: string;
  LCount: integer;
begin
  LCount := tiNumToken(AMessage, CrLf);

  if LCount = 1 then
    OutputDebugString(PChar(tiTrimTrailingWhiteSpace(AMessage)))
  else
    for i := 1 to LCount do
    begin
      LLine := tiTrimTrailingWhiteSpace(tiToken(AMessage, CrLf, i));
      OutputDebugString(PChar(LLine));
    end;
end;

procedure TtiLogToDebugString.WriteToOutput;
var
  LList: TList;
  i: integer;
//  LLogType: string;
//  LLogSource: string;
  LLogEvent: TtiLogEvent;
begin
  LList := FList.LockList;

  try
    for i := 0 to LList.Count - 1 do
    begin
      //     LLogType := TtiLogEvent(LList[i]).SeverityAsGUIString;
      //     LLogSource := tiExtractFileNameOnly(tiGetModuleFileName);
      LLogEvent := TtiLogEvent(LList[i]);

      WriteToDebugString(LLogEvent.AsLeftPaddedString);

      TtiLogEvent(LList[i]).Free;
    end;

    LList.Clear;
  finally
   FList.UnlockList;
  end;
end;

end.

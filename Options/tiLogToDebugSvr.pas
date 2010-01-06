{
  This unit logs to a Debug Server application. The Debug Server can
  be available or not. If not, it will automatically be started or
  log messages will simply be lost in space.

  Delphi:
    The debug interface unit is included with the GExperts add-on package.
  FPC:
    FPC includes the debug interface unit as standard in the FCL. The interface
    is based on the GExperts debug API, but with the addition that it will
    work 100% on all platforms.

  Author: Graeme Geldenhuys <graemeg@gmail.com> - Dec 2009
}
unit tiLogToDebugSvr;

{$I tiDefines.inc}

interface
uses
  tiLog
  ;

type
  {: Log to a debug server }
  TtiLogToDebugSvr = class(TtiLogToAbs)
  protected
    function  AcceptEvent(const ADateTime : string;
                           const AMessage : string;
                           ASeverity : TtiLogSeverity): boolean; override;
    procedure WriteToOutput; override; 
  public
    destructor  Destroy; override;
    procedure   Log(const ADateTime, AThreadID, AMessage: string; ASeverity: TtiLogSeverity); override;
  end;


implementation
uses
  tiCommandLineParams
  ,tiUtils
  ,SysUtils
  ,tiConstants
  ,dbugintf
  ;
  

procedure DebugLn(const s: string);
begin
  SendDebug(s);
end;

{ TLogToConsole }

function TtiLogToDebugSvr.AcceptEvent(const ADateTime, AMessage: string;
  ASeverity: TtiLogSeverity): boolean;
begin
  Assert(ADateTime = ADateTime);  // Getting rid of compiler hints
  Assert(AMessage = AMessage);    // regarding params not used.
  
//  if gCommandLineParams.IsParam(csLogConsole) then
    result := (ASeverity = lsUserInfo) or (ASeverity in SevToLog)
//  else
//    result := ASeverity in [lsUserInfo];
end;

destructor TtiLogToDebugSvr.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TtiLogToDebugSvr.Log(const ADateTime, AThreadID,
  AMessage: string; ASeverity: TtiLogSeverity);

  procedure _WriteRemainingLines(const AText: string);
  var
    LMessage: string;
    i: Integer;
  const
    cMargin = 4;
  begin
    LMessage := WrapText(AText, cLineEnding, [' ', '-', #9, '\'], 79 - cMargin);
    for i := 1 to tiNumToken(LMessage, cLineEnding) do
      DebugLn(tiSpace(cMargin) + tiToken(LMessage, cLineEnding, i));
  end;

  procedure _WriteLines(const AText: string);
  var
    LMessage: string;
    LPos: Integer;
  begin
    LMessage := WrapText(AText, cLineEnding, [' ', '-', #9, '\'], 78);
    LPos := Pos(cLineEnding, LMessage)-1;
    DebugLn(Copy(LMessage, 1, LPos));
    _WriteRemainingLines(Copy(AText, LPos+1, Length(AText) - LPos));
  end;

var
  lLogEvent: TtiLogEvent;
  lMessage: string;
begin
  Assert(AThreadID = AThreadID);  // Getting rid of compiler hints, unused params

  if not AcceptEvent(ADateTime, AMessage, ASeverity) then
    Exit; //==>

  lLogEvent := TtiLogEvent.Create;
  try
    lLogEvent.LogMessage := AMessage;
    lMessage := lLogEvent.AsStringStripCrLf;
    lMessage := Copy(lMessage, cuiWidthSeverity+4, Length(lMessage));
    if Length(lMessage) > 79 then
      _WriteLines(lMessage)
    else
      DebugLn(lMessage);
  finally
    lLogEvent.Free;
  end;
end;

procedure TtiLogToDebugSvr.WriteToOutput;
begin
  // Nothing to do because we don't use caching and threading while logging to DebugServer
end;

end.

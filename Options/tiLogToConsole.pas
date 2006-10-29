unit tiLogToConsole;

interface
uses
  tiLog
 ;

type

  // Log to a console
  TtiLogToConsole = class(TtiLogToAbs)
  private
  protected
    function  AcceptEvent(const ADateTime : string;
                           const AMessage : string;
                           ASeverity : TtiLogSeverity): boolean; override;
    procedure WriteToOutput; override; 
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Log(const ADateTime : string;
                     const AThreadID : string;
                     const AMessage : string;
                     ASeverity : TtiLogSeverity); override;
  end;

implementation
uses
  tiCommandLineParams
  ,tiUtils
  , SysUtils;

{ TLogToConsole }

function TtiLogToConsole.AcceptEvent(const ADateTime, AMessage: string;
  ASeverity: TtiLogSeverity): boolean;
begin
  if gCommandLineParams.IsParam(csLogVisual) then
    result := (ASeverity = lsUserInfo) or (ASeverity in gLog.SevToLog)
  else
    result :=
      ASeverity in [lsUserInfo];
end;

constructor TtiLogToConsole.Create;
begin
  inherited;
end;

destructor TtiLogToConsole.Destroy;
begin
  Terminate;
  inherited;
end;

procedure TtiLogToConsole.Log(const ADateTime, AThreadID,
  AMessage: string; ASeverity: TtiLogSeverity);

  procedure _WriteRemainingLines(const AText: string);
  var
    LMessage: string;
    i: Integer;
  const
    cMargin = 4;
  begin
    LMessage := WrapText(AText, #13#10, [' ', '-', #9, '\'], 79 - cMargin);
    for i := 1 to tiNumToken(LMessage, #13#10) do
      WriteLn(tiSpace(cMargin) + tiToken(LMessage, #13#10, i));
  end;

  procedure _WriteLines(const AText: string);
  var
    LMessage: string;
    LPos: Integer;
  begin
    LMessage := WrapText(AText, #13#10, [' ', '-', #9, '\'], 78);
    LPos := Pos(#13#10, LMessage)-1;
    WriteLn(Copy(LMessage, 1, LPos));
    _WriteRemainingLines(Copy(AText, LPos+1, Length(AText) - LPos));
  end;

var
  lLogEvent : TtiLogEvent;
  lMessage: string;
begin
  if not AcceptEvent(ADateTime, AMessage, ASeverity) then
    Exit; //==>

  lLogEvent := TtiLogEvent.Create;
  try
    lLogEvent.LogMessage := AMessage;
    lMessage   := lLogEvent.AsStringStripCrLf;
    lMessage := Copy(lMessage, cuiWidthSeverity+4, Length(lMessage));
    if Length(lMessage) > 79 then
    begin
      _WriteLines(lMessage);
    end else
      WriteLn(lMessage);
  finally
    lLogEvent.Free;
  end;
end;

procedure TtiLogToConsole.WriteToOutput;
begin

end;

initialization
  gLog.RegisterLog(TtiLogToConsole.Create);

end.

{
  This unit can safely be included in a Console or GUI application. For
  GUI applications the output will just disappear in the void of space. :-)
  
  Note:
    Under Linux, GUI applications still have access to StdOut when they are
    run from a terminal window. Very handly for debugging.
}
unit tiLogToConsole;

{$I tiDefines.inc}

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
    destructor  Destroy; override;
    procedure   Log(const ADateTime, AThreadID, AMessage: string; ASeverity: TtiLogSeverity); override;
  end;


implementation
uses
  tiCommandLineParams
  ,tiUtils
  ,SysUtils
  ,tiConstants
  ;
  

function ConvertLineEndings(const s: string): string;
var
  i: Integer;
  EndingStart: LongInt;
begin
  Result := s;
  i := 1;
  while (i <= Length(Result)) do
  begin
    if Result[i] in [#10,#13] then
    begin
      EndingStart := i;
      inc(i);
      if (i <= Length(Result)) and
         (Result[i] in [#10,#13]) and
         (Result[i] <> Result[i-1]) then
      begin
        inc(i);
      end;
      if (Length(cLineEnding) <> i-EndingStart) or
         (cLineEnding <> Copy(Result, EndingStart, Length(cLineEnding))) then
      begin
        // line end differs => replace with current LineEnding
        Result :=
            Copy(Result, 1, EndingStart-1)
            + cLineEnding
            + Copy(Result, i, Length(Result));
        i := EndingStart + Length(cLineEnding);
      end;
    end
    else
      inc(i);
  end;  { while }
end;

procedure DebugLn(const s: string);
begin
  if not IsConsole then
    Exit; //==>
  WriteLn(ConvertLineEndings(s));
end;

{ TLogToConsole }

function TtiLogToConsole.AcceptEvent(const ADateTime, AMessage: string;
  ASeverity: TtiLogSeverity): boolean;
begin
  Assert(ADateTime = ADateTime);  // Getting rid of compiler hints
  Assert(AMessage = AMessage);    // regarding params not used.
  
//  if gCommandLineParams.IsParam(csLogConsole) then
    result := (ASeverity = lsUserInfo) or (ASeverity in SevToLog)
//  else
//    result := ASeverity in [lsUserInfo];
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
  lLogEvent : TtiLogEvent;
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
    begin
      _WriteLines(lMessage);
    end else
      DebugLn(lMessage);
  finally
    lLogEvent.Free;
  end;
end;

procedure TtiLogToConsole.WriteToOutput;
begin

end;

end.

unit tiLogToConsole;

{$I tiDefines.inc}

interface
uses
  tiLog
  ;


type
  { Log to a console and log immediately, not using any cache }
  TtiLogToConsole = class( TtiLogToAbs )
  private
  protected
    function  AcceptEvent( const psDateTime : string ;
                           const psMessage  : string;
                           pSeverity  : TtiLogSeverity ) : boolean ; override ;
    procedure WriteToOutput; override; 
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Log( const psDateTime : string ;
                     const psThreadID : string ;
                     const psMessage  : string;
                     pSeverity  : TtiLogSeverity ) ; override ;
  end;

implementation
uses
  tiCommandLineParams
  ,tiUtils
  ,SysUtils
  ;

{ TLogToConsole }

{ Seems we always want to log lsUserInfo events }
function TtiLogToConsole.AcceptEvent(const psDateTime, psMessage: string;
  pSeverity: TtiLogSeverity): boolean;
begin
//  if gCommandLineParams.IsParam( csLogVisual ) then
    result := ( pSeverity = lsUserInfo ) or (pSeverity in gLog.SevToLog)
//  else
//    result := pSeverity in [lsUserInfo];
end;


constructor TtiLogToConsole.Create;
begin
  inherited;
end;


destructor TtiLogToConsole.Destroy;
begin
  Terminate ;
  inherited;
end;


procedure TtiLogToConsole.Log(const psDateTime, psThreadID,
    psMessage: string; pSeverity: TtiLogSeverity);

  //------------
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

  //------------
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
  lLogEvent: TtiLogEvent;
  lMessage: string;
begin
  if not AcceptEvent( psDateTime, psMessage, pSeverity ) then
    Exit; //==>

  lLogEvent := TtiLogEvent.Create;
  try
    lLogEvent.LogMessage := psMessage;
    lMessage    := lLogEvent.AsStringStripCrLf ;
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
  { do nothing }
end;


end.

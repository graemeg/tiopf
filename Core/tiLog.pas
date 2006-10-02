unit tiLog;

{$I tiDefines.inc}

interface
uses
  Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,SyncObjs   // This unit must always appear after the Windows unit!
  ,Contnrs
  ,SysUtils
  ,tiBaseObject
  ,tiThread
  ,types
  ;

const
  crsSeverityNotFound = 'Severity <%s> not found';
  cErrorCanNotCreateLogDirectory = 'Can not create directory for log files <%s>';
  // Command line parameters
  csLog         = 'l' ; // Command line parameter to turn logging on (default log to file)
  csLogVisual   = 'lv'; // Command line parameter to turn visual logging on
  csLogConsole  = 'lc'; // Command line parameter to turn console logging on

type

  // I use this set to provide additional log severity types
  // If you change the set of log event types, then remember to change the
  // strings in caLogSeverityStrings (below) too.
  TtiLogSeverity = (
                    lsNormal
                   ,lsUserInfo
                   ,lsObjCreation
                   ,lsVisitor
                   ,lsConnectionPool
                   ,lsAcceptVisitor
                   ,lsQueryTiming
                   ,lsDebug
                   ,lsWarning
                   ,lsError
                   ,lsSQL
                );

const

  ctiLogSeverityStrings: array[ TtiLogSeverity ] of String = (
                    'Norm'
                   ,'Info'
                   ,'Obj'
                   ,'Vis'
                   ,'DBCon'
                   ,'AVis'
                   ,'Query'
                   ,'Debug'
                   ,'Warn'
                   ,'Error'
                   ,'SQL'
                );


type
  { Forward declaration of the main logging class }
  TtiLog = class;


  { It is possible to filter for a group of log events }
  TtiSevToLog = set of TtiLogSeverity;


  { A class to hold a logged event while in the cache }
  TtiLogEvent = class(TtiBaseObject)
  private
    FLogMessage : String;
    FsDateTime  : String;
    FSeverity   : TtiLogSeverity;
    FsThreadID  : string;
    function  GetSeverityAsString: string;
    procedure SetSeverityAsString(const Value: string);
    function  GetShortLogMessage: string;
    function  GetFormattedMessageTimeStamp: string;
  published
    property DateTime   : string       read FsDateTime  write FsDateTime ;
    property LogMessage : String       read FLogMessage write FLogMessage;
    property ShortLogMessage : string  read GetShortLogMessage;
    property Severity   : TtiLogSeverity read FSeverity   write FSeverity ;
    property ThreadID   : string       read FsThreadID  write FsThreadID;
    property SeverityAsString : string read GetSeverityAsString write SetSeverityAsString;
  public
    function AsString   : string;
    function AsStringStripCrLf : string;
    function AsLeftPaddedString: string;
  end;


  { Holds a list of TtiLogEvent objects }
  TtiLogEvents = class(TtiBaseObject)
  private
    FList: TObjectList;
    function GetItems(pIndex: Integer): TtiLogEvent;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(pItem: TtiLogEvent);
    property    Items[pIndex: Integer]: TtiLogEvent Read GetItems;
    function    Count: Integer;
    procedure   Clear;
  end;


  { Abstract base class to manage logging to anything }
  TtiLogToAbs = class(TtiBaseObject)
  private
    FSevToLog: TtiSevToLog;
    FTerminated: Boolean;
  protected
    function  AcceptEvent(const psDateTime : string;
                           const psMessage  : string;
                           pSeverity  : TtiLogSeverity) : boolean; virtual;
    { Only used by decendant classes that use caching and threading while logging }
    procedure WriteToOutput; virtual; abstract;
    procedure SetSevToLog(const Value: TtiSevToLog); virtual;
  public
    constructor Create; virtual;
    procedure   Log(const psDateTime : string;
                     const psThreadID : string;
                     const psMessage  : string;
                     pSeverity  : TtiLogSeverity); virtual; abstract;
    property    SevToLog : TtiSevToLog read FSevToLog Write SetSevToLog;
    { Placeholder method for any terminating code you might require. }
    procedure   Terminate; virtual;
    property    Terminated : boolean read FTerminated;
  end;


  TtiLogToClass = class of TtiLogToAbs;
  TtiLogToCacheAbs = class;


  // A thread class to manage the writing out of events when the
  // system has some free time
  TtiThrdLog = class(TtiSleepThread)
  private
    FLogTo : TtiLogToCacheAbs;
    procedure   SetLogTo(const Value: TtiLogToCacheAbs);
    procedure   WriteToOutput;
  public
    constructor CreateExt(pLogTo : TtiLogToCacheAbs);
    procedure   Execute; override;
    property    Terminated;  // surfaced from protected
    procedure   tiSynchronize(Method: TThreadMethod); // surfaced Synchronize from protected
    property    LogTo : TtiLogToCacheAbs  read FLogTo write SetLogTo;
  end;


  // Abstract base class to manage cached logging
  TtiLogToCacheAbs = class(TtiLogToAbs)
  private
    FList: TList;
    FListWorking: TtiLogEvents;
    FCritSect: TCriticalSection;
    FThrdLog: TtiThrdLog;
  protected
    property  ThrdLog: TtiThrdLog read FThrdLog;
    property  ListWorking: TtiLogEvents read FListWorking;
    procedure WriteToOutput; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Log(const psDateTime : string;
                    const psThreadID : string;
                    const psMessage  : string;
                    pSeverity: TtiLogSeverity); override;
    procedure Terminate; override;
  end;


  // The main logging class
  TtiLog = class(TtiBaseObject)
  private
    FLogToList : TList;
    FSevToLog: TtiSevToLog;
    procedure SetSevToLog(const Value: TtiSevToLog);
    function  IsRegistered(const ALogToClass : TtiLogToClass) : boolean;
  public
    constructor Create ;
    destructor  Destroy; override;
    procedure   RegisterLog(ALogTo : TtiLogToAbs); overload;
    procedure   RegisterLog(ALogTo : TtiLogToClass); overload;
    function    FindByLogClass(ALogToClass : TtiLogToClass) : TtiLogToAbs;
    procedure   Log(const psMessage : string;
                     const pSeverity : TtiLogSeverity = lsNormal);
    property    SevToLog : TtiSevToLog read FSevToLog write SetSevToLog;
    function    LogToFileName: string;
  end;


// The log object is a singleton
function  gLog : TtiLog;

// Some global proces to make logging easier
procedure Log(const pMessage : string ; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure Log(const pMessage : integer; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure Log(const pMessage : Extended; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure Log(const pMessage : boolean; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure Log(const pA : Array of Const; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure Log(const psMessage : string; const pA : Array of Const; pSeverity : TtiLogSeverity = lsNormal); overload;
procedure LogWarning(const psMessage : string); overload;
procedure LogError(const psMessage : string; pRaiseException : boolean = true); overload;
procedure LogError(const pException : Exception; pRaiseException : boolean = true); overload;
procedure LogError(const psMessage : string; const pA : Array of Const); overload;


const
  // Constants for formatting the error log
  cuiWidthDate         = 19;
  cuiWidthThread       =  4;
  cuiWidthSeverity     =  5;
  cuiWidthShortMessage = 60;


implementation
uses
   tiConstants
  ,tiUtils
  ,tiObject
  ,tiLogToFile
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ELSE}
  ,FileCtrl
  {$ENDIF}
  ;


var
  uLog : TtiLog;
  ubFinalization : boolean;


const
  // Default set of log events to write out. Any events not in this set will
  // be ignored.
  cSevToLog = [
                    lsNormal
                    ,lsUserInfo
//                   ,lsObjCreation
//                   ,lsVisitor
//                   ,lsConnectionPool
                   ,lsAcceptVisitor
                   ,lsQueryTiming
                   ,lsDebug
                   ,lsWarning
                   ,lsError
              ];


// The log is a singleton
function gLog : TtiLog;
begin
  if ubFinalization then
  begin
    result := nil;
    Exit; //==>
  end;
  if uLog = nil then
    uLog := TtiLog.Create;
  result := uLog;
end;


function _IsParam(const psParam: string): boolean;
  //------------
  function _IsThisParam(const psParam, psDelim, psCommandLineParams: string): boolean;
  begin
    result := (pos(psDelim + UpperCase(psParam) + ' ',
                     psCommandLineParams) <> 0)
  end;

  //------------
  function _ReadCommandLineParams : string;
  var
    i : integer;
  begin
    result := '';
    for i := 1 to ParamCount do begin
      result :=
        result +
        upperCase(ParamStr(i)) + ' ';
    end ;
  end;
var
  lsCommandLineParams : string;
begin
  lsCommandLineParams := _ReadCommandLineParams;
  result := _IsThisParam(psParam, '-', lsCommandLineParams) or
            _IsThisParam(psParam, '/', lsCommandLineParams) or
            _IsThisParam(psParam, '\', lsCommandLineParams);
end;


procedure Log(const pMessage : string; pSeverity : TtiLogSeverity = lsNormal);
begin
  if ubFinalization then
    Exit; //==>
  gLog.Log(pMessage, pSeverity);
end;


procedure Log(const pMessage : integer; pSeverity : TtiLogSeverity = lsNormal);
begin
  Log(IntToStr(pMessage), pSeverity);
end;


procedure Log(const pMessage: Extended; pSeverity : TtiLogSeverity = lsNormal); overload;
begin
  Log(FloatToStr(pMessage), pSeverity);
end;


procedure Log(const pMessage : boolean; pSeverity : TtiLogSeverity = lsNormal); overload;
begin
  Log(tiBoolToStr(pMessage), pSeverity);
end;


procedure LogError(const psMessage : string; pRaiseException : boolean = true);
begin
  if ubFinalization then
    Exit; //==>
  gLog.Log(psMessage, lsError);
  {$IFDEF ThirdPartyExceptionHandling}
    if pRaiseException then
      raise exception.Create(psMessage);
  {$ENDIF}
end;


procedure LogError(const pException : Exception; pRaiseException : boolean = true);
begin
  if ubFinalization then
    Exit; //==>
  gLog.Log(pException.Message, lsError);
  {$IFDEF ThirdPartyExceptionHandling}
    if pRaiseException then
      raise Exception(pException.ClassType).Create(pException.Message);
  {$ENDIF}
end;


procedure LogError(const psMessage : string; const pA : Array of Const);
var
  ls : string;
begin
  if ubFinalization then
    Exit; //==>
  try
    ls := Format(psMessage, pA);
  except
    on e:exception do
      ls := 'Unable to evaluate log message <' + psMessage + '> reason: ' + e.Message;
  end;
  gLog.Log(ls, lsError);
end;


procedure LogWarning(const psMessage : string);
begin
  if ubFinalization then
    Exit; //==>
  gLog.Log(psMessage, lsWarning);
end;


procedure Log(const pA : Array of Const; pSeverity : TtiLogSeverity = lsNormal);
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine : string;
begin
  lsLine := '';
  for I := 0 to High(pA) do begin
    if lsLine <> '' then
      lsLine := lsLine + ', ';
    with pA[i] do
      case VType of
        vtInteger:    lsLine := lsLine + IntToStr(VInteger);
        vtBoolean:    lsLine := lsLine + BoolChars[VBoolean];
        vtChar:       lsLine := lsLine + VChar;
        vtExtended:   lsLine := lsLine + FloatToStr(VExtended^);
        vtString:     lsLine := lsLine + VString^;
        vtPChar:      lsLine := lsLine + VPChar;
        vtObject:     lsLine := lsLine + VObject.ClassName;
        vtClass:      lsLine := lsLine + VClass.ClassName;
        vtAnsiString: lsLine := lsLine + string(VAnsiString);
        vtCurrency:   lsLine := lsLine + CurrToStr(VCurrency^);
        vtVariant:
        begin
          if not VarIsNull(VVariant^) then
            lsLine := lsLine + string(VVariant^);
        end;
        vtInt64:      lsLine := lsLine + IntToStr(VInt64^);
      else
        raise exception.Create('Invalid variant type passed to LogArray');
    end;
  end;
  Log(lsLine, pSeverity);
end;


procedure Log(const psMessage : string; const pA : Array of Const; pSeverity : TtiLogSeverity = lsNormal);
var
  lMessage : string;
begin
  try
    lMessage := Format(psMessage, pA);
  except
    on e:exception do
      LogError('Unable to evaluate log message <' + psMessage + '> reason: ' + e.Message);
  end;
  Log(lMessage, pSeverity);
end;


function _StrTran(pStrValue, pStrDel, pStrIns : string) : string;
var i : integer;
    sToChange : string;
begin
  result := '';
  sToChange := pStrValue;
  i := pos(pStrDel, sToChange);
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + pStrIns;
    delete(sToChange, 1, i+length(pStrDel)-1);
    i := pos(pStrDel, sToChange);
  end;
  result := result + sToChange;
end;


function _PadR(pStrValue : string; pIntLen : integer) : string;
begin
  if length(pStrValue) < pIntLen then begin
    while length(pStrValue) < pIntLen do begin
      pStrValue := pStrValue + ' ';
    end;
  end
  else if length(pStrValue) > pIntLen then
    pStrValue := copy(pStrValue, 1, pIntLen);
  result := pStrValue;
end;


function _PadL(pStrValue : string; pIntLen : integer) : string;
begin
  if length(pStrValue) < pIntLen then begin
    while length(pStrValue) < pIntLen do begin
      pStrValue := ' ' + pStrValue;
    end;
  end
  else if length(pStrValue) > pIntLen then
    pStrValue := copy(pStrValue, length(pStrValue)-pIntLen, pIntLen);
  result := pStrValue;
end;


  { TtiLog }

constructor TtiLog.Create;
begin
  inherited;
  FLogToList := TList.Create ;
  FSevToLog  := cSevToLog;
end;


destructor TtiLog.Destroy;
var
  i : integer;
  lLog : TtiLogToAbs;
begin
  for i  := FLogToList.Count - 1 downto 0 do
  begin
    lLog := TtiLogToAbs(FLogToList.Items[i]);
    FLogToList.Delete(i);
    lLog.Free;
  end;
  FLogToList.Free;
  inherited;
end;


function TtiLog.FindByLogClass(ALogToClass: TtiLogToClass): TtiLogToAbs;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FLogToList.Count - 1 do
    if TObject(FLogToList.Items[i]) is ALogToClass then
      result := TtiLogToAbs(FLogToList.Items[i]);
end;


function TtiLog.IsRegistered(const ALogToClass: TtiLogToClass): boolean;
begin
  result := FindByLogClass(ALogToClass) <> nil;
end;


procedure TtiLog.Log(const psMessage: string; const pSeverity: TtiLogSeverity = lsNormal);
var
  lsNow      : string;
  i          : integer;
  lsMessage  : string;
  lsThreadID : string;
begin
  if ubFinalization then
    Exit; //==>

  lsNow := _PadR(FormatDateTime(cIntlDateTimeDisp, Now), Length(cIntlDateTimeDisp));
  lsMessage  := psMessage;

  lsThreadID := IntToStr(GetCurrentThreadID);
  lsThreadID := _PadL(lsThreadID, cuiWidthThread);

  for i := 0 to FLogToList.Count - 1 do
    TtiLogToAbs(FLogToList.Items[i]).Log(lsNow,
                                          lsThreadID,
                                          lsMessage,
                                          pSeverity);
end;


constructor TtiLogToAbs.Create;
begin
  inherited Create;
  FSevToLog := cSevToLog;
  FTerminated := False;
end;


function TtiLogToAbs.AcceptEvent(const psDateTime : string;
                                const psMessage  : string;
                                pSeverity  : TtiLogSeverity): boolean;
begin
  result := (pSeverity in FSevToLog);
end;


constructor TtiLogToCacheAbs.Create;
begin
  inherited Create;
  FList         := TList.Create;
  FListWorking  := TtiLogEvents.Create;
  FCritSect     := TCriticalSection.Create;
  FThrdLog      := TtiThrdLog.CreateExt(self); // Must call FThrdLog.Resume in the descandant classes
end;


destructor TtiLogToCacheAbs.Destroy;
var
  i: integer;
begin
  Terminate;
  for i := FList.Count - 1 downto 0 do
    TObject(FList.Items[i]).Free;
  FThrdLog.Free;  // <== Add this, round about line 597 of tiLog.pas
  FList.Free;
  FListWorking.Free;
  FCritSect.Free;
  inherited;
end;


procedure TtiLogToCacheAbs.Log(const psDateTime : string;
                              const psThreadID : string;
                              const psMessage  : string;
                              pSeverity  : TtiLogSeverity);
var
  lLogEvent : TtiLogEvent;
begin

  if not AcceptEvent(psDateTime, psMessage, pSeverity) then
    Exit; //==>

  FCritSect.Enter;
  try
    lLogEvent := TtiLogEvent.Create;
    lLogEvent.DateTime   := psDateTime;
    lLogEvent.LogMessage := psMessage;
    lLogEvent.Severity   := pSeverity;
    lLogEvent.ThreadID   := psThreadID;
    FList.Add(lLogEvent);
  finally
    FCritSect.Leave;
  end;
end;


procedure TtiLogToCacheAbs.Terminate;
begin
  FThrdLog.Terminate;
  FThrdLog.WaitFor;
end;


procedure TtiLogToCacheAbs.WriteToOutput;
var
  i : integer;
begin
  FCritSect.Enter;
  try
    for i := 0 to FList.Count - 1 do
      FListWorking.Add(TtiLogEvent(FList.Items[i]));
    FList.Clear;
  finally
    FCritSect.Leave;
  end;
end;


function TtiLogEvent.AsLeftPaddedString: string;
var
  LMessagePrefix: string;
  LMessagePrefixLen: integer;
  LMessage: string;
  i: Integer;
begin
  if Pos(Cr, LogMessage) = 0 then
    Result := GetFormattedMessageTimeStamp + LogMessage
  else
  begin
    LMessagePrefix:= GetFormattedMessageTimeStamp;
    LMessagePrefixLen:= Length(LMessagePrefix) - 1;
    Result := LMessagePrefix;
    LMessage := tiStrTran(LogMessage, Lf, #0);
    for i:= 1 to tiNumToken(LMessage, Cr) do
    begin
      if i > 1 then
      begin
        Result := Result + CrLf + tiSpace(LMessagePrefixLen) + tiToken(LMessage, Cr, i);
      end else
        Result := Result + tiToken(LMessage, Cr, i);
    end;
  end;
end;


function TtiLogEvent.AsString: string;
begin
  result := GetFormattedMessageTimestamp +
            tiStrTran(LogMessage, Lf, '');
end;


function TtiLogEvent.AsStringStripCrLf: string;
begin
  result := _StrTran(AsString, Cr, ' ');
end;


function TtiLogEvent.GetFormattedMessageTimeStamp: string;
begin
  Result :=
    DateTime   + ' ' +
              FsThreadID + ' ' +
              _PadR(cTILogSeverityStrings[ Severity ], cuiWidthSeverity) + ' ';
end;


function TtiLogEvent.GetSeverityAsString: string;
begin
  result := cTILogSeverityStrings[ Severity ];
end;


function TtiLogEvent.GetShortLogMessage: string;
var
  ls : string;
begin
  ls := LogMessage;
  if Length(ls) + 3 > cuiWidthShortMessage then
    result := Copy(ls, 1, cuiWidthShortMessage - 3) + '...'
  else
    result := ls;
end;


procedure TtiLogEvent.SetSeverityAsString(const Value: string);
var
  i : TtiLogSeverity;
  lsSeverity : string;
begin
  lsSeverity := Trim(Value);
  for i := Low(TtiLogSeverity) to High(TtiLogSeverity) do
  begin
    if lsSeverity = cTILogSeverityStrings[ i ] then
    begin
      Severity := i;
      Exit; //==>
    end;
  end;
  Assert(false, 'Severity <' + Value + '> unknown');
end;


constructor TtiThrdLog.CreateExt(pLogTo : TtiLogToCacheAbs);
begin
  Create(true);
  Priority  := tpLower;
  FLogTo    := pLogTo;
end;


procedure TtiThrdLog.Execute;
begin
  while SleepAndCheckTerminated(200) do
    WriteToOutput;
end;

procedure TtiThrdLog.SetLogTo(const Value: TtiLogToCacheAbs);
begin
  FLogTo := Value;
end;

function TtiLog.LogToFileName: string;
var
  LLogTo: TtiLogToFile;
begin
  LLogTo:= FindByLogClass(TtiLogToFile) as TtiLogToFile;
  if LLogTo <> nil then
    Result:= LLogTo.FileName
  else
    Result := '';
end;


procedure TtiLog.RegisterLog(ALogTo : TtiLogToAbs);
begin
  Assert(ALogTo.TestValid, cErrorTIPerObjAbsTestValid);
  if IsRegistered(TtiLogToClass(ALogTo.ClassType)) then
  begin
    ALogTo.Free;
    Exit; //==>
  end;
  FLogToList.Add(ALogTo);
end;


procedure TtiLog.RegisterLog(ALogTo : TtiLogToClass);
begin
  Assert(ALogTo <> nil, 'ALogTo not assigned');
  RegisterLog(ALogTo.Create);
end;


procedure TtiThrdLog.tiSynchronize(Method: TThreadMethod);
begin
{ I just wanted to surface this from protected for tiLogErrorForm
  (in another unit). Brain went to sleep on correct syntax.
  (OK with property, but not with procedure.) }
  Synchronize(Method);
end;


procedure TtiThrdLog.WriteToOutput;
begin
  FLogTo.WriteToOutput;
end;


procedure TtiLogToAbs.SetSevToLog(const Value: TtiSevToLog);
begin
  FSevToLog := Value;
end;


procedure TtiLogToAbs.Terminate;
begin
  // Do nothing, implement if required in the concrete
end;


procedure TtiLogEvents.Add(pItem: TtiLogEvent);
begin
  FList.Add(pItem);
end;


procedure TtiLogEvents.Clear;
begin
  FList.Clear;
end;


function TtiLogEvents.Count: Integer;
begin
  Result := FList.Count;
end;


constructor TtiLogEvents.Create;
begin
  inherited;
  FList:= TObjectList.Create(True);
end;


destructor TtiLogEvents.Destroy;
begin
  FList.Free;
  inherited;
end;


function TtiLogEvents.GetItems(pIndex: Integer): TtiLogEvent;
begin
  Result := FList.Items[pIndex] as TtiLogEvent;
end;


procedure TtiLog.SetSevToLog(const Value: TtiSevToLog);
var
  lCritSect: TCriticalSection ;
  i : integer ;
begin
  lCritSect := TCriticalSection.Create ;
  try
    lCritSect.Enter ;
    try
      FSevToLog := Value;
      for i := 0 to FLogToList.Count - 1 do
        TtiLogToAbs( FLogToList.Items[i] ).SevToLog := Value ;
    finally
      lCritSect.Leave ;
    end ;
  finally
    lCritSect.Free ;
  end ;
end;


initialization
  ubFinalization := false;

finalization
  ubFinalization := true;
  uLog.Free;

end.


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/
                                          
  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
                          
  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    January, 1999, Peter Hinrichsen, Created
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    A thread safe log of events to text file or screen.

  Classes:
    TLogSeverity   - Ordinal log severities (you may like to edit these to suit
                     your project)
    TSevToLog      - Set of log severities
    TLogEvent      - An object to hold a logged event in a cache where it will
                     sit until it is written out
    TLogToAbs      - Abstract log type

    // These classes form the cached logging system
    TThrdLog       - Thread class to write out the cached events
    TLogToCacheAbs - Abstract, cached log
    TLogToForm     - Cached log to a form
    TLogToFile     - Cached log to a file

    TLogToError    - Log an error event and show a dialog on the screen.

    TtiLog         - The main log class

  Usage:

    1. Include this unit in your project.
    2. Add the command line parameters (-l to log to a file, or -lv to log
       to a file and the screen) when calling the application.
    3. Add one of the following lines before the Application.Initialize call
       in your DPR:

         // This will set the log up for a client application and will
         // allow an error dialog to show, and visual logging to work
         procedure SetupLogForClient( pErrorDialog : boolean = true ) ;

         // This will supress error dialogs, and will only log to a file
         procedure SetupLogForServer ;

         // This will log to a file, and the standard output
         // (ie, console dialog)
         procedure SetupLogForConsole ;

    4. Use tiLog in the unit you want to log from.
    5. Call the procedure Log( sMessage ) to log some text at a severity of normal; or
       Log( sMessage, slSeverity ) to log some text at severity other than normal.
       You can also call:
         LogInt( 123456 ) ;
         LogArray([ 'String', 123456, 123.456 ] ) ;
         LogFmt( 'Error number %d in %s', [ 100, 'MyFile' ]);
         LogError( 'My error message' ) ;
         LogWarning( 'My warning' ) ;

  ToDo:
    1. Log TPersistent descendants
    2. Give control over the types of events that are logged from the log form
    3. Log file to be XML format, which will give better control over multi line
       messages.
    4. Improve log viewer.


    Attempted enhancements to TLogToForm by Ian K (2003-10-07):
     - add new popup menu with severity flags
     - (tried to) add a TToolBar (with TToolButtons)
     - 2nd attempt was to add a TPanel (with TSpeedButtons)
    This was so that various logging severity could be turned on or off at will.
    Problem:  Sometimes it worked (beautifully), but often no messages would show
              on the form at all.  I suspect it has to do with thread safety-ness
              (or the lack thereof).  If someone wants to have a go, I've left both
              bits of code commented out.  Remember, just TToolBar/TToolButtons, or
              TPanel/TSpeedButtons referenced code, not both.
              I'll clean out both hacks if no further interest is shown.
    At least the popup menu facility worked.:-)

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiLog;

interface
uses
  Classes
  ,SyncObjs
  {$IFDEF MSWINDOWS}
  ,Windows
  ,Forms
  ,StdCtrls
//  ,ComCtrls
  ,Controls
  ,Menus
  ,ExtCtrls
  ,Buttons
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,Libc
  ,QForms
  ,QStdCtrls
  ,QControls
  ,QMenus
  ,QExtCtrls
  ,QButtons
  {$ENDIF LINUX}
  ,Contnrs
  ,SysUtils
  ,tiObjAbs
  ;

const
  crsSeverityNotFound = 'Severity <%s> not found' ;
  // Don't change the width of this withoug also changing cuiWidthDate
  crsDateFormat       = 'dd/mm/yy hh:mm:ss' ;
  cErrorCanNotCreateLogDirectory = 'Can not create directory for log files <%s>';

type

  // Types of log events, eg normal, warning, error, etc
  { // You may like to start with this set
  TLogSeverity = (  lsNormal
                   ,lsWarning
                   ,lsError
                 ) ;
  }

  // I use this set to provide additional log severity types
  // If you change the set of log event types, then remember to change the
  // strings in caLogSeverityStrings (below) too.
  TLogSeverity = (
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
                 ) ;


  TLogSeverityStrings = array[ TLogSeverity ] of String ;

const

  // Strings representing log severity types
  // If you change the log strings, remember to change the set of
  // TLogSeveirty (above) too.

  caLogSeverityStrings: TLogSeverityStrings = (
                    'Normal'
                   ,'User info'
                   ,'ObjCreation'
                   ,'Visitor'
                   ,'DBConnection'
                   ,'AcceptVisitor'
                   ,'QueryTiming'
                   ,'Debug info'
                   ,'Warning'
                   ,'Error'
                 ) ;

  {$IFDEF LINUX}
  // approximate values, its finer grained on Linux
  tpIdle = 19;
  tpLowest = 12;
  tpLower = 6;
  tpNormal = 0;
  tpHigher = -7;
  tpHighest = -13;
  tpTimeCritical = -20;
  {$ENDIF LINUX}

type

  // Forward declaration of the main logging class
  TtiLog = class ;

  // It is possible to filter for a group of log events
  // ---------------------------------------------------------------------------
  TSevToLog = set of TLogSeverity ;

  // A class to hold a logged event while in the cache
  // ---------------------------------------------------------------------------
  TLogEvent = class( TtiObjAbs )
  private
    FLogMessage : String ;
    FsDateTime  : String ;
    FSeverity   : TLogSeverity ;
    FsThreadID  : string;
    function  GetSeverityAsString: string;
    procedure SetSeverityAsString(const Value: string);
    function  GetShortLogMessage: string;
  published
    property DateTime   : string       read FsDateTime  write FsDateTime  ;
    property LogMessage : String       read FLogMessage write FLogMessage ;
    property ShortLogMessage : string  read GetShortLogMessage ;
    property Severity   : TLogSeverity read FSeverity   write FSeverity  ;
    property ThreadID   : string       read FsThreadID  write FsThreadID ;
    property SeverityAsString : string read GetSeverityAsString write SetSeverityAsString ;
  public
    function AsString   : string ;
    function AsStringStripCrLf : string ;
  end ;

  TLogReadFromFile = class( TtiObjAbs )
  private
    FList : TList ;
    FFileName: TFileName ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    List : TList read FList ;
    property    FileName : TFileName read FFileName write FFileName ;
    procedure   Read( const psFileName : string ) ;
    procedure   Clear ;
  end ;

  // Abstract base class to manage logging to anything
  // ---------------------------------------------------------------------------
  TLogToAbs = class( TtiObjAbs )
  private
    FSevToLog  : TSevToLog ;
    FTerminated : boolean ;
  protected
    function  AcceptEvent( const psDateTime : string ;
                           const psMessage  : string;
                           pSeverity  : TLogSeverity ) : boolean ; virtual ;
    procedure WriteToOutput ; virtual ; abstract ;
    procedure SetSevToLog(const Value: TSevToLog); virtual ;
  public
    constructor Create ; virtual ;
    procedure   Log( const psDateTime : string ;
                     const psThreadID : string ;
                     const psMessage  : string;
                     pSeverity  : TLogSeverity ) ; virtual ; abstract ;
    property    SevToLog : TSevToLog read FSevToLog Write SetSevToLog ;
    procedure   Terminate ; virtual ;
    property    Terminated : boolean read FTerminated ;
  end ;

  // Define a TLogToAbs class reference
  // This allows us to create TLogToAbs descendants and register them with
  // the main log class from another unit.
  TLogClass = class of TLogToAbs ;

  TLogToCacheAbs = class ;

  // A thread class to manage the writing out of events when the
  // system has some free time
  // ---------------------------------------------------------------------------
  TThrdLog = class( TThread )
  private
    FLogTo : TLogToCacheAbs ;
    procedure   SetLogTo(const Value: TLogToCacheAbs);
    procedure   WriteToOutput ;
  public
    constructor CreateExt( pLogTo : TLogToCacheAbs ) ;
    procedure   Execute ; override ;
    property    Terminated;  // surfaced from protected
    procedure   tiSynchronize(Method: TThreadMethod); // surfaced Synchronize from protected
    property    LogTo : TLogToCacheAbs  read FLogTo write SetLogTo;
  end ;

  // Abastract base class to manage cached logging
  // ---------------------------------------------------------------------------
  TLogToCacheAbs = class( TLogToAbs )
  private
    FList : TList ;
    FListWorking : TObjectList ;
    {$IFDEF MSWINDOWS}
    FSemInput  : THandle ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FSemInput  : TSemaphore ;
    {$ENDIF LINUX}
    FThrdLog   : TThrdLog ;
  protected
    property ThrdLog : TThrdLog read FThrdLog;
    property ListWorking : TObjectList read FListWorking;
    procedure WriteToOutput ; override ;
    procedure WaitForSemaphore ;
    procedure UnLockSemaphore ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Log( const psDateTime : string ;
                     const psThreadID : string ;
                     const psMessage  : string;
                     pSeverity  : TLogSeverity ) ; override ;
    procedure Terminate ; override ;
  end ;


  // Log to a file
  // ---------------------------------------------------------------------------
  TLogToFile = class( TLogToCacheAbs )
  private
    FFileName  : TFileName ;
    FbOverwriteOldFile: boolean;
    FDateInFileName: boolean;
    procedure SetOverwriteOldFile(const Value: boolean);
    function  GetDefaultFileName : TFileName ;
    function  GetFileName : TFileName ;
  protected
    procedure WriteToOutput ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    FileName : TFileName read GetFileName ;
    property    OverwriteOldFile : boolean read FbOverwriteOldFile write SetOverwriteOldFile ;
    property    DateInFileName : boolean read FDateInFileName write FDateInFileName ;
    procedure   Terminate ; override ;
  end;

  // Log to a console
  // ---------------------------------------------------------------------------
  TLogToConsole = class( TLogToCacheAbs )
  private
  protected
    procedure WriteToOutput ; override ;
    function  AcceptEvent( const psDateTime : string ;
                           const psMessage  : string;
                           pSeverity  : TLogSeverity ) : boolean ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  end;

  // The main logging class
  // ---------------------------------------------------------------------------
  TtiLog = class( TtiObjAbs )
  private
    FLogToList : TList ;
    FSevToLog: TSevToLog;
    procedure SetSevToLog(const Value: TSevToLog);
    function  GetLogFileName: string;
    function  IsRegistered(const pLogClass : TLogClass ) : boolean ;
  public
    constructor Create  ;
    destructor  Destroy ; override ;
    procedure   Log( const psMessage : string ;
                     const pSeverity : TLogSeverity = lsNormal ) ;
    function    RegisterLog( const pLogClass : TLogClass ) : TLogToAbs ;
    property    SevToLog : TSevToLog read FSevToLog write SetSevToLog ;
    function    FindByLogClass( pLogClass : TLogClass ) : TLogToAbs ;
    property    LogFileName : string read GetLogFileName ;
    procedure   TerminateGUILogs;
    procedure   RegisterGUILog;

  end ;

procedure SetupLogForClient( pErrorDialog : boolean = true; pOverwriteOld : Boolean = False ) ;
procedure SetupLogForServer ;
procedure SetupLogForConsole ;

// Some global proces to make logging easier
// Log a string message
procedure Log( const psMessage : string  ; pSeverity : TLogSeverity = lsNormal ) ;

// Log an integer value
procedure LogInt( const piMessage : integer ; pSeverity : TLogSeverity = lsDebug ) ;

// Log an array of variants
// This is useful for debugging, as it lets you write code like this:
// Log( [ 123.456, 123456, '123456', false ]) ;
procedure LogArray( const pA : Array of Const ; pSeverity : TLogSeverity = lsDebug ) ;

procedure LogFmt( const psMessage : string ; const pA : Array of Const ; pSeverity : TLogSeverity = lsNormal) ;

// Log a string message, but flag it as an error
procedure LogWarning( const psMessage : string ) ; overload ;

// Log a string message, but flag it as an error
procedure LogError( const psMessage : string ; pRaiseException : boolean = true) ; overload ;
procedure LogError( const pException : Exception ; pRaiseException : boolean = true) ; overload ;

// As for LogError, but call Format( psMessage, pA ) first
procedure LogError( const psMessage : string ; const pA : Array of Const ) ; overload ;

// The log object is a singleton
function  gLog : TtiLog ;

// Surface this for tiLogErrorForm (should remove leading '_')
function _StrTran( pStrValue, pStrDel, pStrIns : string ) : string ;

const
  cuiQueryTimingSpacing = 40 ;

implementation
uses
{$IFDEF MSWINDOWS}
  Dialogs
  ,Messages
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ELSE}
  ,FileCtrl
  {$ENDIF}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  QDialogs
  ,Variants
{$ENDIF LINUX}
  ,tiLogViewForm_Cli
  ,tiLogErrorForm_Cli
  ,ctiPersist
  ,tiUtils
  ;

var
  uLog : TtiLog ;
  ubFinalization : boolean ;

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
               ] ;

  // Command line parameters
  csLog         = 'l'  ; // Command line parameter to turn logging on
  csLogVisual   = 'lv' ; // Command line parameter to turn visual logging on


  // Constants for formatting the error log
  cuiWidthDate         = 17 ;
  cuiWidthThread       =  8 ;
  cuiWidthSeverity     = 13 ;
  cuiWidthShortMessage = 50 ;

{$R tiLog.res}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  Application wide, global functions
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

// The log is a singleton
// -----------------------------------------------------------------------------
function gLog : TtiLog ;
begin
  if ubFinalization then
  begin
    result := nil ;
    Exit ; //==>
  end;
  if uLog = nil then
    uLog := TtiLog.Create ;
  result := uLog ;
end ;

// Was a parameter to activate logging passed on the command line?
// -----------------------------------------------------------------------------
function _IsParam(const psParam: string): boolean;
  function _IsThisParam(const psParam, psDelim, psCommandLineParams: string): boolean;
  begin
    result := ( pos( psDelim + UpperCase( psParam ) + ' ',
                     psCommandLineParams ) <> 0 )
  end ;

  function _ReadCommandLineParams : string ;
  var
    i : integer ;
  begin
    result := '' ;
    for i := 1 to ParamCount do begin
      result :=
        result +
        upperCase( ParamStr( i )) + ' ';
    end  ;
  end;
var
  lsCommandLineParams : string ;
begin
  lsCommandLineParams := _ReadCommandLineParams ;
  result := _IsThisParam( psParam, '-', lsCommandLineParams ) or
            _IsThisParam( psParam, '/', lsCommandLineParams ) or
            _IsThisParam( psParam, '\', lsCommandLineParams ) ;
end;

// -----------------------------------------------------------------------------
procedure SetupLogForClient( pErrorDialog : boolean = true; pOverwriteOld : Boolean = False ) ;
var
  lLog : TLogToFile ;
begin

  // Turn off the tiOPF logging if MadExcept is being used.
  // Should also check for other exception utilities.
  {$IFNDEF madExcept}
    // Visual error logging
    if pErrorDialog then
      gLog.RegisterLog( TLogToError ) ;
  {$ENDIF}

  // Visual logging
  if _IsParam( csLogVisual ) then
    gLog.RegisterGUILog;

  // Log to file
  if ( _IsParam( csLog ) or
       _IsParam( csLogVisual )) then
  begin
    lLog := TLogToFile(gLog.RegisterLog( TLogToFile )) ;
    lLog.OverwriteOldFile := pOverWriteOld;
  end ;

end;

// -----------------------------------------------------------------------------
procedure SetupLogForServer ;
var
  lLog : TLogToFile ;
begin
  lLog := TLogToFile(gLog.RegisterLog( TLogToFile )) ;
  lLog.OverwriteOldFile := false ;
  lLog.DateInFileName := true ;
  Log( 'Application <' + ParamStr( 0 ) + '> started' ) ;
end ;

// -----------------------------------------------------------------------------
procedure SetupLogForConsole ;
var
  lLog : TLogToFile ;
begin
  gLog.RegisterLog( TLogToConsole ) ;
  lLog := TLogToFile( gLog.RegisterLog( TLogToFile )) ;
  lLog.OverwriteOldFile := false ;
  lLog.DateInFileName := true ;
  if _IsParam( csLogVisual ) then
    gLog.SevToLog := gLog.SevToLog + [lsVisitor] ;
  {$IFDEF MSWINDOWS}
  Log( 'Application <' + ParamStr( 0 ) + '> started' ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Log( 'Application <' + Application.ExeName + '> started' ) ;
  {$ENDIF LINUX}
  Log( 'Log file name: ' + lLog.FileName ) ;
  Log( '' ) ;

end ;

// -----------------------------------------------------------------------------
procedure Log( const psMessage : string ; pSeverity : TLogSeverity = lsNormal ) ;
begin
  if ubFinalization then
    Exit ; //==>
  gLog.Log( psMessage, pSeverity ) ;
end ;
// -----------------------------------------------------------------------------

procedure LogInt( const piMessage : integer ; pSeverity : TLogSeverity = lsDebug ) ;
begin
  if ubFinalization then
    Exit ; //==>
  gLog.Log( IntToStr( piMessage ), pSeverity ) ;
end ;


// -----------------------------------------------------------------------------
procedure LogError( const psMessage : string ; pRaiseException : boolean = true ) ;
begin
  if ubFinalization then
    Exit ; //==>
  gLog.Log( psMessage, lsError ) ;
  {$IFDEF ThirdPartyExceptionHandling}
    if pRaiseException then
      raise exception.create(psMessage);
  {$ENDIF}
end ;

procedure LogError( const pException : Exception ; pRaiseException : boolean = true) ;
begin
  if ubFinalization then
    Exit ; //==>
  gLog.Log( pException.Message, lsError ) ;
  {$IFDEF ThirdPartyExceptionHandling}
    if pRaiseException then
      raise Exception(pException.ClassType).Create(pException.Message);
  {$ENDIF}
end ;

procedure LogError( const psMessage : string ; const pA : Array of Const ) ;
var
  ls : string ;
begin
  if ubFinalization then
    Exit ; //==>
  try
    ls := Format( psMessage, pA ) ;
  except
    on e:exception do
      ls := 'Unable to evaluate log message <' + psMessage + '> reason: ' + e.Message ;
  end ;
  gLog.Log( ls, lsError ) ;
end ;

// -----------------------------------------------------------------------------
procedure LogWarning( const psMessage : string ) ;
begin
  if ubFinalization then
    Exit ; //==>
  gLog.Log( psMessage, lsWarning ) ;
end ;

// -----------------------------------------------------------------------------
procedure LogArray( const pA : Array of Const ; pSeverity : TLogSeverity = lsDebug ) ;
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine : string ;
begin
  lsLine := '';
  for I := 0 to High(pA) do begin
    if lsLine <> '' then
      lsLine := lsLine + ', ' ;
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
        end ;
        vtInt64:      lsLine := lsLine + IntToStr(VInt64^);
      else
        raise exception.Create( 'Invalid variant type passed to LogArray' ) ;
    end;
  end ;
  Log( lsLine, pSeverity ) ;
end ;

//------------------------------------------------------------------------------
procedure LogFmt( const psMessage : string ; const pA : Array of Const ; pSeverity : TLogSeverity = lsNormal ) ;
var
  lMessage : string ;
begin
  try
    lMessage := Format( psMessage, pA ) ;
  except
    on e:exception do
      LogError( 'Unable to evaluate log message <' + psMessage + '> reason: ' + e.Message ) ;
  end ;
  Log( lMessage, pSeverity ) ;
end ;

//------------------------------------------------------------------------------
function _StrTran( pStrValue, pStrDel, pStrIns : string ) : string ;
var i : integer ;
    sToChange : string ;
begin
  result := '' ;
  sToChange := pStrValue ;
  i := pos( pStrDel, sToChange ) ;
  while i <> 0 do begin
    result := result + copy( sToChange, 1, i-1 ) + pStrIns ;
    delete( sToChange, 1, i+length( pStrDel )-1) ;
    i := pos( pStrDel, sToChange ) ;
  end ;
  result := result + sToChange ;
end ;

//------------------------------------------------------------------------------
function _PadR( pStrValue : string; pIntLen : integer ) : string ;
begin
  if length( pStrValue ) < pIntLen then begin
    while length( pStrValue ) < pIntLen do begin
      pStrValue := pStrValue + ' ' ;
    end ;
  end
  else if length( pStrValue ) > pIntLen then
    pStrValue := copy( pStrValue, 1, pIntLen ) ;
  result := pStrValue ;
end ;

//------------------------------------------------------------------------------
function _PadL( pStrValue : string; pIntLen : integer ) : string ;
begin
  if length( pStrValue ) < pIntLen then begin
    while length( pStrValue ) < pIntLen do begin
      pStrValue := ' ' + pStrValue ;
    end ;
  end
  else if length( pStrValue ) > pIntLen then
    pStrValue := copy( pStrValue, length( pStrValue )-pIntLen, pIntLen ) ;
  result := pStrValue ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TtiLog
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiLog.Create ;
begin
  inherited;
  FLogToList := TList.Create  ;
end;

// -----------------------------------------------------------------------------
destructor TtiLog.Destroy;
var
  i : integer ;
  lLog : TLogToAbs ;
begin
  for i  := FLogToList.Count - 1 downto 0 do
  begin
    lLog := TLogToAbs( FLogToList.Items[i] ) ;
    if not lLog.TestValid then
      ShowMessage( cTIInvalidObjectError + ' TtiLog.Destroy' );
    FLogToList.Delete( i ) ;
    lLog.Free ;
  end ;
  FLogToList.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
function TtiLog.FindByLogClass(pLogClass: TLogClass): TLogToAbs;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FLogToList.Count - 1 do
    if TObject(FLogToList.Items[i]) is pLogClass then
      result := TLogToAbs( FLogToList.Items[i] ) ;
end;

function TtiLog.GetLogFileName: string;
var
  lLogToAbs : TLogToAbs ;
begin
  lLogToAbs := FindByLogClass( TLogToFile ) ;
  if lLogToAbs <> nil then
    result := TLogToFile( lLogToAbs ).FileName
  else
    result := '' ;
end;

function TtiLog.IsRegistered(const pLogClass: TLogClass): boolean;
begin
  result := FindByLogClass( pLogClass ) <> nil ;
end;

procedure TtiLog.Log(const psMessage: string; const pSeverity: TLogSeverity = lsNormal );
var
  lsNow      : string ;
  i          : integer ;
  lsMessage  : string ;
  lsThreadID : string ;
begin

  if ubFinalization then
    Exit ; //==>

  //       Width: cuiWidthDate
  lsNow := _PadR( FormatDateTime( crsDateFormat, Now ), cuiWidthDate ) ;

  lsMessage  := psMessage ;

  // Width: cuiWidthThread
  lsThreadID := IntToStr( GetCurrentThreadID ) ;
  lsThreadID := _PadL( lsThreadID, cuiWidthThread ) ;

  for i := 0 to FLogToList.Count - 1 do
    TLogToAbs( FLogToList.Items[i] ).Log( lsNow,
                                          lsThreadID,
                                          lsMessage,
                                          pSeverity ) ;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogToAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TLogToAbs.Create;
begin
  inherited Create ;
  FSevToLog := cSevToLog ;
  FTerminated := false ;
end;

// -----------------------------------------------------------------------------
function TLogToAbs.AcceptEvent( const psDateTime : string ;
                                const psMessage  : string ;
                                pSeverity  : TLogSeverity): boolean;
begin
  result := ( pSeverity in FSevToLog ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogToCacheAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TLogToCacheAbs.Create;
var
  lsSemaphoreName : string;
  {$IFDEF LINUX}
  error: integer;
  {$ENDIF LINUX}
begin
  inherited Create ;
  FList := TList.Create ;
  FListWorking := TObjectList.Create ;
  lsSemaphoreName := ClassName ;
  {$IFDEF MSWINDOWS}
  FSemInput  := CreateSemaphore( nil, 1, 1, PChar( lsSemaphoreName + 'Input' )) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_init(FSemInput, false, 2);
  if error <> 0 then
    raise Exception.Create('Failed to create the semaphore');
  {$ENDIF LINUX}
  FThrdLog   := TThrdLog.CreateExt( self ) ;
end;

// -----------------------------------------------------------------------------
destructor TLogToCacheAbs.Destroy;
  {$IFDEF LINUX}
var
  error: integer;
  {$ENDIF LINUX}
begin
  Terminate ;
  FList.Free ;
  FListWorking.Free ;
  {$IFDEF MSWINDOWS}
  CloseHandle( FSemInput ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_destroy( FSemInput );
  if error <> 0 then
    raise Exception.Create('Failed to destroy the semaphore');
  {$ENDIF LINUX}
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TLogToCacheAbs.Log( const psDateTime : string ;
                              const psThreadID : string ;
                              const psMessage  : string;
                              pSeverity  : TLogSeverity ) ;
var
  lLogEvent : TLogEvent ;
begin

  if not AcceptEvent( psDateTime, psMessage, pSeverity ) then
    Exit ; //==>

  WaitForSemaphore;
  try
    lLogEvent := TLogEvent.Create ;
    lLogEvent.DateTime   := psDateTime ;
    lLogEvent.LogMessage := psMessage ;
    lLogEvent.Severity   := pSeverity ;
    lLogEvent.ThreadID   := psThreadID ;
    FList.Add( lLogEvent ) ;
  finally
    UnLockSemaphore;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TLogToCacheAbs.Terminate;
var
  i : integer ;
begin
  FThrdLog.Terminate ;
  FThrdLog.WaitFor ;
  for i := FList.Count-1 downto 0 do
    TObject( FList.Items[i] ).Free ;
end;

// -----------------------------------------------------------------------------
procedure TLogToCacheAbs.UnLockSemaphore;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
{$IFDEF MSWINDOWS}
  ReleaseSemaphore( FSemInput, 1, nil ) ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  error := sem_post(FSemInput);
  if error <> 0 then
    raise Exception.Create('Failed to unlock the semaphore');
{$ENDIF LINUX}
end;

// -----------------------------------------------------------------------------
procedure TLogToCacheAbs.WaitForSemaphore;
begin
{$IFDEF MSWINDOWS}
  if WaitForSingleObject( FSemInput, 600000 ) = WAIT_TIMEOUT then
    raise exception.Create( 'Timed out waiting for semaphore' ) ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  // no need to test return value, it always returns 0
  sem_wait(FSemInput);
//  sem_timedwait(pSemaphore, 2000)
{$ENDIF LINUX}
end;

// -----------------------------------------------------------------------------
procedure TLogToCacheAbs.WriteToOutput;
var
  i : integer ;
begin
  WaitForSemaphore;
  try
    for i := 0 to FList.Count - 1 do
      FListWorking.Add( FList.Items[i] ) ;
    FList.Clear ;
  finally
    UnLockSemaphore;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogEvent
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TLogEvent.AsString: string;
begin
  result := DateTime   + ' ' +
            FsThreadID + ' ' +
            _PadR( caLogSeverityStrings[ Severity ], cuiWidthSeverity ) + ' ' +
            _StrTran( LogMessage, #10, '' ) ;
end;

// -----------------------------------------------------------------------------
function TLogEvent.AsStringStripCrLf: string;
begin
  result := _StrTran( AsString, #13, ' ' ) ;
end;

// -----------------------------------------------------------------------------
function TLogEvent.GetSeverityAsString: string;
begin
  result := caLogSeverityStrings[ Severity ] ;
end;

// -----------------------------------------------------------------------------
function TLogEvent.GetShortLogMessage: string;
var
  ls : string ;
begin
  ls := LogMessage ;
  if Length( ls ) + 3 > cuiWidthShortMessage then
    result := Copy( ls, 1, cuiWidthShortMessage - 3 ) + '...'
  else
    result := ls ;
end;

// -----------------------------------------------------------------------------
procedure TLogEvent.SetSeverityAsString(const Value: string);
var
  i : TLogSeverity ;
  lsSeverity : string ;
begin
  lsSeverity := Trim( Value ) ;
  for i := Low( TLogSeverity ) to High( TLogSeverity ) do
  begin
    if lsSeverity = caLogSeverityStrings[ i ] then
    begin
      Severity := i ;
      Exit ; //==>
    end ;
  end ;
  Assert( false, 'Severity <' + Value + '> unknown' ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TThrdLog
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TThrdLog.CreateExt( pLogTo : TLogToCacheAbs ) ;
begin
  Create( true ) ;
  {$IFDEF MSWINDOWS}
  Priority := tpLower ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
//  Priority := 1 ;   // don't screw with Linux priorities.
  {$ENDIF LINUX}
  FLogTo := pLogTo ;
  resume ;
end;

// -----------------------------------------------------------------------------
procedure TThrdLog.Execute;
begin
  while not Terminated do begin
    WriteToOutput ;
    Sleep( 500 ) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TThrdLog.SetLogTo(const Value: TLogToCacheAbs);
begin
  FLogTo := Value;
end;

// -----------------------------------------------------------------------------
procedure TtiLog.RegisterGUILog;
begin
  RegisterLog( TLogToForm ) ;
end;

function TtiLog.RegisterLog(const pLogClass: TLogClass) : TLogToAbs ;
begin
  result := nil ;
  if IsRegistered(pLogClass) then
    Exit ; //==>
  result := pLogClass.Create ;
  FLogToList.Add( result ) ;
end;

// -----------------------------------------------------------------------------
procedure TThrdLog.tiSynchronize(Method: TThreadMethod);
begin
{ I just wanted to surface this from protected for tiLogErrorForm
  (in another unit). Brain went to sleep on correct syntax.
  (OK with property, but not with procedure.) }
  Synchronize( Method );
end;

// -----------------------------------------------------------------------------
procedure TThrdLog.WriteToOutput;
begin
  FLogTo.WriteToOutput ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogToFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TLogToFile.Create;
begin
  inherited;
  FbOverwriteOldFile := false ;
  FDateInFileName := false ;
  // This will create a log file in the same directory as the application.
  FFileName := GetDefaultFileName ;
end;

// -----------------------------------------------------------------------------
destructor TLogToFile.Destroy;
begin
  Terminate ;
  inherited;
end;

// -----------------------------------------------------------------------------
function TLogToFile.GetDefaultFileName: TFileName;
var
  path: array[0..MAX_PATH - 1] of char;
  lFileName : string ;
  lFilePath : string ;
begin
  if IsLibrary then
    SetString(lFileName, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
    lFileName := paramStr( 0 ) ;
  lFilePath := tiAddTrailingSlash(ExtractFilePath(lFileName)) + 'Log' ;
  lFileName := ExtractFileName(lFileName);
  lFileName := ChangeFileExt( lFileName, '.Log' ) ;
  if not DirectoryExists(lFilePath) then
    ForceDirectories(lFilePath);
  if not DirectoryExists(lFilePath) then
    raise Exception.CreateFmt(cErrorCanNotCreateLogDirectory, [lFilePath]);
  Result    := tiAddTrailingSlash(lFilePath) + lFileName ;
end;

// -----------------------------------------------------------------------------
function TLogToFile.GetFileName : TFileName ;
begin
  if not FDateInFileName then
    result := FFileName
  else
    result :=
      ChangeFileExt( FFileName, '' ) +
      '_' + FormatDateTime( 'YYYY-MM-DD', Date ) +
      '.Log' ;
end;

procedure TLogToFile.SetOverwriteOldFile(const Value: boolean);
begin
  FbOverwriteOldFile := Value;
  if not Value then
    Exit ; //==>
  if FileExists( FileName ) then
    DeleteFile( FileName ) ;
end;

// -----------------------------------------------------------------------------
procedure TLogToFile.Terminate;
begin
  WriteToOutput ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TLogToFile.WriteToOutput ;
var
  i         : integer ;
  lLogEvent : TLogEvent ;
  lBuffer   : PChar ;
  liLen     : integer ;
  lsLine    : string ;
  lFileStream : TFileStream ;
  lFileName : TFileName ;
begin
  inherited WriteToOutput ;
  for i := 0 to FListWorking.Count - 1 do begin
    lLogEvent := TLogEvent( FListWorking.Items[i] ) ;
    lsLine    := lLogEvent.AsStringStripCrLf + #13 + #10 ;

    lFileName := GetFileName ;

    if FileExists( lFileName ) then
      lFileStream := TFileStream.Create( lFileName,
                                         fmOpenReadWrite or fmShareDenyNone )
    else
      lFileStream := TFileStream.Create( lFileName,
                                         fmCreate or fmShareDenyNone ) ;

    try
      lBuffer := PChar( lsLine ) ;
      liLen := length( lsLine ) ;
      lFileStream.Seek( 0, soFromEnd ) ;
      lFileStream.write( lBuffer^, liLen ) ;
    finally
      lFileStream.Free ;
    end ;
  end ;
  FListWorking.Clear ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLogReadFromFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TLogReadFromFile.Create;
begin
  inherited ;
  FList := TList.Create ;
end;

// -----------------------------------------------------------------------------
destructor TLogReadFromFile.Destroy;
begin
  Clear ;
  FList.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TLogReadFromFile.Clear;
var
  i : integer ;
begin
  for i := 0 to FList.Count - 1 do
    TObject( FList.Items[i] ).Free ;
  FList.Clear ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TLogReadFromFile.Read(const psFileName: string);
var
  ls : TStringList ;
  lsLine : string ;
  lData : TLogEvent ;
  i : integer ;
  liStart : integer ;
begin

  FFileName := psFileName ;
  ls := TStringList.Create ;
  Clear ;
  try
    ls.LoadFromFile( psFileName ) ;
    for i := 0 to ls.Count - 1 do
    begin
      lData := TLogEvent.Create ;
      lsLine := ls.Strings[i] ;

      liStart                := 1 ;
      lData.DateTime         := Copy( lsLine, liStart, cuiWidthDate ) ;

      liStart                := liStart + 1 + cuiWidthDate ;
      lData.ThreadID         := Copy( lsLine, liStart, cuiWidthThread ) ;

      liStart                := liStart + 1 + cuiWidthThread ;
      lData.SeverityAsString := Copy( lsLine, liStart, cuiWidthSeverity ) ;

      liStart                := liStart + 1 + cuiWidthSeverity ;
      lData.LogMessage := Copy( lsLine, liStart, Length( lsLine ) - liStart + 1 ) ;

      FList.Add( lData ) ;

    end ;
  finally
    ls.Free ;
  end ;
end;

{ TLogToConsole }

function TLogToConsole.AcceptEvent(const psDateTime, psMessage: string;
  pSeverity: TLogSeverity): boolean;
begin
  if _IsParam( csLogVisual ) then
    // ToDo: Tidy this call to gLog up with a back pointer.
    result := pSeverity in gLog.SevToLog
  else
    result :=
      pSeverity in [ lsUserInfo, lsQueryTiming, {lsWarning,} lsError ] ;
end;

constructor TLogToConsole.Create;
begin
  inherited;
end;

destructor TLogToConsole.Destroy;
begin
  Terminate ;
  inherited;
end;

procedure TLogToConsole.WriteToOutput;
var
  i         : integer ;
  lLogEvent : TLogEvent ;
  lsLine    : string ;
begin
  inherited WriteToOutput ;
  for i := 0 to FListWorking.Count - 1 do begin
    lLogEvent := TLogEvent( FListWorking.Items[i] ) ;
    lsLine    := lLogEvent.AsStringStripCrLf ;
    lsLine    := Copy( lsLine,
                       cuiWidthDate + cuiWidthThread +
                       cuiWidthSeverity + 4, Length( lsLine )) ;
    WriteLn( lsLine ) ;
  end ;
  FListWorking.Clear ;
end;

procedure TtiLog.SetSevToLog(const Value: TSevToLog);
var
  lCritSect : TCriticalSection ;
  i : integer ;
begin
  lCritSect := TCriticalSection.Create ;
  try
    lCritSect.Enter ;
    try
      FSevToLog := Value;
      for i := 0 to FLogToList.Count - 1 do
        TLogToAbs( FLogToList.Items[i] ).SevToLog := Value ;
    finally
      lCritSect.Leave ;
    end ;
  finally
    lCritSect.Free ;
  end ;
end;

procedure TLogToAbs.SetSevToLog(const Value: TSevToLog);
begin
  FSevToLog := Value;
end;

procedure TLogToAbs.Terminate;
begin
  // Do nothing, implement if required in the concrete
end;

procedure TtiLog.TerminateGUILogs;
var
  i : integer ;
begin
  for i := 0 to FLogToList.Count - 1 do
    if ( TObject(FLogToList.Items[i]) is TLogToError ) or
       ( TObject(FLogToList.Items[i]) is TLogToForm ) then
       TLogToAbs(FLogToList.Items[i]).Terminate ;
end;

initialization
  ubFinalization := false ;

finalization
  ubFinalization := true ;
  uLog.free ;

end.







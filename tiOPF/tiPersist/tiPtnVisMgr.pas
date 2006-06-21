{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

  Purpose: Family of abstract classes to provide functionality of
           Visitor Pattern

  Revision History:
    Sept 1999, PWH, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiPtnVisMgr ;

interface
uses
   tiPtnVis
  ,tiPtnVisSQL
  ,Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  ,Controls       // crHourGlass, crDefault
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QControls      // crHourGlass, crDefault
  ,Libc
  {$ENDIF LINUX}
  ;

type

  //----------------------------------------------------------------------------
  TVisClassRef = class of TVisitorAbs ;

  //----------------------------------------------------------------------------
  TVisMapping = class( TObject )
  private
    FsGroupName : string;
    FClassRef   : TVisClassRef;
  public
    constructor CreateExt( const psGroupName : string ;
                           const pClassRef : TVisClassRef ) ;
    property    GroupName : string read FsGroupName write FsGroupName ;
    property    ClassRef  : TVisClassRef read FClassRef write FClassRef ;
  end ;

  // A procedural type to define the signature used for
  // BeforeExecute, AfterExecute and AfterExecuteError
  TProcessVisitorMgrs = procedure( pVisitorController : TVisitorCtrlr ;
                                   pVisitors   : TList ) of object ;

  // The Visitor Manager
  //----------------------------------------------------------------------------
  TtiVisMgr = class( TObject )
  private
    FVisMappings : TStringList ;
    FHourGlassCount : integer ;
    {$IFDEF MSWINDOWS}
    FSemaphore : THandle ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FSemaphore : TSemaphore ;
    {$ENDIF LINUX}
    FThreadIDList : TList ;
    FBreakOnException: boolean;
    FSavedCursor : TCursor ;
    procedure GetVisitors(       pVisitors : TList ; const psGroupName : string ) ;
    procedure GetVisitorControllers( const pVisitors         : TList ;
                                     const pVisitorMgrs      : TList ;
                                     const pDBConnectionName : string ;
                                     const pPerLayerName     : string ) ;
    procedure ProcessVisitorControllers(
        pVisitors, pVisitorControllers : TList ;
        pProc : TProcessVisitorMgrs ;
        psMethodName : string ) ;
    // These call ProcessVisitorMgrs to scan for visitors and visitorMgrs
    // by passing the appropriate method of VisitorMgr to execute.
    procedure DoBeforeExecute(     pVisitorMgr : TVisitorCtrlr ; pVisitors   : TList ) ;
    procedure DoAfterExecute(      pVisitorMgr : TVisitorCtrlr ; pVisitors   : TList ) ;
    procedure DoAfterExecuteError( pVisitorMgr : TVisitorCtrlr ; pVisitors   : TList ) ;
    procedure ExecuteVisitors(   pVisitors    : TList ; pVisited : TVisitedAbs ) ;
    procedure ProcessVisitors( const pGroupName : string ;
                               const pVisited : TVisitedAbs ;
                               const pDBConnectionName : string;
                               const pPerLayerName      : string) ;
    procedure Lock ;
    procedure UnLock ;
    procedure AddThreadID(    pThreadID : LongWord ) ;
    procedure RemoveThreadID( pThreadID : LongWord ) ;
    function  GetThreadCount : LongWord ;
  public
    constructor create ; virtual ;
    destructor  destroy ; override ;
    procedure   RegisterVisitor( const psGroupName : string ;
                                 const pClassRef   : TVisClassRef ) ;
    procedure   UnRegisterVisitors( const psGroupName : string ) ;
    function    Execute( const psGroupName       : string ;
                         const pVisited          : TVisitedAbs ;
                         const pDBConnectionName : string = '' ;
                         const pPerLayerName     : string = '' ) : string ;
    property    ThreadCount : LongWord read GetThreadCount ;
    property    BreakOnException : boolean read FBreakOnException write FBreakOnException ;
  end ;

// Do not use this, it can be uncommented for backward compatability only,
// use gTIPerMgr instead.
function gVisMgr  : TtiVisMgr ;

implementation
uses
  SysUtils
  ,tiLog
  ,tiUtils
  ,Contnrs
  ,tiPersist
  {$IFDEF MSWINDOWS}
  ,Forms          // Screen.Cursor
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  {$ENDIF LINUX}
  ,tiRegPerLayer
  ,ctiPersist
  ;

const
  cuVisMgrSemaphoreName = 'VisMgrSemaphoreName' ;

// Do not use this, it can be uncommented for backward compatability only,
// use gTIPerMgr instead.
//------------------------------------------------------------------------------
function gVisMgr   : TtiVisMgr ;
begin
  result := gTIPerMgr.VisMgr ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiVisMgr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiVisMgr.AddThreadID(pThreadID: LongWord);
begin
  if ( pThreadID = MainThreadID ) then
    Exit ; //==>
  Lock ;
  try
    if FThreadIDList.IndexOf( TObject( pThreadID )) = -1 then
      FThreadIDList.Add( TObject( pThreadID )) ;
  finally
    UnLock ;
  end;
end;

constructor TtiVisMgr.create;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
  inherited ;
  FThreadIDList := TList.Create ;
  {$IFDEF MSWINDOWS}
  FSemaphore := CreateSemaphore( nil, 1, 1, PChar( cuVisMgrSemaphoreName )) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_init(FSemaphore, false, 0);
  if error <> 0 then
    raise Exception.Create('Failed to create the semaphore');
  {$ENDIF LINUX}
  FVisMappings := TStringList.Create ;
  FHourGlassCount := 0 ;
  FBreakOnException := false ;
end;

//------------------------------------------------------------------------------
destructor TtiVisMgr.destroy;
var
  i : integer ;
  {$IFDEF LINUX}
  error: integer;
  {$ENDIF LINUX}
begin
  for i := FVisMappings.Count-1 downto 0 do
    TObject( FVisMappings.Objects[i] ).Free ;
  FVisMappings.Free ;
  {$IFDEF MSWINDOWS}
  CloseHandle( FSemaphore ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_destroy( FSemaphore );
  if error <> 0 then
    raise Exception.Create('Failed to destroy the semaphore');
  {$ENDIF LINUX}
  FThreadIDList.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TtiVisMgr.DoAfterExecute( pVisitorMgr : TVisitorCtrlr ;
                                        pVisitors   : TList ) ;
begin
  pVisitorMgr.AfterExecuteAll(pVisitors) ;
end;

procedure TtiVisMgr.DoAfterExecuteError(pVisitorMgr: TVisitorCtrlr;
                                            pVisitors: TList);
begin
  pVisitorMgr.AfterExecuteError(pVisitors) ;
end;

procedure TtiVisMgr.DoBeforeExecute(pVisitorMgr: TVisitorCtrlr;
                                        pVisitors: TList);
begin
  pVisitorMgr.BeforeExecuteAll(pVisitors) ;
end;

function TtiVisMgr.Execute( const psGroupName       : string;
                            const pVisited          : TVisitedAbs ;
                            const pDBConnectionName : string = '' ;
                            const pPerLayerName     : string = '' ) : string ;
var
  lbHourGlassRequired : boolean ;
  lPerLayerName       : string ;
  lDBConnectionName   : string ;
begin

  // Don't go any further if terminated
  if gTIPerMgr.Terminated then
    Exit ; //==>

  AddThreadID( GetCurrentThreadID ) ;

  try

    Log( 'About to process visitors for <' + psGroupName + '>', lsVisitor ) ;

    if pPerLayerName = '' then
    begin
      Assert( gTIPerMgr.DefaultPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );
      lPerLayerName := gTIPerMgr.DefaultPerLayer.PerLayerName
    end else
      lPerLayerName := pPerLayerName ;

    if pDBConnectionName = '' then
      lDBConnectionName := gTIPerMgr.DefaultDBConnectionName
    else
      lDBConnectionName := pDBConnectionName ;

    Assert( lDBConnectionName <> '',
            'Either the gTIPerMgr.DefaultDBConnectionName must be set, ' +
            'or the DBConnectionName must be passed as a parameter to ' +
            'gVisMgr.Execute( )' ) ;

    // If we are in the main thread, and Application.MainForm <> nil,
    // then we require an hourglass
    lbHourGlassRequired :=
      ( GetCurrentThreadID = MainThreadID ) and
      ( Application.MainForm <> nil ) ;

    // If an hourglass is required, then turn it on and inc the counter
    if lbHourGlassRequired then
    begin
      if ( FHourGlassCount = 0 ) then
      begin
        FSavedCursor := Screen.Cursor ;
        Screen.Cursor := crHourGlass ;
      end ;
      Inc( FHourGlassCount ) ;
    end ;

    try
      Result := '' ;
      try
        ProcessVisitors( psGroupName, pVisited, lDBConnectionName, lPerLayerName ) ;
      finally
        // If an hourglass was required, then dec the counter and turn it off
        if lbHourGlassRequired then
        begin
          Dec( FHourGlassCount ) ;
          if ( FHourGlassCount = 0 ) then
            Screen.Cursor := FSavedCursor ;
        end ;
      end ;

    except
      // Log and display any error messages
      on e:exception do
      begin
        Result := e.message ;
        LogError( e.message, false ) ;
        if BreakOnException then
          raise ;
      end ;
    end ;

    Log( 'Finished process visitors for <' + psGroupName + '>', lsVisitor ) ;
  finally
    RemoveThreadID( GetCurrentThreadID ) ;
  end ;

end;

//------------------------------------------------------------------------------
procedure TtiVisMgr.ExecuteVisitors(pVisitors: TList; pVisited: TVisitedAbs);
  procedure _RunBeforeExecuteOne( pVisitor : TVisitorAbs ) ;
  var
    lsVisitor : string ;
  begin
    try
      lsVisitor := pVisitor.ClassName ;
      pVisitor.VisitorController.BeforeExecuteOne( pVisitor ) ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_RunBeforeExecuteOne' ) ;
    end ;
  end ;

  procedure _RunAfterExecuteOne( pVisitor : TVisitorAbs ) ;
  var
    lsVisitor : string ;
  begin
    // Don't go any further if terminated
    if gTIPerMgr.Terminated then
      Exit ; //==>
    try
      lsVisitor := pVisitor.ClassName ;
      pVisitor.VisitorController.AfterExecuteOne( pVisitor ) ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_RunAfterExecuteOne' ) ;
    end ;
  end ;

  procedure _RunIterate( pVisited : TVisitedAbs ; pVisitor : TVisitorAbs );
  var
    lsVisited : string ;
    lsVisitor : string ;
  begin
    try
      lsVisited := pVisited.ClassName ;
      lsVisitor := pVisitor.ClassName ;
      if pVisitor.IterateDirection = vidTopDown then
        pVisited.Iterate( pVisitor )
      else
        pVisited.IterateBottomUp( pVisitor ) ;

    except
      on e:exception do
          tiFmtException( e,
                          'Visited: ' + lsVisited + Cr +
                          'Visitor: ' + lsVisitor,
                          ClassName, '_RunIterate' ) ;
    end ;
  end ;

  procedure _RunExecute( pVisitor : TVisitorAbs ) ;
  var
    lsVisitor : string ;
  begin
    lsVisitor := pVisitor.ClassName ;
    try
      pVisitor.Execute( nil ) ;
    except
      on e:exception do
        tiFmtException( e, ClassName, '_RunExecute' ) ;
    end ;
  end ;

var
  lVisitor : TVisitorAbs ;
  i : integer ;
begin
  for i := 0 to pVisitors.Count - 1 do
  begin

    // Don't go any further if terminated
    if gTIPerMgr.Terminated then
      Exit ; //==>

    lVisitor  := TVisitorAbs( pVisitors.Items[i] ) ;
    _RunBeforeExecuteOne( lVisitor ) ;
    try
      if pVisited <> nil then
        _RunIterate( pVisited, lVisitor )
      else
        _RunExecute( lVisitor ) ;
    finally
      _RunAfterExecuteOne( lVisitor ) ;
    end ;

    // 13 Sept 2000, Peter Hinrichsen, Added to break out of ReadPK visitor
    //                                 used in auto OO-DB map framework.
    if not lVisitor.ContinueVisiting then
      Break ; //==>

  end ;

end ;

// Search for the appropriate VisitorController for each visitor
procedure TtiVisMgr.GetVisitorControllers( const pVisitors    : TList ;
                                           const pVisitorMgrs : TList ;
                                           const pDBConnectionName : string ;
                                           const pPerLayerName : string );
var
  i, j : integer ;
  lVisitor : TVisitorAbs ;
begin
  Log( 'Getting visitor controllers', lsVisitor ) ;
  try
    // Scan all the visitors
    for i := 0 to pVisitors.Count - 1 do begin
      // Get a local pointer to the visitor
      lVisitor := TVisitorAbs( pVisitors.Items[i] ) ;

      // Search the list of visitor controllers already created for a match
      // with this visitor.
      for j := 0 to pVisitorMgrs.Count - 1 do begin
        if (lVisitor.VisitorControllerClass.ClassName = TObject( pVisitorMgrs.Items[j] ).ClassName ) then begin
          lVisitor.VisitorController := TVisitorCtrlr( pVisitorMgrs.Items[j] ) ;
          break ; //==>
        end ;
      end ;

      // The visitor controller was not found, so add a new one.
      if lVisitor.VisitorController = nil then begin
        lVisitor.VisitorController := lVisitor.VisitorControllerClass.Create ;
        lVisitor.VisitorController.PerLayerName := pPerLayerName ;
        lVisitor.VisitorController.DBConnectionName := pDBConnectionName ;
        // ToDo: Remove the need to set lVisitor.VisitorController.SQLMgrDataSource
        lVisitor.VisitorController.SQLMgrDataSource := pDBConnectionName ;
        pVisitorMgrs.Add( lVisitor.VisitorController ) ;
      end ;
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'GetVisitorControllers' ) ;
  end ;
  Log( 'Done getting visitor controllers', lsVisitor ) ;
end ;

procedure TtiVisMgr.GetVisitors(pVisitors: TList;const psGroupName: string);
var
  i : integer ;
  lsGroupName : string ;
begin
  try
    pVisitors.Clear ;
    lsGroupName := upperCase( psGroupName ) ;
    for i := 0 to FVisMappings.Count - 1 do
      if FVisMappings.Strings[i] = lsGroupName then
        pVisitors.Add( TVisMapping( FVisMappings.Objects[i] ).ClassRef.Create ) ;
  except
      on e:exception do
        tiFmtException( e, ClassName, 'GetVisitors' ) ;
  end ;
end;

procedure TtiVisMgr.Lock;
begin
  {$IFDEF MSWINDOWS}
  if WaitForSingleObject( FSemaphore, 60000 ) = WAIT_TIMEOUT then
    LogError( 'Timed out waiting for a PooledItem.' ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  sem_wait(FSemaphore);
  {$ENDIF LINUX}
end;

procedure TtiVisMgr.ProcessVisitorControllers(pVisitors, pVisitorControllers: TList;
  pProc: TProcessVisitorMgrs ; psMethodName : string );
var
  i, j : integer ;
  lVisitorController : TVisitorCtrlr ;
  lVisitors   : TList ;
begin

  if gTIPerMgr.Terminated then
    Exit ; //==>

  try
    lVisitors := TList.Create;
    try
      // Scan all the visitor controllers in the list
      for i := 0 to pVisitorControllers.Count - 1 do
      begin

        // Don't go any further if terminated
        if gTIPerMgr.Terminated then
          Exit ; //==>

        lVisitorController := TVisitorCtrlr( pVisitorControllers.Items[i] ) ;
        for j := 0 to pVisitors.Count-1 do
          if ( TVisitorAbs( pVisitors.Items[j] ).VisitorControllerClass =
             lVisitorController.ClassType ) and
             ( not gTIPerMgr.Terminated ) then
            lVisitors.Add( pVisitors.Items[j] ) ;
          pProc( lVisitorController, lVisitors ) ;

      end ;
    finally
      lVisitors.Free ;
    end ;
  except
      on e:exception do
        tiFmtException( e,
                        'Calling method: ' + psMethodName,
                        ClassName, 'ProcessVistorControllers' ) ;
  end ;
end;

procedure TtiVisMgr.ProcessVisitors( const pGroupName        : string;
                                     const pVisited          : TVisitedAbs;
                                     const pDBConnectionName : string;
                                     const pPerLayerName     : string );
var
  lVisitors           : TObjectList ;
  lVisitorMgrs        : TObjectList ;
begin
  lVisitors := TObjectList.Create ;
  try
    lVisitorMgrs := TObjectList.Create ;
    try
      GetVisitors(    lVisitors, pGroupName  ) ;
      GetVisitorControllers( lVisitors, lVisitorMgrs, pDBConnectionName, pPerLayerName ) ;
      Log( 'Visitor count: ' +
           IntToStr( lVisitors.Count ) +
           ' VisitorMgr count: ' +
           IntToStr( lVisitorMgrs.Count ), lsVisitor ) ;
      ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoBeforeExecute, 'DoBeforeExecute' ) ;
      try
        ExecuteVisitors( lVisitors, pVisited ) ;
        ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoAfterExecute, 'DoAfterExecute' ) ;
      except
        on e:exception do
        begin
          ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoAfterExecuteError, 'DoAfterExecuteError ' ) ;
          tiFmtException( e, ClassName, 'ProcessVisitors' ) ;
        end ;
      end ;
    finally
      lVisitorMgrs.Free ;
    end ;
  finally
    lVisitors.Free ;
  end ;
end;

procedure TtiVisMgr.RegisterVisitor( const psGroupName : string ;
                                         const pClassRef : TVisClassRef ) ;
var
  lVisMapping : TVisMapping ;
  lsGroupName : string ;
begin
  lsGroupName := UpperCase( psGroupName ) ;
  lVisMapping := TVisMapping.CreateExt( lsGroupName, pClassRef ) ;
  FVisMappings.AddObject( lsGroupName, lVisMapping ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiVisMgr.RemoveThreadID(pThreadID: LongWord);
var
  i : integer ;
begin
  if ( pThreadID = MainThreadID ) then
    Exit ; //==>
  if gTIPerMgr.Terminated then
    Exit ; //==>
  Lock ;
  try
    if not gTIPerMgr.Terminated then
    begin
      i := -1 ;
      try
        i := FThreadIDList.IndexOf( TObject( pThreadID )) ;
      except end ;
      if i <> -1 then
       FThreadIDList.Delete( i ) ;
    end ;
  finally
    UnLock ;
  end;
end;

function TtiVisMgr.GetThreadCount: LongWord;
begin
  Lock ;
  try
    result := FThreadIDList.Count ;
  finally
    UnLock ;
  end;
end;

procedure TtiVisMgr.UnLock;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
  {$IFDEF MSWINDOWS}
  ReleaseSemaphore( FSemaphore, 1, nil ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_post(FSemaphore);
  if error <> 0 then
    raise Exception.Create('Failed to unlock the semaphore');
  {$ENDIF LINUX}
end;

procedure TtiVisMgr.UnRegisterVisitors(const psGroupName: string);
var
  i : integer ;
  lsGroupName : string ;
begin
  try
    lsGroupName := upperCase( psGroupName ) ;
    for i := FVisMappings.Count - 1 downto 0 do
      if FVisMappings.Strings[i] = lsGroupName then
      begin
        TVisMapping( FVisMappings.Objects[i] ).Free ;
        FVisMappings.Delete( i ) ;
      end ;
  except
      on e:exception do
        tiFmtException( e, ClassName, 'UnRegisterVisitor' ) ;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisMapping
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisMapping.CreateExt(const psGroupName: string;
  const pClassRef: TVisClassRef);
begin
  Create ;
  FClassRef   := pClassRef ;
  FsGroupName := upperCase( psGroupName ) ;
end;

end.




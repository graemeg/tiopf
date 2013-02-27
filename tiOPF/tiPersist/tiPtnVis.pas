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
           Iterator Pattern

  Classes: TVisitorAbs - An abstract visitor class
           TVisitedAbs - An abstract visited class
           TVisList    - A TList which has been wrappered in a TVisitedAbs
           TVisStream  - A TVisitedAbs with a stream for writing out text files.

  Revision History:
    Sept 1999, PWH, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{$I tiDefines.inc}

unit tiPtnVis ;

interface
uses
  Classes
  ,tiObjAbs
  ,tiUtils   // GetPropNames
  ,TypInfo
  ;

type

  // TVisitedAbs forward declaration
  TVisitedAbs = class ;
  TVisitorAbs = class ;

  TVisitorCtrlr = class( TtiObjAbs )
  private
    FDBConnectionName: string;
    FSQLMgrDataSource: string;
    FPerLayerName    : string ;
  protected
    procedure SetPerLayerName(const Value: string); virtual ;
  public
    constructor Create ; virtual ;
    procedure BeforeExecuteAll( pVisitors : TList )      ; virtual ;
    procedure BeforeExecuteOne( pVisitor : TVisitorAbs ) ; virtual ;
    // Visitors are executed here...
    procedure AfterExecuteOne( pVisitor : TVisitorAbs  ) ; virtual ;
    procedure AfterExecuteAll( pVisitors : TList )       ; virtual ;
    // Executed if there was an error
    procedure AfterExecuteError( pVisitors : TList )     ; virtual ;
    // The property DBConnectionName is really only required in DBVisitors, but
    // must be introduce here so it can be set at a generic level by the
    // VisitorMgr. The alternative is to use RTTI or TypeInfo and only set the
    // property on DBVisitorMgr(s), but that would be an ever worse hack.
    property  PerLayerName     : string read FPerLayerName     write SetPerLayerName ;
    property  DBConnectionName : string read FDBConnectionName write FDBConnectionName ;
    // ToDo: Remove SQLMgrDataSource from TVisitorController
    property  SQLMgrDataSource : string read FSQLMgrDataSource write FSQLMgrDataSource ;
  end ;

  TVisitorControllerClass = class of TVisitorCtrlr ;

  TVisitorIterateDirection = ( vidTopDown, vidBottomUp ) ;

  // TVisitorAbs: The class that does the visiting
  //----------------------------------------------------------------------------
  TVisitorAbs = class( TtiObjAbs )
  private
    FVisited           : TVisitedAbs ;
    FbContinueVisiting : boolean;
    FVisitorController : TVisitorCtrlr;
    FiDepth: integer;
    FIterateDirection: TVisitorIterateDirection;
    FVisitedsOwner: TVisitedAbs;
  protected
    function    AcceptVisitor : boolean ; overload ; virtual ;
    function    DoAcceptVisitor : boolean ;
    function    GetVisited: TVisitedAbs ; virtual ;
    procedure   SetVisited(const Value: TVisitedAbs); virtual ;
  public
    constructor Create ; virtual;

    procedure   Execute( const pVisited : TVisitedAbs ) ; virtual ;
    function    VisitorControllerClass : TVisitorControllerClass ; virtual ;

    property    Visited : TVisitedAbs read GetVisited write SetVisited ;
    property    ContinueVisiting : boolean read FbContinueVisiting write FbContinueVisiting ;
    property    VisitorController : TVisitorCtrlr read FVisitorController write FVisitorController ;
    property    Depth : integer read FiDepth write FiDepth ;
    property    IterateDirection : TVisitorIterateDirection
                  read  FIterateDirection
                  write FIterateDirection ;
    property    VisitedsOwner : TVisitedAbs read FVisitedsOwner write FVisitedsOwner ;
  end ;

  TVisGetAllToVisit = class( TVisitorAbs )
  private
    FList : TList ;
    FVisitor : TVisitorAbs ;
  protected
    function AcceptVisitor : boolean ; override ;
  public
    constructor create ; override ;
    destructor  destroy ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    Visitor : TVisitorAbs read FVisitor write FVisitor ;
    property    List : TList read FList ;
  end ;

  // TVisitorClass reference
  //----------------------------------------------------------------------------
  TVisitorClass = class of TVisitorAbs ;

  // TVisitedAbs class reference
  // ---------------------------------------------------------------------------
  TVisitedClass = class of TVisitedAbs ;

  // TVisitedAbs
  // The class that gets visited.
  //----------------------------------------------------------------------------
  {$M+} // Turn on RTTI for TVisitedAbs
  TVisitedAbs = class( TtiObjAbs )
  private
    FbSelfIterate: boolean;
  protected
    function    GetCaption : string ; virtual ;

  published
    property    Caption    : string  read GetCaption ;
  public
    constructor create ; virtual ;
    procedure   Iterate( pVisitor : TVisitorAbs ) ; virtual ;
    procedure   IterateBottomUp( pVisitor: TVisitorAbs ) ; virtual ;
    property    SelfIterate : boolean read FbSelfIterate write FbSelfIterate ;
    procedure   FindAllByClassType( pClass : TVisitedClass ; pList : TList ) ;
    function    CountByClass( pClass : TVisitedClass ) : integer ;
    function    PropCount( pPropFilter : TTypeKinds = ctkSimple ) : integer ;
  end ;
  {$M-} // Turn off RTTI for TVisitedAbs

  // A wrapper for the TStream which allows text to be written to the stream
  // with each visit.
  //----------------------------------------------------------------------------
  TVisStream = class( TVisitorAbs )
  private
    FStream : TStream ;
  protected
    procedure Write( const psValue : string ) ; virtual ;
    procedure WriteLn( const psValue : string = '' ) ; virtual ;
    procedure SetStream(const Value: TStream) ; virtual ;
  public
    property  Stream : TStream read FStream write SetStream ;
  end ;

  //----------------------------------------------------------------------------
  TVisStringStream = class( TVisStream )
  private
    function GetText: string;
  protected
  public
    Constructor Create ; override ;
    Destructor  Destroy ; override ;
    Property    Text : string read GetText ;
  end ;

  // A visitor to count the number of instances of each class owned by the
  // passed object
  //----------------------------------------------------------------------------
  TVisClassCount = class( TVisitorAbs )
  private
    FList: TStringList;
    function GetClassCount(pClass : TClass): integer;
    procedure SetClassCount(pClass : TClass; const Value: integer);
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    ClassCount[ pClass : TClass]: integer
                  read GetClassCount
                  write SetClassCount ;
  end ;

  // A visitor to find all owned objects of a given class
  //----------------------------------------------------------------------------
  TVisFindAllByClass = class( TVisitorAbs )
  private
    FList: TList;
    FClassTypeToFind: TVisitedClass;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    ClassTypeToFind : TVisitedClass read FClassTypeToFind write FClassTypeToFind ;
    property    List : TList read FList write FList ;
  end ;

  TVisStreamClass = class of TVisStream ;

// Global proc to write a apply a TVisStream (as a TFileStream) to a TVisitedAbs.
procedure VisStreamToFile( pData        : TVisitedAbs ;
                           psFileName   : string ;
                           pVisClassRef : TVisitorClass ;
                           pFileStream  : TFileStream = nil );


implementation
uses
   SysUtils  // Exception
  ,tiLog     // Logging
  ,tiPersist
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI5}
  ,FileCtrl
    {$ENDIF}
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QFileCtrls
  {$ENDIF LINUX}
  ;

//------------------------------------------------------------------------------
procedure VisStreamToFile( pData : TVisitedAbs ;
                           psFileName : string ;
                           pVisClassRef : TVisitorClass ;
                           pFileStream : TFileStream = nil );
var
  lVisitor    : TVisStream ;
  lFileStream : TFileStream ;
  lDir : string ;
begin
  lDir := ExtractFilePath( psFileName ) ;
  if not DirectoryExists( lDir ) then
  begin
    ForceDirectories(lDir);
    if not DirectoryExists(lDir) then
      tiFmtException( 'Unable to create directory <' +
                      lDir + '>', '',
                      'VissStreamToFile') ;
  end;

  if pFileStream = nil then
    lFileStream := TFileStream.Create( psFileName, fmCreate or fmShareDenyNone )
  else
    lFileStream := pFileStream ;
  try
    lVisitor   := TVisStream( pVisClassRef.Create ) ;
    try
      lVisitor.Stream := lFileStream ;
      pData.Iterate( lVisitor ) ;
    finally
      lVisitor.Free ;
    end ;
  finally
    if pFileStream = nil then
      lFileStream.Free ;
  end ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisitedAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisitedAbs.CountByClass(pClass: TVisitedClass): integer;
var
  lList : TList ;
begin
  lList := TList.Create ;
  try
    FindAllByClassType( pClass, lList ) ;
    result := lList.Count ;
  finally
    lList.Free ;
  end ;
end;

constructor TVisitedAbs.create ;
begin
  inherited create ;
  FbSelfIterate := true ;
end;

//------------------------------------------------------------------------------
procedure TVisitedAbs.FindAllByClassType(pClass: TVisitedClass; pList: TList);
var
  lVis : TVisFindAllByClass ;
begin
  Assert( pList <> nil, 'pList not assigned' ) ;
  pList.Clear ;
  lVis := TVisFindAllByClass.Create ;
  try
    lVis.ClassTypeToFind := pClass ;
    lVis.List := pList ;
    Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end;

function TVisitedAbs.GetCaption: string;
begin
  result := className ;
end;

//------------------------------------------------------------------------------
procedure TVisitedAbs.Iterate(pVisitor: TVisitorAbs) ;
var
  lClassPropNames : TStringList ;
  i        : integer ;
  j        : integer ;
  lVisited : TObject ;
  lVisitedsOwner : TVisitedAbs ;
begin
  Assert( pVisitor <> nil, 'Visitor unassigned' ) ;

  // Don't go any further if terminated
  if gTIPerMgr.Terminated then
    Exit ; //==>

  if not pVisitor.ContinueVisiting then
    Exit ; //==>

  pVisitor.Depth := pVisitor.Depth + 1 ;
  try
    try
      pVisitor.Execute( self ) ;
      lVisitedsOwner := pVisitor.VisitedsOwner ;
      pVisitor.VisitedsOwner := Self ;

      // If SelfIterate is true, then use RTTI to scan through all the
      // properties of type TVisitedAbs
      if SelfIterate and
         ( not gTIPerMgr.Terminated ) then
      begin
        // Create a string list to hold the property names
        lClassPropNames := TStringList.Create ;
        try
          // Get all property names of type tkClass
          tiGetPropertyNames( self, lClassPropNames, [tkClass] ) ;

          // Scan through these properties
          for i := 0 to lClassPropNames.Count - 1 do
          begin

            // Get a pointer to the property
            lVisited := GetObjectProp( self, lClassPropNames.Strings[i] ) ;

            // If the property is a TVisitedAbs, then visit it.
            if ( lVisited is TVisitedAbs ) and
               ( pVisitor.ContinueVisiting ) and
               ( not gTIPerMgr.Terminated ) then
            begin
              TVisitedAbs( lVisited ).Iterate( pVisitor ) ;
              continue ; //==>
            end ;

            // If the property is a TList, then visit it's items
            if (lVisited is TList ) then
            begin
              for j := 0 to TList( lVisited ).Count - 1 do
                if ( TObject( TList( lVisited ).Items[j] ) is TVisitedAbs ) and
                   ( pVisitor.ContinueVisiting ) and
                   ( not gTIPerMgr.Terminated ) then
                begin
                  TVisitedAbs( TList( lVisited ).Items[j] ).Iterate( pVisitor ) ;
                end ;
              continue ; //==>
            end ;

          end ;
          pVisitor.VisitedsOwner := lVisitedsOwner ;
        finally
          lClassPropNames.Free ;
        end ;
      end ;

    except
      on e:exception do
        tiFmtException( e, ClassName, 'Iterate' ) ;
    end ;
  finally
    pVisitor.Depth := pVisitor.Depth - 1 ;
  end ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisitorAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisitorAbs.create;
begin
  inherited create ;
  FbContinueVisiting    := true ;
  FVisitorController := nil ;
  FiDepth := 0 ;
  FIterateDirection := vidTopDown ;
end;

//------------------------------------------------------------------------------
function TVisitorAbs.AcceptVisitor: boolean;
begin
  result := true ;
end;

//------------------------------------------------------------------------------
procedure TVisStream.SetStream(const Value: TStream);
begin
  FStream := Value;
end;

//------------------------------------------------------------------------------
procedure TVisStream.Write(const psValue: string);
var
  lpcValue : PChar ;
begin
  Assert( FStream <> nil, 'Stream unassigned.' ) ;
  lpcValue := PChar( psValue ) ;
  FStream.WriteBuffer( lpcValue^, length( lpcValue )) ;
end;

//------------------------------------------------------------------------------
procedure TVisStream.WriteLn(const psValue: string = '' );
begin
  Write( psValue + CrLf ) ;
end ;

//------------------------------------------------------------------------------
procedure TVisitorAbs.Execute( const pVisited: TVisitedAbs);
begin
//Log( ClassName + '.Execute' ) ;
  FVisited := pVisited ;
end;

function TVisitorAbs.VisitorControllerClass : TVisitorControllerClass ;
begin
  result := TVisitorCtrlr ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  TVisitorCtrlr
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TVisitorCtrlr.AfterExecuteAll( pVisitors : TList );
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
procedure TVisitorCtrlr.AfterExecuteError( pVisitors : TList );
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
procedure TVisitorCtrlr.AfterExecuteOne(pVisitor : TVisitorAbs);
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
procedure TVisitorCtrlr.BeforeExecuteAll( pVisitors : TList );
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
procedure TVisitorCtrlr.BeforeExecuteOne(pVisitor : TVisitorAbs);
begin
  // Do nothing
end;

//------------------------------------------------------------------------------
constructor TVisitorCtrlr.Create;
begin
  // So we can create an instance ot TVisitorMgr from a class reference var.
  inherited ;
end;

function TVisitorAbs.DoAcceptVisitor: boolean;
begin
  try
    result := AcceptVisitor ;
  except
    on e:exception do
    begin
      result := false ; // To shut up the compiler
      tiFmtException( e, ClassName, 'AcceptVisitor' ) ;
    end ;
  end ;
end;

function TVisitorAbs.GetVisited: TVisitedAbs;
begin
  result := FVisited ;
end;

procedure TVisitorAbs.SetVisited(const Value: TVisitedAbs);
begin
  FVisited := Value ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisClassCount
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisClassCount.Create;
begin
  inherited;
  FList := TStringList.Create;
end;

// -----------------------------------------------------------------------------
destructor TVisClassCount.Destroy;
begin
  FList.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TVisClassCount.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute(pVisited);
  ClassCount[ pVisited.ClassType ] := ClassCount[ pVisited.ClassType ] + 1 ;
end;

// -----------------------------------------------------------------------------
function TVisClassCount.GetClassCount(pClass : TClass): integer;
begin
  Result := StrToIntDef( FList.Values[ pClass.ClassName ], 0 ) ;
end;

// -----------------------------------------------------------------------------
procedure TVisClassCount.SetClassCount(pClass : TClass; const Value: integer);
begin
  FList.Values[ pClass.ClassName ] := IntToStr( value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisStringStream
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TVisStringStream.Create;
begin
  inherited;
  Stream := TStringStream.Create( '' ) ;
end;

//------------------------------------------------------------------------------
destructor TVisStringStream.Destroy;
begin
  Stream.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
function TVisStringStream.GetText: string;
begin
  result := TStringStream( Stream ).DataString ;
end;

//------------------------------------------------------------------------------
procedure TVisitedAbs.IterateBottomUp(pVisitor: TVisitorAbs);
var
  lVisitor : TVisGetAllToVisit ;
  i : integer ;
begin
  lVisitor := TVisGetAllToVisit.Create ;
  try
    lVisitor.Visitor := pVisitor ;
    Self.Iterate( lVisitor ) ;
    for i := lVisitor.List.Count - 1 downto 0 do
      pVisitor.Execute( TVisitedAbs( lVisitor.List.Items[i] )) ;
  finally
    lVisitor.Free ;
  end ;

end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisGetAllToVisit
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisGetAllToVisit.AcceptVisitor: boolean;
begin
  result := FVisitor.AcceptVisitor ;
end;

//------------------------------------------------------------------------------
constructor TVisGetAllToVisit.create;
begin
  inherited;
  FList := TList.Create ;
end;

//------------------------------------------------------------------------------
destructor TVisGetAllToVisit.destroy;
begin
  FList.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TVisGetAllToVisit.execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited ) ;
  FVisitor.Visited := pVisited ;
  if AcceptVisitor then
    List.Add( pVisited ) ;
end;

{ TVisFindAllByClass }

function TVisFindAllByClass.AcceptVisitor: boolean;
begin
  result := Visited is FClassTypeToFind ;
end;

procedure TVisFindAllByClass.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>
  FList.Add( pVisited ) ;
end;

function TVisitedAbs.PropCount(pPropFilter: TTypeKinds = ctkSimple): integer;
var
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    tiGetPropertyNames( Self, lsl, pPropFilter ) ;
    result := lsl.Count ;
  finally
    lsl.Free ;
  end ;
end;

procedure TVisitorCtrlr.SetPerLayerName(const Value: string);
begin
  FPerLayerName := Value ;
end;

end.





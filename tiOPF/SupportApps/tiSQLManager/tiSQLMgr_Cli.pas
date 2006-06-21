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
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    Maintain a single instance of the SQLMagager data

  Classes:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiSQLMgr_Cli;

interface
uses
  tiSQLMgr_BOM
  ,tiPtnVisMgr
  ,tiPtnVis
  ,tiPtnVisPerObj
  ,tiThreadProgress
  ,Classes
  ,controls;

type

  // Write out a pas file of constants to allow mapping to the query names
  // ---------------------------------------------------------------------------
  TVisConstants = class( TVisStream )
  protected
    function AcceptVisitor : boolean ; override ;
    procedure SetStream(const Value: TStream) ; override ;
  public
    destructor Destroy ; override ;
    procedure Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  TQueryFindParam = (qfpGroupName, qfpQueryName, qfpQueryDesc, qfpSQL, qfpParamName, qfpQueryLocked, qfpTestInclude);
  TQueryFindParams = set of TQueryFindParam;

  TThrdCustomQueryFind = class;

  TVisCustomQueryFind = class( TVisitorAbs )
  private
    FbTestInclude,
    FbQueryLocked: boolean;
    FStrGroupName,
    FStrSQL,
    FStrQueryName,
    FStrQueryDesc,
    FStrParamName: string;
    FFindParams: TQueryFindParams;
    FList: TStringList;
    FThrdCustomQueryFind: TThrdCustomQueryFind;
    function GetList: TStringList;
    function ConformToPattern(const pFindParams: TQueryFindParams;
      const psGroupName, psQueryName, psQueryDesc, psSQL,
      psParamName: string; const pbQueryLocked,
      pbTestInclude: boolean): Boolean;
  protected
    function    AcceptVisitor : boolean  ; override ;
    function    GetCaption    : TCaption ; virtual  ;
  public
    constructor Create(AThrdCustomQueryFind: TThrdCustomQueryFind); reintroduce; virtual;
    destructor  Destroy; override;

    property    GroupName     : string        read FStrGroupName write FStrGroupName ;
    property    QueryName     : string        read FStrQueryName write FStrQueryName ;
    property    QueryDesc     : string        read FStrQueryDesc write FStrQueryDesc ;
    property    SQL           : string        read FStrSQL       write FStrSQL       ;
    property    ParamName     : string        read FStrParamName write FStrParamName ;
    property    QueryLocked   : boolean       read FbQueryLocked write FbQueryLocked ;
    property    TestInclude   : boolean       read FbTestInclude write FbTestInclude ;
    property    FindParams    : TQueryFindParams read FFindParams write FFindParams;
    property    List          : TStringList   read GetList;
    property    ThrdCustomQueryFind: TThrdCustomQueryFind read FThrdCustomQueryFind;
    property    Caption: TCaption read GetCaption;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    procedure   SetParams(const pFindParams: TQueryFindParams; const psGroupName, psQueryName, psQueryDesc, psSQL, psParamName: string; const pbQueryLocked, pbTestInclude: boolean); virtual;

  end ;

  TVisCustomQueryFindClass = class of TVisCustomQueryFind;

  // ToDo: Move this class to another unit because it is dragging TtiThreadProgress into
  //       the core package.
  TThrdCustomQueryFind = class(TtiThreadProgress)
  private
    FVisCustomQueryFind: TVisCustomQueryFind;
    FSQLMgr: TSQLMgr;
  protected
    procedure DoOnTerminate( sender : TObject ) ; override ;
  public
    property    VisCustomQueryFind: TVisCustomQueryFind read FVisCustomQueryFind;
    Constructor CreateExt(AVisCustomQueryFindClass: TVisCustomQueryFindClass); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   SetParams(const pFindParams: TQueryFindParams; const psGroupName, psQueryName, psQueryDesc, psSQL, psParamName: string; const pbQueryLocked, pbTestInclude: boolean); virtual;
    Procedure   Execute ; override ;
    Procedure   IncPosition; override;
    property    SQLMgr : TSQLMgr read FSQLMgr write FSQLMgr ;
  end;

  TVisQueryFind = class(TVisCustomQueryFind)
  protected
    function GetCaption: TCaption; override;
  end;

implementation
uses
  tiUtils
  ,SysUtils
  ,tiLog
  ,tiPersist
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisConstants
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisConstants.AcceptVisitor: boolean;
begin
  result := ( visited is TSQLMgrQuery ) and
            ( not TSQLMgrQuery( Visited ).Deleted ) and
            ( UpperCase( TSQLMgrGroup( TSQLMgrQuery( Visited ).Owner ).GroupName ) <> 'DELETED' );
end;

//------------------------------------------------------------------------------
destructor TVisConstants.Destroy;
begin
  WriteLn( '' ) ;
  WriteLn( 'implementation' ) ;
  WriteLn( '' ) ;
  WriteLn( 'end.' ) ;
  inherited ;
end;

//------------------------------------------------------------------------------
procedure TVisConstants.Execute(const pVisited: TVisitedAbs );
begin

  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  with TSQLMgrQuery( Visited ) do begin
    Write( '  cgQry' ) ;
    Write( tiPadR( QueryName, 50 )) ;
    Write( ' = ''' ) ;
    Write( tiPadR( QueryName + ''' ;' , 53 )) ;
    WriteLn( '' ) ;
  end ;

end;

//------------------------------------------------------------------------------
procedure TVisConstants.SetStream(const Value: TStream);
begin
  inherited SetStream( Value ) ;
  if Value <> nil then begin
    WriteLn( '{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' ) ;
    WriteLn( '  Purpose: Constants used to identify SQL read from the SQLManager.' ) ;
    WriteLn( '           Warning: Do not edit this file directory.' ) ;
    WriteLn( '           Run the Edit| Create constants file' ) ;
    WriteLn( '           menu choice from SQLManager' ) ;
    WriteLn( '' ) ;
    WriteLn( '  File generated: ' + tiDateTimeToStr( Now ) ) ;
    WriteLn( '' ) ;
    WriteLn( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }' ) ;
    WriteLn( 'unit cQueryNames ;' ) ;
    WriteLn( '' ) ;
    WriteLn( 'interface' ) ;
    WriteLn( '' ) ;
    WriteLn( 'const' ) ;
  end ;
end;

{ TVisQueryTest }

function TVisCustomQueryFind.ConformToPattern(
  const pFindParams: TQueryFindParams;
  const psGroupName, psQueryName, psQueryDesc, psSQL, psParamName: string;
  const pbQueryLocked, pbTestInclude: boolean): Boolean;

  function ParamNameMatch(pParams: TSQLMgrParams; const psParamPattern: string): boolean;
  var i: integer;
  begin
    Result := False;
    for i := 0 to (pParams.Count - 1) do begin
      Result := tiWildcardMatch(TSQLMgrParam( pParams.Items[i] ).ParamName, psParamPattern, True);
      if Result then Break; // only needs one match in list to be true
    end;//for
  end;

var
  i: TQueryFindParam;
begin
  Result := True;
  if (pFindParams = []) then Exit;
  with TSQLMgrQuery( Visited ) do begin
    //if the intersection of ([qfpQueryDesc, qfpSQL, qfpParamName] and pFindParams) is not equal to empty set then
    if (ObjectState = posPK) and (([qfpQueryDesc, qfpSQL, qfpParamName] * pFindParams) <> []) then
      gTIPerMgr.Read( Visited );

    for i := Low(TQueryFindParam) to High(TQueryFindParam) do begin
      if (i in pFindParams) then
        case i of
          qfpGroupName:
            if (Owner is TSQLMgrGroup) then Result := tiWildcardMatch(TSQLMgrGroup(Owner).GroupName, psGroupName, True)
            else Result := False;
          qfpQueryName:   Result := tiWildcardMatch(QueryName, psQueryName, True);
          qfpQueryDesc:   Result := tiWildcardMatch(QueryDesc, psQueryDesc, True);
          qfpSQL:         Result := tiWildcardMatch(SQL,       psSQL,       True);
          qfpParamName:   Result := ParamNameMatch(Params, psParamName);
          qfpQueryLocked: Result := (QueryLocked = pbQueryLocked);
          qfpTestInclude: Result := (TestInclude = pbTestInclude);
        end;//case
      if not(Result) then Break;
    end;//for
  end;//with
end;

function TVisCustomQueryFind.AcceptVisitor: boolean;
begin
  Result := ( visited is TSQLMgrQuery ) and ( not TSQLMgrQuery( Visited ).Deleted ) and
            ( TSQLMgrQuery( Visited ).ObjectState <> posEmpty ) and
            ConformToPattern(FindParams, GroupName, QueryName, QueryDesc, SQL,
                             ParamName, QueryLocked, TestInclude);
end;

procedure TVisCustomQueryFind.SetParams(const pFindParams: TQueryFindParams; const psGroupName, psQueryName, psQueryDesc, psSQL, psParamName: string; const pbQueryLocked, pbTestInclude: boolean);
begin
  FindParams  := pFindParams;
  GroupName   := psGroupName;
  QueryName   := psQueryName;
  QueryDesc   := psQueryDesc;
  SQL         := psSQL;
  ParamName   := psParamName;
  QueryLocked := pbQueryLocked;
  TestInclude := pbTestInclude;
end;

constructor TVisCustomQueryFind.Create(AThrdCustomQueryFind: TThrdCustomQueryFind);
begin
  inherited Create;
  FThrdCustomQueryFind := AThrdCustomQueryFind;
end;

destructor TVisCustomQueryFind.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TVisCustomQueryFind.Execute(const pVisited: TVisitedAbs);
begin
  Inherited Execute( pVisited ) ;

  //List.Append(pVisited.ClassName + #9 + pVisited.Caption);

  if ThrdCustomQueryFind.Terminated then exit ; //==>

  if (Visited is TSQLMgrQuery) then ThrdCustomQueryFind.IncPosition;

  if not AcceptVisitor then exit ; //==>
  List.Append(TSQLMgrGroup( TSQLMgrQuery( Visited ).Owner ).GroupName + #9 + TSQLMgrQuery( Visited ).QueryName);
end;

function TVisCustomQueryFind.GetList: TStringList;
begin
  if not(Assigned(FList)) then FList := TStringList.Create;
  Result := FList;
end;

function TVisCustomQueryFind.GetCaption: TCaption;
begin
  Result := ClassName;
end;

{ TThrdCustomQueryFind }

constructor TThrdCustomQueryFind.CreateExt(AVisCustomQueryFindClass: TVisCustomQueryFindClass);
begin
  inherited Create(true);
  AutoProgress := False;
  FVisCustomQueryFind := AVisCustomQueryFindClass.Create(Self);
  Caption := FVisCustomQueryFind.Caption;
end;

destructor TThrdCustomQueryFind.Destroy;
begin
  FVisCustomQueryFind.Free;
  inherited;
end;

procedure TThrdCustomQueryFind.DoOnTerminate(sender: TObject);
begin
  VisCustomQueryFind.List.SaveToFile('C:\Temp\TEST.TXT');
  inherited;
end;

procedure TThrdCustomQueryFind.Execute;
var
  // Local vars are prefixed with 'l'
  lVisClassCount: TVisClassCount;
begin

  lVisClassCount := TVisClassCount.Create;
  try
    FSQLMgr.Iterate( lVisClassCount ) ;
    Max := lVisClassCount.ClassCount[TSQLMgrQuery];
    // Some debugging code to check the VisClassCount still works after PH's hacking
    //Log([ TSQLMgr.ClassName, lVisClassCount.ClassCount[TSQLMgr]]) ;
    //Log([ TSQLMgrGroup.ClassName, lVisClassCount.ClassCount[TSQLMgrGroup]]) ;
    //Log([ TSQLMgrQuery.ClassName, lVisClassCount.ClassCount[TSQLMgrQuery]]) ;

  finally
    lVisClassCount.Free;
  end;//finally

  FSQLMgr.Iterate(VisCustomQueryFind);

end;

procedure TThrdCustomQueryFind.IncPosition;
begin
  inherited;
  Text := IntToStr(Position) + ' of ' + IntToStr(Max);
end;

procedure TThrdCustomQueryFind.SetParams(const pFindParams: TQueryFindParams; const psGroupName, psQueryName,
                                         psQueryDesc, psSQL, psParamName: string; const pbQueryLocked, pbTestInclude: boolean);
begin
  VisCustomQueryFind.SetParams(pFindParams, psGroupName, psQueryName, psQueryDesc,
                               psSQL, psParamName, pbQueryLocked, pbTestInclude);
end;

{ TVisQueryFind }

function TVisQueryFind.GetCaption: TCaption;
begin
  Result := 'Find Query';
end;

end.


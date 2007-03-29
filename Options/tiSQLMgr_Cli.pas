{

  Purpose:
    Maintain a single instance of the SQLMagager data

}


unit tiSQLMgr_Cli;

{$I tiDefines.inc}

interface
uses
  tiSQLMgr_BOM
  ,tiVisitor
  ,tiObject
  ,tiThreadProgress
  ,tiStreams
  ,Classes
  ,controls;

type

  // Write out a pas file of constants to allow mapping to the query names
  // ---------------------------------------------------------------------------
  TVisConstants = class( TVisStream )
  protected
    function AcceptVisitor : boolean ; override ;
    procedure SetStream(const Value: TtiPreSizedStream) ; override ;
  public
    destructor Destroy ; override ;
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;

  TQueryFindParam = (qfpGroupName, qfpQueryName, qfpQueryDesc, qfpSQL, qfpParamName, qfpQueryLocked, qfpTestInclude);
  TQueryFindParams = set of TQueryFindParam;

  TThrdCustomQueryFind = class;

  TVisCustomQueryFind = class( TtiVisitor )
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
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
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
  ,tiOPFManager
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

destructor TVisConstants.Destroy;
begin
  WriteLn( '' ) ;
  WriteLn( 'implementation' ) ;
  WriteLn( '' ) ;
  WriteLn( 'end.' ) ;
  inherited ;
end;

procedure TVisConstants.Execute(const pVisited: TtiVisited );
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

procedure TVisConstants.SetStream(const Value: TtiPreSizedStream);
begin
  inherited SetStream( Value ) ;
  if Value <> nil then
  begin
    Value.WriteLn( '{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' ) ;
    Value.WriteLn( '  Purpose: Constants used to identify SQL read from the SQLManager.' ) ;
    Value.WriteLn( '           Warning: Do not edit this file directory.' ) ;
    Value.WriteLn( '           Run the Edit| Create constants file' ) ;
    Value.WriteLn( '           menu choice from SQLManager' ) ;
    Value.WriteLn( '' ) ;
    Value.WriteLn( '  File generated: ' + tiDateTimeToStr( Now ) ) ;
    Value.WriteLn( '' ) ;
    Value.WriteLn( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }' ) ;
    Value.WriteLn( 'unit cQueryNames ;' ) ;
    Value.WriteLn( '' ) ;
    Value.WriteLn( 'interface' ) ;
    Value.WriteLn( '' ) ;
    Value.WriteLn( 'const' ) ;
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
      gTIOPFManager.Read( Visited );

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

procedure TVisCustomQueryFind.Execute(const pVisited: TtiVisited);
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
  // TODO: Yikes! Where did this come from?
//  VisCustomQueryFind.List.SaveToFile('C:\Temp\TEST.TXT');
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


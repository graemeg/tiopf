{$I tiDefines.inc}

unit tiListViewDif;

interface
uses
  tiListView
  ,tiObject
  ,tiSplitter
  ,tiListViewPlus
  ,Classes
  ,ExtCtrls
  ,Buttons
  ,ComCtrls
  ,Controls
  ,Forms
  ,Graphics
  ,Messages
  ;

type

  TtiLVDifPKProp  = class ;
  TtiLVDifPKProps = class ;

  TtiLVDifPKProp = class( TCollectionItem )
  private
    FPropName: string;
  protected
    function    GetDisplayName : string ; override ;
  published
    property    PropName    : string read FPropName    write FPropName ;
  public
    constructor Create( ACollection : TCollection ) ; override ;
    function    Clone : TtiLVDifPKProp ;
  end ;

  TtiLVDifPKProps = class( TCollection )
  private
    FOwner : TComponent ;
    function  GetItem( Index : integer ) : TtiLVDifPKProp ;
    procedure SetItem( Index : integer ; const Value : TtiLVDifPKProp ) ;
  published
  protected
    function  GetOwner : TPersistent ; override ;
  public
    constructor Create( AOwner : TComponent ) ;
    property    Items[Index: integer ] : TtiLVDifPKProp read GetItem write SetItem ;
    function    Add : TtiLVDifPKProp ;
  end ;

  TtiListViewDif = class( TCustomPanel )
  private
    FsbPrevDif      : TSpeedButton ;
    FsbNextDif      : TSpeedButton ;
    FsbEditDifRules : TSpeedButton ;
    FsbCols         : TSpeedButton ;
    FsbFind         : TSpeedButton ;
    FspViewDif      : TtiSplitterPanel ;
    FlvLHS          : TtiListViewPlus ;
    FlvRHS          : TtiListViewPlus ;
    FlvDif          : TtiListView ;
    FSB             : TStatusBar ;
    FtmrShowDif     : TTimer ;
    FDataLHS: TList;
    FDataRHS: TList;
    FDifDataLHS: TList;
    FDifDataRHS: TList;
    FDifData   : TList;
    FPKProperties: TtiLVDifPKProps;
    FOnMoveSplitter: TMoveSplitterEvent;
    procedure SetDataLHS(const Value: TList);
    procedure SetDataRHS(const Value: TList);
    procedure DoLVLHSItemArive(pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
    procedure DoLVLHSScroll(Sender: TObject);
    procedure DoLVRHSItemArive(pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
    procedure DoLVRHSScroll(Sender: TObject);
    function  IsDiference(pLHData, pRHData: TList; pIndex: integer): boolean;
    procedure lvLHSRHSGetFont(pLV: TtiCustomListView; pCanvas: TCanvas; pItem: TListItem; pData: TtiObject);
    procedure DoTMRShowDif(Sender: TObject);
    procedure DoSBEditDifRulesClick(Sender: TObject);
    procedure DoSBNextDifClick(Sender: TObject);
    procedure DoSBPrevDifClick(Sender: TObject);
    procedure DoSBColsClick(Sender: TObject);
    procedure DoSBFindClick(Sender: TObject);
    function  FindNextDifIndex(pDirection: integer) : integer ;
    procedure GoToNextDif(pDirection: integer);
    procedure DoSPViewDifMoveSplitter(pTISplitterPanel: TtiSplitterPanel);
    procedure PopulateDifLists(const pDataGroupLHS, pDataGroupRHS, pLHDifData, pRHDifData: TList; const pPKProps: array of string);
    function  GetRunTimeGenCols: boolean;
    procedure SetRunTimeGenCols(const Value: boolean);
    function  GetOnRuntimeGenCol: TtiRuntimeGenColEvent;
    procedure SetOnRuntimeGenCol(const Value: TtiRuntimeGenColEvent);
    function  GetSplitterPos: integer;
    procedure SetSplitterPos(const Value: integer);
    procedure DoResize(Sender: TObject);
  protected
    procedure Refresh ;
  public
    constructor Create( AOwner : TComponent ) ; override ;
    destructor  Destroy ; override ;
    property    DataLHS : TList read FDataLHS write SetDataLHS ;
    property    DataRHS : TList read FDataRHS write SetDataRHS ;
  published
    property    Anchors ;
    property    Align ;
    property    Visible ;
    property    PKProperties : TtiLVDifPKProps read FPKProperties ;
    property    RunTimeGenCols : boolean
                  read  GetRunTimeGenCols
                  write SetRunTimeGenCols default true ;
    property    OnRuntimeGenCol : TtiRuntimeGenColEvent
                  read  GetOnRuntimeGenCol
                  write SetOnRuntimeGenCol ;
    property    OnMoveSplitter : TMoveSplitterEvent
                  read  FOnMoveSplitter
                  write FOnMoveSplitter ;
    property    SplitterPos : integer
                  read GetSplitterPos
                  write SetSplitterPos ;
  end ;

implementation
uses
  TypInfo
  ,SysUtils
  ,Dialogs
  ;

var
  uPKProps : TtiLVDifPKProps ;

{ TtiListViewDif }

const
  cWidth         = 290 ;
  cLVLeft        = 28 ;
  cLVWidth       = 250 ;

  cSBTop         = 185 ;

constructor TtiListViewDif.Create(AOwner: TComponent);

begin
  inherited Create( AOwner ) ;
  Width  := cWidth ;
  Height := 208 ;

  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
  if ( csDesigning in ComponentState ) then
    ControlStyle   := ControlStyle - [csAcceptsControls] ;

  ControlStyle   := ControlStyle - [csSetCaption] ;

  BevelInner     := bvNone ;
  BevelOuter     := bvNone ;
  BorderStyle    := bsNone ;

  FsbPrevDif := TSpeedButton.Create( self ) ;
  FsbPrevDif.Parent := self ;
  FsbPrevDif.Left := 0 ;
  FsbPrevDif.Top := 8 ;
  FsbPrevDif.Width := 23 ;
  FsbPrevDif.Height := 22 ;
  FsbPrevDif.Hint := 'Previous difference' ;
  FsbPrevDif.Flat := True ;
  FsbPrevDif.ParentShowHint := False ;
  FsbPrevDif.ShowHint := True ;
  {$IFNDEF FPC}
  FsbPrevDif.Glyph.LoadFromResourceName( HInstance, 'TILVDIFARROWUP' ) ;
  {$ELSE}
  FsbPrevDif.Glyph.LoadFromLazarusResource('TILVDIFARROWUP' ) ;
  {$ENDIF}
  FsbPrevDif.OnClick := DoSBPrevDifClick ;

  FsbNextDif := TSpeedButton.Create( self ) ;
  FsbNextDif.Parent := self ;
  FsbNextDif.Left := 0 ;
  FsbNextDif.Top := 32 ;
  FsbNextDif.Width := 23 ;
  FsbNextDif.Height := 22 ;
  FsbNextDif.Hint := 'Next difference' ;
  FsbNextDif.Flat := True ;
  FsbNextDif.ParentShowHint := False ;
  FsbNextDif.ShowHint := True ;
  {$IFNDEF FPC}
  FsbNextDif.Glyph.LoadFromResourceName( HInstance, 'TILVDIFARROWDOWN' ) ;
  {$ELSE}
  FsbNextDif.Glyph.LoadFromLazarusResource('TILVDIFARROWDOWN' ) ;
  {$ENDIF}
  FsbNextDif.OnClick := DoSBNextDifClick ;

  FsbEditDifRules := TSpeedButton.Create( self ) ;
  FsbEditDifRules.Parent := self ;
  FsbEditDifRules.Left := 0 ;
  FsbEditDifRules.Top := 56 ;
  FsbEditDifRules.Width := 23 ;
  FsbEditDifRules.Height := 22 ;
  FsbEditDifRules.Hint := 'Edit difference rules' ;
  FsbEditDifRules.Flat := True ;
  FsbEditDifRules.ParentShowHint := False ;
  FsbEditDifRules.ShowHint := True ;
  {$IFNDEF FPC}
  FsbEditDifRules.Glyph.LoadFromResourceName( HInstance, 'TILVDIFEDITDIFRULES' ) ;
  {$ELSE}
  FsbEditDifRules.Glyph.LoadFromLazarusResource('TILVDIFEDITDIFRULES' ) ;
  {$ENDIF}
  FsbEditDifRules.OnClick := DoSBEditDifRulesClick ;

  FsbCols := TSpeedButton.Create( self ) ;
  FsbCols.Parent := self ;
  FsbCols.Left := 0 ;
  FsbCols.Top := 80 ;
  FsbCols.Width := 23 ;
  FsbCols.Height := 22 ;
  FsbCols.Hint := 'Select columns' ;
  FsbCols.Flat := True ;
  FsbCols.ParentShowHint := False ;
  FsbCols.ShowHint := True ;
  {$IFNDEF FPC}
  FsbCols.Glyph.LoadFromResourceName( HInstance, 'TILVDIFCOLS' ) ;
  {$ELSE}
  FsbCols.Glyph.LoadFromLazarusResource('TILVDIFCOLS' ) ;
  {$ENDIF}
  FsbCols.OnClick := DoSBColsClick ;

  FsbFind := TSpeedButton.Create( self ) ;
  FsbFind.Parent := self ;
  FsbFind.Left := 0 ;
  FsbFind.Top := 104 ;
  FsbFind.Width := 23 ;
  FsbFind.Height := 22 ;
  FsbFind.Hint := 'Find a row' ;
  FsbFind.Flat := True ;
  FsbFind.ParentShowHint := False ;
  FsbFind.ShowHint := True ;
  {$IFNDEF FPC}
  FsbFind.Glyph.LoadFromResourceName( HInstance, 'TILVDIFFIND' ) ;
  {$ELSE}
  FsbFind.Glyph.LoadFromLazarusResource('TILVDIFFIND' ) ;
  {$ENDIF}
  FsbFind.OnClick := DoSBFindClick ;

  FspViewDif := TtiSplitterPanel.Create( self ) ;
  FspViewDif.Parent := self ;
  FspViewDif.Left := cLVLeft ;
  FspViewDif.Top := 8 ;
  FspViewDif.Width := cLVWidth ;
  FspViewDif.Height := 97 ;
  FspViewDif.Aligned := alNone ;
//  FspViewDif.Anchors := [akLeft, akTop, akRight, akBottom] ;
  FspViewDif.PanelStyle := spsNone ;
  FspViewDif.KeepSplitterPosPercent := True ;
  FspViewDif.OnMoveSplitter := DoSPViewDifMoveSplitter ;

  FlvLHS := TtiListViewPlus.Create( Self ) ;
  FlvLHS.Parent := FspViewDif.Panel1 ;
  FlvLHS.Align := alClient ;
  FlvLHS.MultiSelect := False ;
  FlvLHS.RowSelect := True ;
  FlvLHS.OnGetFont := lvLHSRHSGetFont ;
  FlvLHS.SortOnHeadingClick := False ;
  FlvLHS.ConfigFeatures := [ lvcfCols, lvcfFind, lvcfExport ] ;
  FlvLHS.ShowFocusRect := false ;

  FlvRHS := TtiListViewPlus.Create( Self ) ;
  FlvRHS.Parent := FspViewDif.Panel2 ;
  FlvRHS.Align := alClient ;
  FlvRHS.RowSelect := True ;
  FlvRHS.OnGetFont := lvLHSRHSGetFont ;
  FlvRHS.SortOnHeadingClick := False ;
  FlvRHS.ConfigFeatures := [ lvcfCols, lvcfFind, lvcfExport ] ;
  FlvRHS.ShowFocusRect := false ;

  FlvDif := TtiListView.Create( self ) ;
  FlvDif.Parent := self ;
  FlvDif.Left := cLVLeft ;
  FlvDif.Top := 109 ;
  FlvDif.Width := cLVWidth ;
  FlvDif.Height := 72 ;
  FlvDif.Anchors := [akLeft, akRight, akBottom] ;

  FlvDif.MultiSelect := False ;
  FlvDif.RowSelect := True ;
  FlvDif.SortOnHeadingClick := False ;
  FlvDif.ShowFocusRect := false ;

  FSB := TStatusBar.Create( Self ) ;
  FSB.Parent := self ;
  FSB.Left := cLVLeft ;
  FSB.Top := cSBTop ;
  FSB.Width := cLVWidth + 2 ;
  FSB.Height := 19 ;
  FSB.Align := alNone ;
  FSB.Anchors := [akLeft, akRight, akBottom]  ;
  FSB.SimplePanel := False ;
  FSB.Panels.Add.Width := cLVWidth div 2 ;
  FSB.Panels.Add ;

  FDifDataLHS := TList.Create ;
  FDifDataRHS := TList.Create ;
  FDifData    := TList.Create ;

  FlvLHS.OnItemArive := DoLVLHSItemArive ;
  FlvLHS.OnVScroll   := DoLVLHSScroll ;

  FlvRHS.OnItemArive := DoLVRHSItemArive ;
  FlvRHS.OnVScroll   := DoLVRHSScroll ;

  FtmrShowDif     := TTimer.Create( Self ) ;
  FtmrShowDif.Enabled := false ;
  FtmrShowDif.Interval := 500 ;
  FtmrShowDif.OnTimer  := DoTMRShowDif ;

  FPKProperties := TtiLVDifPKProps.Create( Self ) ;
  OnResize := DoResize ;
  
end;

destructor TtiListViewDif.Destroy;
begin
  FDifDataLHS.Free ;
  FDifDataRHS.Free ;
  FDifData.Free ;
  inherited ;
end;

procedure TtiListViewDif.DoLVLHSItemArive(pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
begin
  FtmrShowDif.Enabled := false ;
  FlvRHS.SynchroniseWith( FlvLHS ) ;
  FtmrShowDif.Enabled := true ;
end;

procedure TtiListViewDif.DoLVLHSScroll(Sender: TObject);
begin
  FlvRHS.SynchroniseWith( FlvLHS ) ;
end;

procedure TtiListViewDif.DoLVRHSItemArive(pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
begin
  FtmrShowDif.Enabled := false ;
  FlvLHS.SynchroniseWith( FlvRHS ) ;
  FtmrShowDif.Enabled := true ;
end;

procedure TtiListViewDif.DoLVRHSScroll(Sender: TObject);
begin
  FlvLHS.SynchroniseWith( FlvRHS ) ;
end;

procedure TtiListViewDif.Refresh;
begin

  if ( FDataLHS = nil ) or
     ( FDataRHS = nil ) then
    Exit ; //==>

  FlvLHS.Data := nil ;
  FlvRHS.Data := nil ;

  PopulateDifLists(
                    FDataLHS,
                    FDataRHS,
                    FDifDataLHS,
                    FDifDataRHS,
                    ['OID'] ) ;

  FlvLHS.Data := FDifDataLHS ;
  FlvRHS.Data := FDifDataRHS ;

  FSB.Panels[0].Text := 'Count: ' + IntToStr( FDataLHS.Count ) ;
  FSB.Panels[1].Text := 'Count: ' + IntToStr( FDataRHS.Count ) ;
  
end;

procedure TtiListViewDif.SetDataLHS(const Value: TList);
begin
  FDataLHS := Value;
  Refresh ;
end;

procedure TtiListViewDif.SetDataRHS(const Value: TList);
begin
  FDataRHS := Value;
  Refresh ;
end;

procedure TtiListViewDif.lvLHSRHSGetFont(pLV: TtiCustomListView;
  pCanvas: TCanvas; pItem: TListItem; pData: TtiObject);
begin
  if IsDiference( FDifDataLHS, FDifDataRHS, pItem.Index ) then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack
end;

function TtiListViewDif.IsDiference( pLHData, pRHData : TList ; pIndex : integer ) : boolean ;
begin
  // Require better dif evaluation here
  result := ( pLHData.Items[pIndex] = nil ) or
            ( pRHData.Items[pIndex] = nil ) ;
end ;

procedure TtiListViewDif.DoTMRShowDif( Sender : TObject ) ;
var
  lIndex : integer ;
begin
  FtmrShowDif.Enabled := false ;

  FlvDif.Visible := false ;
  FlvDif.Data := nil ;
  FDifData.Clear ;
  FDifData.Add( FlvLHS.SelectedData ) ;
  FDifData.Add( FlvRHS.SelectedData ) ;
  FlvDif.Data := FDifData ;
  FlvDif.Columns.Assign( FlvLHS.Columns ) ;
  FlvDif.Visible := true ;

  lIndex := FindNextDifIndex( 1 ) ;
  FsbNextDif.Enabled :=
    ( lIndex < FDifDataLHS.Count ) and
    ( lIndex < FDifDataRHS.Count ) ;
  FsbPrevDif.Enabled := FindNextDifIndex( -1 ) <> -1 ;

end ;

procedure TtiListViewDif.DoSBPrevDifClick(Sender: TObject);
begin
  GoToNextDif( -1 ) ;
end ;

procedure TtiListViewDif.DoSBNextDifClick(Sender: TObject);
begin
  GoToNextDif( 1 ) ;
end;

procedure TtiListViewDif.DoSBEditDifRulesClick(Sender: TObject);
begin
  Assert( false, 'Under construction' ) ;
end ;

function TtiListViewDif.FindNextDifIndex(pDirection: integer) : integer ;
var
  lSelected : TListItem ;

begin

  result := -1 ;
  lSelected := FlvLHS.Selected ;
  if lSelected.Data <> nil then
    result := FDifDataLHS.IndexOf( lSelected.Data ) ;

  if result = -1 then
  begin
    lSelected := FlvRHS.Selected ;
    result := FDifDataRHS.IndexOf( lSelected.Data ) ;
    if result = -1 then
      Exit ; //==>
  end ;

  result := result+pDirection ;

  while ( result < FDifDataLHS.Count ) and
        ( result > -1 ) do
  begin
    // Replace this with a call to IsDif( )
    if ( FDifDataLHS.Items[result] = nil ) or
       ( FDifDataRHS.Items[result] = nil ) then
    begin
      Break ; //==>>
    end ;
    Inc( result, pDirection ) ;
  end ;

end ;

procedure TtiListViewDif.GoToNextDif(pDirection: integer) ;
var
  lIndex : integer ;
begin
  lIndex := FindNextDifIndex( pDirection ) ;
  if lIndex = -1 then
    Exit ; //==>
  FlvLHS.OnVScroll   := nil ;
  FlvRHS.OnVScroll   := nil ;
  FlvLHS.OnItemArive := nil ;
  FlvRHS.OnItemArive := nil ;
  FlvLHS.PositionCursor(lIndex);
  FlvRHS.PositionCursor(lIndex);
  FlvLHS.OnVScroll   := DolvLHSScroll ;
  FlvRHS.OnVScroll   := DolvRHSScroll ;
  FlvLHS.OnItemArive := DolvLHSItemArive ;
  FlvRHS.OnItemArive := DolvRHSItemArive ;
  FtmrShowDif.Enabled := true ;
end;

procedure TtiListViewDif.DoSPViewDifMoveSplitter( pTISplitterPanel: TtiSplitterPanel);
begin
  DoResize(Self);
  FSB.Panels[0].Width := pTISplitterPanel.SplitterPos ;
  if Assigned( FOnMoveSplitter ) then
    FOnMoveSplitter( FspViewDif ) ;
end;

{
function _Compare( pItem1   : TtiObject ;
                   pItem2   : TtiObject ;
                   pPKProps : array of string ) : integer ;
var
  i : integer ;
  lsPropName : string ;
  lValue1 : variant ;
  lValue2 : variant ;
begin

  result := 0 ;
  try
    for i := Low( pPKProps ) to High( pPKProps ) do
    begin
      lsPropName := pPKProps[i] ;
      lValue1 := GetPropValue( pItem1, lsPropName ) ;
      lValue2 := GetPropValue( pItem2, lsPropName ) ;

      if lValue1 < lValue2 then
        result := -1
      else if lValue1 > lValue2 then
        result := 1 ;
      if result <> 0 then
        Break ; //==>
    end ;

  except
    on e:exception do
      ShowMessage( 'Error in tiListViewDif._Compare. PropName <' +
                   lsPropName + '>' + e.Message ) ;
  end ;

end ;

function _DoSort( pItem1, pItem2 : pointer ) : integer ;
begin
  result := _Compare( TtiObject( pItem1 ),
                      TtiObject( pItem2 ),
                      uPKProps ) ;
end ;

}


function _DoCompare( pItem1   : Pointer ;
                     pItem2   : Pointer ) : integer ;
var
  i : integer ;
  lsPropName : string ;
  lValue1 : variant ;
  lValue2 : variant ;
begin

  result := 0 ;
  try
    for i := 0 to uPKProps.Count - 1 do
    begin
      lsPropName := uPKProps.Items[i].PropName ;
      if not ( IsPublishedProp( TtiObject( pItem1 ), lsPropName )) then
        raise exception.Create( '<' + lsPropName + '> is not a published property on <' +
                                TtiObject( pItem1 ).ClassName + '>' ) ;
      lValue1 := GetPropValue( TtiObject( pItem1 ), lsPropName ) ;
      lValue2 := GetPropValue( TtiObject( pItem2 ), lsPropName ) ;

      if lValue1 < lValue2 then
        result := -1
      else if lValue1 > lValue2 then
        result := 1 ;
      if result <> 0 then
        Break ; //==>
    end ;

  except
    on e:exception do
      raise exception.Create(
        'Error in tiListViewDif._Compare. PropName <' +
        lsPropName + '>' + e.Message ) ;
  end ;

end ;



// Not thread safe :(

procedure TtiListViewDif.PopulateDifLists( const pDataGroupLHS : TList ;
                            const pDataGroupRHS : TList ;
                            const pLHDifData    : TList ;
                            const pRHDifData    : TList ;
                            const pPKProps : array of string
                           ) ;

  procedure _DataToSortedList( pListFrom, pListTo : TList ) ;
  var
    i : integer ;
  begin
    pListTo.Clear ;
    for i := 0 to pListFrom.Count - 1 do
      pListTo.Add( pListFrom.Items[i] ) ;
    pListTo.Sort( _DoCompare ) ;
  end;

var
  lLHIndex : integer ;
  lRHIndex : integer ;
  lRHRow   : TtiObject ;
  lLHRow   : TtiObject ;
  lLHSorted : TList ;
  lRHSorted : TList ;
  lCompareResult : integer ;
begin

  pLHDifData.Clear ;
  pRHDifData.Clear ;

  lLHSorted := TList.Create ;
  lRHSorted := TList.Create ;

  uPKProps := FPKProperties ;
  _DataToSortedList( pDataGroupLHS, lLHSorted ) ;
  _DataToSortedList( pDataGroupRHS, lRHSorted ) ;

  try
    lLHIndex := 0 ;
    lRHIndex := 0 ;

    while true do
    begin

      if lLHIndex < lLHSorted.Count then
        lLHRow := TtiObject( lLHSorted.Items[lLHIndex] )
      else
        lLHRow := nil ;

      if lRHIndex < lRHSorted.Count then
        lRHRow := TtiObject( lRHSorted.Items[lRHIndex] )
      else
        lRHRow := nil;

      // * * * Loop exits here * * *
      if ( lLHRow = nil ) and ( lRHRow = nil ) then
        Exit ; //==>

      if ( lLHRow <> nil ) and
         ( lRHRow <> nil ) then
        lCompareResult := _DoCompare( lLHRow, lRHRow )
      else
        lCompareResult := -2 ;

      // The primary keys (OID or element_id) match.
      if ( lLHRow <> nil ) and
         ( lRHRow <> nil ) and
         ( lCompareResult = 0 ) then
      begin
        pLHDifData.Add( lLHRow ) ;
        pRHDifData.Add( lRHRow ) ;
        inc( lLHIndex ) ;
        inc( lRHIndex ) ;
      end

      // A row exists in the LHS, but not the RHS
      else if ( lRHRow = nil ) or
              (( lLHRow <> nil ) and ( lRHRow <> nil ) and
               ( lCompareResult  = -1 )) then
      begin
        pLHDifData.Add( lLHRow ) ;
        pRHDifData.Add( nil ) ;
        inc( lLHIndex ) ;
      end

      // A row exists in the RHS, but not in the LHS
      else if (  lLHRow = nil ) or
              (( lLHRow <> nil ) and ( lRHRow <> nil ) and
               ( lCompareResult = 1 )) then
      begin
        pLHDifData.Add( nil ) ;
        pRHDifData.Add( lRHRow ) ;
        inc( lRHIndex ) ;
      end ;

    end ;
  finally
    lLHSorted.Free ;
    lRHSorted.Free ;
  end ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVDifPKProps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiLVDifPKProps.Add: TtiLVDifPKProp;
begin
  result := TtiLVDifPKProp( inherited add ) ;
end;

constructor TtiLVDifPKProps.Create(AOwner: TComponent);
begin
  inherited Create( TtiLVDifPKProp ) ;
  FOwner := AOwner ;
end;

function TtiLVDifPKProps.GetItem(Index: integer): TtiLVDifPKProp;
begin
  result := TtiLVDifPKProp( inherited GetItem( Index )) ;
end;

function TtiLVDifPKProps.GetOwner: TPersistent;
begin
  result := FOwner ;
end;

procedure TtiLVDifPKProps.SetItem(Index: integer; const Value: TtiLVDifPKProp);
begin
  inherited SetItem( Index, Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiLVDifPKProp
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiLVDifPKProp.Clone: TtiLVDifPKProp;
begin
  result                := TtiLVDifPKProp.Create( nil ) ;
  result.PropName      := Self.PropName ;
end;

constructor TtiLVDifPKProp.Create(ACollection: TCollection);
begin
  inherited Create( ACollection ) ;
  FPropName := 'EnterValue' ;
end;

function TtiLVDifPKProp.GetDisplayName: string;
begin
  result := FPropName ;
end;

function TtiListViewDif.GetRunTimeGenCols: boolean;
begin
  result := FlvLHS.RuntimeGenCols ;
end;

procedure TtiListViewDif.SetRunTimeGenCols(const Value: boolean);
begin
  FlvLHS.RuntimeGenCols := Value ;
  FlvRHS.RuntimeGenCols := Value ;
  FlvDif.RuntimeGenCols := Value ;
end;

function TtiListViewDif.GetOnRuntimeGenCol: TtiRuntimeGenColEvent;
begin
  result := FlvLHS.OnRuntimeGenCol ;
end;

procedure TtiListViewDif.SetOnRuntimeGenCol( const Value: TtiRuntimeGenColEvent);
begin
  FlvLHS.OnRuntimeGenCol := Value ;
  FlvRHS.OnRuntimeGenCol := Value ;
  FlvDif.OnRuntimeGenCol := Value ;
end;

function TtiListViewDif.GetSplitterPos: integer;
begin
  result := FspViewDif.SplitterPos ;
end;

procedure TtiListViewDif.SetSplitterPos(const Value: integer);
begin
  FspViewDif.SplitterPos := Value ;
end;

procedure TtiListViewDif.DoSBColsClick(Sender: TObject);
begin
  FlvLHS.DoColumns ;
  FlvRHS.Columns.Assign( FlvLHS.Columns ) ;
  FlvDif.Columns.Assign( FlvLHS.Columns ) ;
end;

procedure TtiListViewDif.DoSBFindClick(Sender: TObject);
begin
  if FlvLHS.Focused then
    FlvLHS.DoFind
  else
    FlvRHS.DoFind ;
end;

procedure TtiListViewDif.DoResize(Sender: TObject);
var
  lWidth : integer ;
begin
  lWidth := ClientWidth - cLVLeft - 8 ;
  FspViewDif.Width := lWidth ;
  FlvDif.Width := lWidth ;
  FlvDif.Top := ClientHeight - FlvDif.Height - 2 ;
  FSB.Top := FlvDif.Top - 2 - FSB.Height ;
  FSB.Width := lWidth ;
  FspViewDif.Height := FSB.Top - 4 - FspViewDif.Top ;
end;

end.



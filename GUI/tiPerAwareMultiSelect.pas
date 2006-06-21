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

  Change history:
    June 1998, PWH, Created
    July 2001, PHW, Changed to be TtiListView based

  Purpose:
    Component for drag-and-drop selection of 0-n items from a list.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerAwareMultiSelect ;

interface
uses
  Windows
  ,SysUtils
  ,Classes
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,tiSpeedButton
  ,extCtrls
  ,comctrls
  ,Messages
  ,tiListView
  ,ImgList
  ,tiObject
  ;

type

  TtiMultiSelectListView = class( TtiCustomListView )
  public
    property MultiSelect;
  end;

  // TtiPerAwareMultiSelect
  TtiPerAwareMultiSelect = class( TCustomGroupBox )
  private
    FlblAvailable     : TLabel ;
    FlblSelected      : TLabel ;
    FlvAvailable      : TtiMultiSelectListView ;
    FlvSelected       : TtiMultiSelectListView ;
    FsbSelectMarked   : TtiSpeedButton ;
    FsbDeSelectMarked : TtiSpeedButton ;
    FsbSelectAll      : TtiSpeedButton ;
    FsbDeSelectAll    : TtiSpeedButton ;

    FAvailable : TtiObjectList ;
    FSelected  : TtiObjectList ;

    FbBorder: boolean;
    FOnChange: TNotifyEvent;

    FSelectedIndex : integer ;
    FAvailableIndex : integer ;
//    FListColumns : TtiListColumns ;
    FApplyFilter: Boolean;
    FOnFilterData: TtiLVOnFilterDataEvent;

    FTimerSetData : TTimer ;

    procedure WMSize( var Message: TWMSize ) ; message WM_SIZE ;
    procedure SelectOne( pData : TtiObject ) ;
    procedure DeSelectOne( pData : TtiObject ) ;

    procedure DosbSelectMarkedClick( sender: TObject ) ;
    procedure DosbDeSelectMarkedClick( sender: TObject ) ;

    procedure DosbSelectAllClick( sender: TObject ) ;
    procedure DosbDeSelectAllClick( sender: TObject ) ;

    procedure DoLVAvailableKeyPress( Sender: TObject; var Key: Char ) ;
    procedure DoLVSelectedKeyPress( Sender: TObject; var Key: Char ) ;

    procedure DoLVAvailableDblClick( pLV: TtiCustomListView;
                                     pData: TtiObject;
                                     pItem: TListItem);

    procedure DoLVSelectedDblClick(  pLV: TtiCustomListView;
                                     pData: TtiObject;
                                     pItem: TListItem);
    procedure DoLVAvailableFilterData( pData: TtiObject;
                                       var pbInclude: Boolean);

    procedure SetBorder(const Value: boolean);
    procedure SetAvailable( Value: TtiObjectList);
    procedure SetSelected( Value: TtiObjectList);
    function  GetCaptionAvailable: string;
    function  GetCaptionSelected: string;
    procedure SetCaptionAvailable(const Value: string);
    procedure SetCaptionSelected(const Value: string);
    procedure BeginUpdate ;
    procedure EndUpdate ;
    procedure DoLVAvailableCanAcceptDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
      pDataTarget: TtiObject; var pbConfirm: Boolean);
    procedure DoLVAvailableDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
      pDataTarget: TtiObject);
    procedure DoLVSelectedCanAcceptDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
      pDataTarget: TtiObject; var pbConfirm: Boolean);
    procedure DoLVSelectedDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
      pDataTarget: TtiObject);
    procedure DoOnChange ;
    function  GetRunTimeSelectedCols: boolean;
    procedure SetRunTimeSelectedCols(const Value: boolean);
    procedure DoOnTimerSetData(Sender: TObject);
    function  GetImages: TCustomImageList;
    procedure SetImages(const pImages: TCustomImageList);
    function  GetOnGetImageIndex: TtiLVGetImageIndexEvent;
    procedure SetOnGetImageIndex(const Value: TtiLVGetImageIndexEvent);
  protected
    procedure Paint ; override ;
  published
    property    Align ;
    property    Caption ;
    property    Anchors ;
    property    ShowHint ;
    property    Border : boolean read FbBorder write SetBorder ;
    property    OnChange : TNotifyEvent read FOnChange write FOnChange ;
    property    CaptionAvailable : string read GetCaptionAvailable write SetCaptionAvailable ;
    property    CaptionSelected  : string read GetCaptionSelected  write SetCaptionSelected ;
//    property    ListColumns : TtiListColumns
//                  read FListColumns
//                  write FListColumns ;
    property    RunTimeSelectedCols : boolean
                    read GetRunTimeSelectedCols
                    write SetRunTimeSelectedCols ;
    property    Visible ;
    property    Images: TCustomImageList read GetImages Write SetImages;
    property    OnGetImageIndex : TtiLVGetImageIndexEvent read GetOnGetImageIndex Write SetOnGetImageIndex ;
    property    ApplyFilter : boolean read FApplyFilter write FApplyFilter ;
    property    OnFilterData : TtiLVOnFilterDataEvent read  FOnFilterData write FOnFilterData ;

  public
    constructor Create( Owner : TComponent ) ; override ;
    destructor  Destroy ; override ;
    procedure   DoSelectAll;
    procedure   DoDeSelectAll;
    property    Available : TtiObjectList read FAvailable write SetAvailable ;
    property    Selected  : TtiObjectList read FSelected  write SetSelected  ;
    procedure   RefreshData ;
    procedure   AddColumn( const pFieldName : string ;
                           const pDataType  : TlvTypeKind ;
                           const pDisplayLabel : string = '' ;
                           pColWidth : integer = -1 );
  end;

implementation
uses
  tiResources
  ,tiConstants
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareMultiSelect
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPerAwareMultiSelect.Create( Owner : TComponent ) ;
begin
  FbBorder          := true ;

  inherited Create( Owner ) ;

  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
  if ( csDesigning in ComponentState ) then
    ControlStyle   := ControlStyle - [csAcceptsControls] ;

  FlblAvailable     := TLabel.Create( self ) ;
  FlblSelected      := TLabel.Create( self ) ;
  FlvAvailable      := TtiMultiSelectListView.Create( self ) ;
  FlvSelected       := TtiMultiSelectListView.Create( self ) ;
  FsbSelectMarked   := TtiSpeedButton.Create( self ) ;
  FsbDeSelectMarked := TtiSpeedButton.Create( self ) ;
  FsbSelectAll      := TtiSpeedButton.Create( self ) ;
  FsbDeSelectAll    := TtiSpeedButton.Create( self ) ;

  self.height      := 200 ;
  self.width       := 200 ;
  self.Constraints.MinHeight := 157 ;
  self.Constraints.MinWidth  := 149 ;

  with FlblAvailable do begin
    parent := self ;
    top    :=  16 ;
    left   :=   8 ;
    caption := '&Available' ;
    FocusControl := FlvAvailable ;
  end ;

  with FlblSelected do begin
    parent := self ;
    top    :=  16 ;
    left   := 272 ;
    caption := '&Selected' ;
    FocusControl := FlvSelected ;
  end ;

  with FlvAvailable do begin
    parent := self ;
    top    :=  40 ;
    left   :=   8 ;
    width  := 193 ;
    height := 201 ;
    multiSelect := true ;
    onDblClick  := DoLVAvailableDblClick ;
    onKeyPress  := DoLVAvailableKeyPress ;
    OnCanAcceptDrop := DoLVAvailableCanAcceptDrop ;
    OnDrop       := DoLVAvailableDrop ;
    CanStartDrag := true ;
    name         := 'lbAvailable' ;
    OnFilterData := DoLVAvailableFilterData ;
    ApplyFilter  := true ;
  end ;

  with FlvSelected do begin
    parent := self ;
    top    :=  40 ;
    left   := 272 ;
    width  := 193 ;
    height := 201 ;
    multiSelect := true ;
    onDblClick  := DoLVSelectedDblClick ;
    onKeyPress  := DoLVSelectedKeyPress ;
    OnCanAcceptDrop := DoLVSelectedCanAcceptDrop ;
    OnDrop          := DoLVSelectedDrop ;
    CanStartDrag := true ;
    name        := 'lbSelected' ;
  end ;

  with FsbSelectMarked do begin
    parent := self ;
    top    :=  56 ;
    left   := 224 ;
    width  :=  25 ;
    height :=  25 ;
    hint   := 'Select marked' ;
    showHint := true ;
    Glyph.LoadFromResourceName(         HInstance, cResTI_Copy1Right + cResTI_16N ) ;
    GlyphHot.LoadFromResourceName(      HInstance, cResTI_Copy1Right + cResTI_16H ) ;
    GlyphDisabled.LoadFromResourceName( HInstance, cResTI_Copy1Right + cResTI_16D ) ;
    onClick := DosbSelectMarkedClick ;
    flat := true ;
  end ;

  with FsbDeSelectMarked do begin
    parent := self ;
    top    :=  88 ;
    left   := 224 ;
    width  :=  25 ;
    height :=  25 ;
    hint   := 'Deselect marked' ;
    showHint := true ;
    Glyph.LoadFromResourceName(         HInstance, cResTI_Copy1Left + cResTI_16N ) ;
    GlyphHot.LoadFromResourceName(      HInstance, cResTI_Copy1Left + cResTI_16H ) ;
    GlyphDisabled.LoadFromResourceName( HInstance, cResTI_Copy1Left + cResTI_16D ) ;
    onClick := DosbDeSelectMarkedClick ;
    flat := true ;
  end ;

  with FsbSelectAll do begin
    parent := self ;
    top    := 152 ;
    left   := 224 ;
    width  :=  25 ;
    height :=  25 ;
    hint   := 'Select all' ;
    Glyph.LoadFromResourceName(         HInstance, cResTI_CopyAllRight + cResTI_16N ) ;
    GlyphHot.LoadFromResourceName(      HInstance, cResTI_CopyAllRight + cResTI_16H ) ;
    GlyphDisabled.LoadFromResourceName( HInstance, cResTI_CopyAllRight + cResTI_16D ) ;
    onClick := DosbSelectAllClick ;
    flat := true ;
  end ;

  with FsbDeSelectAll do begin
    parent := self ;
    top    := 184 ;
    left   := 224 ;
    width  :=  25 ;
    height :=  25 ;
    hint   := 'Deselect all' ;
    Glyph.LoadFromResourceName(         HInstance, cResTI_CopyAllLeft + cResTI_16N ) ;
    GlyphHot.LoadFromResourceName(      HInstance, cResTI_CopyAllLeft + cResTI_16H ) ;
    GlyphDisabled.LoadFromResourceName( HInstance, cResTI_CopyAllLeft + cResTI_16D ) ;
    onClick := DosbDeSelectAllClick ;
    flat := true ;
  end ;

//  FListColumns := TtiListColumns.Create( Self ) ;

  FTimerSetData := TTimer.Create( nil ) ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Interval := 15 ;
  FTimerSetData.OnTimer := DoOnTimerSetData ;

  Application.processMessages ;
end ;

destructor TtiPerAwareMultiSelect.destroy ;
begin
  FAvailable := nil ;
  FSelected  := nil ;
//  FListColumns.Free ;
  FTimerSetData.Free ;
  inherited destroy ;
end ;

procedure TtiPerAwareMultiSelect.WMSize(var Message: TWMSize);
const
  ciBorder = 8 ;
  ciSBHeightSmall = 19 ;
var
  liLBHeight : integer ;
  liTop      : integer ;
  liSBHeight : integer ;
  liSBLeft   : integer ;
begin
  Inherited ;

  // Set the height of the listBoxes
  liLBHeight          := ClientHeight - FlvAvailable.Top - ciBorder ;
  FlvAvailable.Height := liLBHeight ;
  FlvSelected.Height  := liLBHeight ;

  // Set the size of the speed buttons
  liTop                 := FlvAvailable.Top ;
  liSBHeight := ciSBHeightSmall ;

  FsbSelectMarked.Height   := liSBHeight ;
  FsbDeSelectMarked.Height := liSBHeight ;
  FsbSelectAll.Height      := liSBHeight ;
  FsbDeSelectAll.Height    := liSBHeight ;

  FsbSelectMarked.Width    := liSBHeight ;
  FsbDeSelectMarked.Width  := liSBHeight ;
  FsbSelectAll.Width       := liSBHeight ;
  FsbDeSelectAll.Width     := liSBHeight ;

  FlvAvailable.Width := ( ClientWidth - ( ciBorder * 4 ) - liSBHeight ) div 2 ;
  FlvSelected.Width  := FlvAvailable.Width ;
  FlvAvailable.Left := ciBorder ;
  FlvSelected.Left := ClientWidth - ciBorder - FlvSelected.Width ;
  FlblSelected.Left := FlvSelected.Left ;

  liSBLeft               := ( FlvAvailable.Left + FlvAvailable.Width ) +
                            ( FlvSelected.Left -
                              ( FlvAvailable.Left + FlvAvailable.Width ) - liSBHeight ) div 2;

  FsbSelectMarked.Left   := liSBLeft ;
  FsbDeSelectMarked.Left := liSBLeft ;
  FsbSelectAll.Left      := liSBLeft ;
  FsbDeSelectAll.Left    := liSBLeft ;

  FsbSelectMarked.Top   := liTop + ( liLBHeight div 4 ) - liSBHeight - ( ciBorder div 2 );
  FsbDeSelectMarked.Top := liTop + ( liLBHeight div 4 ) + ( ciBorder div 2 );
  FsbSelectAll.Top      := liTop + ( liLBHeight div 4 * 3 ) - liSBHeight - ( ciBorder div 2 );
  FsbDeSelectAll.Top    := liTop + ( liLBHeight div 4 * 3 ) + ( ciBorder div 2 );

  // The LH list view is not re-painting when the width or caption are changed. Why?
  FlvAvailable.LV.Invalidate;

end;

procedure TtiPerAwareMultiSelect.DosbSelectMarkedClick( sender: TObject ) ;
var
  i : integer ;
  lList : TList ;
begin
  if ( FlvAvailable = nil )or
     ( FlvSelected  = nil ) then
    Exit ; //==>

  BeginUpdate ;
  try
    lList := FLVAvailable.SelectedDataList ;
    for i := 0 to lList.Count - 1 do
      FSelected.Add( TtiObject( lList.Items[i] )) ;
    if lList.Count > 0 then
    begin
      RefreshData ;
      DoOnChange ;
    end ;
  finally
    EndUpdate ;
  end ;
end;

procedure TtiPerAwareMultiSelect.DosbDeSelectMarkedClick( sender: TObject ) ;
var
  i : integer ;
  lList : TList ;
begin
  if ( FlvAvailable = nil )or
     ( FlvSelected  = nil ) then
    Exit ; //==>

  BeginUpdate ;
  try
    lList := FLVSelected.SelectedDataList ;
    for i := 0 to lList.Count - 1 do
      FSelected.Remove( TtiObject( lList.Items[i] )) ;
    if lList.Count > 0 then
    begin
      RefreshData ;
      DoOnChange ;
    end ;
  finally
    EndUpdate ;
  end ;
end;

procedure TtiPerAwareMultiSelect.DosbSelectAllClick(Sender: TObject);
begin
  DoSelectAll ;
end ;

procedure TtiPerAwareMultiSelect.DosbDeSelectAllClick(Sender: TObject);
begin
  DoDeSelectAll ;
end;

procedure TtiPerAwareMultiSelect.DoSelectAll ;
var
  i : integer ;
  lbChange : boolean ;
begin
  BeginUpdate ;
  try
    FSelected.Clear ;
    lbChange := ( FSelected.Count < FAvailable.Count ) ;
    for i := 0 to FAvailable.Count - 1 do
      if not FAvailable.Items[i].Deleted then
        FSelected.Add( FAvailable.Items[i] ) ;
  finally
    EndUpdate ;
  end ;
  if lbChange then
    DoOnChange ;
end;

procedure TtiPerAwareMultiSelect.DoDeSelectAll ;
var
  lbChange : boolean ;
begin
  BeginUpdate ;
  try
    lbChange := ( FSelected.Count > 0 ) and ( FAvailable.Count > 0 ) ;
    FSelected.Clear ;
  finally
    EndUpdate ;
  end ;
  if lbChange then
    DoOnChange ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = ' ' ) and
     ( FLVAvailable.SelectedData <> nil ) then
    SelectOne( FLVAvailable.SelectedData as TtiObject ) ;
end;

procedure TtiPerAwareMultiSelect.DoLVSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = ' ' ) and
     ( FLVSelected.SelectedData <> nil ) then
    DeSelectOne( FLVSelected.SelectedData as TtiObject ) ;
end;

// This Paint method is cloned from TCustomGroupBox, with some
// modifications to hide the border if FbBorder is false
procedure TtiPerAwareMultiSelect.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
begin
  inherited Paint ;
  with Canvas do
  begin
    Font := Self.Font;
    H := TextHeight('0');
    R := Rect(0, H div 2 - 1, Width, Height);

    if Ctl3D then begin
      Inc(R.Left);
      Inc(R.Top);

      if FbBorder then
        Brush.Color := clBtnHighlight
      else
        Brush.Color := Color ;

      FrameRect(R);
      OffsetRect(R, -1, -1);

      if FbBorder then
        Brush.Color := clBtnShadow
      else
        Brush.Color := Color ;

    end else begin

      if FbBorder then
        Brush.Color := clWindowFrame
      else
        Brush.Color := Color ;

    end ;

    FrameRect(R);

    if Text <> '' then
    begin
      if not UseRightToLeftAlignment then
        R := Rect(8, 0, 0, H)
      else
        R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
      Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
      DrawText(Handle, PChar(Text), Length(Text), R, Flags or DT_CALCRECT);
      Brush.Color := Color;
      DrawText(Handle, PChar(Text), Length(Text), R, Flags);
    end;
  end;
end;

procedure TtiPerAwareMultiSelect.SetBorder(const Value: boolean);
begin
  FbBorder := Value;
  Paint ;
end;

procedure TtiPerAwareMultiSelect.SetAvailable( Value: TtiObjectList);
begin
  Assert( Value.TestValid(TtiObjectList), cTIInvalidObjectError );
  FAvailable := Value ;
  if FAvailable <> nil then
    FLVAvailable.Data := FAvailable.List
  else
    FLVAvailable.Data := nil ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Enabled := true ;
end;

procedure TtiPerAwareMultiSelect.SetSelected( Value: TtiObjectList);
begin
  Assert( Value.TestValid(TtiObjectList), cTIInvalidObjectError );
  if Value <> nil then
  begin
    Assert( Value.OwnsObjects = False, 'Value.OwnsObjects <> false' );
    Assert( Value.AutoSetItemOwner = False, 'Value.AutoSetItemOwner <> false');
  end;
  FSelected := Value ;
  if FLVSelected <> nil then
    FLVSelected.Data := FSelected.List
  else
    FLVSelected.Data := nil ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Enabled := true ;
end;

function TtiPerAwareMultiSelect.GetCaptionAvailable: string;
begin
  result := FlblAvailable.Caption ;
end;

function TtiPerAwareMultiSelect.GetCaptionSelected: string;
begin
  result := FlblSelected.Caption ;
end;

procedure TtiPerAwareMultiSelect.SetCaptionAvailable(const Value: string);
begin
  FlblAvailable.Caption := Value ;
end;

procedure TtiPerAwareMultiSelect.SetCaptionSelected(const Value: string);
begin
  FlblSelected.Caption := Value ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableDblClick(
  pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
begin
  SelectOne( pData as TtiObject ) ;
end;

procedure TtiPerAwareMultiSelect.DoLVSelectedDblClick(
  pLV: TtiCustomListView; pData: TtiObject; pItem: TListItem);
begin
  DeSelectOne( pData as TtiObject ) ;
end;

procedure TtiPerAwareMultiSelect.DeSelectOne(pData: TtiObject);
begin
  BeginUpdate ;
  try
    FSelected.Remove( pData ) ;
  finally
    EndUpdate ;
  end ;
  DoOnChange ;
end;

procedure TtiPerAwareMultiSelect.SelectOne(pData: TtiObject);
begin
  BeginUpdate ;
  try
    FSelected.Add( pData ) ;
  finally
    EndUpdate ;
  end ;
  DoOnChange ;
end;

procedure TtiPerAwareMultiSelect.RefreshData;
begin
  FlvAvailable.Refresh ;
  FlvSelected.Refresh ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableFilterData(
  pData: TtiObject; var pbInclude: Boolean);
begin
  if FSelected = nil then
    Exit ; //==>
  pbInclude := FSelected.IndexOf( pData ) = -1 ;
  if pbInclude and FApplyFilter and Assigned(FOnFilterData) then
    FOnFilterData(pData, pbInclude);
end;

procedure TtiPerAwareMultiSelect.BeginUpdate;
begin
  FSelectedIndex  := FLVSelected.SelectedIndex ;
  FAvailableIndex := FLVAvailable.SelectedIndex ;
  FLVSelected.BeginUpdate ;
  FLVAvailable.BeginUpdate ;
end;

procedure TtiPerAwareMultiSelect.EndUpdate;
begin
  FLVSelected.EndUpdate ;
  FLVAvailable.EndUpdate ;
  RefreshData ;
  FLVSelected.PositionCursor( FSelectedIndex ) ;
  FLVAvailable.PositionCursor( FAvailableIndex ) ;
end;

procedure TtiPerAwareMultiSelect.DoLVSelectedCanAcceptDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
  pDataTarget: TtiObject; var pbConfirm: Boolean);
begin
  pbConfirm := true ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableCanAcceptDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
  pDataTarget: TtiObject; var pbConfirm: Boolean);
begin
  pbConfirm := ptiLVSource <> ptiLVTarget ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
  pDataTarget: TtiObject);
var
  lList : TList ;
begin
  if ptiLVSource = ptiLVTarget then
    Exit ; //==>

  lList := ptiLVSource.Data ;
  if lList = nil then
    Exit ; //==>
  BeginUpdate ;
  try
    lList.Remove( pDataSource ) ;
    DoOnChange ;
  finally
    EndUpdate ;
  end ;

end;

procedure TtiPerAwareMultiSelect.DoLVSelectedDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TtiObject; ptiLVTarget: TtiCustomListView;
  pDataTarget: TtiObject);
var
  i : integer ;
  lList : TList ;
begin
  lList := ptiLVTarget.Data ;
  if lList = nil then
    Exit ; //==>

  BeginUpdate ;
  try
    if ptiLVSource = ptiLVTarget then
      lList.Remove( pDataSource ) ;
    if pDataTarget = nil then
      lList.Add( pDataSource )
    else
    begin
      // If we are dragging and dropping from the same list
      i := lList.IndexOf( pDataTarget ) ;
      if i = -1 then
        lList.Add( pDataSource )
      else
        lList.Insert( i, pDataSource ) ;
    end ;
    DoOnChange ;
  finally
    EndUpdate ;
  end ;
end;

procedure TtiPerAwareMultiSelect.DoOnChange;
begin
  if Assigned( FOnChange ) then
    FOnChange( self ) ;
end;

function TtiPerAwareMultiSelect.GetRunTimeSelectedCols: boolean;
begin
  result := FLVAvailable.RuntimeGenCols ;
end;

procedure TtiPerAwareMultiSelect.SetRunTimeSelectedCols(const Value: boolean);
begin
  FLVAvailable.RuntimeGenCols := Value ;
  FLVSelected.RuntimeGenCols  := Value ;
end;

procedure TtiPerAwareMultiSelect.DoOnTimerSetData(Sender: TObject);
begin
  if not Showing then
    Exit ; //==>

  TTimer( Sender ).Enabled := false ;
  RefreshData ;
  
end;

function TtiPerAwareMultiSelect.GetImages: TCustomImageList;
begin
  Result := FlvAvailable.SmallImages
end;

procedure TtiPerAwareMultiSelect.SetImages(const pImages: TCustomImageList);
begin
  FlvAvailable.SmallImages := pImages;
  FlvSelected.SmallImages := pImages;
end;

procedure TtiPerAwareMultiSelect.AddColumn(const pFieldName: string;
  const pDataType: TlvTypeKind; const pDisplayLabel: string;
  pColWidth: Integer);
//var
//  lCol: TtiListColumn;
begin
  FlvAvailable.AddColumn(pFieldName, pDataType, pDisplayLabel, pColWidth);
  FlvSelected.AddColumn(pFieldName, pDataType, pDisplayLabel, pColWidth);
//  lCol := FListColumns.Add;
//  lCol.FieldName := pFieldName;
//  lCol.DataType := pDataType;
//  lCol.DisplayLabel := pDisplayLabel;
//  lCol.Width := pColWidth;
end;

function TtiPerAwareMultiSelect.GetOnGetImageIndex: TtiLVGetImageIndexEvent;
begin
  Result := FlvAvailable.OnGetImageIndex;
end;

procedure TtiPerAwareMultiSelect.SetOnGetImageIndex(const Value: TtiLVGetImageIndexEvent);
begin
  FlvAvailable.OnGetImageIndex := Value ;
  FlvSelected.OnGetImageIndex := Value ;
end;

end.


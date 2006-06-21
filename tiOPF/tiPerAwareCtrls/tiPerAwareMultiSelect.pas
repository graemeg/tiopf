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
  ;

type

  // TtiPerAwareMultiSelect
  //------------------------------------------------------------------------------
  TtiPerAwareMultiSelect = class( TCustomGroupBox )
  private
    FlblAvailable     : TLabel ;
    FlblSelected      : TLabel ;
    FlvAvailable      : TtiListView ;
    FlvSelected       : TtiListView ;
    FsbSelectMarked   : TtiSpeedButton ;
    FsbDeSelectMarked : TtiSpeedButton ;
    FsbSelectAll      : TtiSpeedButton ;
    FsbDeSelectAll    : TtiSpeedButton ;

    FAvailable : TList ;
    FSelected  : TList ;

    FbBorder: boolean;
    FOnChange: TNotifyEvent;

    FSelectedIndex : integer ;
    FAvailableIndex : integer ;
    FListColumns : TtiListColumns ;

    FTimerSetData : TTimer ;

    procedure WMSize( var Message: TWMSize ) ; message WM_SIZE ;
    procedure SelectOne( pData : TPersistent ) ;
    procedure DeSelectOne( pData : TPersistent ) ;

    procedure DosbSelectMarkedClick( sender: TObject ) ;
    procedure DosbDeSelectMarkedClick( sender: TObject ) ;

    procedure DosbSelectAllClick( sender: TObject ) ;
    procedure DosbDeSelectAllClick( sender: TObject ) ;

    procedure DoLVAvailableKeyPress( Sender: TObject; var Key: Char ) ;
    procedure DoLVSelectedKeyPress( Sender: TObject; var Key: Char ) ;

    procedure DoLVAvailableDblClick( pLV: TtiCustomListView;
                                     pData: TPersistent;
                                     pItem: TListItem);

    procedure DoLVSelectedDblClick(  pLV: TtiCustomListView;
                                     pData: TPersistent;
                                     pItem: TListItem);
    procedure DoLVAvailableFilterData( pData: TPersistent;
                                       var pbInclude: Boolean);

    procedure SetBorder(const Value: boolean);
    procedure SetAvailable( Value: TList);
    procedure SetSelected( Value: TList);
    function  GetCaptionAvailable: string;
    function  GetCaptionSelected: string;
    procedure SetCaptionAvailable(const Value: string);
    procedure SetCaptionSelected(const Value: string);
    procedure BeginUpdate ;
    procedure EndUpdate ;
    procedure DoLVAvailableCanAcceptDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
      pDataTarget: TPersistent; var pbConfirm: Boolean);
    procedure DoLVAvailableDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
      pDataTarget: TPersistent);
    procedure DoLVSelectedCanAcceptDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
      pDataTarget: TPersistent; var pbConfirm: Boolean);
    procedure DoLVSelectedDrop(ptiLVSource: TtiCustomListView;
      pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
      pDataTarget: TPersistent);
    procedure DoOnChange ;
    function  GetRunTimeSelectedCols: boolean;
    procedure SetRunTimeSelectedCols(const Value: boolean);
    procedure DoOnTimerSetData(Sender: TObject);

  protected
    procedure Paint ; override ;
  published
    property  Align ;
    property  Caption ;
    property  Anchors ;
    property  ShowHint ;
    property  Border : boolean read FbBorder write SetBorder ;
    property  OnChange : TNotifyEvent read FOnChange write FOnChange ;
    property  CaptionAvailable : string read GetCaptionAvailable write SetCaptionAvailable ;
    property  CaptionSelected  : string read GetCaptionSelected  write SetCaptionSelected ;
    property    ListColumns : TtiListColumns
                  read FListColumns
                  write FListColumns ;
    property  RunTimeSelectedCols : boolean
                  read GetRunTimeSelectedCols
                  write SetRunTimeSelectedCols ;
    property  Visible ;
  public
    constructor create( Owner : TComponent ) ; override ;
    destructor  destroy ; override ;
    procedure   DoSelectAll;
    procedure   DoDeSelectAll;
    property    Available : TList read FAvailable write SetAvailable ;
    property    Selected  : TList read FSelected  write SetSelected  ;
    procedure   RefreshData ;
  end;

implementation
uses
  tiResources
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPerAwareMultiSelect
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPerAwareMultiSelect.create( Owner : TComponent ) ;
begin
  FbBorder          := true ;

  inherited create( Owner ) ;

  // Had problems with csAcceptsControls being removed at runtime.
  // It was causing flicker when the panel was resized and owned components
  // where not being redrawn properly.
  if ( csDesigning in ComponentState ) then
    ControlStyle   := ControlStyle - [csAcceptsControls] ;

  FlblAvailable     := TLabel.create( self ) ;
  FlblSelected      := TLabel.create( self ) ;
  FlvAvailable      := TtiListView.create( self ) ;
  FlvSelected       := TtiListView.create( self ) ;
  FsbSelectMarked   := TtiSpeedButton.create( self ) ;
  FsbDeSelectMarked := TtiSpeedButton.create( self ) ;
  FsbSelectAll      := TtiSpeedButton.create( self ) ;
  FsbDeSelectAll    := TtiSpeedButton.create( self ) ;

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

  FListColumns := TtiListColumns.Create( Self ) ;

  FTimerSetData := TTimer.Create( nil ) ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Interval := 15 ;
  FTimerSetData.OnTimer := DoOnTimerSetData ;

  Application.processMessages ;
end ;

//------------------------------------------------------------------------------
destructor TtiPerAwareMultiSelect.destroy ;
begin
  FAvailable := nil ;
  FSelected  := nil ;
  FListColumns.Free ;
  FTimerSetData.Free ;
  inherited destroy ;
end ;

//------------------------------------------------------------------------------
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

end;

//------------------------------------------------------------------------------
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
      FSelected.Add( TPersistent( lList.Items[i] )) ;
    if lList.Count > 0 then
    begin
      RefreshData ;
      DoOnChange ;
    end ;
  finally
    EndUpdate ;
  end ;
end;

//------------------------------------------------------------------------------
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
      FSelected.Remove( TPersistent( lList.Items[i] )) ;
    if lList.Count > 0 then
    begin
      RefreshData ;
      DoOnChange ;
    end ;
  finally
    EndUpdate ;
  end ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.DosbSelectAllClick(Sender: TObject);
begin
  DoSelectAll ;
end ;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.DosbDeSelectAllClick(Sender: TObject);
begin
  DoDeSelectAll ;
end;

//------------------------------------------------------------------------------
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
      FSelected.Add( FAvailable.Items[i] ) ;
  finally
    EndUpdate ;
  end ;
  if lbChange then
    DoOnChange ;
end;

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.DoLVAvailableKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = ' ' ) and
     ( FLVAvailable.SelectedData <> nil ) then
    SelectOne( FLVAvailable.SelectedData ) ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.DoLVSelectedKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = ' ' ) and
     ( FLVSelected.SelectedData <> nil ) then
    DeSelectOne( FLVSelected.SelectedData ) ;
end;

// This Paint method is cloned from TCustomGroupBox, with some
// modifications to hide the border if FbBorder is false
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.SetBorder(const Value: boolean);
begin
  FbBorder := Value;
  Paint ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.SetAvailable( Value: TList);
begin
  FAvailable := Value ;
  FLVAvailable.Data := FAvailable ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Enabled := true ;
//  RefreshData ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.SetSelected( Value: TList);
begin
  FSelected := Value ;
  FLVSelected.Data := FSelected ;
  FTimerSetData.Enabled := false ;
  FTimerSetData.Enabled := true ;
//  RefreshData ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMultiSelect.GetCaptionAvailable: string;
begin
  result := FlblAvailable.Caption ;
end;

//------------------------------------------------------------------------------
function TtiPerAwareMultiSelect.GetCaptionSelected: string;
begin
  result := FlblSelected.Caption ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.SetCaptionAvailable(const Value: string);
begin
  FlblAvailable.Caption := Value ;
end;

//------------------------------------------------------------------------------
procedure TtiPerAwareMultiSelect.SetCaptionSelected(const Value: string);
begin
  FlblSelected.Caption := Value ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableDblClick(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  SelectOne( pData ) ;
end;

procedure TtiPerAwareMultiSelect.DoLVSelectedDblClick(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  DeSelectOne( pData ) ;
end;

procedure TtiPerAwareMultiSelect.DeSelectOne(pData: TPersistent);
begin
  BeginUpdate ;
  try
    FSelected.Remove( pData ) ;
  finally
    EndUpdate ;
  end ;
  DoOnChange ;
end;

procedure TtiPerAwareMultiSelect.SelectOne(pData: TPersistent);
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
var
  i : integer;
  lListColumn : TtiListColumn ;
begin
  if not FlvAvailable.RuntimeGenCols then
  begin
    FlvAvailable.ListColumns.Clear ;
    FlvSelected.ListColumns.Clear ;
    for i := 0 to FListColumns.Count - 1 do
    begin
      lListColumn := FlvAvailable.ListColumns.Add ;
      lListColumn.DisplayName  := FListColumns.Items[i].DisplayName  ;
      lListColumn.DisplayLabel := FListColumns.Items[i].DisplayLabel ;
      lListColumn.FieldName    := FListColumns.Items[i].FieldName    ;
      lListColumn.DisplayMask  := FListColumns.Items[i].DisplayMask  ;
      lListColumn.DataType     := FListColumns.Items[i].DataType     ;
      lListColumn := FlvSelected.ListColumns.Add ;
      lListColumn.DisplayName  := FListColumns.Items[i].DisplayName  ;
      lListColumn.DisplayLabel := FListColumns.Items[i].DisplayLabel ;
      lListColumn.FieldName    := FListColumns.Items[i].FieldName    ;
      lListColumn.DisplayMask  := FListColumns.Items[i].DisplayMask  ;
      lListColumn.DataType     := FListColumns.Items[i].DataType     ;
    end ;
  end ;
  FlvAvailable.Refresh ;
  FlvSelected.Refresh ;
{
  // A hack to get the LVs drawn at create time...
  // Gotta do better than this :(
  FlvAvailable.LV.ShowColumnHeaders := false ;
  FlvAvailable.LV.ShowColumnHeaders := true ;
  FlvAvailable.LV.Repaint ;
  FlvSelected.LV.ShowColumnHeaders := false ;
  FlvSelected.LV.ShowColumnHeaders := true ;
  FlvSelected.LV.RePaint ;
}  
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableFilterData(
  pData: TPersistent; var pbInclude: Boolean);
begin
  if FSelected = nil then
    Exit ; //==>
  pbInclude := FSelected.IndexOf( pData ) = -1 ;
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
  pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
  pDataTarget: TPersistent; var pbConfirm: Boolean);
begin
  pbConfirm := true ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableCanAcceptDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
  pDataTarget: TPersistent; var pbConfirm: Boolean);
begin
  pbConfirm := ptiLVSource <> ptiLVTarget ;
end;

procedure TtiPerAwareMultiSelect.DoLVAvailableDrop(ptiLVSource: TtiCustomListView;
  pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
  pDataTarget: TPersistent);
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
  pDataSource: TPersistent; ptiLVTarget: TtiCustomListView;
  pDataTarget: TPersistent);
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

end.


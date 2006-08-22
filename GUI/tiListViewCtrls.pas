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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiListViewCtrls;

interface
uses
   tiListView
  ,tiObject
  ,Classes
  ,Controls
  ,ComCtrls
  ,Menus
  ;

type

  TtiListViewListBoxStyle = ( lvlbStyleRadioButton, lvlbStyleCheckBox ) ;
  TtiLVLBGetCheckedEvent  = procedure( pData : TtiObject ; var pbChecked : boolean ) of object ;

  TtiListViewListBox = class( TtiCustomListView )
  private
    FImageList : TImageList ;
    FStyle: TtiListViewListBoxStyle;
    FOnGetChecked: TtiLVLBGetCheckedEvent;
    FOnCheck: TtiLVItemEvent;
    FPopupMenu : TPopupMenu ;
    FmiSelectAll : TMenuItem ;
    FmiSelectNone : TMenuItem ;
    procedure DoOnGetImageIndex( pData : TtiObject ; var ImageIndex : integer ) ;
    procedure DoOnLVClick( Sender  : TtiCustomListView ;
                           pItem   : TListItem   ;
                           pData   : TtiObject ;
                           pColumn : TListColumn ) ;

    procedure DoOnKeyPress( Sender: TObject; var Key: Char ) ;
    procedure DoSelectAll( Sender : TObject ) ;
    procedure DoSelectNone( Sender : TObject ) ;
    procedure DoToggleAllSelected(pbSelect: boolean);
    function  GetIsSelected(pData: TtiObject): boolean;
  protected

  published


    property    Align         ;
    property    Anchors       ;
    //property    MultiSelect   ;
    //property    OnDblClick    ;
    //property    OnChange      ;
    //property    OnChanging    ;
    //property    OnKeyDown     ;
    //property    OnKeyPress    ;
    //property    SmallImages   ;
    //property    ViewStyle     ;
    property    RowSelect     ;
    property    Constraints   ;
    property    Visible       ;

    // These three properties are needed for drag-and-drop
    //property    OnDragOver  ;
    //property    OnDragDrop  ;
    //property    OnMouseDown ;

    //property    OnLVClick ;

    //property    OnEdit   ;
    //property    OnNew    ;
    //property    OnDelete ;
    property    OnFilterData ;
    property    OnGetFont ;

    property    ApplyFilter ;
    property    ApplySort   ;

    //property    OnGetImageIndex ;

    property    ListColumns ;
    property    SortOrders ;
    property    SortOnHeadingClick ;
    //property    AfterRefreshData ;
    property    OnItemArive ;
    property    OnItemLeave ;
    property    SelectFirstRow ;

    property    Style : TtiListViewListBoxStyle
                  read FStyle
                  write FStyle default lvlbStyleRadioButton ;

    property    OnGetChecked : TtiLVLBGetCheckedEvent
                  read  FOnGetChecked
                  write FOnGetChecked ;

    property    OnCheck : TtiLVItemEvent
                  read  FOnCheck
                  write FOnCheck ;

    property    RunTimeGenCols ;
    property    ReadOnly ;

  public
    Constructor Create( AOwner : TComponent ) ; override ;
    Destructor  Destroy ; override ;
  end ;

implementation
uses
  ImgList
  ,Graphics
  ,SysUtils
  ,tiResources
  {$IFDEF FPC},lclproc{$ENDIF}
  ;

const

  cuiCheckBoxChecked              = 0 ;
  cuiCheckBoxUnChecked            = 1 ;
  cuiRadioButtonChecked           = 2 ;
  cuiRadioButtonUnChecked         = 3 ;
  cuiCheckBoxCheckedDisabled      = 4 ;
  cuiCheckBoxUnCheckedDisabled    = 5 ;
  cuiRadioButtonCheckedDisabled   = 6 ;
  cuiRadioButtonUnCheckedDisabled = 7 ;


{ TtiListViewListBox }

constructor TtiListViewListBox.Create(AOwner: TComponent);
  // We cant use the ImageList's GetResource as we may be loading from
  // inside a runtime package and this will cause it to fail.
  procedure LoadImage( const psImageName : string ) ;
  var
    lBMP : TBitMap ;
  begin
    try
      lBMP := TBitMap.Create ;
      try
        lBMP.LoadFromResourceName( HInstance, psImageName ) ;
        if FImageList.Add( lBMP, nil ) = -1 then
          raise Exception.Create( 'Unable to add image to ImageList.' ) ;
      finally
        lBMP.Free ;
      end ;
    except
      on e:exception do
        raise exception.CreateFmt( 'Unable to read image <%s> Error message: %s',
                                   [psImageName, e.message] ) ;
    end ;
  end ;
begin
  inherited;
  FImageList      := TImageList.Create( nil ) ;
  SmallImages     := FImageList ;
  FStyle          := lvlbStyleRadioButton ;
  OnGetImageIndex := DoOnGetImageIndex ;
  OnLVClick       := DoOnLVClick ;
  LV.OnKeyPress      := DoOnKeyPress ;

  if not ( csDesigning in ComponentState ) then
  begin
    LoadImage( cResTI_CheckBoxChecked16N ) ;
    LoadImage( cResTI_CheckBoxUnChecked16N    ) ;
    LoadImage( cResTI_RadioButtonChecked16N   ) ;
    LoadImage( cResTI_RadioButtonUnChecked16N ) ;
    LoadImage( cResTI_CheckBoxChecked16D ) ;
    LoadImage( cResTI_CheckBoxUnChecked16D    ) ;
    LoadImage( cResTI_RadioButtonChecked16D   ) ;
    LoadImage( cResTI_RadioButtonUnChecked16D ) ;
  end ;

  FPopupMenu    := TPopupMenu.Create( nil ) ;
  PopupMenu     := FPopupMenu ;
  FmiSelectAll  := TMenuItem.Create( nil ) ;
  FmiSelectAll.Caption := 'Select &all' ;
  FmiSelectAll.ShortCut := TextToShortCut( 'Ctrl+A' ) ;
  FmiSelectAll.OnClick := DoSelectAll ;
  FPopupMenu.Items.Add( FmiSelectAll ) ;

  FmiSelectNone  := TMenuItem.Create( nil ) ;
  FmiSelectNone.Caption := 'Select &none' ;
  FmiSelectNone.ShortCut := TextToShortCut( 'Ctrl+N' ) ;
  FmiSelectNone.OnClick := DoSelectNone ;
  FPopupMenu.Items.Add( FmiSelectNone ) ;

end;

destructor TtiListViewListBox.Destroy;
begin
  FImageList.Free ;
  FPopupMenu.Free ;
  inherited;
end;

procedure TtiListViewListBox.DoOnGetImageIndex(pData: TtiObject;
  var ImageIndex: integer);
var
  lbChecked : boolean ;
  lbGreyOut : boolean ;
begin
  lbChecked := GetIsSelected( pData ) ;
  lbGreyOut := (not Enabled) or ReadOnly ;
  Case FStyle of
  lvlbStyleRadioButton : begin
                           if lbChecked and not lbGreyOut then
                             ImageIndex := cuiRadioButtonChecked
                           else if not lbChecked and not lbGreyOut then
                             ImageIndex := cuiRadioButtonUnChecked
                           else if lbChecked and lbGreyOut then
                             ImageIndex := cuiRadioButtonCheckedDisabled
                           else
                             ImageIndex := cuiRadioButtonUnCheckedDisabled ;
                         end ;

  lvlbStyleCheckBox    : begin
                           if lbChecked and not lbGreyOut then
                             ImageIndex := cuiCheckBoxChecked
                           else if not lbChecked and not lbGreyOut then
                             ImageIndex := cuiCheckBoxUnChecked
                           else if lbChecked and lbGreyOut then
                             ImageIndex := cuiCheckBoxCheckedDisabled
                           else
                             ImageIndex := cuiCheckBoxUnCheckedDisabled ;
                         end ;
  end ;
end;

function TtiListViewListBox.GetIsSelected( pData: TtiObject ) : boolean ;
var
  lbResult : boolean ;
begin
  if ( pData <> nil ) and
     Assigned( FOnGetChecked ) then
    FOnGetChecked( pData, lbResult )
  else
    lbResult := false ;
  result := lbResult ;
end ;

procedure TtiListViewListBox.DoOnKeyPress(Sender: TObject; var Key: Char);
begin
  if ( Key = ' ' ) and
     ( Selected <> nil ) and
     ( Selected.Data <> nil ) then
    DoOnLVClick( Self,
                 Selected,
                 TtiObject(Selected.Data),
                 Columns[0] );
end;

procedure TtiListViewListBox.DoOnLVClick( Sender: TtiCustomListView;
                                          pItem: TListItem;
                                          pData: TtiObject;
                                          pColumn: TListColumn);
begin

  if ReadOnly then
    Exit ; //==>

  if not Assigned( FOnCheck ) then
    Exit ; //==>

  if pColumn.Index <> 0 then
    Exit ; //==>

  if pItem = nil then
    Exit ; //==>

  if pData = nil then
    Exit ; //==>

  FOnCheck( Self, pData, pItem ) ;

  Refresh( false ) ;

  if Assigned( OnItemArive ) then
    OnItemArive( Sender, pData, pItem ) ;

end;

procedure TtiListViewListBox.DoSelectAll(Sender: TObject);
begin
  DoToggleAllSelected( true ) ;
end;

procedure TtiListViewListBox.DoSelectNone(Sender: TObject);
begin
  DoToggleAllSelected( false ) ;
end ;

procedure TtiListViewListBox.DoToggleAllSelected(pbSelect : boolean );
var
  i : integer ;
  lSelected : TListItem ;
begin
  lSelected := LV.Selected ;
  for i := 0 to Items.Count - 1 do
  begin
    LV.Selected := Items[i] ;
    if GetIsSelected( TtiObject(Items[i].Data) ) <> pbSelect then
      DoOnLVClick( Self,
                   Selected,
                   TtiObject(Selected.Data),
                   Columns[0] );
  end ;
  LV.Selected := lSelected ;
end;

end.

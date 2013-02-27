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
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    For to demonstrate the features of the TtiListView

  ToDo:
    Nothing

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiListViewDemo;

interface

uses    
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, tiListView, ImgList, ToolWin, ExtCtrls, StdCtrls, DemoData,
  Spin, ActnList, Buttons;

type
  TFormListViewDemo = class(TForm)
    ToolBar1: TToolBar;
    tbNew: TToolButton;
    tbEdit: TToolButton;
    tbDelete: TToolButton;
    ToolButton4: TToolButton;
    tbClose: TToolButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    cbImages: TCheckBox;
    cbColours: TCheckBox;
    Memo1: TMemo;
    cbFilter: TCheckBox;
    SB: TStatusBar;
    seGoToRecord: TSpinEdit;
    Label1: TLabel;
    ActionList1: TActionList;
    aMoveDown: TAction;
    aMoveUp: TAction;
    MemoLog: TMemo;
    Panel2: TPanel;
    LV: TtiListView;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    aEdit: TAction;
    aNew: TAction;
    aDelete: TAction;
    aClose: TAction;
    procedure FormCreate(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
    procedure LVGetImageIndex(Data: TPersistent; var ImageIndex: Integer);
    procedure cbColoursClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVFilterData(pData: TPersistent; var pbInclude: Boolean);
    procedure cbFilterClick(Sender: TObject);
    procedure LVAfterRefreshData(Sender: TtiListView);
    procedure seGoToRecordChange(Sender: TObject);
    procedure LVLVClick(Sender: TtiListView; pItem: TListItem;
      pData: TPersistent; pColumn: TListColumn);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure LVMoveItem(pList: TList; pDataMove, pDataBefore,
      pDataAfter: TPersistent);
    procedure LVGetFont(Sender: TtiCustomListView; pCanvas: TCanvas;
      pItem: TListItem; pData: TPersistent);
    procedure LVItemArive(Sender: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemLeave(Sender: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure aMoveDownExecute(Sender: TObject);
    procedure aMoveUpExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure LVItemEdit(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
    procedure LVItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVListColumns4DeriveColumn(const pLV: TtiCustomListView;
      const pData: TPersistent; const ptiListColumn: TtiListColumn;
      var pResult: String);
  private
    FData : TDemoData ;
  public
    { Public declarations }
  end;

var
  FormListViewDemo: TFormListViewDemo;

implementation
uses
  FEdit
  ,typinfo
  ;

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.FormCreate(Sender: TObject);
begin
  // Create some demo data
  FData := TDemoData.Create ;
  // Connect the demo data to the TtiListView
  LV.Data := FData ;
  seGoToRecord.MinValue := 0 ;
  seGoToRecord.MaxValue := FData.Count - 1 ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.FormDestroy(Sender: TObject);
begin
  // Must set LV.Data := nil as the list view may try to redraw itself after
  // its data has been deleted giving a hard to tract AV.
  LV.Data := nil ;
  FData.Free ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aNewExecute(Sender: TObject);
begin
  LV.DoNew ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aEditExecute(Sender: TObject);
begin
  LV.DoEdit ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aDeleteExecute(Sender: TObject);
begin
  LV.DoDelete ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

// Filter out all odd values
// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVFilterData(pData: TPersistent;
                                         var pbInclude: Boolean);
begin

  // Check that a data object was passed
  if pData = nil then
    Exit ; //==>

  pbInclude := (( TDataRow( pData ).IntegerData mod 2 ) = 0 ) ;

end;

// Runtime calculation of ImageIndex
// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVGetImageIndex(Data: TPersistent;
                                            var ImageIndex: Integer);
var
  lData : TDataRow ;
begin

  // Check that a data object was passed
  if Data = nil then
    Exit ; //==>

  if not cbImages.Checked then begin
    ImageIndex := -1 ;
    Exit ; //==>
  end ;

  lData := TDataRow( Data ) ;

  // If the day of week is a weekend, then draw a X in the grid.
  if ( DayOfWeek( lData.DateData ) = 1 ) or
     ( DayOfWeek( lData.DateData ) = 7 ) then
    ImageIndex := 3
  else
    ImageIndex := -1 ;

end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.tbCloseClick(Sender: TObject);
begin
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.cbColoursClick(Sender: TObject);
begin
  LV.Refresh ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.cbFilterClick(Sender: TObject);
begin
  LV.ApplyFilter := TCheckBox( Sender ).Checked ;
  LV.Refresh ;
end;

procedure TFormListViewDemo.LVAfterRefreshData(Sender: TtiListView);
begin
  SB.SimpleText := 'Number of items in list: ' +
                   IntToStr( Sender.Items.Count ) ;
end;

procedure TFormListViewDemo.seGoToRecordChange(Sender: TObject);
begin
  LV.PositionCursor( TSpinEdit( sender ).Value ) ;
end;

procedure TFormListViewDemo.LVLVClick(Sender: TtiListView;
  pItem: TListItem; pData: TPersistent; pColumn: TListColumn);
begin
//  Log([ pItem<>Nil, pData<>nil, pColumn<>nil]) ;
//  Log([ pItem.Caption, GetPropValue( pData, 'StringData' ), pColumn.Caption, pColumn.Index ]) ;
end;

// Runtime calculation of display properties
// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVGetFont(Sender: TtiCustomListView;
  pCanvas: TCanvas; pItem: TListItem; pData: TPersistent);
var
  lData : TDataRow ;
begin

  // Are we on a node with data?
  if pData = nil then
    Exit ; //==>

  // Is the Colours check box checked?
  if not cbColours.Checked then begin
    pCanvas.Font.Color  := clBlack ;
    pCanvas.Brush.Color := clWhite ;
    Exit ; //==>
  end ;

  lData := TDataRow( pData ) ;

  // if IntegerData is an even multiple of 2, then set the font to navy
  if ( lData.IntegerData mod 2 ) = 0 then
    pCanvas.Font.Color := clNavy
  else
    pCanvas.Font.Color := clRed ;

  // If IntegerData is an even multiple of 3, then set the background to silver
  if ( lData.IntegerData mod 3 ) = 0 then
    pCanvas.Brush.Color := clSilver
  else
    pCanvas.Brush.Color := clWhite ;

end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVItemArive( Sender: TtiCustomListView;
                                         pData: TPersistent;
                                         pItem: TListItem);
begin
  if pData = nil then
    Exit ; //==>
  MemoLog.Lines.Add( 'You have arived at ' +
                     TDataRow( pData ).StringData ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVItemLeave( Sender: TtiCustomListView;
                                         pData: TPersistent;
                                         pItem: TListItem);
begin
  if pData = nil then
    Exit ; //==>
  MemoLog.Lines.Add( 'You are leaving ' +
                     TDataRow( pData ).StringData ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aNew.Enabled      := LV.CanInsert ;
  aEdit.Enabled     := LV.CanEdit ;
  aDelete.Enabled   := LV.CanDelete ;
  aMoveDown.Enabled := LV.CanMoveItem( LV.Selected,  1 ) ;
  aMoveUp.Enabled   := LV.CanMoveItem( LV.Selected, -1 ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aMoveDownExecute(Sender: TObject);
begin
  LV.MoveItem( LV.Selected, 1 ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.aMoveUpExecute(Sender: TObject);
begin
  LV.MoveItem( LV.Selected, -1 ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVMoveItem(pList: TList; pDataMove,
  pDataBefore, pDataAfter: TPersistent);
var
  i : integer ;
begin

  if LV.ApplySort then
    LV.ApplySort := false ;

  // Remove the moved data from the list. Call Extract on a TObjectList, or
  // Remove on a TList.
  FData.Extract( pDataMove ) ;

  i := FData.IndexOf( pDataAfter ) ;
  if i >= 0 then
    FData.Insert( i, pDataMove )
  else
    FData.Add( pDataMove ) ;

end;

// Edit the data being displayed in a row
// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lForm : TFormEdit ;
begin

  // Create an edit form
  lForm := TFormEdit.Create( nil ) ;
  try
    // Map the edit form to the data under the TtiListView
    lForm.Data := TDataRow( pData ) ;
    // If the user clicked OK, then refrehs the current record
    lForm.ShowModal ;
//    if lForm.ShowModal = mrOK then
//      LV.Refresh( true ) ;
  finally
    lForm.Free ;
  end ;
end;

// Delete an existing item
// -----------------------------------------------------------------------------
procedure TFormListViewDemo.LVItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  // Confirm with the user they really want to delete.
  if MessageDlg( 'Are you sure you want to delete this row?',
                 mtConfirmation,
                 [mbYes,mbNo],
                 0 ) = mrYes then
    FData.Remove( pData ) ;
end;

procedure TFormListViewDemo.LVItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lForm : TFormEdit ;
  lData : TDataRow ;
begin
  // Create an edit form
  lForm := TFormEdit.Create( nil ) ;
  try
    // Create an empty data object
    lData := TDataRow.Create ;
    // Map the new data object to the form for editing
    lForm.Data := lData;
    // If the user clicked OK, add the new data object to the list and refresh.
    if lForm.ShowModal = mrOK then
      pLV.Data.Add( lData )
    else
      // User clicked cancel, so cleanup the mess
      lData.Free ;
  finally
    lForm.Free ;
  end ;
end;

procedure TFormListViewDemo.LVListColumns4DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
begin
  pResult := FormatFloat( '#,##0.000',
                         ( pData as TDataRow ).FloatData +
                         ( pData as TDataRow ).IntegerData ) ; 
end;

end.

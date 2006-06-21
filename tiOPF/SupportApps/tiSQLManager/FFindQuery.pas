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
    Nov 2000, SM, Created

  Purpose:
    SQLManager dialog to find query by name or SQL text

  Classes:
    TFormFindQuery - the form

  ToDo:
    Find query by SQL Text

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FFindQuery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls
  ,tiPtnVisPerObj
  ,tiSQLMgr_BOM
  ,tiSQLMgr_Cli, tiListView, tiButtons, tiPerAwareCtrls, ComCtrls,
  ActnList, Buttons, tiListViewPlus, tiFocusPanel
  ;

type
  TFormFindQuery = class(TForm)
    rbQueryName: TRadioButton;
    rbQueryText: TRadioButton;
    Label1: TLabel;
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    AL: TActionList;
    aOK: TAction;
    aCancel: TAction;
    aPerformSearch: TAction;
    Tmr: TTimer;
    LVResult: TtiListViewPlus;
    paeSearchText: TtiPerAwareComboBoxHistory;
    lblCount: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVResultDblClick(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure aOKExecute(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);
    procedure ALUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure TmrTimer(Sender: TObject);
    procedure hcSearchTextChange(Sender: TObject);
    procedure LVResultKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    function  GetLVVisible: boolean;
    procedure SetLVVisible(const Value: boolean);
    procedure PerformSearch;
    procedure PerformSelected;
  private
    FSQLMgr: TSQLMgr;
    FSQLMgrQuery: TSQLMgrQuery;
    FSearchResult : TList ;
    { Private declarations }
    property LVVisible : boolean read GetLVVisible write SetLVVisible ;
  public
    property SQLMgr : TSQLMgr read FSQLMgr write FSQLMgr ;
    property SQLMgrQuery : TSQLMgrQuery read FSQLMgrQuery write FSQLMgrQuery ;
  end;

implementation

{$R *.DFM}

uses
   tiUtils
  ,FSQLMgrBrowse
  ,FMainSQLManager
  ,Math
  ,tiDialogs
  ,tiRegINI
  ;

const
  cuiHeightHideLV  = 139 ;
  cuiHeightShowLV  = 380 ;
  cuiWidth         = 450 ;

procedure TFormFindQuery.PerformSearch ;
begin

  if not rbQueryName.Checked then
  begin
    tiAppMessage('Search by Query Text yet not supported');
    Exit; //==>
  end;

  AL.OnUpdate := nil ;
  try
    LVResult.Data := nil ;
    FSQLMgr.FindQueriesByName( '*' + paeSearchText.Value + '*' , FSearchResult ) ;
    LVVisible := FSearchResult.Count >= 1 ;
    LVResult.Data := FSearchResult ;
    lblCount.Caption := 'Count: ' + IntToStr( FSearchResult.Count ) ;
  finally
    AL.OnUpdate := ALUpdate ;
  end ;
end;

procedure TFormFindQuery.PerformSelected ;
begin
  if LVResult.SelectedData <> nil then
  begin
    FSQLMgrQuery := TSQLMgrQuery( LVResult.SelectedData ) ;
    ModalResult := mrOK ;
  end ;
end ;

procedure TFormFindQuery.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  if FSearchResult.Count > 0 then
    gINI.WriteInteger( Name, 'Index', lvResult.SelectedIndex ) ;
end;

procedure TFormFindQuery.FormCreate(Sender: TObject);
begin
  FSearchResult := TList.Create ;
  gINI.ReadFormState( self ) ;
  LVVisible := false ;
end;

procedure TFormFindQuery.FormDestroy(Sender: TObject);
begin
  FSearchResult.Free ;
  gINI.WriteFormState( self ) ;
end;

function TFormFindQuery.GetLVVisible: boolean;
begin
  result := lvResult.Visible ;
end;

procedure TFormFindQuery.SetLVVisible(const Value: boolean);
begin
  // Show the list view
  if Value then
  begin
//    Constraints.MaxHeight := 0 ;
//    Constraints.MinHeight := 0 ;
//    Constraints.MaxWidth  := 0 ;
//    Constraints.MinWidth  := 0 ;
//    BorderStyle := bsSizeable ;
    LVResult.Anchors := [] ;
    Height      :=
      Max( gINI.ReadInteger( Name, 'HeightShowLV', cuiHeightShowLV ),
           cuiHeightShowLV ) ;
    Width       :=
      Max( gINI.ReadInteger( Name, 'WidthShowLV',  cuiWidth ),
           cuiWidth ) ;
    LVResult.Top := 88 ;
    LVResult.Left := 16 ;
    LVResult.Height := ClientHeight - LVResult.Top - bbOK.Height - 16 ;
    LVResult.Width := ClientWidth - LVResult.Left - 8 ;
    LVResult.Anchors := [akLeft,akTop,akRight,akBottom] ;
    LVResult.Visible := true ;
  end
  else
  // Hide the list view
  begin
    if LVResult.Visible <> Value then
    begin
      gINI.WriteInteger( Name, 'HeightShowLV', Height ) ;
      gINI.WriteInteger( Name, 'WidthShowLV',  Width  ) ;
    end ;
//    BorderStyle := bsDialog ;
    Height := cuiHeightHideLV ;
    Width  := cuiWidth ;
//    Constraints.MaxHeight := cuiHeightHideLV ;
//    Constraints.MinHeight := cuiHeightHideLV ;
//    Constraints.MaxWidth  := cuiWidth ;
//    Constraints.MinWidth  := cuiWidth ;
    LVResult.Anchors      := [] ;
    LVResult.Visible      := false ;
    LVResult.Height       := 0 ;
  end ;
end;

procedure TFormFindQuery.LVResultDblClick(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  PerformSelected ;
end;

procedure TFormFindQuery.aOKExecute(Sender: TObject);
begin
  if LVVisible then
    PerformSelected
end;

procedure TFormFindQuery.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormFindQuery.ALUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  aOK.Enabled := lvResult.SelectedData <> nil ;
  aPerformSearch.Enabled := paeSearchText.Value <> '' ;
  bbOK.Default := aOK.Enabled ;
  Handled := true ;
end;

procedure TFormFindQuery.TmrTimer(Sender: TObject);
begin
  tmr.Enabled := false ;
  PerformSearch ;
end;

procedure TFormFindQuery.hcSearchTextChange(Sender: TObject);
begin
  tmr.Enabled := true ;
end;

procedure TFormFindQuery.LVResultKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    PerformSelected ;
end;

procedure TFormFindQuery.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (paeSearchText.Focused) and
     (Key = VK_Down) then
  begin
    lvResult.SetFocus ;
    Key := 0 ;
  end ;
end;

procedure TFormFindQuery.FormShow(Sender: TObject);
begin
  PerformSearch ;
  if FSearchResult.Count > 0 then
    lvResult.PositionCursor(
      gINI.ReadInteger( Name, 'Index', 0 )) ;
end;

end.

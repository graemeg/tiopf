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
    Nov 2000, Peter Hinrichsen, Made open source
    DEC 2000, DJW, implemented the locked feature
    DEC 2000, PWH, implemented some persistent aware edit controls

  Purpose:
    Edit a query - activated by a parent TtiTreeView

  Classes:
    TFormChildQuery

  Conditional defs
    MWEDIT - Turn on the use of mwEdit (or wmEdit or what ever it is called) for
             colour syntax hilighting - requires a third party control.

  ToDo:
    1. Package the use of mwEdit so the SQLManager can be distributed without
       a dependency on someone elses component.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FChildQuery;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, ExtCtrls, ComCtrls, ActnList,
  tiListView, tiPerAwareCtrls, tiSplitter,
  FEditParam,
  tiPtnVisPerObj,
  tiSQLMgr_BOM,
  tiDataSet_BOM,
  FSQLMgrBrowse
  ,FSQLEditor, tiButtons, tiFocusPanel
 ;

type
  TFormChildQuery = class(TForm)
    PC: TPageControl;
    tsDesign: TTabSheet;
    panelButtons: TPanel;
    pnlTop: TPanel;
    paeQueryName: TtiPerAwareEdit;
    pacbLocked: TtiPerAwareCheckBox;
    pacbTestInclude: TtiPerAwareCheckBox;
    pnlMiddle: TPanel;
    tiSplitter1: TtiSplitter;
    pnlNotes: TPanel;
    pamQueryDescription: TtiPerAwareMemo;
    pnlParams: TPanel;
    lbQueryParameters: TLabel;
    mbParseSQLForParams: TtiMicroButton;
    lvParams: TtiListView;
    tiSplitter2: TtiSplitter;
    pnlSQL: TPanel;
    lbSQL: TLabel;
    mbRun: TtiMicroButton;
    tsGrid: TTabSheet;
    LV: TListView;
    btnRefreshLV: TBitBtn;
    mbParamsToSetupParamsDelphiCode: TtiMicroButton;
    procedure eQueryNameChange(Sender: TObject);
    procedure eQueryNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetBOMAsDirty(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvParamsDblClick(Sender: TObject);
    procedure lvParamsFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure aRunQueryExecute(Sender: TObject);
    procedure aParamInsertExecute(Sender: TObject);
    procedure aParamEditExecute(Sender: TObject);
    procedure aParamDeleteExecute(Sender: TObject);
    procedure aDefaultParamsExecute(Sender: TObject);
    procedure lvParamsItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvParamsItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure lvParamsItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure LVData(Sender: TObject; Item: TListItem);
    procedure PCChange(Sender: TObject);
    procedure btnRefreshLVClick(Sender: TObject);
    procedure mbParamsToSetupParamsDelphiCodeClick(Sender: TObject);
  private

    FData: TSQLMgrQuery;
    FTreeNode: TTreeNode;
    FbFormCreateCalled : boolean ;
    FSQLEditor : TFormSQLEditor ;

    FDataSet : TtiDataSet ;

    function  GetValid: boolean;
    procedure SetData(const Value: TSQLMgrQuery);
    procedure GUIToBOM ;
    procedure SetLocked ;
    procedure QueryToListView;
  published
    property Data : TSQLMgrQuery read FData write SetData ;
    property Valid : boolean    read GetValid ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
  public
    procedure Execute;
    procedure ExecuteThenSave;
  end;

implementation
uses
  tiUtils
  ,tiDialogs
  ,tiLog
  ,tiQuery
  ,FMainSQLManager
  ,tiSQLMgr_Cli
  ,tiDataSet_Cli
  ,tiSQLMgrDataSet_Cli
  ,tiPtnVisPerObj_Cli
  ,tiRegINI
  ,ClipBrd
  ,ctiPersist
  ;

{$R *.DFM}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TFormChildQuery
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TFormChildQuery.FormCreate(Sender: TObject);
begin
  inherited;
  PC.Align := alClient ;
  PC.ActivePage := tsDesign ;

  pnlSQL.BevelInner := bvNone ;
  pnlSQL.Align     := alClient ;

  panelButtons.Visible := false ;

  FSQLEditor := TFormSQLEditor.Create( Self ) ;
  FSQLEditor.Parent := pnlSQL ;
  FSQLEditor.Top := 16 ;
  FSQLEditor.Left := 4 ;
  FSQLEditor.Height := pnlSQL.ClientHeight - 20 ;
  FSQLEditor.Width  := pnlSQL.ClientWidth - 8  ;
  FSQLEditor.Anchors := [akTop, akLeft, akBottom, akRight ] ;
  FSQLEditor.Visible := true ;

  pnlMiddle.Height := gINI.ReadInteger( name, 'pnlTopHeight', 100 ) ;
  pnlNotes.Width   := gINI.ReadInteger( name, 'pnlLeftWidth', 245 ) ;

  FbFormCreateCalled := true ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.FormDestroy(Sender: TObject);
begin

  // Chasing an AV on shutdown....
  LV.OwnerData := false ;
  LV.OnData := nil ;

  gINI.WriteInteger( name, 'pnlTopHeight', pnlMiddle.Height ) ;
  gINI.WriteInteger( name, 'pnlLeftWidth', pnlNotes.Width ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.Execute ;
begin
  FSQLEditor.Execute ;
end ;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.ExecuteThenSave ;
begin
  FSQLEditor.ExecuteThenSave ;
end ;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.eQueryNameChange(Sender: TObject);
begin
  inherited;
  FTreeNode.Text    := FData.QueryName ;
  FData.Dirty       := true ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.eQueryNameKeyPress(Sender: TObject; var Key: Char);
var lKey : string ;
begin
  // Cloned code, also found in FEditParam
  inherited;
  lKey := Key ;
  if (( lKey >= 'A' ) and
      ( lKey <= 'Z' )) or
     (( lKey >= 'a' ) and
      ( lKey <= 'z' )) or
     (( lKey >= '0' ) and
      ( lKey <= '9' )) or
     ( lKey = ' ' ) or
     ( lKey = '_' ) or
     ( lKey = chr( 8 ) ) then begin
    if lKey = ' ' then
      lKey := '_' ;
    Key := lKey[1] ;
  end else begin
    Key := #0 ;
    MessageBeep( 0 ) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.FormShow(Sender: TObject);
begin
  if ( FData.ObjectState = posCreate ) and
     ( not FData.QueryLocked ) then
    paeQueryName.SetFocus ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.SetBOMAsDirty(Sender: TObject);
begin
  FData.Dirty := true ;
  GUIToBOM;
end;


// -----------------------------------------------------------------------------
function TFormChildQuery.GetValid: boolean;
begin
  result := false ;

  if paeQueryName.Value = '' then
    raise exception.Create( 'Please enter a query name' ) ;

  if not FData.Owner.Owner.IsQueryNameUnique( FData ) then
  begin
    tiAppError( FData.Caption + ' is not unique' ) ;
    Exit ; //==>
  end ;

  result := true ;

  if result then
    GUIToBOM ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.SetData(const Value: TSQLMgrQuery);
//var
//  lSL : TStringList ;
begin

  if not FbFormCreateCalled then
    FormCreate( Self ) ;

  PC.ActivePage := tsDesign ;

  LV.Items.Clear ;
  LV.Columns.Clear ;

  FData                        := Value ;

  if FData = nil then
  begin
    lvParams.Data := nil ;
    Exit ; //==>
  end ;

  Assert( FData.TestValid(TSQLMgrQuery), cTIInvalidObjectError );
  Assert( FData.Owner.TestValid(TSQLMgrGroup), cTIInvalidObjectError );
  Assert( FData.Owner.Owner.TestValid(TSQLMgr), cTIInvalidObjectError );

  paeQueryName.LinkToData(        FData, 'QueryName' ) ;
  pamQueryDescription.LinkToData( FData, 'QueryDesc' ) ;
  pacbLocked.LinkToData(          FData, 'QueryLocked' ) ;
  pacbTestInclude.LinkToData(     FData, 'TestInclude' ) ;
  FSQLEditor.Data := FData ;

  lvParams.Data := FData.Params.List ;

  paeQueryName.OnChange        := eQueryNameChange ;
  pamQueryDescription.OnChange := SetBOMAsDirty ;

  pacbLocked.OnChange           := SetBOMAsDirty ;
  pacbTestInclude.OnChange      := SetBOMAsDirty ;

  // ToDo_DJW: Can the calls to SetLocked be moved into the ActionLists OnUpdate event?
  // Come to think of it, this form is a real mess, and there is plenty that could
  // be moved into the action list. One day I must clean this up.
  SetLocked ;

  PCChange( nil ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.GUIToBOM;
begin
  if FData = nil then
    Exit ; //==>
  SetLocked ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.lvParamsDblClick(Sender: TObject);
begin
  if lvParams.Selected <> nil then
    lvParams.DoEdit
  else
    lvParams.DoNew ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.lvParamsFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.aRunQueryExecute(Sender: TObject);
begin
  FSQLEditor.Execute ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.aParamInsertExecute(Sender: TObject);
begin
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.aParamEditExecute(Sender: TObject);
begin
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.aParamDeleteExecute(Sender: TObject);
begin
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.aDefaultParamsExecute(Sender: TObject);
  procedure _DefaultParamValue( const pParam : TSQLMgrParam ) ;
  begin
    // ToDo: Change this to a data driven system...
    if pParam.ParamName = 'OID' then begin
      pParam.ParamType  := qfkInteger ;
      pParam.ParamValue := '-1' ;
    end else if SameText( pParam.ParamName, 'EFFECTIVE_START' ) then begin
      pParam.ParamType  := qfkDateTime ;
      pParam.ParamValue := '17/12/1999' ;
    end else if SameText( pParam.ParamName, 'EFFECTIVE_END' )   then begin
      pParam.ParamType  := qfkDateTime ;
      pParam.ParamValue := '31/12/2999' ;
    end else if SameText( pParam.ParamName, 'LASTCHANGED_BY' )  then begin
      pParam.ParamType  := qfkString ;
      pParam.ParamValue := 'SQLMgr' ;
    end else begin
      pParam.ParamType  := qfkString ;
      pParam.ParamValue := '<Enter value>' ;
    end ;

  end ;

var
  i : integer ;
  lsLine : string ;
  lParam : TSQLMgrParam ;
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    lsl.Text := FData.SQL ;
    FData.Params.MarkListItemsForDeletion ;
    for i := 0 to lsl.Count-1 do
    begin
      lsLine := lsl.Strings[i];
      // If there is no ':' in the line, then probably not a parameter line
      if Pos( ':', lsLine ) = 0 then
        Continue ; //==>
      lsLine := tiTrimL( lsLine, ':' ) ;
      lsLine := tiStrTran( lsLine, ')', '' ) ;
      lsLine := tiStrTran( lsLine, ',', '' ) ;
      lsLine := Trim( lsLine ) ;
      Log( lsLine ) ;
      if lsLine <> '' then
      begin
        lParam            := TSQLMgrParam.CreateNew ;
        lParam.ParamName  := lsLine ;
        lParam.IsNull     := false ;
        _DefaultParamValue( lParam ) ;
        FData.Params.Add( lParam ) ;
      end ;
    end ;
    LVParams.Refresh ;
  finally
    lsl.Free ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.SetLocked ;
var
  lbValue : boolean ;
begin

  lbValue := FData.QueryLocked ;

  // Use this as a flag to see if the form's read only properties have been
  // set correctly.
  if paeQueryName.Enabled = not lbValue then
    Exit ; //==>

  paeQueryName.Enabled         := not(lbValue);
  pamQueryDescription.Enabled  := not(lbValue);
//  aDefaultParams.Enabled       := not(lbValue);
  lvParams.Enabled             := not(lbValue);

//  if lbValue then
//    lvParams.VisibleButtons := [tiLVBtnVisEdit]
//  else
//    lvParams.VisibleButtons := [tiLVBtnVisNew,tiLVBtnVisEdit,tiLVBtnVisDelete] ;

  FSQLEditor.ReadOnly := lbValue ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.lvParamsItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lSQLMgrParam : TSQLMgrParam ;
begin

  if not tiAppConfirmation( 'Are you sure you want to ' +
                            'delete this parameter?' ) then
    exit ; //==>
  lSQLMgrParam := TSQLMgrParam( pData ) ;
  lSQLMgrParam.Deleted := true ;
  pLV.Refresh ;

end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.lvParamsItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEditParam.Execute( TSQLMgrParam( pData )) then
    pLV.Refresh( true ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormChildQuery.lvParamsItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lSQLMgrParam   : TSQLMgrParam ;
begin
  lSQLMgrParam := TSQLMgrParam.CreateNew ;
  if TFormEditParam.Execute( lSQLMgrParam ) then
  begin
    FData.Params.Add( lSQLMgrParam ) ;
    pLV.Refresh ;
  end
  else
    lSQLMgrParam.Free ;
end;

procedure TFormChildQuery.LVData(Sender: TObject; Item: TListItem);
begin
  tiDataSetToListItem( FDataSet, Item ) ;
end ;

procedure TFormChildQuery.QueryToListView ;
begin
  LV.Items.Count := 0 ;
  LV.Columns.Clear ;

  if FData = nil then
    Exit ; //==>

  FDataSet := gTIDataSetMgr.FindBySQLMgrQuery( FData ) ;
  tiDataSetToListView( FDataSet, LV ) ;

end;

procedure TFormChildQuery.PCChange(Sender: TObject);
begin
  if PC.ActivePage = tsGrid then
  begin
    QueryToListView ;
  end ;
end;


procedure TFormChildQuery.btnRefreshLVClick(Sender: TObject);
begin
  QueryToListView ;
end;

procedure TFormChildQuery.mbParamsToSetupParamsDelphiCodeClick(
  Sender: TObject);
begin
  ClipBoard.AsText := FData.Params.AsSetupParamsDelphiCode ;
end;

end.



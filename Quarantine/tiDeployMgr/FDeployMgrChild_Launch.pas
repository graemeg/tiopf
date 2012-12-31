tiGUIUtils{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

unit FDeployMgrChild_Launch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, tiListView, tiDeployMgr_BOM, ComCtrls, StdCtrls ;

type

  TLaunchHotSpot = class( TCustomPanel )
  private
    FImgIcon    : TImage ;
    FlblAppName : TLabel ;
    FMemoNotes  : TMemo ;
    FDeployLaunchApp: TtiDeployLaunchApp;
    FDatabaseName: string;
    procedure SetDeployLaunchApp(const Value: TtiDeployLaunchApp);
    procedure DoOnClick( Sender : TObject ) ;
//    Procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
//    Procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetIsMouseOver: boolean;
//    procedure CMMouseEnter(var Message: TMessage);
//    procedure CMMouseLeave(var Message: TMessage);
  public
    constructor create( Owner : TComponent ) ; override ;
    property    DeployLaunchApp : TtiDeployLaunchApp read FDeployLaunchApp write SetDeployLaunchApp ;
    property    MemoNotes : TMemo read FMemoNotes write FMemoNotes ;
    property    DatabaseName : string read FDatabaseName write FDatabaseName ;
    property    IsMouseOver : boolean read GetIsMouseOver ;
  end ;


  TFormTIDeployChild_Launch = class(TForm)
    pnlForm: TPanel;
    SRB: TScrollBox;
    memoNotes: TMemo;
    tmrMouse: TTimer;
    procedure LVDblClick(pLV: TtiCustomListView; pData: TtiObject;pItem: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure tmrMouseTimer(Sender: TObject);
  private
    FData: TtiDeployApps;
    FTreeNode: TTreeNode;
    FDatabaseName: string;
    function  GetValid: boolean;
    procedure SetData(const Value: TtiDeployApps);
    { Private declarations }
  public
  published
    property Data : TtiDeployApps read FData write SetData ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid : boolean read GetValid ;
    property DatabaseName : string read FDatabaseName write FDatabaseName ;
  end;

implementation
uses
  tiCommandLineParams
  ,tiDeployMgr_Cli
  ,tiGUIUtils
  ,tiLog
  ,tiUtils
  ;

{$R *.DFM}

{ TFormTIDeployChild_Launch }

function TFormTIDeployChild_Launch.GetValid: boolean;
begin
  result := true ;
end;

procedure TFormTIDeployChild_Launch.SetData(const Value: TtiDeployApps);
var
  lDeployLaunch : TtiDeployLaunchApp ;
  lLaunchHotSpot : TLaunchHotSpot ;
  i : integer ;
const
  clBorder = 8 ;
begin
  FData := Value;
  tmrMouse.Enabled := false ;
  
  if FData = nil then
    Exit ; //==>

  // Delete any pre-existing TLaunchHotSpot(s)
  for i := SRB.ComponentCount - 1 downto 0 do
    if SRB.Components[i] is TLaunchHotSpot then
      SRB.RemoveComponent( SRB.Components[i] ) ;

  FData.DeployLaunchApps.Refresh ;

  for i := 0 to FData.DeployLaunchApps.Count - 1 do
  begin
    lDeployLaunch := FData.DeployLaunchApps.Items[i] ;
    lLaunchHotSpot := TLaunchHotSpot.Create( SRB ) ;
    lLaunchHotSpot.DatabaseName := FDatabaseName ;
    lLaunchHotSpot.Left := clBorder ;
    lLaunchHotSpot.Top := clBorder+ ( lLaunchHotSpot.Height + clBorder ) * i ;
    lLaunchHotSpot.Parent := SRB ;
    lLaunchHotSpot.DeployLaunchApp := lDeployLaunch ;
    lLaunchHotSpot.MemoNotes := MemoNotes ;
    lLaunchHotSpot.Visible := true ;
  end ;

  tmrMouse.Enabled := true ;

end;

procedure TFormTIDeployChild_Launch.LVDblClick(
  pLV: TtiCustomListView;
  pData: TtiObject;
  pItem: TListItem);
begin
  DeployAndLaunch( pData as TtiDeployLaunchApp,
                   gCommandLineParams.GetParam( 'd' ) ) ;
end ;

{ TLaunchHotSpot }


//procedure TLaunchHotSpot.CMMouseEnter(var Message: TMessage);
//begin
//  Assert( FMemoNotes <> Nil, 'MemoNotes not assigned' ) ;
//  FMemoNotes.Lines.Text := FDeployLaunchApp.AppDescription ;
//end;
//
//procedure TLaunchHotSpot.CMMouseLeave(var Message: TMessage);
//begin
//  Assert( FMemoNotes <> Nil, 'MemoNotes not assigned' ) ;
//  FMemoNotes.Lines.Text := ''
//end;

constructor TLaunchHotSpot.create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  Width      := 313         ;
  Height     := 69          ;
  Cursor     := crHandPoint ;
  BevelOuter := bvNone      ;
  Color      := clWindow    ;
  Font.Color := clBlue      ;
  OnClick    := DoOnClick   ;

  FImgIcon := TImage.Create( Self ) ;
  with FImgIcon do
  begin
    Parent := Self ;
    Left   := 12 ;
    Top    := 19 ;
    Width  := 32 ;
    Height := 32 ;
    Cursor := crHandPoint ;
    OnClick    := DoOnClick   ;
  end ;

  FlblAppName := TLabel.Create( Self ) ;
  with FlblAppName do
  begin
    Parent := Self ;
    Left   := 56 ;
    Top    := 29 ;
    Width  := 57 ;
    Height := 13 ;
    Cursor := crHandPoint ;
    OnClick := DoOnClick   ;
  end ;

end ;

procedure TLaunchHotSpot.DoOnClick(Sender: TObject);
begin
  DeployAndLaunch( FDeployLaunchApp, FDatabaseName) ;
end;

function TLaunchHotSpot.GetIsMouseOver: boolean;
var
  lPoint : TPoint ;
  lRect  : TRect ;
begin
  lPoint := Mouse.CursorPos ;
  lPoint := ScreenToClient( lPoint ) ;
  lRect  := GetClientRect ;
  result := PtInRect( lRect, lPoint ) ;
end;

procedure TLaunchHotSpot.SetDeployLaunchApp( const Value: TtiDeployLaunchApp);
begin
  FDeployLaunchApp := Value;
  FlblAppName.Caption := FDeployLaunchApp.DisplayText ;
  FImgIcon.Picture.Assign( FDeployLaunchApp.DeployApp.Image ) ;
end;

procedure TFormTIDeployChild_Launch.FormCreate(Sender: TObject);
begin
  SRB.BorderStyle := bsNone ;
  pnlForm.Align := alClient ;
  MemoNotes.BorderStyle := bsNone ;
end;

procedure TFormTIDeployChild_Launch.tmrMouseTimer(Sender: TObject);
var
  i : integer ;
  lMouseOverControl : boolean ;
begin
  lMouseOverControl := false ;
  for i := 0 to SRB.ComponentCount - 1 do
    if SRB.Components[i] is TLaunchHotSpot then
      if TLaunchHotSpot( SRB.Components[i] ).IsMouseOver then
      begin
        memoNotes.Lines.Text :=
          TLaunchHotSpot( SRB.Components[i] ).DeployLaunchApp.AppDescription ;
        lMouseOverControl := true;
      end ;
  if not lMouseOverControl then
    memoNotes.Lines.Text := '' ;
end;

end.

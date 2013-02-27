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
    January 2001, Peter Hinrichsen, Made open source

  Purpose:
    Component editor for the TtiSplitterPanel

  Classes:
    TtiSplitterPanelEditor  - The component editor
    TtiSplitterPanelEditDlg - The edit dialog

  ToDo:
    Good grahical editing of border styles.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiSplitterEditor;

interface
uses
   tiSplitter
   {$IFNDEF VER130}
     ,DesignIntf
     ,DesignEditors
   {$ELSE}
     ,DsgnIntf
   {$ENDIF}
  ,Forms
  ,Classes
  ,Controls
  ,StdCtrls
  ,ExtCtrls
  ,ComCtrls
  ,Buttons
  ;

type

  // ---------------------------------------------------------------------------
  TtiSplitterPanelEditor = class( TComponentEditor )
  public
    procedure ExecuteVerb(Index: Integer); override ;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override ;
  end ;

  // ---------------------------------------------------------------------------
  TtiSplitterPanelEditDlg = class( TForm )
  private
    lblSplitterPos           : TLabel ;
    rgSplitterOrientation    : TRadioGroup ;
    rgPanelStyle             : TRadioGroup ;
    cbKeepSplitterPosPercent : TCheckBox ;
    tbSplitterPos            : TTrackBar ;
    bbOK                     : TBitBtn ;
    bbCancel                 : TBitBtn ;
    FData : TtiSplitterPanel ;
    procedure bbOKClick( Sender : TObject ) ;
    procedure SetData(const Value: TtiSplitterPanel);
    procedure DoOnChange( Sender : TObject ) ;
  public
    Constructor CreateNew( Owner : TComponent ; Dummy : integer = 0 ) ; override ;
    Property    Data : TtiSplitterPanel read FData write SetData ;
  end ;

procedure Register;

implementation
uses
  Dialogs
  ,SysUtils
  ,Graphics
  ,Windows
  ;

procedure Register;
begin
  RegisterComponentEditor( TtiSplitterPanel, TtiSplitterPanelEditor ) ;
end ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitterPanelEditor
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiSplitterPanelEditor.ExecuteVerb(Index: Integer);
var
  lForm : TtiSplitterPanelEditDlg ;
begin
  if Index <> 0 then
    raise exception.create( 'Invalid VerbIndex' ) ;
  lForm := TtiSplitterPanelEditDlg.CreateNew( Nil ) ;
  try
    lForm.Data := TtiSplitterPanel( Component ) ;
    if lForm.ShowModal = mrOK then
      Designer.Modified ;
  finally
    lForm.Free ;
  end;
end;

// -----------------------------------------------------------------------------
function TtiSplitterPanelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0 : result := '&Edit TtiSplitterPanel...' ;
  else
    raise exception.create( 'Invalid VerbIndex' ) ;
  end ;
end;

// -----------------------------------------------------------------------------
function TtiSplitterPanelEditor.GetVerbCount: Integer;
begin
  result :=  1 ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitterPanelEditDlg
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiSplitterPanelEditDlg.CreateNew(Owner: TComponent ; Dummy : Integer = 0 );
begin
  inherited CreateNew( Owner ) ;
  Left := 359 ;
  Top := 219 ;
  BorderIcons := [biSystemMenu] ;
  BorderStyle := bsDialog ;
  Caption := 'Edit TtiSplitterPanel' ;
  ClientHeight := 258 ;
  ClientWidth := 196 ;
  Color := clBtnFace ;
  Font.Charset := DEFAULT_CHARSET ;
  Font.Color := clWindowText ;
  Font.Height := -11 ;
  Font.Name := 'MS Sans Serif' ;
  Font.Style := [] ;
  OldCreateOrder := False ;
  Position := poScreenCenter ;
  //PixelsPerInch := 96 ;

  lblSplitterPos := TLabel.Create( Self ) ;
  with lblSplitterPos do
  begin
    Parent := Self ;
    Left := 4 ;
    Top := 100 ;
    Width := 71 ;
    Height := 13 ;
    Caption := 'Splitter &position' ;
  end ;

  rgSplitterOrientation := TRadioGroup.Create( self ) ;
  with rgSplitterOrientation do
  begin
    Parent := Self ;
    Left := 4 ;
    Top := 4 ;
    Width := 185 ;
    Height := 69 ;
    Caption := ' Splitter alignment ' ;
    Columns := 2 ;
    Items.Add( 'Vertical' ) ;
    Items.Add( 'Horizontal' ) ;
    TabOrder := 0 ;
    OnClick := DoOnChange ;
  end ;

  rgPanelStyle := TRadioGroup.Create( self ) ;
  with rgPanelStyle do
  begin
    Parent := Self ;
    Left := 4 ;
    Top := 77 ;
    Width := 185 ;
    Height := 77 ;
    Caption := ' Panel style ' ;
    Columns := 2 ;
    Items.Add( 'None' ) ;
    Items.Add( 'User' ) ;
    Items.Add( 'Lowered' ) ;
    Items.Add( 'Raised' ) ;
    Items.Add( 'Framed' ) ;
    Items.Add( 'Shadow' ) ;
    Items.Add( 'Bump' ) ;
    TabOrder := 1;
    OnClick := DoOnChange ;
  end ;

  cbKeepSplitterPosPercent := TCheckBox.Create( self ) ;
  with cbKeepSplitterPosPercent do
  begin
    Parent := Self ;
    Left := 4 ;
    Top := 158;
    Width := 137 ;
    Height := 17 ;
    Alignment := taLeftJustify ;
    Caption := 'Keep splitter position %' ;
    TabOrder := 1 ;
    OnClick := DoOnChange ;
  end ;

  tbSplitterPos := TTrackBar.Create( Self ) ;
  with tbSplitterPos do
  begin
    Parent := Self ;
    Left := 8 ;
    Top := 178 ;
    Width := 181 ;
    Height := 45 ;
    Max := 100 ;
    Min := 3 ;
    Orientation := trHorizontal ;
    Frequency := 10 ;
    Position := 3 ;
    SelEnd := 0 ;
    SelStart := 0 ;
    TabOrder := 2 ;
    TickMarks := tmBottomRight ;
    TickStyle := tsAuto ;
    OnChange := DoOnChange ;
  end ;

  bbOK := TBitBtn.Create( Self ) ;
  with bbOK do
  begin
    Parent := Self ;
    Left := 32 ;
    Top := 227 ;
    Width := 75 ;
    Height := 25 ;
    TabOrder := 3 ;
    Kind := bkOK ;
    OnClick := bbOKClick ;
  end ;

  bbCancel := TBitBtn.Create( Self ) ;
  with bbCancel do
  begin
    Parent := Self ;
    Left := 112 ;
    Top := 227 ;
    Width := 75 ;
    Height := 25 ;
    TabOrder := 4 ;
    Kind := bkCancel ;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TtiSplitterPanelEditDlg.bbOKClick(Sender: TObject);
begin
  DoOnChange( nil ) ;
  ModalResult := mrOK;
end;

// -----------------------------------------------------------------------------
procedure TtiSplitterPanelEditDlg.SetData(const Value: TtiSplitterPanel);
begin
  rgSplitterOrientation.ItemIndex := Ord( Value.SplitterOrientation ) ;
  rgPanelStyle.ItemIndex          := Ord( Value.PanelStyle ) ;
  cbKeepSplitterPosPercent.Checked := Value.KeepSplitterPosPercent ;
  tbSplitterPos.Position           := Value.SplitterPosPercent ;
  FData := Value ;
end;

// -----------------------------------------------------------------------------
procedure TtiSplitterPanelEditDlg.DoOnChange(Sender: TObject);
begin
  if FData = nil then
    Exit ; //==>
  FData.SplitterOrientation := TtiSplitterOrientation( rgSplitterOrientation.ItemIndex ) ;
  FData.PanelStyle          := TtiSplitterPanelStyle( rgPanelStyle.ItemIndex ) ;
  FData.KeepSplitterPosPercent := cbKeepSplitterPosPercent.Checked ;
  FData.SplitterPosPercent := tbSplitterPos.Position ;
end;

end.

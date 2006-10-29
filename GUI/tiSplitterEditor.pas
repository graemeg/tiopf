unit tiSplitterEditor;

{$I tiDefines.inc}

interface
uses
   tiSplitter
   {$IFNDEF FPC}
   {$IFNDEF VER130}
     ,DesignIntf
     ,DesignEditors
   {$ELSE}
     ,DsgnIntf
   {$ENDIF}
   {$ELSE}
    ,ComponentEditors
    ,PropEdits
    ,LazarusPackageIntf
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

  TtiSplitterPanelEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): string; override;
    function  GetVerbCount: Integer; override;
  end;

  TtiSplitterPanelEditDlg = class(TForm)
  private
    lblSplitterPos          : TLabel;
    rgSplitterOrientation   : TRadioGroup;
    rgPanelStyle            : TRadioGroup;
    cbKeepSplitterPosPercent : TCheckBox;
    tbSplitterPos           : TTrackBar;
    bbOK                    : TBitBtn;
    bbCancel                : TBitBtn;
    FData : TtiSplitterPanel;
    procedure bbOKClick(Sender : TObject);
    procedure SetData(const AValue: TtiSplitterPanel);
    procedure DoOnChange(Sender : TObject);
  public
    Constructor CreateNew(AOwner : TComponent; Dummy : integer = 0); override;
    Property    Data : TtiSplitterPanel read FData write SetData;
  end;

implementation
uses
  Dialogs
  ,SysUtils
  ,Graphics
{$IFNDEF FPC}
  ,Windows
{$ENDIF}
 ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitterPanelEditor
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiSplitterPanelEditor.ExecuteVerb(Index: Integer);
var
  lForm : TtiSplitterPanelEditDlg;
begin
  if Index <> 0 then
    raise exception.Create('Invalid VerbIndex');
  lForm := TtiSplitterPanelEditDlg.CreateNew(Nil);
  try
    lForm.Data := TtiSplitterPanel(Component);
    if lForm.ShowModal = mrOK then
      Designer.Modified;
  finally
    lForm.Free;
  end;
end;

function TtiSplitterPanelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0 : result := '&Edit TtiSplitterPanel...';
  else
    raise exception.Create('Invalid VerbIndex');
  end;
end;

function TtiSplitterPanelEditor.GetVerbCount: Integer;
begin
  result :=  1;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiSplitterPanelEditDlg
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiSplitterPanelEditDlg.CreateNew(AOwner: TComponent; Dummy : Integer = 0);
begin
  inherited CreateNew(AOwner ,0);
  Left := 359;
  Top := 219;
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;
  Caption := 'Edit TtiSplitterPanel';
  ClientHeight := 258;
  ClientWidth := 196;
  Color := clBtnFace;
  {$IFNDEF FPC}
  Font.Charset := DEFAULT_CHARSET;
  OldCreateOrder := False;
  {$ENDIF}
  Font.Color := clWindowText;
  Font.Height := -11;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  Position := poScreenCenter;
  //PixelsPerInch := 96;

  lblSplitterPos := TLabel.Create(Self);
  with lblSplitterPos do
  begin
    Parent := Self;
    Left := 4;
    Top := 100;
    Width := 71;
    Height := 13;
    Caption := 'Splitter &position';
  end;

  rgSplitterOrientation := TRadioGroup.Create(self);
  with rgSplitterOrientation do
  begin
    Parent := Self;
    Left := 4;
    Top := 4;
    Width := 185;
    Height := 69;
    Caption := ' Splitter alignment ';
    Columns := 2;
    Items.Add('Vertical');
    Items.Add('Horizontal');
    TabOrder := 0;
    OnClick := DoOnChange;
  end;

  rgPanelStyle := TRadioGroup.Create(self);
  with rgPanelStyle do
  begin
    Parent := Self;
    Left := 4;
    Top := 77;
    Width := 185;
    Height := 77;
    Caption := ' Panel style ';
    Columns := 2;
    Items.Add('None');
    Items.Add('User');
    Items.Add('Lowered');
    Items.Add('Raised');
    Items.Add('Framed');
    Items.Add('Shadow');
    Items.Add('Bump');
    TabOrder := 1;
    OnClick := DoOnChange;
  end;

  cbKeepSplitterPosPercent := TCheckBox.Create(self);
  with cbKeepSplitterPosPercent do
  begin
    Parent := Self;
    Left := 4;
    Top := 158;
    Width := 137;
    Height := 17;
    {$IFNDEF FPC}
    Alignment := taLeftJustify;
    {$ENDIF}
    Caption := 'Keep splitter position %';
    TabOrder := 1;
    OnClick := DoOnChange;
  end;

  tbSplitterPos := TTrackBar.Create(Self);
  with tbSplitterPos do
  begin
    Parent := Self;
    Left := 8;
    Top := 178;
    Width := 181;
    Height := 45;
    Max := 100;
    Min := 3;
    Orientation := trHorizontal;
    Frequency := 10;
    Position := 3;
    {$IFNDEF FPC}
    SelEnd := 0;
    SelStart := 0;
    {$ENDIF}
    TabOrder := 2;
    TickMarks := tmBottomRight;
    TickStyle := tsAuto;
    OnChange := DoOnChange;
  end;

  bbOK := TBitBtn.Create(Self);
  with bbOK do
  begin
    Parent := Self;
    Left := 32;
    Top := 227;
    Width := 75;
    Height := 25;
    TabOrder := 3;
    Kind := bkOK;
    OnClick := bbOKClick;
  end;

  bbCancel := TBitBtn.Create(Self);
  with bbCancel do
  begin
    Parent := Self;
    Left := 112;
    Top := 227;
    Width := 75;
    Height := 25;
    TabOrder := 4;
    Kind := bkCancel;
  end;

end;

procedure TtiSplitterPanelEditDlg.bbOKClick(Sender: TObject);
begin
  DoOnChange(nil);
  ModalResult := mrOK;
end;

procedure TtiSplitterPanelEditDlg.SetData(const AValue: TtiSplitterPanel);
begin
  rgSplitterOrientation.ItemIndex := Ord(AValue.SplitterOrientation);
  rgPanelStyle.ItemIndex         := Ord(AValue.PanelStyle);
  cbKeepSplitterPosPercent.Checked := AValue.KeepSplitterPosPercent;
  tbSplitterPos.Position          := AValue.SplitterPosPercent;
  FData := AValue;
end;

procedure TtiSplitterPanelEditDlg.DoOnChange(Sender: TObject);
begin
  if FData = nil then
    Exit; //==>
  FData.SplitterOrientation := TtiSplitterOrientation(rgSplitterOrientation.ItemIndex);
  FData.PanelStyle         := TtiSplitterPanelStyle(rgPanelStyle.ItemIndex);
  FData.KeepSplitterPosPercent := cbKeepSplitterPosPercent.Checked;
  FData.SplitterPosPercent := tbSplitterPos.Position;
end;

end.

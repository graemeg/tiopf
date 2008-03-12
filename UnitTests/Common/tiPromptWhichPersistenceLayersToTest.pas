unit tiPromptWhichPersistenceLayersToTest;

{$I tiDefines.inc}

interface
uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, Menus;

type

  TtiPromptWhichPersistenceLayersToTest = class(TForm)
  private
    Tmr: TTimer;
    pnlCheckBoxes: TPanel;
    cbTestNonPersistentClasses: TCheckBox;
    Bevel1: TBevel;
    pnlButtons: TPanel;
    btnEditLocalSettings: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblMessage: TLabel;
    FPopupMenu: TPopupMenu;
    procedure btnPnlBtn1Click(Sender: TObject);
    procedure btnPnlBtn2Click(Sender: TObject);
    procedure btnEditLocalSettingsClick(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
    procedure cbTestNonPersistentClassesClick(Sender: TObject);
    procedure DoCheckBoxClick(Sender: TObject);
    procedure DoSelectAll(Sender: TObject);
    procedure DoSelectNone(Sender: TObject);
    procedure Save;
    procedure BuildForm;
    procedure CreatePersistenceLayerSelectionCheckBoxes;
  public
    class function Execute: boolean;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;



implementation
uses
   tiOPFTestManager
  ,tiTestDependencies
  ,tiDUnitINI
  ,tiGUIUtils
 ;


{ TtiPromptWhichPersistenceLayersToTest }

class function TtiPromptWhichPersistenceLayersToTest.Execute: boolean;
var
  lForm: TtiPromptWhichPersistenceLayersToTest;
begin
  lForm := TtiPromptWhichPersistenceLayersToTest.CreateNew(nil);
  try
    result := lForm.ShowModal = mrOK;
  finally
    lForm.Free;
  end;
end;


procedure TtiPromptWhichPersistenceLayersToTest.Save;
var
  i: integer;
  lCheckBox: TCheckBox;
  lPersistenceLayerName: string;
  lPerFrameworkSetup: TtiOPFTestSetupData;
begin
  Tmr.Enabled := False;
  TDUntiLocalSettings.CreateDefaultFile;
  GTIOPFTestManager.TestNonPersistentClasses := cbTestNonPersistentClasses.Checked;
  for i := GTIOPFTestManager.Count - 1 downto 0 do
  begin
    lPersistenceLayerName := GTIOPFTestManager.Items[i].PersistenceLayerName;
    lCheckBox := TCheckBox(FindComponent('cb' + lPersistenceLayerName));
    lPerFrameworkSetup := GTIOPFTestManager.FindByPersistenceLayerName(lPersistenceLayerName);
    lPerFrameworkSetup.Selected := lCheckBox.Checked;
  end;
  GTIOPFTestManager.Save;
  GTIOPFTestManager.UnloadPersistenceLayersNotSelected;
end;


procedure TtiPromptWhichPersistenceLayersToTest.btnEditLocalSettingsClick(Sender: TObject);
begin
  Save;
  tiEditFile(TDUntiLocalSettings.FileName);
end;

procedure TtiPromptWhichPersistenceLayersToTest.btnPnlBtn1Click(Sender: TObject);
begin
  Save;
  ModalResult := mrOK;
end;


procedure TtiPromptWhichPersistenceLayersToTest.btnPnlBtn2Click(Sender: TObject);
begin
  Save;
  ModalResult := mrCancel;
end;


procedure TtiPromptWhichPersistenceLayersToTest.TmrTimer(Sender: TObject);
begin
  btnPnlBtn1Click(nil);
end;


procedure TtiPromptWhichPersistenceLayersToTest.DoCheckBoxClick(Sender: TObject);
begin
  Tmr.Enabled := false;
end;


procedure TtiPromptWhichPersistenceLayersToTest.DoSelectAll(Sender: TObject);
//var
//  i: integer;
begin
  Assert(False, 'Under construction');
//  for i := 0 to ControlCount - 1 do
//    if Controls[i] is TCheckBox then
//      (Controls[i] as TCheckBox).Checked:= True;
end;

procedure TtiPromptWhichPersistenceLayersToTest.DoSelectNone(Sender: TObject);
begin
  Assert(False, 'Under construction');
end;

procedure TtiPromptWhichPersistenceLayersToTest.cbTestNonPersistentClassesClick(Sender: TObject);
begin
  DoCheckBoxClick(nil);
end;


procedure TtiPromptWhichPersistenceLayersToTest.BuildForm;
var
  LMenuItem: TMenuItem;
begin
  Name       := 'WhichPersistenceLayersForm';
  Caption    := 'Which persistence layers?';
  Height     := 400;
  Width      := 400;
  BorderIcons := [biSystemMenu];
  Position   := poScreenCenter;
  BorderStyle:= bsDialog;

  FPopupMenu:= TPopupMenu.Create(Self);
  LMenuItem:= TMenuItem.Create(FPopupMenu);
  FPopupMenu.Items.Add(LMenuItem);
  LMenuItem.Caption:= 'Select all';
  LMenuItem.OnClick:= DoSelectAll;
  LMenuItem:= TMenuItem.Create(FPopupMenu);
  FPopupMenu.Items.Add(LMenuItem);
  LMenuItem.Caption:= 'Select none';
  LMenuItem.OnClick:= DoSelectNone;

  PopupMenu:= FPopupMenu;

  cbTestNonPersistentClasses := TCheckBox.Create(self);
  with cbTestNonPersistentClasses do
  begin
    Parent   := Self;
    Left     := 15;
    Top      := 5;
    Width    := 203;
    Height   := 21;
    Caption  := 'Test non-persistent classes?';
    TabOrder := 1;
    OnClick  := cbTestNonPersistentClassesClick;
    PopupMenu:= FPopupMenu;
  end;

  Bevel1 := TBevel.Create(self);
  with Bevel1 do
  begin
    Parent   := Self;
    Left     := 15;
    Top      := 30;
    Width    := 370;
    Height   := 5;
    Shape    := bsTopLine;
    Anchors  := [akLeft, akTop, akRight];
  end;

  pnlCheckBoxes := TPanel.Create(self);
  with pnlCheckBoxes do
  begin
    Parent   := Self;
    Left     := 15;
    Top      := 44;
    Width    := 370;
    Height   := 300;
    {$IFNDEF FPC}
    Anchors  := [akLeft, akTop, akRight, akBottom];
    {$ENDIF}
    AutoSize := True;
    BevelOuter := bvNone;
    TabOrder := 0;
    PopupMenu:= FPopupMenu;
  end;

  pnlButtons := TPanel.Create(self);
  with pnlButtons do
  begin
    Parent   := Self;
    Left     := 0;
    Top      := 210;
    Width    := 330;
    Height   := 60;
    Align    := alBottom;
    TabOrder := 2;
  end;

  lblMessage:= TLabel.Create(Self);
  lblMessage.Parent:= pnlButtons;
  lblMessage.Caption:= 'Some local constants are missing. Click <Edit local consts> to edit.';
  lblMessage.Left     := 16;
  lblMessage.Top      := 4;

  btnCancel := TButton.Create(self);
  with btnCancel do
  begin
    Parent   := pnlButtons;
    Left     := 222;
    Top      := 24;
    Width    := 92;
    Height   := 31;
    Cancel   := True;
    Caption  := 'Cancel';
    TabOrder := 1;
    OnClick  := btnPnlBtn2Click;
    Anchors  := [akRight, akBottom];
  end;

  btnOK := TButton.Create(self);
  with btnOK do
  begin
    Parent   := pnlButtons;
    Left     := 118;
    Top      := 24;
    Width    := 92;
    Height   := 31;
    Caption  := 'OK';
    default  := True;
    TabOrder := 0;
    OnClick  := btnPnlBtn1Click;
    Anchors  := [akRight, akBottom];
  end;

  btnEditLocalSettings := TButton.Create(self);
  with btnEditLocalSettings do
  begin
    Parent   := pnlButtons;
    Left     := 14;
    Top      := 24;
    Width    := 92;
    Height   := 31;
    Caption  := 'Edit local consts';
    default  := True;
    TabOrder := 0;
    OnClick  := btnEditLocalSettingsClick;
    Anchors  := [akRight, akBottom];
  end;

  Tmr := TTimer.Create(self);
  with Tmr do
  begin
    Enabled  := False;
    Interval := 3000;
    OnTimer  := TmrTimer;
  end;
end;

procedure TtiPromptWhichPersistenceLayersToTest.CreatePersistenceLayerSelectionCheckBoxes;
var
  i: integer;
  lCheckBox: TCheckBox;
  lPersistenceLayerName: string;
  lPerFrameworkSetup: TtiOPFTestSetupData;
const
  cBorder = 8;
begin
  cbTestNonPersistentClasses.Checked := GTIOPFTestManager.TestNonPersistentClasses;
  lCheckBox := nil;
  for i := 0 to GTIOPFTestManager.Count - 1 do
  begin
    lPersistenceLayerName      := GTIOPFTestManager.Items[i].PersistenceLayerName;
    lPerFrameworkSetup := GTIOPFTestManager.FindByPersistenceLayerName(lPersistenceLayerName);
    lCheckBox := TCheckBox.Create(self);
    lCheckBox.Parent := pnlCheckBoxes;
    lCheckBox.Top    := i * (cBorder + lCheckBox.Height) + cBorder;
    lCheckBox.Left   := cBorder;
    lCheckBox.Caption := lPersistenceLayerName;
    lCheckBox.Name   := 'cb' + lPersistenceLayerName;
    lCheckBox.Tag    := i;
    lCheckBox.Checked := lPerFrameworkSetup.Selected;
    lCheckBox.OnClick := DoCheckBoxClick;
    lCheckBox.PopupMenu:= FPopupMenu;
  end;
  if lCheckBox <> nil then
  begin
    pnlCheckBoxes.ClientHeight := lCheckBox.Top + lCheckBox.Height;
    ClientHeight := pnlCheckBoxes.Height + pnlButtons.Height +
                    pnlCheckBoxes.Top + cBorder;
  end
  else
  begin
  ClientHeight := pnlCheckBoxes.Height + pnlButtons.Height +
                  pnlCheckBoxes.Top + cBorder;
  end;
  Tmr.Enabled := True;
end;


constructor TtiPromptWhichPersistenceLayersToTest.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  GTIOPFTestManager.Read;
  BuildForm;
  CreatePersistenceLayerSelectionCheckBoxes;
end;

end.

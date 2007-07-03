unit tiPromptWhichPersistenceLayersToTest;

{$I tiDefines.inc}

interface
uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons;

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
    procedure btnPnlBtn1Click(Sender: TObject);
    procedure btnPnlBtn2Click(Sender: TObject);
    procedure btnEditLocalSettingsClick(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
    procedure cbTestNonPersistentClassesClick(Sender: TObject);
    procedure DoCheckBoxClick(Sender: TObject);
    procedure Save;
    procedure BuildForm;
    procedure SetupGUI;
  public
    class function Execute: boolean;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;



implementation
uses
   tiOPFTestManager
  ,tiDUnitDependencies
  ,tiOPFManager
  ,tiDUnitINI
  ,tiUtils
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
  lPerLayerName: string;
  lPerFrameworkSetup: TtiOPFTestSetupData;
begin
  Tmr.Enabled := False;
  TDUntiLocalSettings.CreateDefaultFile;
  gTIOPFTestManager.TestNonPersistentClasses := cbTestNonPersistentClasses.Checked;
  for i := gTIOPFTestManager.Count - 1 downto 0 do
  begin
    lPerLayerName := gTIOPFTestManager.Items[i].PerLayerName;
    lCheckBox := TCheckBox(FindComponent('cb' + lPerLayerName));
    lPerFrameworkSetup := gTIOPFTestManager.FindByPerLayerName(lPerLayerName);
    lPerFrameworkSetup.Selected := lCheckBox.Checked;
  end;
  gTIOPFTestManager.Save;
  gTIOPFTestManager.UnloadPersistenceLayersNotSelected;
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


procedure TtiPromptWhichPersistenceLayersToTest.cbTestNonPersistentClassesClick(Sender: TObject);
begin
  DoCheckBoxClick(nil);
end;


procedure TtiPromptWhichPersistenceLayersToTest.BuildForm;
begin
  Name       := 'WhichPersistenceLayersForm';
  Caption    := 'Which persistence layers?';
  Height     := 400;
  Width      := 400;
  BorderIcons := [biSystemMenu];
  Position   := poScreenCenter;
  BorderStyle:= bsDialog;

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


procedure TtiPromptWhichPersistenceLayersToTest.SetupGUI;
var
  i: integer;
  lCheckBox: TCheckBox;
  lPerLayerName: string;
  lPerFrameworkSetup: TtiOPFTestSetupData;
const
  cBorder = 8;
begin
  cbTestNonPersistentClasses.Checked := gTIOPFTestManager.TestNonPersistentClasses;
  lCheckBox := nil;
  for i := 0 to gTIOPFTestManager.Count - 1 do
  begin
    lPerLayerName      := gTIOPFTestManager.Items[i].PerLayerName;
    lPerFrameworkSetup := gTIOPFTestManager.FindByPerLayerName(lPerLayerName);
    lCheckBox := TCheckBox.Create(self);
    lCheckBox.Parent := pnlCheckBoxes;
    lCheckBox.Top    := i * (cBorder + lCheckBox.Height) + cBorder;
    lCheckBox.Left   := cBorder;
    lCheckBox.Caption := lPerLayerName;
    lCheckBox.Name   := 'cb' + lPerLayerName;
    lCheckBox.Tag    := i;
    lCheckBox.Checked := lPerFrameworkSetup.Enabled and lPerFrameworkSetup.Selected;
    lCheckBox.OnClick := DoCheckBoxClick;
    lCheckBox.Enabled := lPerFrameworkSetup.Enabled;
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
  gTIOPFTestManager.Read;
  BuildForm;
  SetupGUI;
end;

end.

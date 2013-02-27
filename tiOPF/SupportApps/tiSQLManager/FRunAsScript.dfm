object FormRunAsScript: TFormRunAsScript
  Left = 374
  Top = 229
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Enter application to run'
  ClientHeight = 155
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 298
    Height = 13
    Alignment = taRightJustify
    Caption = 'The key word <FILE> will be replaced with the script'#39's file name'
  end
  object tiButtonPanel1: TtiButtonPanel
    Left = 0
    Top = 124
    Width = 357
    Height = 31
    OnBtn1Click = tiButtonPanel1Btn1Click
    OnBtn2Click = tiButtonPanel1Btn2Click
    Btn1Enabled = True
    Btn2Enabled = True
    DesignSize = (
      357
      31)
  end
  object hcAppToRun: TtiPerAwareComboBoxHistory
    Left = 8
    Top = 8
    Width = 343
    Height = 41
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    LabelStyle = lsTopLeft
    Caption = '&Application to run'
    LabelWidth = 90
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
    Items.Strings = (
      '')
    HistoryCount = 5
  end
  object hcParams: TtiPerAwareComboBoxHistory
    Left = 8
    Top = 56
    Width = 343
    Height = 38
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    LabelStyle = lsTopLeft
    Caption = '&Parameters'
    LabelWidth = 90
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
    Items.Strings = (
      '')
    HistoryCount = 5
  end
end

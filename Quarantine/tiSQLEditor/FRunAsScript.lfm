object FormRunAsScript: TFormRunAsScript
  Left = 374
  Top = 229
  Width = 454
  Height = 268
  BorderIcons = [biSystemMenu]
  Caption = ' Enter application to run'
  Color = clBtnFace
  Constraints.MaxHeight = 268
  Constraints.MinHeight = 268
  Constraints.MinWidth = 365
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    446
    234)
  PixelsPerInch = 96
  TextHeight = 13
  object tiButtonPanel1: TtiButtonPanel
    Left = 0
    Top = 203
    Width = 446
    Height = 31
    OnBtn1Click = tiButtonPanel1Btn1Click
    OnBtn2Click = tiButtonPanel1Btn2Click
    Btn1Enabled = True
    Btn2Enabled = True
    DesignSize = (
      446
      31)
  end
  object hcParams: TtiPerAwareComboBoxHistory
    Left = 8
    Top = 56
    Width = 432
    Height = 38
    Anchors = [akLeft, akTop, akRight]
    Constraints.MinHeight = 23
    TabOrder = 1
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
  object memoMacros: TtiMemoReadOnly
    Left = 8
    Top = 104
    Width = 432
    Height = 89
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'The following macros will be expanded:'
      ''
      '%DatabaseName%'
      '%UserName%'
      '%Password%'
      '%ScriptFileName%')
  end
  object paeApplicationToRun: TtiPerAwarePickFile
    Left = 8
    Top = 8
    Width = 432
    Height = 45
    Constraints.MinHeight = 23
    TabOrder = 3
    LabelStyle = lsTopLeft
    Caption = '&Application to run'
    ReadOnly = False
    Filter = 'Program files|*.exe|All files|*.*'
    FilterIndex = 0
  end
end

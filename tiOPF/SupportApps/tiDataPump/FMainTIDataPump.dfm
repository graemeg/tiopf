object Form1: TForm1
  Left = 215
  Top = 249
  Width = 567
  Height = 539
  Caption = '  TechInsite Data Pump'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    559
    505)
  PixelsPerInch = 96
  TextHeight = 13
  object gbSource: TGroupBox
    Left = 8
    Top = 8
    Width = 221
    Height = 245
    Caption = ' Source '
    TabOrder = 0
    object paeSourcePL: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 24
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Persistence layer'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeSourceDatabaseName: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 52
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Database name'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeSourceUserName: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 80
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = 'User name'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeSourcePassword: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 108
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 3
      Caption = 'Password'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object memoTables: TMemo
      Left = 8
      Top = 140
      Width = 185
      Height = 89
      Lines.Strings = (
        'SQLMAN_GROUP'
        'SQLMAN_SQL'
        'SQLMAN_PARAM')
      TabOrder = 4
    end
  end
  object gbTarget: TGroupBox
    Left = 336
    Top = 12
    Width = 217
    Height = 245
    Caption = ' Target '
    TabOrder = 1
    object paeTargetPL: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 24
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Persistence layer'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeTargetDatabaseName: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 52
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Database name'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeTargetUserName: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 80
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = 'User name'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
    object paeTargetPassword: TtiPerAwareComboBoxHistory
      Left = 8
      Top = 108
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 3
      Caption = 'Password'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        '')
      HistoryCount = 5
    end
  end
  object BitBtn1: TBitBtn
    Left = 240
    Top = 128
    Width = 85
    Height = 25
    Caption = 'Copy data >>'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object memoLog: TMemo
    Left = 8
    Top = 264
    Width = 545
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end

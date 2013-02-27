object FormEdit: TFormEdit
  Left = 403
  Top = 130
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Edit'
  ClientHeight = 234
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tiButtonPanel1: TtiButtonPanel
    Left = 0
    Top = 203
    Width = 249
    Height = 31
    OnBtn1Click = tiButtonPanel1Btn1Click
    OnBtn2Click = tiButtonPanel1Btn2Click
    Btn1Enabled = True
    Btn2Enabled = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 233
    Height = 101
    Caption = ' Database details '
    TabOrder = 0
    object paeConnectionName: TtiPerAwareEdit
      Left = 12
      Top = 20
      Width = 210
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = '&Connection name'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeDatabaseName: TtiPerAwareEdit
      Left = 12
      Top = 44
      Width = 210
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = '&Database name'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeHostName: TtiPerAwareEdit
      Left = 12
      Top = 68
      Width = 210
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = '&Host name'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 112
    Width = 233
    Height = 85
    Caption = ' Application connection details'
    TabOrder = 1
    object paeUserName: TtiPerAwareEdit
      Left = 12
      Top = 24
      Width = 210
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = '&User name'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeUserPassword: TtiPerAwareEdit
      Left = 12
      Top = 52
      Width = 210
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = '&Password'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
  end
end

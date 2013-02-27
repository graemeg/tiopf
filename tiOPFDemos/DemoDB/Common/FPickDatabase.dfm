object FormPickDatabase: TFormPickDatabase
  Left = 371
  Top = 234
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Enter database connection details'
  ClientHeight = 161
  ClientWidth = 488
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
  PixelsPerInch = 96
  TextHeight = 13
  object paePersistenceLayer: TtiPerAwareEdit
    Left = 8
    Top = 12
    Width = 313
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'P&ersistence layer name'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeDatabaseName: TtiPerAwareEdit
    Left = 8
    Top = 40
    Width = 313
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = '&Database name'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeUserName: TtiPerAwareEdit
    Left = 8
    Top = 68
    Width = 313
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = '&User name'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paePassword: TtiPerAwareEdit
    Left = 8
    Top = 96
    Width = 313
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = '&Password'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object btnSetupForIBX: TButton
    Left = 329
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Interbase'
    TabOrder = 4
    OnClick = btnSetupForIBXClick
  end
  object btnSetupForXML: TButton
    Left = 329
    Top = 40
    Width = 75
    Height = 25
    Caption = 'XML'
    TabOrder = 5
    OnClick = btnSetupForXMLClick
  end
  object btnSetupForCSV: TButton
    Left = 329
    Top = 68
    Width = 75
    Height = 25
    Caption = 'CSV'
    TabOrder = 6
    OnClick = btnSetupForCSVClick
  end
  object btnSetupForADOAccess: TButton
    Left = 328
    Top = 96
    Width = 75
    Height = 25
    Caption = 'ADO Access'
    TabOrder = 7
    OnClick = btnSetupForADOAccessClick
  end
  object btnSetupForRemote: TButton
    Left = 408
    Top = 12
    Width = 75
    Height = 25
    Caption = 'XML Remote'
    TabOrder = 8
    OnClick = btnSetupForRemoteClick
  end
end

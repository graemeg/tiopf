object Form2: TForm2
  Left = 197
  Top = 204
  Width = 634
  Height = 154
  Caption = ' Insert or delete data from a tiOPF database'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnInsertRow: TButton
    Left = 520
    Top = 40
    Width = 101
    Height = 25
    Caption = 'Insert row'
    TabOrder = 0
    OnClick = btnInsertRowClick
  end
  object btnDeleteRow: TButton
    Left = 520
    Top = 72
    Width = 101
    Height = 25
    Caption = 'Delete row'
    TabOrder = 1
    OnClick = btnDeleteRowClick
  end
  object btnClear: TButton
    Left = 520
    Top = 8
    Width = 101
    Height = 25
    Caption = 'Clear && New OID'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object paeOID: TtiPerAwareEdit
    Left = 184
    Top = 12
    Width = 329
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = 'OID'
    ReadOnly = False
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientName: TtiPerAwareEdit
    Left = 184
    Top = 40
    Width = 329
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 4
    Caption = 'Client name'
    ReadOnly = False
    MaxLength = 200
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientID: TtiPerAwareEdit
    Left = 184
    Top = 68
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
    Caption = 'Company ID'
    ReadOnly = False
    MaxLength = 9
    CharCase = ecNormal
    PasswordChar = #0
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 8
    Width = 161
    Height = 105
    TabStop = False
    Lines.Strings = (
      'This demo will show how to insert '
      'or delete data in a tiOPF database '
      'without using SQL. '
      ''
      'This is necessary if you want to '
      'share the same code to access '
      'SQL, XML or text file databases.')
  end
end

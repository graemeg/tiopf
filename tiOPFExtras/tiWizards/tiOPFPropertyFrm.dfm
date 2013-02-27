object frmProperty: TfrmProperty
  Left = 376
  Top = 228
  Width = 361
  Height = 419
  Caption = 'Property Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object edName: TEdit
    Left = 48
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object rgVisibility: TRadioGroup
    Left = 8
    Top = 40
    Width = 337
    Height = 49
    Caption = 'Visibility'
    Columns = 4
    ItemIndex = 3
    Items.Strings = (
      'Private'
      'Protected'
      'Published'
      'Public')
    TabOrder = 1
  end
  object gbType: TGroupBox
    Left = 8
    Top = 96
    Width = 337
    Height = 129
    Caption = 'Type'
    TabOrder = 2
    object rbInteger: TRadioButton
      Left = 256
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Integer'
      TabOrder = 12
      OnClick = rbTypeClick
    end
    object rbBoolean: TRadioButton
      Left = 8
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Boolean'
      TabOrder = 0
      OnClick = rbTypeClick
    end
    object rbByte: TRadioButton
      Left = 8
      Top = 40
      Width = 73
      Height = 17
      Caption = 'Byte'
      TabOrder = 1
      OnClick = rbTypeClick
    end
    object rbOther: TRadioButton
      Left = 8
      Top = 96
      Width = 57
      Height = 17
      Caption = 'Other'
      Checked = True
      TabOrder = 16
      TabStop = True
      OnClick = rbOtherClick
    end
    object edType: TEdit
      Left = 88
      Top = 96
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 17
    end
    object rbWord: TRadioButton
      Left = 8
      Top = 56
      Width = 73
      Height = 17
      Caption = 'Word'
      TabOrder = 2
      OnClick = rbTypeClick
    end
    object rbLongWord: TRadioButton
      Left = 8
      Top = 72
      Width = 73
      Height = 17
      Caption = 'LongWord'
      TabOrder = 3
      OnClick = rbTypeClick
    end
    object RadioButton1: TRadioButton
      Left = 256
      Top = 40
      Width = 73
      Height = 17
      Caption = 'Int64'
      TabOrder = 13
      OnClick = rbTypeClick
    end
    object RadioButton2: TRadioButton
      Left = 256
      Top = 56
      Width = 73
      Height = 17
      Caption = 'LongInt'
      TabOrder = 14
      OnClick = rbTypeClick
    end
    object RadioButton3: TRadioButton
      Left = 88
      Top = 24
      Width = 73
      Height = 17
      Caption = 'Currency'
      TabOrder = 4
      OnClick = rbTypeClick
    end
    object RadioButton4: TRadioButton
      Left = 88
      Top = 40
      Width = 73
      Height = 17
      Caption = 'Double'
      TabOrder = 5
      OnClick = rbTypeClick
    end
    object RadioButton5: TRadioButton
      Left = 88
      Top = 56
      Width = 73
      Height = 17
      Caption = 'Float'
      TabOrder = 6
      OnClick = rbTypeClick
    end
    object RadioButton6: TRadioButton
      Left = 88
      Top = 72
      Width = 73
      Height = 17
      Caption = 'Extended'
      TabOrder = 7
      OnClick = rbTypeClick
    end
    object RadioButton7: TRadioButton
      Left = 168
      Top = 24
      Width = 73
      Height = 17
      Caption = 'string'
      TabOrder = 8
      OnClick = rbTypeClick
    end
    object RadioButton8: TRadioButton
      Left = 168
      Top = 56
      Width = 73
      Height = 17
      Caption = 'PChar'
      TabOrder = 10
      OnClick = rbTypeClick
    end
    object RadioButton9: TRadioButton
      Left = 168
      Top = 72
      Width = 73
      Height = 17
      Caption = 'Char'
      TabOrder = 11
      OnClick = rbTypeClick
    end
    object RadioButton10: TRadioButton
      Left = 168
      Top = 40
      Width = 73
      Height = 17
      Caption = 'AnsiString'
      TabOrder = 9
      OnClick = rbTypeClick
    end
    object RadioButton11: TRadioButton
      Left = 256
      Top = 72
      Width = 73
      Height = 17
      Caption = 'Variant'
      TabOrder = 15
      OnClick = rbTypeClick
    end
  end
  object rgRead: TRadioGroup
    Left = 8
    Top = 232
    Width = 169
    Height = 113
    Caption = 'Read Access'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Field'
      'Method'
      'Method + Field')
    TabOrder = 3
  end
  object rgWrite: TRadioGroup
    Left = 184
    Top = 232
    Width = 161
    Height = 113
    Caption = 'Write Access'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Field'
      'Method'
      'Method + Field')
    TabOrder = 4
  end
  object btnCancel: TBitBtn
    Left = 184
    Top = 352
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 270
    Top = 352
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = btnOKClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
end

object EditAddressForm: TEditAddressForm
  Left = 265
  Top = 184
  Width = 317
  Height = 307
  Caption = 'Edit address'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object LCBType: TLabel
    Left = 18
    Top = 4
    Width = 32
    Height = 16
    Caption = 'T&ype'
    Color = clBtnFace
    FocusControl = CBType
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LENumber: TLabel
    Left = 18
    Top = 54
    Width = 48
    Height = 16
    Caption = 'N&umber'
    Color = clBtnFace
    FocusControl = ENumber
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LEStreet: TLabel
    Left = 113
    Top = 54
    Width = 35
    Height = 16
    Caption = '&Street'
    Color = clBtnFace
    FocusControl = EStreet
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LCBCity: TLabel
    Left = 18
    Top = 101
    Width = 22
    Height = 16
    Caption = '&City'
    Color = clBtnFace
    FocusControl = CBCity
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LETelephone1: TLabel
    Left = 18
    Top = 151
    Width = 83
    Height = 16
    Caption = 'Telephone #&1'
    Color = clBtnFace
    FocusControl = ETelephone1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LEFax: TLabel
    Left = 18
    Top = 200
    Width = 22
    Height = 16
    Caption = '&Fax'
    Color = clBtnFace
    FocusControl = EFax
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LETelephone2: TLabel
    Left = 160
    Top = 151
    Width = 83
    Height = 16
    Caption = 'Telephone #&2'
    Color = clBtnFace
    FocusControl = ETelephone2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object CBType: TComboBox
    Left = 18
    Top = 24
    Width = 262
    Height = 24
    AutoComplete = False
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    MaxLength = 65535
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    Text = 'CBType'
  end
  object ENumber: TEdit
    Left = 18
    Top = 72
    Width = 80
    Height = 23
    AutoSelect = False
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = -1
    ParentFont = False
    TabOrder = 1
    Text = 'ENumber'
  end
  object EStreet: TEdit
    Left = 114
    Top = 74
    Width = 166
    Height = 23
    AutoSelect = False
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = -1
    ParentFont = False
    TabOrder = 2
    Text = 'EStreet'
  end
  object CBCity: TComboBox
    Left = 18
    Top = 120
    Width = 260
    Height = 24
    AutoComplete = False
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    MaxLength = 65535
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 3
    Text = 'CBCity'
  end
  object ETelephone1: TEdit
    Left = 18
    Top = 168
    Width = 118
    Height = 23
    AutoSelect = False
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = -1
    ParentFont = False
    TabOrder = 4
    Text = 'ETelephone1'
  end
  object ETelephone2: TEdit
    Left = 160
    Top = 168
    Width = 118
    Height = 23
    AutoSelect = False
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = -1
    ParentFont = False
    TabOrder = 5
    Text = 'ETelephone2'
  end
  object EFax: TEdit
    Left = 18
    Top = 216
    Width = 118
    Height = 23
    AutoSelect = False
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = -1
    ParentFont = False
    TabOrder = 6
    Text = 'EFax'
  end
  object BCancel: TButton
    Left = 216
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 7
  end
  object BSave: TButton
    Left = 136
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Sa&ve'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
end

object EditAddressForm: TEditAddressForm
  Left = 265
  Top = 184
  BorderStyle = bsSingle
  Caption = 'Edit address'
  ClientHeight = 304
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    275
    304)
  PixelsPerInch = 96
  TextHeight = 16
  object LCBType: TLabel
    Left = 8
    Top = 8
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
    Left = 8
    Top = 60
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
    Left = 94
    Top = 60
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
    Left = 8
    Top = 111
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
    Left = 8
    Top = 163
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
    Left = 8
    Top = 214
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
    Left = 132
    Top = 163
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
    Left = 8
    Top = 30
    Width = 257
    Height = 24
    AutoComplete = False
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 65535
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    Text = 'CBType'
  end
  object ENumber: TEdit
    Left = 8
    Top = 82
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
    Left = 94
    Top = 82
    Width = 171
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
    Left = 8
    Top = 133
    Width = 257
    Height = 24
    AutoComplete = False
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 65535
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 3
    Text = 'CBCity'
  end
  object ETelephone1: TEdit
    Left = 8
    Top = 185
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
    Left = 132
    Top = 185
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
    Left = 8
    Top = 236
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
    Left = 192
    Top = 271
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
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
  object BOK: TButton
    Left = 111
    Top = 271
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 8
  end
end

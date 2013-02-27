object XmlRpcObjectLockingClientMainForm: TXmlRpcObjectLockingClientMainForm
  Left = 350
  Top = 227
  Width = 316
  Height = 205
  Caption = 'XmlRpcObjectLockingClientMainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 68
    Height = 13
    Caption = 'Object Identity'
  end
  object ObjectLockLabel: TLabel
    Left = 8
    Top = 96
    Width = 58
    Height = 13
    Caption = 'Object Lock'
  end
  object HostNameEdit: TEdit
    Left = 80
    Top = 22
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object ObjectIdentityEdit: TEdit
    Left = 80
    Top = 62
    Width = 209
    Height = 21
    TabOrder = 1
    Text = 'OID:12345'
  end
  object Button1: TButton
    Left = 216
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Get Lock'
    TabOrder = 2
    OnClick = Button1Click
  end
  object ObjectLockEdit: TEdit
    Left = 80
    Top = 94
    Width = 209
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
end

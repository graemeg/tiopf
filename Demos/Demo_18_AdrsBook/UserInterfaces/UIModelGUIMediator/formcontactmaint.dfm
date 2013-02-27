object ContactMaintFrm: TContactMaintFrm
  Left = 346
  Top = 197
  Width = 316
  Height = 421
  ActiveControl = edFName
  Caption = 'Contact Maintenance'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 48
    Height = 13
    Caption = 'Firstname:'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 12
    Top = 64
    Width = 49
    Height = 13
    Caption = 'Lastname:'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 12
    Top = 112
    Width = 29
    Height = 13
    Caption = 'EMail:'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
  end
  object Label4: TLabel
    Left = 12
    Top = 165
    Width = 34
    Height = 13
    Caption = 'Mobile:'
    Color = clBtnFace
    ParentColor = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 12
    Top = 224
    Width = 52
    Height = 13
    Caption = 'Comments:'
    Color = clNone
    ParentColor = False
    Transparent = True
  end
  object edFName: TEdit
    Left = 12
    Top = 32
    Width = 231
    Height = 21
    TabOrder = 0
    Text = 'edFName'
    OnChange = edFNameChange
  end
  object edLName: TEdit
    Left = 12
    Top = 80
    Width = 231
    Height = 21
    TabOrder = 1
    Text = 'edLName'
    OnChange = edLNameChange
  end
  object edEmail: TEdit
    Left = 12
    Top = 128
    Width = 231
    Height = 21
    TabOrder = 2
    Text = 'edEmail'
    OnChange = edEmailChange
  end
  object edMobile: TEdit
    Left = 12
    Top = 184
    Width = 165
    Height = 21
    TabOrder = 3
    Text = 'edMobile'
    OnChange = edMobileChange
  end
  object meComments: TMemo
    Left = 12
    Top = 240
    Width = 284
    Height = 90
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    OnEnter = meCommentsEnter
  end
  object btnSave: TButton
    Left = 144
    Top = 345
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 221
    Top = 345
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end

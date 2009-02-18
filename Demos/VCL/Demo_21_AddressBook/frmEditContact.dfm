object ContactEditForm: TContactEditForm
  Left = 231
  Top = 180
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Contact edit form'
  ClientHeight = 330
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object LEFirstName: TLabel
    Left = 10
    Top = 7
    Width = 62
    Height = 16
    Caption = '&First name'
    Color = clBtnFace
    FocusControl = EFirstName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LELastName: TLabel
    Left = 10
    Top = 56
    Width = 62
    Height = 16
    Caption = '&Last name'
    Color = clBtnFace
    FocusControl = ELastName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LEEmail: TLabel
    Left = 10
    Top = 104
    Width = 34
    Height = 16
    Caption = '&Email'
    Color = clBtnFace
    FocusControl = EEmail
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LEMobile: TLabel
    Left = 10
    Top = 152
    Width = 41
    Height = 16
    Caption = '&Mobile'
    Color = clBtnFace
    FocusControl = EMobile
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LMComments: TLabel
    Left = 10
    Top = 200
    Width = 64
    Height = 16
    Caption = '&Comments'
    Color = clBtnFace
    FocusControl = MComments
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 256
    Top = 10
    Width = 69
    Height = 16
    Caption = 'Add&resses:'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object EFirstName: TEdit
    Left = 10
    Top = 24
    Width = 206
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
    TabOrder = 0
    Text = 'EFirstName'
  end
  object ELastName: TEdit
    Left = 10
    Top = 72
    Width = 206
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
    Text = 'ELastName'
  end
  object EEmail: TEdit
    Left = 10
    Top = 120
    Width = 206
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
    Text = 'EEmail'
  end
  object EMobile: TEdit
    Left = 10
    Top = 168
    Width = 206
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
    TabOrder = 3
    Text = 'EMobile'
  end
  object MComments: TMemo
    Left = 10
    Top = 216
    Width = 206
    Height = 104
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    MaxLength = -1
    ParentFont = False
    TabOrder = 4
  end
  object LVAddresses: TListView
    Left = 256
    Top = 32
    Width = 267
    Height = 115
    Columns = <>
    TabOrder = 5
    ViewStyle = vsList
  end
  object BAdd: TButton
    Left = 258
    Top = 158
    Width = 75
    Height = 25
    Caption = '&Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object Button2: TButton
    Left = 448
    Top = 158
    Width = 75
    Height = 25
    Caption = '&Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = BDeleteClick
  end
  object BEdit: TButton
    Left = 352
    Top = 158
    Width = 75
    Height = 25
    Caption = 'Ed&it'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = BEditClick
  end
  object BCancel: TButton
    Left = 440
    Top = 295
    Width = 83
    Height = 25
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 9
  end
  object BSave: TButton
    Left = 352
    Top = 296
    Width = 80
    Height = 25
    Caption = '&Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 10
  end
end

object ContactEditForm: TContactEditForm
  Left = 541
  Top = 502
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Contact edit form'
  ClientHeight = 355
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  DesignSize = (
    508
    355)
  PixelsPerInch = 96
  TextHeight = 16
  object LEFirstName: TLabel
    Left = 8
    Top = 8
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
    Left = 8
    Top = 59
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
    Left = 8
    Top = 162
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
    Left = 8
    Top = 213
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
    Left = 232
    Top = 8
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
    Left = 232
    Top = 140
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
  object lblDOB: TLabel
    Left = 8
    Top = 110
    Width = 74
    Height = 16
    Caption = 'Date Of &Birth'
    FocusControl = dtpDOB
  end
  object EFirstName: TEdit
    Left = 8
    Top = 30
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
    Left = 8
    Top = 81
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
    Left = 8
    Top = 184
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
    Text = 'EEmail'
  end
  object EMobile: TEdit
    Left = 8
    Top = 235
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
    TabOrder = 4
    Text = 'EMobile'
  end
  object MComments: TMemo
    Left = 232
    Top = 30
    Width = 267
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
    TabOrder = 5
  end
  object LVAddresses: TListView
    Left = 232
    Top = 162
    Width = 267
    Height = 115
    Columns = <>
    TabOrder = 6
    ViewStyle = vsList
  end
  object BAdd: TButton
    Left = 232
    Top = 283
    Width = 75
    Height = 25
    Caption = '&Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = BAddClick
  end
  object Button2: TButton
    Left = 394
    Top = 283
    Width = 75
    Height = 25
    Caption = '&Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = BDeleteClick
  end
  object BEdit: TButton
    Left = 313
    Top = 283
    Width = 75
    Height = 25
    Caption = 'Ed&it'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = BEditClick
  end
  object BCancel: TButton
    Left = 417
    Top = 322
    Width = 83
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
    TabOrder = 10
  end
  object BOK: TButton
    Left = 331
    Top = 322
    Width = 80
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
    TabOrder = 11
  end
  object dtpDOB: TDateTimePicker
    Left = 8
    Top = 132
    Width = 97
    Height = 24
    Date = 39926.603568819460000000
    Time = 39926.603568819460000000
    TabOrder = 2
  end
end

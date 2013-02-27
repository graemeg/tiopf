object EditCountryForm: TEditCountryForm
  Left = 280
  Top = 195
  BorderStyle = bsSingle
  Caption = 'Edit Country'
  ClientHeight = 149
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  DesignSize = (
    313
    149)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 16
    Caption = '&ISO code'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LEName: TLabel
    Left = 8
    Top = 59
    Width = 85
    Height = 16
    Caption = 'Country &Name'
    Color = clBtnFace
    FocusControl = EName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object EISo: TEdit
    Left = 8
    Top = 30
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
    TabOrder = 0
    Text = 'EISo'
  end
  object EName: TEdit
    Left = 8
    Top = 81
    Width = 296
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
    Text = 'EName'
  end
  object BCancel: TButton
    Left = 230
    Top = 116
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
    TabOrder = 2
    ExplicitLeft = 253
  end
  object BOK: TButton
    Left = 149
    Top = 116
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
    TabOrder = 3
    ExplicitLeft = 172
  end
end

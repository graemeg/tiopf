object EditCityForm: TEditCityForm
  Left = 265
  Top = 210
  BorderStyle = bsSingle
  Caption = 'Edit city'
  ClientHeight = 201
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    265
    201)
  PixelsPerInch = 96
  TextHeight = 16
  object LEName: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 16
    Caption = 'Cit&y name'
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
  object Label2: TLabel
    Left = 8
    Top = 59
    Width = 53
    Height = 16
    Caption = '&Zip code'
    Color = clBtnFace
    FocusControl = EZip
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LCBCountry: TLabel
    Left = 8
    Top = 110
    Width = 45
    Height = 16
    Caption = 'Co&untry'
    Color = clBtnFace
    FocusControl = CBCountry
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object EName: TEdit
    Left = 8
    Top = 30
    Width = 247
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
    Text = 'EName'
  end
  object EZip: TEdit
    Left = 8
    Top = 81
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
    Text = 'EZip'
  end
  object CBCountry: TComboBox
    Left = 8
    Top = 132
    Width = 247
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
    TabOrder = 2
    Text = 'CBCountry'
  end
  object BOK: TButton
    Left = 101
    Top = 168
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
  end
  object BCancel: TButton
    Left = 182
    Top = 168
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
    TabOrder = 4
  end
end

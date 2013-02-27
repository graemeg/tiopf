object Form2: TForm2
  Left = 550
  Top = 203
  Width = 273
  Height = 156
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object paeThreadCount: TtiPerAwareFloatEdit
    Left = 12
    Top = 16
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Thread count'
    ReadOnly = False
    ValueAsString = '5'
    Value = 5.000000000000000000
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesInteger
  end
  object btnStart: TButton
    Left = 99
    Top = 89
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 183
    Top = 89
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object paeCycles: TtiPerAwareFloatEdit
    Left = 12
    Top = 44
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = 'Cycles'
    ReadOnly = False
    ValueAsString = '100'
    Value = 100.000000000000000000
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesInteger
  end
end

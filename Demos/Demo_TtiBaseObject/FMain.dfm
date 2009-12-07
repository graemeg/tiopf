object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TtiBaseObject demo'
  ClientHeight = 282
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 75
    Width = 161
    Height = 94
    Caption = 'Performance'
    TabOrder = 0
    object NoReferenceCountingPerformance: TButton
      Left = 8
      Top = 24
      Width = 137
      Height = 25
      Caption = 'No reference counting'
      TabOrder = 0
      OnClick = PerformanceTestNoReferenceCounting
    end
    object btnReferenceCountingPerformance: TButton
      Left = 8
      Top = 55
      Width = 137
      Height = 25
      Caption = 'Reference counting'
      TabOrder = 1
      OnClick = PerformanceTestReferenceCounting
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 161
    Height = 61
    Caption = 'TtiBaseObject.TestValid'
    TabOrder = 1
    object btnTestValid: TButton
      Left = 8
      Top = 23
      Width = 141
      Height = 25
      Caption = 'TtiBaseObject.TestValid'
      TabOrder = 0
      OnClick = btnTestValidClick
    end
  end
  object Memo1: TMemo
    Left = 175
    Top = 16
    Width = 174
    Height = 89
    Lines.Strings = (
      'Toggle the conditional defines '
      'REFERENCE_COUNTING and '
      'OBJECT_TRACKING on and off to '
      'examine behaviour.')
    TabOrder = 2
  end
  object memoLog: TMemo
    Left = 8
    Top = 175
    Width = 341
    Height = 99
    TabOrder = 3
  end
end

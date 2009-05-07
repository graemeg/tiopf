object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 348
  ClientWidth = 535
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
  object cxTextEdit: TcxTextEdit
    Left = 56
    Top = 39
    TabOrder = 0
    Text = 'cxTextEdit'
    Width = 121
  end
  object bDebug: TcxButton
    Left = 56
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Debug'
    TabOrder = 1
    OnClick = bDebugClick
  end
  object cxCheckBox: TcxCheckBox
    Left = 56
    Top = 66
    Caption = 'cxCheckBox'
    ParentBackground = False
    ParentColor = False
    Style.Color = clBtnFace
    TabOrder = 2
    Width = 121
  end
  object cxComboBox: TcxComboBox
    Left = 56
    Top = 93
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 3
    Text = 'cxComboBox'
    Width = 121
  end
  object cxItemComboBox: TcxComboBox
    Left = 56
    Top = 120
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 4
    Text = 'cxItemComboBox'
    Width = 121
  end
  object cxDynamicComboBox: TcxComboBox
    Left = 56
    Top = 147
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 5
    Text = 'cxDynamicComboBox'
    Width = 121
  end
  object cxLabel: TcxLabel
    Left = 56
    Top = 174
    AutoSize = False
    Caption = 'cxLabel'
    Height = 17
    Width = 121
  end
  object cxTrackBar: TcxTrackBar
    Left = 56
    Top = 197
    TabOrder = 7
    Height = 20
    Width = 121
  end
  object cxMemo: TcxMemo
    Left = 248
    Top = 8
    Lines.Strings = (
      'cxMemo')
    TabOrder = 8
    Height = 89
    Width = 185
  end
  object cxDateEdit: TcxDateEdit
    Left = 328
    Top = 143
    EditValue = 39938.6979166667d
    TabOrder = 9
    Width = 121
  end
  object cxSpinEdit: TcxSpinEdit
    Left = 56
    Top = 223
    TabOrder = 10
    Width = 121
  end
end

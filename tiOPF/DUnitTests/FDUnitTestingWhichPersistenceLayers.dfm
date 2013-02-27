object FormWhichPersistenceLayers: TFormWhichPersistenceLayers
  Left = 455
  Top = 261
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Which persistence layers?'
  ClientHeight = 217
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 12
    Top = 24
    Width = 237
    Height = 5
    Shape = bsTopLine
  end
  object btnPnl: TtiButtonPanel
    Left = 0
    Top = 182
    Width = 261
    Height = 35
    OnBtn1Click = btnPnlBtn1Click
    OnBtn2Click = btnPnlBtn2Click
    Btn1Enabled = True
    Btn2Enabled = True
  end
  object pnlCheckBoxes: TPanel
    Left = 12
    Top = 72
    Width = 237
    Height = 104
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
  end
  object cbTestNonPersistentClasses: TCheckBox
    Left = 12
    Top = 4
    Width = 165
    Height = 17
    Caption = 'Test non-persistent classes?'
    TabOrder = 2
    OnClick = cbTestNonPersistentClassesClick
  end
  object rgTestPersistenceLayers: TRadioGroup
    Left = 12
    Top = 28
    Width = 237
    Height = 37
    Caption = ' Test persistence layers '
    Columns = 2
    Items.Strings = (
      'One at the time'
      'All at the same time')
    TabOrder = 3
    OnClick = rgTestPersistenceLayersClick
  end
  object Tmr: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = TmrTimer
    Left = 16
    Top = 72
  end
end

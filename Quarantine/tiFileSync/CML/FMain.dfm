object Form1: TForm1
  Left = 413
  Top = 264
  Width = 477
  Height = 345
  BorderIcons = [biSystemMenu]
  Caption = ' OPDMS Deployment Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    469
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TtiMemoReadOnly
    Left = 156
    Top = 8
    Width = 304
    Height = 165
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Getting the latest version the OPDMS'
      '')
    ScrollBars = ssVertical
  end
  object PB: TProgressBar
    Left = 156
    Top = 184
    Width = 308
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Smooth = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 44
    Top = 240
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end

object FormProgress: TFormProgress
  Left = 399
  Top = 225
  Width = 382
  Height = 253
  BorderIcons = [biMinimize, biMaximize]
  Caption = ' File synchronise progress'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    374
    219)
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TMemo
    Left = 4
    Top = 8
    Width = 366
    Height = 149
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    WordWrap = False
  end
  object PB: TProgressBar
    Left = 4
    Top = 167
    Width = 369
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
  end
  object bbCancel: TBitBtn
    Left = 152
    Top = 189
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = bbCancelClick
    Kind = bkCancel
  end
end

object FormAppLaunch: TFormAppLaunch
  Left = 287
  Top = 266
  Width = 454
  Height = 370
  BorderIcons = []
  Caption = ' TechInsite Application Launcher'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    446
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object Animate1: TAnimate
    Left = 8
    Top = 0
    Width = 429
    Height = 60
    Anchors = [akLeft, akTop, akRight]
    CommonAVI = aviCopyFiles
    StopFrame = 34
  end
  object mLog: TMemo
    Left = 8
    Top = 64
    Width = 429
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end

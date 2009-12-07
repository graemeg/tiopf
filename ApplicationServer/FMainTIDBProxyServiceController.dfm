object Form1: TForm1
  Left = 541
  Top = 270
  Width = 447
  Height = 174
  Caption = 'tiDBProxyService Controller'
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
  object lblIsRunning: TLabel
    Left = 12
    Top = 16
    Width = 309
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblIsRunning'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 12
    Top = 112
    Width = 306
    Height = 13
    Caption = 'You can also control the service from Windows Service Manager'
  end
  object btnStartStop: TButton
    Left = 335
    Top = 47
    Width = 95
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Stop service'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = btnStartStopClick
  end
  object btnInstallUnInstall: TButton
    Left = 335
    Top = 16
    Width = 95
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Install service'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = btnInstallUnInstallClick
  end
  object btnClose: TButton
    Left = 335
    Top = 108
    Width = 95
    Height = 25
    Hint = 'Close the service manager'
    Anchors = [akTop, akRight]
    Caption = 'Close'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnCloseClick
  end
end

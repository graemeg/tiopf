object Form1: TForm1
  Left = 205
  Top = 311
  Width = 387
  Height = 218
  Caption = 'TtiLog demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 212
    Top = 56
    Width = 82
    Height = 13
    Caption = 'No of times to &log'
    FocusControl = seLog
  end
  object Label2: TLabel
    Left = 212
    Top = 84
    Width = 64
    Height = 13
    Caption = 'No of &threads'
    FocusControl = seThreads
  end
  object Button1: TButton
    Left = 216
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Run'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object seLog: TSpinEdit
    Left = 304
    Top = 52
    Width = 65
    Height = 22
    Increment = 10
    MaxValue = 10000
    MinValue = 1
    TabOrder = 1
    Value = 100
  end
  object seThreads: TSpinEdit
    Left = 304
    Top = 80
    Width = 65
    Height = 22
    Increment = 5
    MaxValue = 100
    MinValue = 1
    TabOrder = 2
    Value = 10
  end
  object Memo1: TMemo
    Left = 4
    Top = 4
    Width = 197
    Height = 184
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Thread safe, logging demonstration.'
      ''
      'Pass the following command line '
      'parameters to activate logging:'
      ''
      '-l to turn logging to a file on'
      ''
      '-lv to turn visual logging on'
      ''
      'If logging to a file,'
      'C:\Temp\log\<ApplicationName>.log '
      'will be created.')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnLogError: TButton
    Left = 296
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Log an error'
    TabOrder = 4
    OnClick = btnLogErrorClick
  end
  object btnViewLogFile: TButton
    Left = 216
    Top = 152
    Width = 75
    Height = 25
    Caption = 'View log file'
    TabOrder = 5
    OnClick = btnViewLogFileClick
  end
end

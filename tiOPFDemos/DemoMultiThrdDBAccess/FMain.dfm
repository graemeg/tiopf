object Form1: TForm1
  Left = 388
  Top = 319
  Width = 262
  Height = 216
  Caption = 'Form1'
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
  object Button1: TButton
    Left = 20
    Top = 16
    Width = 205
    Height = 37
    Hint = 
      'Read single instance of TAdrsBook in the application'#39's main thre' +
      'ad'
    Caption = 'Read single instance of TAdrsBook'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 20
    Top = 64
    Width = 205
    Height = 37
    Hint = 'Read multiple instances of TAdrsBook inside seperate threads'
    Caption = 'Read multiple instances of TAdrsBook'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object paeThreadCount: TtiPerAwareFloatEdit
    Left = 20
    Top = 160
    Width = 201
    Height = 23
    Hint = 
      'Enter the number of copies of TAdrsBook to read, each in its own' +
      ' thread'
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    ShowHint = True
    Caption = 'Number of threads'
    LabelWidth = 110
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1
    IsKnown = True
    Style = fesInteger
  end
  object Button3: TButton
    Left = 20
    Top = 112
    Width = 205
    Height = 37
    Hint = 
      'Read TAdrsBook with progress bar inside seperate threads (which ' +
      'show a progress bar)'
    Caption = 'Read TAdrsBook with progress bar'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = Button3Click
  end
end

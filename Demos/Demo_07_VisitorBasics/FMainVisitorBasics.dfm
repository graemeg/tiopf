object FormMainVisitorBasics: TFormMainVisitorBasics
  Left = 438
  Top = 183
  Caption = ' Visitor Basics'
  ClientHeight = 116
  ClientWidth = 174
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
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddClient: TButton
    Left = 40
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Add client'
    TabOrder = 0
    OnClick = btnAddClientClick
  end
  object btnShowList: TButton
    Left = 40
    Top = 48
    Width = 105
    Height = 25
    Caption = 'Show list'
    TabOrder = 1
    OnClick = btnShowListClick
  end
  object btnRunClientVisitor: TButton
    Left = 40
    Top = 80
    Width = 105
    Height = 25
    Caption = 'Run client visitor'
    TabOrder = 2
    OnClick = btnRunClientVisitorClick
  end
end

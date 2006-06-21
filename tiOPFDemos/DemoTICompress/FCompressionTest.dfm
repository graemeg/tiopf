object Form1: TForm1
  Left = 230
  Top = 151
  BorderStyle = bsDialog
  Caption = ' Compression test'
  ClientHeight = 164
  ClientWidth = 213
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
    Left = 64
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object mText: TMemo
    Left = 12
    Top = 8
    Width = 185
    Height = 113
    Lines.Strings = (
      'If you see this dialog, the stream '
      'compression test probably worked.')
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
end

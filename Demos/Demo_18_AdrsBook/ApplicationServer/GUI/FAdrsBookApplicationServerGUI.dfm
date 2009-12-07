object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 201
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnStartStop: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    TabOrder = 0
    OnClick = btnStartStopClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 39
    Width = 661
    Height = 154
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end

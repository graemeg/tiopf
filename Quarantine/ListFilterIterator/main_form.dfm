object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Iterator Demo'
  ClientHeight = 428
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    305
    428)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFilter: TLabel
    Left = 8
    Top = 315
    Width = 84
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Enter Filter Below'
    ExplicitTop = 278
  end
  object memOutput: TMemo
    Left = 8
    Top = 8
    Width = 289
    Height = 225
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnIterate: TButton
    Left = 141
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Iterate'
    TabOrder = 1
    OnClick = btnIterateClick
    ExplicitTop = 277
  end
  object btnClose: TButton
    Left = 222
    Top = 397
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
    ExplicitTop = 277
  end
  object memFilter: TMemo
    Left = 8
    Top = 334
    Width = 289
    Height = 57
    Anchors = [akLeft, akBottom]
    Lines.Strings = (
      'FirstName LIKE Bev*')
    TabOrder = 3
  end
  object btn1: TButton
    Left = 8
    Top = 239
    Width = 289
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Reference Counted Iterator'
    TabOrder = 4
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 8
    Top = 270
    Width = 289
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'StopOnFail, Sorted Iterator'
    TabOrder = 5
    OnClick = btn2Click
  end
end

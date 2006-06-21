object XMLRPCServerForm: TXMLRPCServerForm
  Left = 384
  Top = 265
  Width = 208
  Height = 258
  Caption = 'XML-RPC Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Stop Server'
    TabOrder = 1
    OnClick = Button2Click
  end
end

object XMLRPCServerForm: TXMLRPCServerForm
  Left = 644
  Top = 253
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
    Action = StartAction
    TabOrder = 0
  end
  object Button2: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Action = StopAction
    TabOrder = 1
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 16
    Top = 88
    object StartAction: TAction
      Caption = 'Start'
      OnExecute = StartActionExecute
    end
    object StopAction: TAction
      Caption = 'Stop'
      OnExecute = StopActionExecute
    end
  end
end

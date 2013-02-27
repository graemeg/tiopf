object ContractAdditionalAmountForm: TContractAdditionalAmountForm
  Left = 404
  Top = 260
  Width = 278
  Height = 168
  Caption = 'Additional Amount'
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
  object Label2: TLabel
    Left = 8
    Top = 27
    Width = 54
    Height = 13
    Caption = 'Amount    $'
  end
  object AdditionalAmountEdit: TEdit
    Left = 67
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'AdditionalAmountEdit'
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 100
    Width = 270
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object CancelButton: TButton
      Left = 184
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKButton: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end

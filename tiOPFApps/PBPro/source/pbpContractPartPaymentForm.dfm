object ContractPartPaymentForm: TContractPartPaymentForm
  Left = 332
  Top = 265
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Part Payment'
  ClientHeight = 235
  ClientWidth = 348
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
  object Label3: TLabel
    Left = 14
    Top = 124
    Width = 57
    Height = 13
    Caption = 'Value        $'
  end
  object Label4: TLabel
    Left = 14
    Top = 100
    Width = 32
    Height = 13
    Caption = 'Details'
  end
  object Label1: TLabel
    Left = 14
    Top = 78
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object PaymentTypesEdit: TEdit
    Left = 149
    Top = 80
    Width = 121
    Height = 21
    ParentColor = True
    TabOrder = 5
    Text = 'PaymentTypesEdit'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 348
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 8
      Width = 151
      Height = 13
      Caption = 'Enter details for a new payment.'
    end
    object Bevel1: TBevel
      Left = 0
      Top = -1
      Width = 348
      Height = 50
      Align = alBottom
      Shape = bsBottomLine
    end
  end
  object PaymentTypesComboBox: TComboBox
    Left = 77
    Top = 72
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = PaymentTypesComboBoxChange
  end
  object DetailsEdit: TEdit
    Left = 77
    Top = 96
    Width = 249
    Height = 21
    TabOrder = 2
    Text = 'DetailsEdit'
  end
  object ValueEdit: TEdit
    Left = 77
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'ValueEdit'
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 199
    Width = 348
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object OKButton: TButton
      Left = 179
      Top = 4
      Width = 75
      Height = 25
      Action = OKAction
      Anchors = [akTop, akRight]
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 266
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 72
    Top = 168
    object OKAction: TAction
      Caption = 'OK'
      OnExecute = OKActionExecute
    end
  end
end

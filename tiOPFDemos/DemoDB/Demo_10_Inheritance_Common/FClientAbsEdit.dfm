inherited FormClientAbsEdit: TFormClientAbsEdit
  Caption = 'FormClientAbsEdit'
  ClientHeight = 208
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel [0]
    Left = 8
    Top = 68
    Width = 357
    Height = 13
    Shape = bsTopLine
  end
  inherited btnOK: TBitBtn
    Left = 211
    Top = 178
    TabOrder = 3
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 178
    TabOrder = 4
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 182
    TabOrder = 2
  end
  object paeOID: TtiPerAwareEdit [4]
    Left = 10
    Top = 12
    Width = 357
    Height = 23
    ShowFocusRect = False
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'OID'
    ReadOnly = True
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientID: TtiPerAwareEdit [5]
    Left = 10
    Top = 40
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = 'Client ID'
    ReadOnly = False
    MaxLength = 9
    CharCase = ecNormal
    PasswordChar = #0
  end
  object memoErrors: TtiMemoReadOnly [6]
    Left = 8
    Top = 100
    Width = 357
    Height = 69
    TabStop = False
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  inherited RO: TtiReadOnly
    Left = 172
    Top = 172
  end
end

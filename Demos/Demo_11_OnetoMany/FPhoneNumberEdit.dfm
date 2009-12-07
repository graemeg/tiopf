inherited FormPhoneNumberEdit: TFormPhoneNumberEdit
  Caption = 'FormPhoneNumberEdit'
  ClientHeight = 208
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 211
    Top = 178
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 178
    TabOrder = 5
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 182
    TabOrder = 3
  end
  object paeOID: TtiPerAwareEdit
    Left = 10
    Top = 12
    Width = 357
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'OID'
    ReadOnly = True
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeNumberType: TtiPerAwareEdit
    Left = 10
    Top = 40
    Width = 223
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = 'Number type'
    ReadOnly = False
    MaxLength = 20
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeNumberText: TtiPerAwareEdit
    Left = 10
    Top = 68
    Width = 223
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = 'Number text'
    ReadOnly = False
    MaxLength = 19
    CharCase = ecNormal
    PasswordChar = #0
  end
  object memoErrors: TtiMemoReadOnly
    Left = 8
    Top = 100
    Width = 357
    Height = 69
    TabStop = False
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
end

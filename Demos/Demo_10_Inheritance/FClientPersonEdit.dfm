inherited FormClientPersonEdit: TFormClientPersonEdit
  Left = 388
  Top = 301
  Caption = 'FormClientPersonEdit'
  ClientHeight = 244
  ExplicitHeight = 276
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Top = 214
    TabOrder = 5
    ExplicitTop = 246
  end
  inherited btnCancel: TBitBtn
    Top = 214
    TabOrder = 6
    ExplicitTop = 246
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 218
    TabOrder = 4
    ExplicitTop = 250
  end
  inherited paeClientID: TtiPerAwareEdit
    Width = 175
    CharCase = ecUpperCase
    ExplicitWidth = 175
  end
  inherited memoErrors: TtiMemoReadOnly
    Left = 6
    Top = 134
    ExplicitLeft = 6
    ExplicitTop = 164
  end
  object paeGivenName: TtiPerAwareEdit
    Left = 10
    Top = 76
    Width = 357
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = '&Given name'
    ReadOnly = False
    MaxLength = 40
    CharCase = ecUpperCase
    PasswordChar = #0
  end
  object paeFamilyName: TtiPerAwareEdit
    Left = 10
    Top = 104
    Width = 357
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = '&Family name'
    ReadOnly = False
    MaxLength = 40
    CharCase = ecUpperCase
    PasswordChar = #0
  end
end

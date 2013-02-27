inherited FormClientPersonEdit: TFormClientPersonEdit
  Left = 388
  Top = 301
  Caption = 'FormClientPersonEdit'
  ClientHeight = 276
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Top = 246
    TabOrder = 6
  end
  inherited btnCancel: TBitBtn
    Top = 246
    TabOrder = 7
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 250
    TabOrder = 5
  end
  inherited paeClientID: TtiPerAwareEdit
    Width = 175
    CharCase = ecUpperCase
  end
  inherited memoErrors: TtiMemoReadOnly
    Top = 166
  end
  object paeGivenName: TtiPerAwareEdit [7]
    Left = 10
    Top = 104
    Width = 357
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = '&Given name'
    ReadOnly = False
    MaxLength = 0
    CharCase = ecUpperCase
    PasswordChar = #0
  end
  object paeFamilyName: TtiPerAwareEdit [8]
    Left = 10
    Top = 132
    Width = 357
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 4
    Caption = '&Family name'
    ReadOnly = False
    MaxLength = 0
    CharCase = ecUpperCase
    PasswordChar = #0
  end
  object paeNameTitle: TtiPerAwareComboBoxStatic [9]
    Left = 10
    Top = 76
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = 'Name &title'
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
  end
end

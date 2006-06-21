inherited FormClientCompanyEdit: TFormClientCompanyEdit
  Caption = 'FormClientCompanyEdit'
  ClientHeight = 216
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Top = 186
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Top = 186
    TabOrder = 5
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 190
  end
  inherited paeClientID: TtiPerAwareEdit
    CharCase = ecUpperCase
  end
  inherited memoErrors: TtiMemoReadOnly
    Top = 108
  end
  object paeCompanyName: TtiPerAwareEdit [7]
    Left = 10
    Top = 76
    Width = 357
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = 'Company name'
    ReadOnly = False
    MaxLength = 0
    CharCase = ecUpperCase
    PasswordChar = #0
  end
end

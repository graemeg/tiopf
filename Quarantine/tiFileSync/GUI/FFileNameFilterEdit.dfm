inherited FormFileNameFilterEdit: TFormFileNameFilterEdit
  Caption = 'Edit a file name filter'
  ClientHeight = 102
  ClientWidth = 203
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 43
    Top = 72
  end
  inherited btnCancel: TBitBtn
    Left = 123
    Top = 72
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 76
  end
  object paeFilterType: TtiPerAwareComboBoxStatic [3]
    Left = 8
    Top = 12
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = '&Filter type'
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
  end
  object paeWildCard: TtiPerAwareEdit [4]
    Left = 8
    Top = 40
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 4
    Caption = '&Wild card'
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  inherited RO: TtiReadOnly
    Left = 4
    Top = 68
  end
end

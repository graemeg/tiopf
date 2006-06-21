inherited FormEditListMember: TFormEditListMember
  Caption = 'Edit a list member'
  ClientHeight = 87
  ClientWidth = 319
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 159
    Top = 57
  end
  inherited btnCancel: TBitBtn
    Left = 239
    Top = 57
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 61
  end
  object paeEMailAddress: TtiPerAwareEdit [3]
    Left = 8
    Top = 8
    Width = 305
    Height = 41
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    LabelStyle = lsTopLeft
    Caption = 'Enter label &name'
    ReadOnly = False
    MaxLength = 255
    CharCase = ecNormal
    PasswordChar = #0
  end
  inherited RO: TtiReadOnly
    Left = 32
    Top = 52
  end
end

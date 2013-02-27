inherited FormEditParam: TFormEditParam
  Caption = ' Edit a parameter'
  ClientWidth = 288
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 128
  end
  inherited btnCancel: TBitBtn
    Left = 208
  end
  object paeName: TtiPerAwareEdit [3]
    Left = 8
    Top = 8
    Width = 173
    Height = 39
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    LabelStyle = lsTop
    Caption = '&Name'
    ReadOnly = False
    MaxLength = 30
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeType: TtiPerAwareComboBoxStatic [4]
    Left = 8
    Top = 54
    Width = 173
    Height = 39
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 4
    LabelStyle = lsTop
    Caption = '&Type'
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
  end
  object paeValue: TtiPerAwareEdit [5]
    Left = 8
    Top = 100
    Width = 169
    Height = 39
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
    LabelStyle = lsTop
    Caption = '&Value'
    ReadOnly = False
    MaxLength = 50
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeIsNull: TtiPerAwareCheckBox [6]
    Left = 244
    Top = 100
    Width = 33
    Height = 37
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 6
    LabelStyle = lsTop
    Caption = 'N&ull ?'
    ReadOnly = False
    Value = False
  end
  object paeNameAllCaps: TtiPerAwareCheckBox [7]
    Left = 188
    Top = 8
    Width = 53
    Height = 39
    ShowFocusRect = True
    Constraints.MinHeight = 17
    TabOrder = 7
    LabelStyle = lsTop
    Caption = 'All caps?'
    ReadOnly = False
    OnChange = paeNameAllCapsChange
    Value = False
  end
  object paeValueAllCaps: TtiPerAwareCheckBox [8]
    Left = 184
    Top = 100
    Width = 53
    Height = 39
    ShowFocusRect = True
    Constraints.MinHeight = 17
    TabOrder = 8
    LabelStyle = lsTop
    Caption = 'All caps?'
    ReadOnly = False
    OnChange = paeNameAllCapsChange
    Value = False
  end
  inherited RO: TtiReadOnly
    Left = 28
  end
end

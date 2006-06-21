inherited FormEdit_Adrs: TFormEdit_Adrs
  Left = 256
  Top = 195
  Caption = 'Edit an address'
  ClientHeight = 206
  ClientWidth = 261
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 101
    Top = 176
    TabOrder = 7
  end
  inherited btnCancel: TBitBtn
    Left = 181
    Top = 176
    TabOrder = 8
  end
  object paeAdrsLines: TtiPerAwareMemo [2]
    Left = 12
    Top = 36
    Width = 241
    Height = 49
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = 'Address &lines'
    ReadOnly = False
    ScrollBars = ssNone
    WordWrap = True
    MaxLength = 180
  end
  object paeState: TtiPerAwareEdit [3]
    Left = 12
    Top = 120
    Width = 110
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = '&State'
    LabelWidth = 55
    ReadOnly = False
    MaxLength = 20
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paePCode: TtiPerAwareEdit [4]
    Left = 144
    Top = 120
    Width = 110
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
    Caption = '&Post code'
    LabelWidth = 55
    ReadOnly = False
    MaxLength = 10
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeCountry: TtiPerAwareEdit [5]
    Left = 12
    Top = 148
    Width = 241
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 6
    Caption = '&Country'
    ReadOnly = False
    MaxLength = 20
    CharCase = ecNormal
    PasswordChar = #0
  end
  inherited cbEnterAsTab: TCheckBox
    Left = 8
    Top = 178
    TabOrder = 4
  end
  object paeSuburb: TtiPerAwareEdit [7]
    Left = 12
    Top = 92
    Width = 241
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = '&S&uburb'
    ReadOnly = False
    MaxLength = 20
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeAdrsType: TtiPerAwareComboBoxDynamic [8]
    Left = 12
    Top = 4
    Width = 241
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Address &type'
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
    FieldNameDisplay = 'Text'
  end
end

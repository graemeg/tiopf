inherited FormEdit_EAdrs: TFormEdit_EAdrs
  Left = 388
  Top = 263
  Caption = ' Edit an E-Address'
  ClientHeight = 94
  ClientWidth = 331
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 171
    Top = 64
    TabOrder = 3
  end
  inherited btnCancel: TBitBtn
    Left = 251
    Top = 64
    TabOrder = 4
  end
  object paeEAdrs: TtiPerAwareEdit [2]
    Left = 8
    Top = 36
    Width = 317
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = '&Address'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 60
    CharCase = ecNormal
    PasswordChar = #0
  end
  inherited cbEnterAsTab: TCheckBox
    Left = 8
    Top = 70
    Width = 17
    TabOrder = 2
  end
  object paeAdrsType: TtiPerAwareComboBoxDynamic
    Left = 8
    Top = 8
    Width = 317
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Address &type'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
    FieldNameDisplay = 'Text'
  end
end

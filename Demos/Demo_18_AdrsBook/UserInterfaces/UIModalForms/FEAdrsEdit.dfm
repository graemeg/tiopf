inherited FormEAdrsEdit: TFormEAdrsEdit
  Caption = 'Edit an electronic address (phone, fax, email)'
  ClientHeight = 161
  ClientWidth = 318
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblErrors: TLabel
    Top = 71
    Width = 302
  end
  inherited btnOK: TBitBtn
    Left = 158
    Top = 131
    TabOrder = 2
  end
  inherited btnCancel: TBitBtn
    Left = 238
    Top = 131
    TabOrder = 3
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 135
    TabOrder = 4
  end
  object paeEAdrsText: TtiPerAwareEdit
    Left = 8
    Top = 42
    Width = 307
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 1
    LabelLayout = tlTop
    Caption = 'Address &text'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeEAdrsType: TtiPerAwareComboBoxStatic
    Left = 8
    Top = 8
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 0
    LabelLayout = tlTop
    Caption = 'Address t&ype'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    DropDownCount = 8
    CharCase = ecNormal
  end
end

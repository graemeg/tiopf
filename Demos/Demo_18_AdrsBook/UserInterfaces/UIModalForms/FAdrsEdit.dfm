inherited FormAdrsEdit: TFormAdrsEdit
  Caption = 'Edit an address'
  ClientHeight = 266
  ClientWidth = 319
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblErrors: TLabel
    Left = 4
    Top = 181
    Width = 307
  end
  inherited btnOK: TBitBtn
    Left = 159
    Top = 236
    TabOrder = 6
  end
  inherited btnCancel: TBitBtn
    Left = 239
    Top = 236
    TabOrder = 7
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 240
    TabOrder = 8
  end
  object paeAdrsType: TtiPerAwareComboBoxStatic
    Left = 8
    Top = 12
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 0
    LabelLayout = tlTop
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
  end
  object paeSuburb: TtiPerAwareEdit
    Left = 8
    Top = 94
    Width = 307
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 2
    LabelLayout = tlTop
    Caption = '&Suburb'
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
  object paeAdrsLines: TtiPerAwareMemo
    Left = 8
    Top = 42
    Width = 307
    Height = 50
    Constraints.MinHeight = 24
    TabOrder = 1
    LabelLayout = tlTop
    Caption = 'Address &lines'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    ScrollBars = ssNone
    WordWrap = True
    MaxLength = 0
  end
  object paeState: TtiPerAwareEdit
    Left = 8
    Top = 124
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 3
    LabelLayout = tlTop
    Caption = 'S&tate'
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
  object paePCode: TtiPerAwareEdit
    Left = 199
    Top = 124
    Width = 116
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 4
    LabelLayout = tlTop
    Caption = '&Post code'
    LabelWidth = 60
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
  object paeCountry: TtiPerAwareEdit
    Left = 8
    Top = 152
    Width = 307
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 5
    LabelLayout = tlTop
    Caption = '&Country'
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
end

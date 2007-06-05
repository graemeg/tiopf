inherited FormClientEdit: TFormClientEdit
  Left = 461
  Top = 253
  Caption = 'FormClientEdit'
  ClientHeight = 208
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 211
    Top = 178
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 178
    TabOrder = 5
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 182
    TabOrder = 3
  end
  object paeOID: TtiPerAwareEdit
    Left = 10
    Top = 12
    Width = 357
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 0
    LabelLayout = tlTop
    Caption = 'OID'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = True
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientName: TtiPerAwareEdit
    Left = 10
    Top = 40
    Width = 357
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    LabelLayout = tlTop
    Caption = 'Client name'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 200
    CharCase = ecUpperCase
    PasswordChar = #0
  end
  object memoErrors: TtiMemoReadOnly
    Left = 8
    Top = 100
    Width = 357
    Height = 69
    TabStop = False
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object paeSex: TtiPerAwareComboBoxStatic
    Left = 12
    Top = 68
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    LabelLayout = tlTop
    Caption = '&Sex'
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

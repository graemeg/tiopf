inherited FormClientEdit: TFormClientEdit
  Left = 128
  Top = 127
  Caption = 'FormClientEdit'
  ClientHeight = 181
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 211
    Top = 151
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 151
    TabOrder = 5
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 155
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
    Height = 41
    TabStop = False
    Lines.Strings = (
      'memoErrors'
      'line 2'
      'line 3')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object paeClientSource: TtiPerAwareComboBoxDynamic
    Left = 10
    Top = 68
    Width = 185
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 2
    LabelLayout = tlTop
    Caption = 'Client &source'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    OnChange = paeClientSourceChange
    DropDownCount = 8
    CharCase = ecNormal
    FieldNameDisplay = 'DisplayText'
  end
end

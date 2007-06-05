inherited FormClientEdit: TFormClientEdit
  Left = 461
  Top = 253
  Caption = 'FormClientEdit'
  ClientHeight = 180
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  ExplicitWidth = 377
  ExplicitHeight = 212
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 211
    Top = 150
    TabOrder = 4
    ExplicitLeft = 211
    ExplicitTop = 178
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 150
    TabOrder = 5
    ExplicitLeft = 291
    ExplicitTop = 178
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 154
    TabOrder = 3
    ExplicitTop = 182
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
  object paeSex: TtiPerAwareComboBoxStatic
    Left = 12
    Top = 68
    Width = 185
    Height = 23
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

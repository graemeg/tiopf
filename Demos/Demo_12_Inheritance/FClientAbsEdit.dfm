inherited FormClientAbsEdit: TFormClientAbsEdit
  Caption = 'FormClientAbsEdit'
  ClientHeight = 208
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel [0]
    Left = 8
    Top = 68
    Width = 357
    Height = 13
    Shape = bsTopLine
  end
  inherited btnOK: TBitBtn
    Left = 211
    Top = 178
    TabOrder = 3
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 178
    TabOrder = 4
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 182
    TabOrder = 2
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
  object paeClientID: TtiPerAwareEdit
    Left = 10
    Top = 40
    Width = 185
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 1
    LabelLayout = tlTop
    Caption = 'Client ID'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 9
    CharCase = ecNormal
    PasswordChar = #0
  end
  object memoErrors: TtiMemoReadOnly
    Left = 8
    Top = 100
    Width = 357
    Height = 69
    TabStop = False
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
end

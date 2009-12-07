inherited frmPart: TfrmPart
  Caption = 'Part Details'
  ClientHeight = 236
  ClientWidth = 348
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 188
    Top = 206
  end
  inherited btnCancel: TBitBtn
    Left = 268
    Top = 206
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 210
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 348
    Height = 194
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 3
    object editDescription: TtiPerAwareEdit
      Left = 8
      Top = 54
      Width = 332
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 2
      LabelLayout = tlTop
      Caption = 'Description'
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
    object memoErrors: TtiMemoReadOnly
      Left = 8
      Top = 137
      Width = 332
      Height = 49
      TabStop = False
      Align = alBottom
      Lines.Strings = (
        'memoErrors')
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object editCost: TtiPerAwareFloatEdit
      Left = 8
      Top = 78
      Width = 332
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 3
      LabelLayout = tlTop
      Caption = 'Cost'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      ValueAsString = '0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object editListPrice: TtiPerAwareFloatEdit
      Left = 8
      Top = 102
      Width = 332
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 4
      LabelLayout = tlTop
      Caption = 'List Price'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      ValueAsString = '0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object comboVendor: TtiPerAwareComboBoxDynamic
      Left = 8
      Top = 31
      Width = 332
      Height = 23
      ShowFocusRect = True
      Align = alTop
      Constraints.MinHeight = 23
      TabOrder = 1
      LabelLayout = tlTop
      Caption = 'Vendor'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      OnChange = comboVendorChange
      DropDownCount = 8
      CharCase = ecNormal
      FieldNameDisplay = 'VendorName'
    end
    object editOID: TtiPerAwareEdit
      Left = 8
      Top = 8
      Width = 332
      Height = 23
      Align = alTop
      Constraints.MinHeight = 23
      Enabled = False
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
  end
end

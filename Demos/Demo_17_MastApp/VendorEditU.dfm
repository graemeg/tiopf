inherited frmVendor: TfrmVendor
  Caption = 'Vendor Details'
  ClientHeight = 349
  ClientWidth = 399
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 239
    Top = 319
  end
  inherited btnCancel: TBitBtn
    Left = 319
    Top = 319
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 323
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 399
    Height = 307
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 3
    object editFax: TtiPerAwareEdit
      Left = 8
      Top = 200
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 8
      LabelLayout = tlTop
      Caption = 'Fax'
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
    object editAddress2: TtiPerAwareEdit
      Left = 8
      Top = 56
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 2
      LabelLayout = tlTop
      Caption = 'Address 2'
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
    object editAddress1: TtiPerAwareEdit
      Left = 8
      Top = 32
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 1
      LabelLayout = tlTop
      Caption = 'Address 1'
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
    object editVendorName: TtiPerAwareEdit
      Left = 8
      Top = 8
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 0
      LabelLayout = tlTop
      Caption = 'Vendor'
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
      Top = 250
      Width = 383
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
    object editPhone: TtiPerAwareEdit
      Left = 8
      Top = 176
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 7
      LabelLayout = tlTop
      Caption = 'Phone'
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
    object editCountry: TtiPerAwareEdit
      Left = 8
      Top = 152
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 6
      LabelLayout = tlTop
      Caption = 'Country'
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
    object editZip: TtiPerAwareEdit
      Left = 8
      Top = 128
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 5
      LabelLayout = tlTop
      Caption = 'Zip'
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
    object editCity: TtiPerAwareEdit
      Left = 8
      Top = 80
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 3
      LabelLayout = tlTop
      Caption = 'City'
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
    object editState: TtiPerAwareEdit
      Left = 8
      Top = 104
      Width = 383
      Height = 24
      Align = alTop
      Constraints.MinHeight = 24
      TabOrder = 4
      LabelLayout = tlTop
      Caption = 'State'
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
    object checkPreferred: TtiPerAwareCheckBox
      Left = 8
      Top = 224
      Width = 383
      Height = 17
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Constraints.MinHeight = 17
      TabOrder = 10
      LabelLayout = tlTop
      Caption = 'Preferred'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      Value = False
    end
  end
end

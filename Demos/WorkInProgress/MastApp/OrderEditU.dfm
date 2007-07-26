inherited frmOrder: TfrmOrder
  Caption = 'Order Details'
  ClientHeight = 484
  ClientWidth = 509
  Position = poScreenCenter
  OnClose = FormClose
  ExplicitWidth = 515
  ExplicitHeight = 510
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 349
    Top = 454
    ExplicitLeft = 349
    ExplicitTop = 454
  end
  inherited btnCancel: TBitBtn
    Left = 429
    Top = 454
    ExplicitLeft = 429
    ExplicitTop = 454
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 458
    ExplicitTop = 458
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 509
    Height = 442
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 3
    DesignSize = (
      509
      442)
    object Bevel1: TBevel
      Left = 4
      Top = 308
      Width = 499
      Height = 3
      Shape = bsTopLine
    end
    object Bevel2: TBevel
      Left = 4
      Top = 32
      Width = 499
      Height = 3
      Shape = bsTopLine
    end
    object editCustNo: TtiPerAwareEdit
      Left = 202
      Top = 37
      Width = 49
      Height = 42
      Constraints.MinHeight = 24
      TabOrder = 2
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Cust No'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object memoErrors: TtiMemoReadOnly
      Left = 8
      Top = 314
      Width = 243
      Height = 120
      TabStop = False
      Anchors = [akLeft, akBottom]
      Lines.Strings = (
        'memoErrors')
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object editTaxRate: TtiPerAwareFloatEdit
      Left = 265
      Top = 338
      Width = 136
      Height = 27
      Constraints.MinHeight = 24
      TabOrder = 20
      OnExit = editTaxRateExit
      LabelLayout = tlTop
      Caption = 'Tax'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      TextAfter = '%'
      ValueAsString = '0.00%'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object editItemsTotal: TtiPerAwareFloatEdit
      Left = 265
      Top = 314
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 19
      ParentColor = True
      LabelLayout = tlTop
      Caption = 'Subtotal'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      TextBefore = '$'
      ValueAsString = '$0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object comboCustomer: TtiPerAwareComboBoxDynamic
      Left = 8
      Top = 37
      Width = 193
      Height = 42
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Bill To'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      OnChange = comboCustomerChange
      DropDownCount = 8
      CharCase = ecNormal
      FieldNameDisplay = 'Caption'
    end
    object editOID: TtiPerAwareEdit
      Left = 348
      Top = 7
      Width = 155
      Height = 24
      Constraints.MinHeight = 23
      Enabled = False
      TabOrder = 0
      LabelLayout = tlTop
      Caption = 'Order No'
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
    object editCustAddress1: TtiPerAwareEdit
      Left = 9
      Top = 76
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 3
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object editCustAddress2: TtiPerAwareEdit
      Left = 9
      Top = 100
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 4
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object editCustCity: TtiPerAwareEdit
      Left = 9
      Top = 124
      Width = 88
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 5
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object editCustState: TtiPerAwareEdit
      Left = 97
      Top = 124
      Width = 40
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 6
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object editCustZip: TtiPerAwareEdit
      Left = 136
      Top = 124
      Width = 112
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 7
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object editShipToContact: TtiPerAwareEdit
      Left = 265
      Top = 37
      Width = 242
      Height = 42
      Constraints.MinHeight = 24
      TabOrder = 8
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Ship To'
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
    object editShipToAddress1: TtiPerAwareEdit
      Left = 265
      Top = 76
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 9
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
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
    object editShipToAddress2: TtiPerAwareEdit
      Left = 265
      Top = 100
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 10
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
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
    object editShipToCity: TtiPerAwareEdit
      Left = 265
      Top = 124
      Width = 88
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 11
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
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
    object editShipToState: TtiPerAwareEdit
      Left = 353
      Top = 124
      Width = 40
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 12
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
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
    object editShipToZip: TtiPerAwareEdit
      Left = 392
      Top = 124
      Width = 112
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 13
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Enter label &name'
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
    object comboEmployee: TtiPerAwareComboBoxDynamic
      Left = 9
      Top = 154
      Width = 96
      Height = 42
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 14
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Sold By'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      OnChange = comboEmployeeChange
      DropDownCount = 8
      CharCase = ecNormal
      FieldNameDisplay = 'Caption'
    end
    object editPurchaseOrder: TtiPerAwareEdit
      Left = 416
      Top = 154
      Width = 91
      Height = 42
      Constraints.MinHeight = 24
      TabOrder = 18
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Purchase Order'
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
    object editTaxDue: TtiPerAwareFloatEdit
      Left = 409
      Top = 338
      Width = 94
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 21
      LabelStyle = lsNone
      LabelLayout = tlTop
      Caption = 'Tax'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      TextBefore = '$'
      ValueAsString = '$0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object editFreight: TtiPerAwareFloatEdit
      Left = 265
      Top = 362
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 22
      OnExit = editTaxRateExit
      LabelLayout = tlTop
      Caption = 'Freight'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      TextBefore = '$'
      ValueAsString = '$0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object editAmountPaid: TtiPerAwareFloatEdit
      Left = 265
      Top = 386
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 23
      OnExit = editTaxRateExit
      LabelLayout = tlTop
      Caption = 'Paid'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      TextBefore = '$'
      ValueAsString = '$0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object editAmountDue: TtiPerAwareFloatEdit
      Left = 265
      Top = 410
      Width = 239
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 24
      LabelLayout = tlTop
      Caption = 'Due'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      TextBefore = '$'
      ValueAsString = '$0.00'
      Precision = 2
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object comboTerms: TtiPerAwareComboBoxStatic
      Left = 111
      Top = 154
      Width = 96
      Height = 42
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 15
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Terms'
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
    object comboPayment: TtiPerAwareComboBoxStatic
      Left = 213
      Top = 154
      Width = 96
      Height = 42
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 16
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Payment Method'
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
    object comboShipping: TtiPerAwareComboBoxStatic
      Left = 314
      Top = 154
      Width = 96
      Height = 42
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 17
      LabelStyle = lsTop
      LabelLayout = tlTop
      Caption = 'Ship Via'
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
    object gridItems: TDBGrid
      Left = 9
      Top = 202
      Width = 488
      Height = 100
      DataSource = dscItems
      TabOrder = 26
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = gridItemsEditButtonClick
      OnEditButtonClick = gridItemsEditButtonClick
      Columns = <
        item
          ButtonStyle = cbsEllipsis
          Expanded = False
          FieldName = 'PartNo'
          Title.Caption = 'Part'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Description'
          ReadOnly = True
          Width = 120
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'ListPrice'
          ReadOnly = True
          Title.Caption = 'List Price'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Quantity'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Discount'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'TotalPrice'
          ReadOnly = True
          Title.Caption = 'Total Price'
          Visible = True
        end>
    end
    object editSaleDate: TtiPerAwareDateTimePicker
      Left = 16
      Top = 7
      Width = 180
      Height = 24
      Constraints.MinHeight = 24
      TabOrder = 27
      LabelLayout = tlTop
      Caption = 'Sale'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      Value = 39247.000000000000000000
      Kind = dtkDate
      DateMode = dmComboBox
    end
  end
  object dscItems: TDataSource
    Left = 184
    Top = 216
  end
end

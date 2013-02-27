object ContractForm: TContractForm
  Left = 302
  Top = 38
  Align = alClient
  BorderStyle = bsNone
  Caption = 'Contract'
  ClientHeight = 705
  ClientWidth = 699
  Color = 12240841
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 13
  object ClientDetailsPanel: TPanel
    Left = 0
    Top = 136
    Width = 699
    Height = 230
    Align = alTop
    BevelOuter = bvNone
    Color = 9609633
    TabOrder = 3
    object Name: TLabel
      Left = 200
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Name'
    end
    object Label1: TLabel
      Left = 7
      Top = 36
      Width = 28
      Height = 13
      Caption = 'Street'
    end
    object Label9: TLabel
      Left = 335
      Top = 36
      Width = 68
      Height = 13
      Caption = 'Phone (Home)'
    end
    object Label3: TLabel
      Left = 335
      Top = 59
      Width = 66
      Height = 13
      Caption = 'Phone (Work)'
    end
    object Label11: TLabel
      Left = 335
      Top = 82
      Width = 71
      Height = 13
      Caption = 'Phone (Mobile)'
    end
    object Label7: TLabel
      Left = 335
      Top = 106
      Width = 25
      Height = 13
      Caption = 'Email'
    end
    object Label12: TLabel
      Left = 7
      Top = 8
      Width = 44
      Height = 13
      Caption = 'Client no.'
    end
    object Label5: TLabel
      Left = 7
      Top = 136
      Width = 60
      Height = 13
      Caption = 'Identification'
    end
    object Label14: TLabel
      Left = 7
      Top = 57
      Width = 34
      Height = 13
      Caption = 'Suburb'
    end
    object Label20: TLabel
      Left = 7
      Top = 76
      Width = 25
      Height = 13
      Caption = 'State'
    end
    object Label21: TLabel
      Left = 7
      Top = 100
      Width = 46
      Height = 13
      Caption = 'PostCode'
    end
    object ToggleMoreLessClientDetailsButton: TSpeedButton
      Left = 629
      Top = 5
      Width = 57
      Height = 22
      Action = ToggleMoreLessClientDetailsAction
      Caption = 'Less'
      Flat = True
    end
    object ClientNumberEdit: TEdit
      Left = 104
      Top = 5
      Width = 81
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object ClientNameEdit: TEdit
      Left = 241
      Top = 5
      Width = 376
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'ClientNameEdit'
    end
    object PhoneMobileEdit: TEdit
      Left = 416
      Top = 79
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 8
      Text = 'PhoneMobileEdit'
    end
    object PhoneWorkEdit: TEdit
      Left = 416
      Top = 56
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 7
      Text = 'PhoneWorkEdit'
    end
    object PhoneHomeEdit: TEdit
      Left = 416
      Top = 33
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
      Text = 'PhoneHomeEdit'
    end
    object EmailAddressEdit: TEdit
      Left = 416
      Top = 103
      Width = 201
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 9
      Text = 'EmailAddressEdit'
    end
    object StreetEdit: TEdit
      Left = 104
      Top = 33
      Width = 209
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
      Text = 'StreetEdit'
    end
    object SuburbEdit: TEdit
      Left = 104
      Top = 56
      Width = 209
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
      Text = 'SuburbEdit'
    end
    object StateEdit: TEdit
      Left = 104
      Top = 79
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      Text = 'StateEdit'
    end
    object PostCodeEdit: TEdit
      Left = 104
      Top = 103
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 5
      Text = 'PostCodeEdit'
    end
    object ClientIdentityRecordListView: TtiListView
      Left = 104
      Top = 136
      Width = 513
      Height = 81
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = True
      ApplyFilter = False
      ApplySort = False
      ListColumns = <
        item
          DisplayLabel = 'Identity record type'
          FieldName = 'Caption'
          DataType = lvtkString
          Derived = True
          OnDeriveColumn = ClientIdentityRecordListViewListColumns0DeriveColumn
        end
        item
          DisplayLabel = 'Details'
          FieldName = 'Details'
          DataType = lvtkString
          Derived = False
        end>
      SortOrders = <>
      RuntimeGenCols = False
      CanStartDrag = False
      ReadOnly = True
    end
  end
  object PawnedItemsPanel: TPanel
    Left = 0
    Top = 366
    Width = 699
    Height = 204
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    Color = 12240841
    Constraints.MinHeight = 94
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnResize = PawnedItemsPanelResize
    object Label13: TLabel
      Left = 7
      Top = 8
      Width = 66
      Height = 13
      Caption = 'Pawned items'
    end
    object ContractItemsListView: TtiListView
      Left = 104
      Top = 8
      Width = 512
      Height = 129
      Anchors = [akLeft, akTop, akRight]
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = True
      ApplyFilter = False
      ApplySort = False
      ListColumns = <
        item
          DisplayLabel = 'Category'
          FieldName = 'Caption'
          DataType = lvtkString
          Derived = True
          OnDeriveColumn = ContractItemsListViewListColumns0DeriveColumn
        end
        item
          DisplayLabel = 'Quantity'
          FieldName = 'Quantity'
          DisplayMask = '#,##0'
          DataType = lvtkInt
          Derived = False
        end
        item
          DisplayLabel = 'Description'
          FieldName = 'Description'
          DataType = lvtkString
          Derived = False
        end
        item
          DisplayLabel = 'Model No.'
          FieldName = 'ModelNumber'
          DataType = lvtkString
          Derived = False
        end
        item
          DisplayLabel = 'Serial No.'
          FieldName = 'SerialNumber'
          DataType = lvtkString
          Derived = False
        end
        item
          DisplayLabel = 'Manufacturer'
          FieldName = 'Caption'
          DataType = lvtkString
          Derived = True
          OnDeriveColumn = ContractItemsListViewListColumns6DeriveColumn
        end
        item
          DisplayLabel = 'Value'
          FieldName = 'Value'
          DisplayMask = '#,##0.00'
          DataType = lvtkCurrency
          Derived = False
        end>
      SortOrders = <>
      RuntimeGenCols = False
      CanStartDrag = False
      ReadOnly = True
    end
    object SubtotalPanel: TPanel
      Left = 424
      Top = 141
      Width = 201
      Height = 50
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      object Label25: TLabel
        Left = 3
        Top = 7
        Width = 43
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'SubTotal'
      end
      object Label26: TLabel
        Left = 3
        Top = 29
        Width = 52
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '(Payments)'
      end
      object PreviousPaymentsNumericField: TOvcNumericField
        Left = 86
        Top = 26
        Width = 105
        Height = 21
        Cursor = crIBeam
        DataType = nftDouble
        Anchors = [akTop, akRight]
        CaretOvr.Shape = csBlock
        Ctl3D = True
        EFColors.Disabled.BackColor = clWindow
        EFColors.Disabled.TextColor = clGrayText
        EFColors.Error.BackColor = clRed
        EFColors.Error.TextColor = clBlack
        EFColors.Highlight.BackColor = clHighlight
        EFColors.Highlight.TextColor = clHighlightText
        Options = []
        ParentColor = True
        ParentCtl3D = False
        PictureMask = '$######.##'
        TabOrder = 1
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object SubtotalNumericField: TOvcNumericField
        Left = 86
        Top = 6
        Width = 105
        Height = 21
        Cursor = crIBeam
        DataType = nftDouble
        Anchors = [akTop, akRight]
        CaretOvr.Shape = csBlock
        Ctl3D = True
        EFColors.Disabled.BackColor = clWindow
        EFColors.Disabled.TextColor = clGrayText
        EFColors.Error.BackColor = clRed
        EFColors.Error.TextColor = clBlack
        EFColors.Highlight.BackColor = clHighlight
        EFColors.Highlight.TextColor = clHighlightText
        Options = []
        ParentColor = True
        ParentCtl3D = False
        PictureMask = '$######.##'
        TabOrder = 0
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
    end
  end
  object SummaryPanel: TPanel
    Left = 0
    Top = 570
    Width = 699
    Height = 135
    Align = alBottom
    BevelOuter = bvNone
    Color = 9609633
    TabOrder = 1
    object Label15: TLabel
      Left = 449
      Top = 10
      Width = 54
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Loan Value'
    end
    object Label16: TLabel
      Left = 443
      Top = 29
      Width = 61
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Contract Fee'
    end
    object Label19: TLabel
      Left = 399
      Top = 85
      Width = 104
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Redemption Value'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label17: TLabel
      Left = 470
      Top = 48
      Width = 35
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Interest'
    end
    object Label18: TLabel
      Left = 432
      Top = 67
      Width = 73
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '(less Payments)'
    end
    object Label2: TLabel
      Left = 7
      Top = 8
      Width = 28
      Height = 13
      Caption = 'Notes'
    end
    object NotesRichEdit: TRichEdit
      Left = 104
      Top = 5
      Width = 273
      Height = 105
      Lines.Strings = (
        'NotesRichEdit')
      ParentColor = True
      TabOrder = 0
    end
    object RedemptionValueNumericField: TOvcNumericField
      Left = 510
      Top = 86
      Width = 105
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '$######.##'
      TabOrder = 4
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
    object CurrentPaymentsNumericField: TOvcNumericField
      Left = 510
      Top = 66
      Width = 105
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '$######.##'
      TabOrder = 6
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
    object InterestRateNumericField: TOvcNumericField
      Left = 510
      Top = 46
      Width = 53
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '#######.##%'
      TabOrder = 2
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
    object InterestValueNumericField: TOvcNumericField
      Left = 562
      Top = 46
      Width = 53
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '$######.##'
      TabOrder = 3
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
    object ContractFeeNumericField: TOvcNumericField
      Left = 510
      Top = 26
      Width = 105
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '$######.##'
      TabOrder = 1
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
    object LoanValueNumericField: TOvcNumericField
      Left = 510
      Top = 6
      Width = 105
      Height = 21
      Cursor = crIBeam
      DataType = nftDouble
      Anchors = [akTop, akRight]
      CaretOvr.Shape = csBlock
      Ctl3D = True
      EFColors.Disabled.BackColor = clWindow
      EFColors.Disabled.TextColor = clGrayText
      EFColors.Error.BackColor = clRed
      EFColors.Error.TextColor = clBlack
      EFColors.Highlight.BackColor = clHighlight
      EFColors.Highlight.TextColor = clHighlightText
      Options = []
      ParentColor = True
      ParentCtl3D = False
      PictureMask = '$######.##'
      TabOrder = 5
      TabStop = False
      RangeHigh = {73B2DBB9838916F2FE43}
      RangeLow = {73B2DBB9838916F2FEC3}
    end
  end
  object ContractDetailsPanel: TPanel
    Left = 0
    Top = 23
    Width = 699
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    Color = 12240841
    TabOrder = 2
    object Label6: TLabel
      Left = 480
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Status'
    end
    object Label4: TLabel
      Left = 200
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Period'
    end
    object Label8: TLabel
      Left = 7
      Top = 36
      Width = 32
      Height = 13
      Caption = 'History'
    end
    object Label10: TLabel
      Left = 7
      Top = 8
      Width = 58
      Height = 13
      Caption = 'Contract no.'
    end
    object ToggleIsShowingMoreButton: TSpeedButton
      Left = 629
      Top = 3
      Width = 57
      Height = 22
      Caption = 'More'
      Flat = True
      OnClick = ToggleMoreLessContractDetailsActionExecute
    end
    object TransactionListView: TtiListView
      Left = 104
      Top = 33
      Width = 513
      Height = 72
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = True
      ApplyFilter = False
      ApplySort = False
      ListColumns = <
        item
          DisplayLabel = 'Transaction'
          FieldName = 'Transaction'
          DataType = lvtkString
          Derived = True
          OnDeriveColumn = TransactionListViewListColumns4DeriveColumn
        end
        item
          DisplayLabel = 'Extension No.'
          FieldName = 'ExtensionNumber'
          DisplayMask = '#,##0'
          DataType = lvtkInt
          Derived = False
        end
        item
          DisplayLabel = 'Timestamp'
          FieldName = 'TimeStamp'
          DisplayMask = 'dd/mm/yyyy hh:mm:ss'
          DataType = lvtkDateTime
          Derived = False
        end
        item
          DisplayLabel = 'Details'
          FieldName = 'Details'
          DataType = lvtkString
          Derived = False
        end
        item
          DisplayLabel = 'Value'
          FieldName = 'Value'
          DisplayMask = '#,##0.00'
          DataType = lvtkCurrency
          Derived = False
        end>
      SortOrders = <>
      RuntimeGenCols = False
      CanStartDrag = False
      ReadOnly = True
    end
    object StatusEdit: TEdit
      Left = 520
      Top = 5
      Width = 97
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'StatusEdit'
    end
    object ContractPeriodEdit: TEdit
      Left = 241
      Top = 5
      Width = 232
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
      Text = 'ContractPeriodEdit'
    end
    object ContractNumberEdit: TEdit
      Left = 104
      Top = 5
      Width = 81
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
      Text = 'ContractNumberEdit'
    end
  end
  object TBDock: TTBDock
    Left = 0
    Top = 0
    Width = 699
    Height = 23
    object ContractFormToolbar: TTBToolbar
      Left = 0
      Top = 0
      Align = alTop
      Caption = 'ContractFormToolbar'
      CloseButton = False
      DockRow = 1
      DragHandleStyle = dhDouble
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      DockTextAlign = taLeftJustify
      object TBItem5: TTBItem
        Action = NewContractAction
      end
      object TBItem1: TTBItem
        Action = EditAction
      end
      object TBSeparatorItem2: TTBSeparatorItem
      end
      object PrintTBItem: TTBItem
        Action = PrintAction
      end
      object PrintPreviewTBItem: TTBItem
        Action = PrintPreviewAction
      end
      object TBSeparatorItem3: TTBSeparatorItem
      end
      object TBItem11: TTBItem
        Action = ExtendContractAction
        DisplayMode = nbdmImageAndText
      end
      object TBItem10: TTBItem
        Action = RedeemContractAction
        DisplayMode = nbdmImageAndText
      end
      object TBItem12: TTBItem
        Action = PartPaymentAction
        DisplayMode = nbdmImageAndText
      end
      object TBItem13: TTBItem
        Action = ExpireContractAction
        DisplayMode = nbdmImageAndText
      end
      object TBItem14: TTBItem
        Action = AdditionalAmountAction
        DisplayMode = nbdmImageAndText
      end
    end
  end
  object ToolbarActionList: TActionList
    OnUpdate = ToolbarActionListUpdate
    Left = 8
    Top = 88
    object NewContractAction: TAction
      Category = 'MetaOperations'
      Caption = 'New'
      OnExecute = NewContractActionExecute
    end
    object EditAction: TAction
      Category = 'MetaOperations'
      Caption = 'Edit'
      OnExecute = EditActionExecute
    end
    object PartPaymentAction: TAction
      Category = 'Contract'
      Caption = 'Part Pay'
      OnExecute = PartPaymentActionExecute
    end
    object PrintAction: TAction
      Category = 'MetaOperations'
      Caption = '&Print...'
      ShortCut = 16464
      OnExecute = PrintActionExecute
    end
    object ExtendContractAction: TAction
      Category = 'Contract'
      Caption = 'Extend'
      OnExecute = ExtendContractActionExecute
    end
    object RedeemContractAction: TAction
      Category = 'Contract'
      Caption = 'Redeem'
      OnExecute = RedeemContractActionExecute
    end
    object ExpireContractAction: TAction
      Category = 'Contract'
      Caption = 'Expire'
      OnExecute = ExpireContractActionExecute
    end
    object AdditionalAmountAction: TAction
      Category = 'Contract'
      Caption = 'Additional'
      Hint = 'Additional amount'
      OnExecute = AdditionalAmountActionExecute
    end
    object PrintPreviewAction: TAction
      Category = 'MetaOperations'
      Caption = 'Print Preview'
      OnExecute = PrintPreviewActionExecute
    end
    object ExitAction: TAction
      Caption = 'E&xit'
      ShortCut = 32856
      OnExecute = ExitActionExecute
    end
  end
  object ControlsActionList: TActionList
    Left = 8
    Top = 122
    object ToggleMoreLessContractDetailsAction: TAction
      Caption = 'More'
      ImageIndex = 7
      OnExecute = ToggleMoreLessContractDetailsActionExecute
    end
    object ToggleMoreLessClientDetailsAction: TAction
      Caption = 'More'
      OnExecute = ToggleMoreLessClientDetailsActionExecute
    end
    object IdentityRecordAddAction: TAction
      Category = 'IdentityRecords'
      Caption = 'Add'
    end
    object IdentityRecordReplaceAction: TAction
      Category = 'IdentityRecords'
      Caption = 'Replace'
    end
    object IdentityRecordDeleteAction: TAction
      Category = 'IdentityRecords'
      Caption = 'Delete'
    end
  end
  object PrintDialog: TPrintDialog
    Options = [poDisablePrintToFile]
    Left = 40
    Top = 82
  end
end

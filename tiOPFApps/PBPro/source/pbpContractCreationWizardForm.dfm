object ContractCreationWizardForm: TContractCreationWizardForm
  Left = 152
  Top = 62
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'New Contract'
  ClientHeight = 625
  ClientWidth = 861
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Wizard: TKWizard
    Left = 0
    Top = 0
    Width = 861
    Height = 625
    ActivePage = FinishPage
    ButtonStart.Caption = 'To &Start Page'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Layout = blGlyphLeft
    ButtonStart.ModalResult = 0
    ButtonStart.Width = 85
    ButtonLast.Caption = 'To &Last Page'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Layout = blGlyphLeft
    ButtonLast.ModalResult = 0
    ButtonLast.Width = 85
    ButtonBack.Caption = '< &Back'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Layout = blGlyphLeft
    ButtonBack.ModalResult = 0
    ButtonBack.Width = 75
    ButtonNext.Caption = '&Next >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Layout = blGlyphLeft
    ButtonNext.ModalResult = 0
    ButtonNext.Width = 75
    ButtonFinish.Caption = '&Finish'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Layout = blGlyphLeft
    ButtonFinish.ModalResult = 0
    ButtonFinish.Width = 75
    ButtonCancel.Caption = 'Cancel'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.Layout = blGlyphLeft
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 75
    ButtonHelp.Caption = '&Help'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Layout = blGlyphLeft
    ButtonHelp.ModalResult = 0
    ButtonHelp.Width = 75
    ShowRouteMap = True
    OnFinishButtonClick = WizardFinishButtonClick
    OnCancelButtonClick = WizardCancelButtonClick
    object SelectClientPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Select client'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 
        'Select a client from the list of available clients or create a n' +
        'ew client record.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      OnExitPage = SelectClientPageExitPage
      object Bevel3: TBevel
        Left = 8
        Top = 64
        Width = 689
        Height = 50
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object Name: TLabel
        Left = 12
        Top = 152
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label21: TLabel
        Left = 12
        Top = 260
        Width = 48
        Height = 13
        Caption = 'Post code'
      end
      object Label20: TLabel
        Left = 12
        Top = 236
        Width = 25
        Height = 13
        Caption = 'State'
      end
      object Label14: TLabel
        Left = 12
        Top = 217
        Width = 34
        Height = 13
        Caption = 'Suburb'
      end
      object Label1: TLabel
        Left = 12
        Top = 196
        Width = 28
        Height = 13
        Caption = 'Street'
      end
      object Label2: TLabel
        Left = 12
        Top = 128
        Width = 64
        Height = 13
        Caption = 'Client number'
      end
      object Label9: TLabel
        Left = 12
        Top = 300
        Width = 68
        Height = 13
        Caption = 'Phone (Home)'
      end
      object Label3: TLabel
        Left = 12
        Top = 323
        Width = 66
        Height = 13
        Caption = 'Phone (Work)'
      end
      object Label11: TLabel
        Left = 12
        Top = 346
        Width = 71
        Height = 13
        Caption = 'Phone (Mobile)'
      end
      object Label7: TLabel
        Left = 12
        Top = 370
        Width = 25
        Height = 13
        Caption = 'Email'
      end
      object UndesirableNoticeLabel: TLabel
        Left = 257
        Top = 128
        Width = 307
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Note: the selected client is marked as UNDESIRABLE'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label29: TLabel
        Left = 12
        Top = 460
        Width = 37
        Height = 13
        Caption = 'Reason'
      end
      object Label40: TLabel
        Left = 12
        Top = 434
        Width = 25
        Height = 13
        Caption = 'Code'
      end
      object Label41: TLabel
        Left = 12
        Top = 83
        Width = 58
        Height = 13
        Caption = 'Contract no.'
      end
      object Label42: TLabel
        Left = 192
        Top = 83
        Width = 30
        Height = 13
        Caption = 'Period'
      end
      object Label43: TLabel
        Left = 472
        Top = 83
        Width = 30
        Height = 13
        Caption = 'Status'
      end
      object Bevel4: TBevel
        Left = 8
        Top = 352
        Width = 689
        Height = 50
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object UndesirableRichEdit: TRichEdit
        Left = 96
        Top = 456
        Width = 473
        Height = 89
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'UndesirableRichEdit')
        ScrollBars = ssVertical
        TabOrder = 14
      end
      object UndesirableCodeEdit: TEdit
        Left = 96
        Top = 431
        Width = 161
        Height = 21
        TabOrder = 13
        Text = 'PhoneMobileEdit'
      end
      object UndesirableCheckBox: TCheckBox
        Left = 12
        Top = 408
        Width = 97
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Undesirable'
        TabOrder = 12
      end
      object EmailAddressEdit: TEdit
        Left = 96
        Top = 367
        Width = 377
        Height = 21
        TabOrder = 11
        Text = 'EmailAddressEdit'
      end
      object ClientNumberEdit: TEdit
        Left = 96
        Top = 125
        Width = 81
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object ClientNameEdit: TEdit
        Left = 96
        Top = 149
        Width = 473
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'ClientNameEdit'
        OnExit = ClientNameEditExit
      end
      object StreetEdit: TEdit
        Left = 96
        Top = 193
        Width = 209
        Height = 21
        TabOrder = 4
        Text = 'StreetEdit'
      end
      object SuburbEdit: TEdit
        Left = 96
        Top = 216
        Width = 209
        Height = 21
        TabOrder = 5
        Text = 'SuburbEdit'
      end
      object StateEdit: TEdit
        Left = 96
        Top = 239
        Width = 161
        Height = 21
        TabOrder = 6
        Text = 'StateEdit'
      end
      object PostCodeEdit: TEdit
        Left = 96
        Top = 263
        Width = 161
        Height = 21
        TabOrder = 7
        Text = 'PostCodeEdit'
      end
      object PhoneMobileEdit: TEdit
        Left = 96
        Top = 343
        Width = 161
        Height = 21
        TabOrder = 10
        Text = 'PhoneMobileEdit'
      end
      object PhoneWorkEdit: TEdit
        Left = 96
        Top = 320
        Width = 161
        Height = 21
        TabOrder = 9
        Text = 'PhoneWorkEdit'
      end
      object PhoneHomeEdit: TEdit
        Left = 96
        Top = 297
        Width = 161
        Height = 21
        TabOrder = 8
        Text = 'PhoneHomeEdit'
      end
      object NewClientButton: TBitBtn
        Left = 639
        Top = 148
        Width = 57
        Height = 22
        Action = NewClientAction
        Anchors = [akTop, akRight]
        Caption = 'Ne&w'
        TabOrder = 3
      end
      object FindClientButton: TBitBtn
        Left = 575
        Top = 148
        Width = 57
        Height = 22
        Action = FindClientAction
        Anchors = [akTop, akRight]
        Caption = '&Find'
        TabOrder = 2
      end
      object ClientFinderListView: TtiListView
        Left = 393
        Top = 185
        Width = 183
        Height = 136
        MultiSelect = False
        OnDblClick = ClientFinderListViewDblClick
        OnKeyDown = ClientFinderListViewKeyDown
        ViewStyle = vsReport
        RowSelect = True
        OnExit = ClientFinderListViewExit
        ApplyFilter = False
        ApplySort = False
        ListColumns = <
          item
            DisplayLabel = 'Client no.'
            FieldName = 'ClientNumber'
            DataType = lvtkString
            Derived = False
          end
          item
            DisplayLabel = 'Family name'
            FieldName = 'FamilyName'
            DataType = lvtkString
            Derived = False
          end
          item
            DisplayLabel = 'Given names'
            FieldName = 'GivenNames'
            DataType = lvtkString
            Derived = False
          end
          item
            DisplayLabel = 'Undesirable'
            FieldName = 'Undesirable'
            DataType = lvtkString
            Derived = False
          end>
        SortOrders = <>
        RuntimeGenCols = False
        CanStartDrag = False
      end
      object ContractNumberEdit: TEdit
        Left = 96
        Top = 80
        Width = 81
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 16
        Text = 'ContractNumberEdit'
      end
      object ContractPeriodEdit: TEdit
        Left = 233
        Top = 80
        Width = 232
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 17
        Text = 'ContractPeriodEdit'
      end
      object StatusEdit: TEdit
        Left = 512
        Top = 80
        Width = 97
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 18
        Text = 'StatusEdit'
      end
    end
    object ClientIdentityPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Client Identity'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 
        'Select the client identity records that apply to this contract b' +
        'y checking the boxes to the left of each record in the grid. You' +
        ' can also add additional client identity records at this point.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      object Bevel1: TBevel
        Left = 8
        Top = 112
        Width = 693
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object Label8: TLabel
        Left = 12
        Top = 118
        Width = 32
        Height = 13
        Caption = 'Details'
      end
      object Label13: TLabel
        Left = 12
        Top = 88
        Width = 24
        Height = 13
        Caption = 'Type'
      end
      object Label4: TLabel
        Left = 12
        Top = 192
        Width = 72
        Height = 13
        Caption = 'Identity records'
      end
      object ClientIdentityRecordListBox: TtiListViewListBox
        Left = 96
        Top = 192
        Width = 597
        Height = 376
        Anchors = [akLeft, akTop, akRight, akBottom]
        RowSelect = True
        OnFilterData = ClientIdentityRecordListBoxFilterData
        ApplyFilter = True
        ApplySort = False
        ListColumns = <
          item
            DisplayLabel = 'Identity type'
            FieldName = 'Caption'
            DataType = lvtkString
            Derived = True
            OnDeriveColumn = ClientIdentificationListBoxListColumns0DeriveColumn
          end
          item
            DisplayLabel = 'Details'
            FieldName = 'Details'
            DataType = lvtkString
            Derived = False
          end
          item
            DisplayLabel = 'In use'
            FieldName = 'InUse'
            DataType = lvtkString
            Derived = False
          end>
        SortOrders = <>
        Style = lvlbStyleCheckBox
        OnGetChecked = ClientIdentityRecordListBoxGetChecked
        OnCheck = ClientIdentityRecordListBoxCheck
        RunTimeGenCols = False
      end
      object IdentityRecordDetailsEdit: TEdit
        Left = 96
        Top = 112
        Width = 273
        Height = 21
        TabOrder = 1
        Text = 'IdentityRecordDetailsEdit'
      end
      object IdentityRecordTypeComboBox: TComboBox
        Left = 96
        Top = 85
        Width = 273
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'IdentityRecordTypeComboBox'
        OnDropDown = IdentityRecordTypeComboBoxDropDown
      end
      object AddClientIdentityRecordButton: TBitBtn
        Left = 96
        Top = 143
        Width = 57
        Height = 22
        Action = AddClientIdentityRecordAction
        Caption = '&Add'
        TabOrder = 2
      end
      object ReplaceClientIdentityRecordButton: TBitBtn
        Left = 162
        Top = 143
        Width = 57
        Height = 22
        Action = ReplaceClientIdentityRecordAction
        Caption = '&Replace'
        TabOrder = 3
      end
      object DeleteClientIdentityRecordButton: TBitBtn
        Left = 227
        Top = 143
        Width = 57
        Height = 22
        Action = DeleteClientIdentityRecordAction
        Caption = '&Delete'
        TabOrder = 4
      end
    end
    object PawnedItemsPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Pawned items'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 'Enter details for pawned items.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      object Label6: TLabel
        Left = 78
        Top = 237
        Width = 6
        Height = 13
        Caption = '$'
      end
      object Label15: TLabel
        Left = 20
        Top = 237
        Width = 27
        Height = 13
        Caption = 'Value'
      end
      object Label17: TLabel
        Left = 20
        Top = 213
        Width = 46
        Height = 13
        Caption = 'Serial No.'
      end
      object Label18: TLabel
        Left = 20
        Top = 188
        Width = 49
        Height = 13
        Caption = 'Model No.'
      end
      object Label22: TLabel
        Left = 20
        Top = 164
        Width = 63
        Height = 13
        Caption = 'Manufacturer'
      end
      object Label23: TLabel
        Left = 20
        Top = 140
        Width = 53
        Height = 13
        Caption = 'Description'
      end
      object Label24: TLabel
        Left = 20
        Top = 115
        Width = 39
        Height = 13
        Caption = 'Quantity'
      end
      object Label26: TLabel
        Left = 20
        Top = 91
        Width = 42
        Height = 13
        Caption = 'Category'
      end
      object Label27: TLabel
        Left = 20
        Top = 267
        Width = 28
        Height = 13
        Caption = 'Notes'
      end
      object Bevel2: TBevel
        Left = 16
        Top = 280
        Width = 689
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Shape = bsBottomLine
      end
      object Label5: TLabel
        Left = 20
        Top = 379
        Width = 66
        Height = 13
        Caption = 'Pawned items'
      end
      object CategoryComboBox: TComboBox
        Left = 104
        Top = 85
        Width = 233
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'CategoryComboBox'
        OnDropDown = CategoryComboBoxDropDown
      end
      object QuantityEdit: TEdit
        Left = 104
        Top = 109
        Width = 89
        Height = 21
        TabOrder = 1
        Text = 'QuantityEdit'
      end
      object DescriptionEdit: TEdit
        Left = 104
        Top = 134
        Width = 233
        Height = 21
        TabOrder = 2
        Text = 'DescriptionEdit'
      end
      object ManufacturerComboBox: TComboBox
        Left = 104
        Top = 158
        Width = 233
        Height = 21
        ItemHeight = 13
        TabOrder = 3
        Text = 'ManufacturerComboBox'
        OnDropDown = ManufacturerComboBoxDropDown
      end
      object ModelNumberEdit: TEdit
        Left = 104
        Top = 182
        Width = 233
        Height = 21
        TabOrder = 4
        Text = 'ModelNumberEdit'
      end
      object SerialNumberEdit: TEdit
        Left = 104
        Top = 207
        Width = 233
        Height = 21
        TabOrder = 5
        Text = 'SerialNumberEdit'
      end
      object ValueEdit: TEdit
        Left = 104
        Top = 231
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'ValueEdit'
      end
      object ContractItemNotesRichEdit: TRichEdit
        Left = 104
        Top = 261
        Width = 353
        Height = 52
        Lines.Strings = (
          'NotesRichEdit')
        TabOrder = 7
      end
      object AddPawnedItemButton: TBitBtn
        Left = 103
        Top = 322
        Width = 57
        Height = 22
        Action = AddContractItemAction
        Caption = '&Add'
        TabOrder = 8
      end
      object BitBtn5: TBitBtn
        Left = 167
        Top = 322
        Width = 57
        Height = 22
        Action = ReplaceContractItemAction
        Caption = 'Replace'
        TabOrder = 9
      end
      object BitBtn6: TBitBtn
        Left = 231
        Top = 322
        Width = 57
        Height = 22
        Action = DeleteContractItemAction
        Caption = '&Delete'
        TabOrder = 10
      end
      object ContractItemsListView: TtiListView
        Left = 104
        Top = 376
        Width = 597
        Height = 192
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        OnDblClick = ContractItemsListViewDblClick
        ViewStyle = vsReport
        RowSelect = True
        OnFilterData = ContractItemsListViewFilterData
        ApplyFilter = True
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
            OnDeriveColumn = ContractItemsListViewListColumns5DeriveColumn
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
      end
    end
    object FinishPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Finish'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 
        'You are about to commit changes made to this contract. If you ar' +
        'e satisfied with the details below click on the '#39'Finish'#39' button.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      VisibleButtons = [bkBack, bkFinish, bkCancel]
      OnEnterPage = FinishPageEnterPage
      OnExitPage = FinishPageExitPage
      OnFinishButtonClick = FinishPageFinishButtonClick
      object Label12: TLabel
        Left = 12
        Top = 347
        Width = 66
        Height = 13
        Caption = 'Pawned items'
      end
      object Label25: TLabel
        Left = 538
        Top = 483
        Width = 43
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'SubTotal'
      end
      object Label16: TLabel
        Left = 520
        Top = 510
        Width = 61
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Contract Fee'
      end
      object Label19: TLabel
        Left = 477
        Top = 555
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
      object Label10: TLabel
        Left = 546
        Top = 534
        Width = 35
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Interest'
      end
      object Label28: TLabel
        Left = 12
        Top = 240
        Width = 72
        Height = 13
        Caption = 'Identity records'
      end
      object NotesLabel: TLabel
        Left = 12
        Top = 483
        Width = 28
        Height = 13
        Caption = 'N&otes'
        FocusControl = SummaryContractNotesRichEdit
      end
      object Label30: TLabel
        Left = 12
        Top = 204
        Width = 48
        Height = 13
        Caption = 'Post code'
      end
      object Label31: TLabel
        Left = 12
        Top = 180
        Width = 25
        Height = 13
        Caption = 'State'
      end
      object Label32: TLabel
        Left = 12
        Top = 161
        Width = 34
        Height = 13
        Caption = 'Suburb'
      end
      object Label33: TLabel
        Left = 12
        Top = 140
        Width = 28
        Height = 13
        Caption = 'Street'
      end
      object Label34: TLabel
        Left = 319
        Top = 210
        Width = 25
        Height = 13
        Caption = 'Email'
      end
      object Label35: TLabel
        Left = 319
        Top = 186
        Width = 71
        Height = 13
        Caption = 'Phone (Mobile)'
      end
      object Label36: TLabel
        Left = 319
        Top = 163
        Width = 66
        Height = 13
        Caption = 'Phone (Work)'
      end
      object Label37: TLabel
        Left = 319
        Top = 140
        Width = 68
        Height = 13
        Caption = 'Phone (Home)'
      end
      object Label38: TLabel
        Left = 12
        Top = 113
        Width = 64
        Height = 13
        Caption = 'Client number'
      end
      object Label39: TLabel
        Left = 196
        Top = 112
        Width = 28
        Height = 13
        Caption = 'Name'
      end
      object Label44: TLabel
        Left = 12
        Top = 83
        Width = 58
        Height = 13
        Caption = 'Contract no.'
      end
      object Label45: TLabel
        Left = 192
        Top = 83
        Width = 30
        Height = 13
        Caption = 'Period'
      end
      object Label46: TLabel
        Left = 472
        Top = 83
        Width = 30
        Height = 13
        Caption = 'Status'
      end
      object SummaryContractItemsListView: TtiListView
        Left = 96
        Top = 344
        Width = 597
        Height = 127
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
            OnDeriveColumn = ContractItemsListViewListColumns5DeriveColumn
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
      object SummaryClientNumberEdit: TEdit
        Left = 96
        Top = 109
        Width = 81
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object SummaryClientNameEdit: TEdit
        Left = 232
        Top = 109
        Width = 329
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 2
        Text = 'ClientNameEdit'
        OnExit = ClientNameEditExit
      end
      object SummaryContractNotesRichEdit: TRichEdit
        Left = 96
        Top = 480
        Width = 329
        Height = 86
        TabStop = False
        Lines.Strings = (
          'SummaryContractNotesRichEdit')
        TabOrder = 3
      end
      object SummaryStreetEdit: TEdit
        Left = 96
        Top = 137
        Width = 209
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 4
        Text = 'StreetEdit'
      end
      object SummarySuburbEdit: TEdit
        Left = 96
        Top = 160
        Width = 209
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 5
        Text = 'SuburbEdit'
      end
      object SummaryStateEdit: TEdit
        Left = 96
        Top = 183
        Width = 161
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 6
        Text = 'StateEdit'
      end
      object SummaryPostCodeEdit: TEdit
        Left = 96
        Top = 207
        Width = 161
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 7
        Text = 'PostCodeEdit'
      end
      object SummaryPhoneHomeEdit: TEdit
        Left = 400
        Top = 137
        Width = 161
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 8
        Text = 'PhoneHomeEdit'
      end
      object SummaryPhoneWorkEdit: TEdit
        Left = 400
        Top = 160
        Width = 161
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 9
        Text = 'PhoneWorkEdit'
      end
      object SummaryPhoneMobileEdit: TEdit
        Left = 400
        Top = 183
        Width = 161
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 10
        Text = 'PhoneMobileEdit'
      end
      object SummaryEmailAddressEdit: TEdit
        Left = 400
        Top = 207
        Width = 249
        Height = 21
        TabStop = False
        ParentColor = True
        ReadOnly = True
        TabOrder = 11
        Text = 'EmailAddressEdit'
      end
      object SummaryClientIdentityRecordListView: TtiListView
        Left = 96
        Top = 240
        Width = 595
        Height = 97
        Anchors = [akLeft, akTop, akRight]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnFilterData = SummaryClientIdentityRecordListViewFilterData
        ApplyFilter = False
        ApplySort = False
        ListColumns = <
          item
            DisplayLabel = 'Identity record type'
            FieldName = 'Caption'
            DataType = lvtkString
            Derived = True
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
      object SubtotalNumericField: TOvcNumericField
        Left = 589
        Top = 480
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
        TabOrder = 13
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object ContractFeeNumericField: TOvcNumericField
        Left = 589
        Top = 503
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
        TabOrder = 14
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object InterestValueNumericField: TOvcNumericField
        Left = 641
        Top = 526
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
        TabOrder = 15
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object InterestRateNumericField: TOvcNumericField
        Left = 589
        Top = 526
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
        TabOrder = 16
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object RedemptionValueNumericField: TOvcNumericField
        Left = 589
        Top = 551
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
        TabOrder = 17
        TabStop = False
        RangeHigh = {73B2DBB9838916F2FE43}
        RangeLow = {73B2DBB9838916F2FEC3}
      end
      object SummaryContractNumberEdit: TEdit
        Left = 96
        Top = 80
        Width = 81
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 18
        Text = 'ContractNumberEdit'
      end
      object SummaryContractPeriodEdit: TEdit
        Left = 233
        Top = 80
        Width = 232
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 19
        Text = 'ContractPeriodEdit'
      end
      object SummaryStatusEdit: TEdit
        Left = 512
        Top = 80
        Width = 97
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 20
        Text = 'StatusEdit'
      end
    end
    object KWizardRouteMapNodes1: TKWizardRouteMapNodes
      Left = 0
      Top = 0
      Width = 153
      Height = 583
      ItemHeight = 20
      Color = 11683841
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Indent = 8
      NodeColors.Selected = clSilver
      NodeColors.Unselected = clWhite
      NodeColors.Line = clBtnShadow
      NodeColors.Disabled = clBtnFace
      UsePageTitle = True
    end
  end
  object ActionList: TActionList
    Images = ImageList
    OnUpdate = ActionListUpdate
    Left = 25
    Top = 120
    object AddClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Add'
      ImageIndex = 13
      ShortCut = 32833
      OnExecute = AddClientIdentityRecordActionExecute
    end
    object ReplaceClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Replace'
      ImageIndex = 27
      ShortCut = 32850
      OnExecute = ReplaceClientIdentityRecordActionExecute
    end
    object DeleteClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Delete'
      ImageIndex = 4
      ShortCut = 32836
      OnExecute = DeleteClientIdentityRecordActionExecute
    end
    object NewClientAction: TAction
      Category = 'Client'
      Caption = 'Ne&w'
      ShortCut = 32855
      OnExecute = NewClientActionExecute
    end
    object FindClientAction: TAction
      Category = 'Client'
      Caption = '&Find'
      ShortCut = 32838
      OnExecute = FindClientActionExecute
    end
    object AddContractItemAction: TAction
      Category = 'Contract items'
      Caption = '&Add'
      ShortCut = 32833
      OnExecute = AddContractItemActionExecute
    end
    object ReplaceContractItemAction: TAction
      Category = 'Contract items'
      Caption = 'Replace'
      OnExecute = ReplaceContractItemActionExecute
    end
    object DeleteContractItemAction: TAction
      Category = 'Contract items'
      Caption = '&Delete'
      ShortCut = 32836
      OnExecute = DeleteContractItemActionExecute
    end
  end
  object ImageList: TImageList
    Left = 65
    Top = 120
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000100000000100180000000000000C
      0000000000000000000000000000000000000000000000000000000000000000
      0000007200007200006B00006600006500006500000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000001B70001
      B7050DAC393EB0585AB25657AB2E2F9000006F00006900006900000000000000
      0000000000000000864200824000783B006E36006A34006A34006A34006A3400
      6A34006A34006A34005A2C00000000000000000000000000864200824000783B
      006E36006A34006A34006A34006A34006A34006A34006A34005A2C0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000016CE0915C66C74
      D9CED1F2FFFFFFFFFFFFFFFFFFFFFFFFC1C2DF5A5A9F01016900007D00000000
      0000000000AC5500CA6400C46100BC5D00B45900AE5600AC5500AC5500AE5600
      AE5600AC5500B258009449005A2C00000000000000AC5500CA6400C46100BC5D
      00B45900AE5600AC5500AC5500AE5600AE5600AC5500B258009449005A2C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018DF0A1DD3A8B0EDFFFF
      FFFFFFFFBDBEE98C8CD18D8DD0CACAEAFFFFFFFFFFFF8C8CBD01016900007200
      0000000000C86300EA7400DE6E00D86B00D26800CA6400C26000C05F00C05F00
      C05F00C05F00C66200B258006A3400000000000000C86300EA7400DE6E00D86B
      00D26800CA6400C26000C05F00C05F00C05F00C05F00C66200B258006A340000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000018DF919DEFFFFFFFE6E8
      F94F53CE0002AB00009D0000950000975F5FBEF0F0FAFFFFFF6565A600007200
      0000000000D46900F47900E87300DC6D00D46900CE6600C66200C05F00BC5D00
      BA5C00B85B00C05F00AE56006C3500000000000000D46900F47900E87300DC6D
      00D46900CE6600C66200C05F00BC5D00BA5C00B85B00C05F00AE56006C350000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001EF12743E7FBFBFFF7F8FD3B4B
      DA0000C0161DBEB0B4E7A3A5E00A0CA000008F5252B9FFFFFFD9D9E90B0B7F00
      007A000000D66A00FF8207F87B00EA7400DE6E00D66A00FFFFFFFFFFFFC26000
      BC5D00BA5C00C05F00AE56006A3400000000000000D66A00FF8207F87B00EA74
      00DE6E00D66A00D06700CA6400C26000BC5D00BA5C00C05F00AE56006A340000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000001EF17287F6FFFFFF91A1F4000D
      DA000BD0161DCBF1F4FEDEE1F60508A900009A000093ACACDDFFFFFF5353AF00
      007A000000D66A00FF9329FF860FF87B00EA7400FEFEFEFFFFFFFFFFFFFFFFFF
      C46100BE5E00C05F00AE56006A3400000000000000D66A00FF9329FF860FFFFF
      FFFFFFFFE06F00DA6C00D46900CC6500FFFFFFFFFFFFC05F00AE56006A340000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000023F8A8B8FCFFFFFF4060F61734
      ECA0AEF2BABEF1F8F9FEF3F4FBB6B8E99799DC0D0EA25A5BBFFFFFFF8487D600
      0079000000D66A00FFA64FFF8E1FFF8003FEFEFEFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFC86300C66200B057006C3500000000000000D66A00FFA64FFF8E1FFFFF
      FFFFFFFFFFFFFFE47100DE6E00FFFFFFFFFFFFFFFFFFC66200B057006C350000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000E3EFEC5CFFEFFFFFF3259FE2649
      F9FAFCFFFFFFFFFFFFFFFFFFFFFFFFFFEBECF91519B14A4FC1FFFFFF9094D900
      00A2000000D66A00FFB369FF952DFFFFFFFFFFFFFFFFFFEC7500E67200FFFFFF
      FFFFFFFFFFFFCE6600B65A00743900000000000000D66A00FFB369FF952DFF84
      0BFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD06700CE6600B65A0074390000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003C63FFC4D0FFFFFFFF5979FF052E
      FF375CFB586DF4F0F3FEE3E6FA4D5ADE3446D20004B7757CD6FFFFFF797DD500
      00A8000000D66A00FFB975FF9A37FFFFFFFFFFFFF47900F27800F07700E87300
      FFFFFFFFFFFFD86B00C26000844100000000000000D66A00FFB975FF9A37FF88
      13FF8003FEFEFEFFFFFFFFFFFFFFFFFFE27000DA6C00D86B00C2600084410000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000103EFFB6C5FFFFFFFFC7D2FF032C
      FF0020FF1739FBF3F6FFE1E5FA071FDC0007CE0C1CCBD9DCF5FDFDFE313CC800
      00A8000000D66A00FFBF81FFA145FF9125FF8C1BFF8309FF7E00FA7C00F27800
      EC7500E67200E27000CC6500924800000000000000D66A00FFBF81FFA145FF91
      25FF8C1BFF8309FFFFFFFFFFFFF27800EC7500E67200E27000CC650092480000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000008DA4FFFFFFFFFFFFFF8AA0
      FF0027FF002CFF466AFF4163F8001DE9061CDFA1ACF1FFFFFFB4BAED0007BB00
      0000000000D66A00FFC68FFFAC5BFF962FFF9329FF8B19FF850DFF840BFF7E00
      F47900EE7600EC7500D469009E4E00000000000000D66A00FFC68FFFAC5BFF96
      2FFF9329FF8B19FF850DFF840BFF7E00F47900EE7600EC7500D469009E4E0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000718DFFDAE1FFFFFFFFFFFF
      FFB1C0FF3B5DFF1538FF1739FE4966F8C0CAFAFFFFFFEDEFFC3041D30007BB00
      0000000000D66A00FFCA97FFC48BFFB771FFAF61FFA64FFF9C3BFF9329FF850D
      FA7C00F47900F47900E27000AE5600000000000000D66A00FFCA97FFC48BFFB7
      71FFAF61FFA64FFF9C3BFF9329FF850DFA7C00F47900F47900E27000AE560000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000088A0FFE6EBFFFFFF
      FFFFFFFFFDFDFFE2E9FFE4EBFFFFFFFFFFFFFFDBE0FA3D50E0000BCC00000000
      0000000000D66A00FFB873FFCA97FFCC9BFFC68FFFBE7FFFB165FF9E3FFF8D1D
      FF8003FA7C00FC7D00EA7400B85B00000000000000D66A00FFB873FFCA97FFCC
      9BFFC68FFFBE7FFFB165FF9E3FFF8D1DFF8003FA7C00FC7D00EA7400B85B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008AA1FFBAC7
      FEE8ECFFFFFFFFFFFFFFFFFFFFE4E9FE889BF61738E6000BCC00000000000000
      0000000000000000F27800FF8A17FF8B19FF8A17FF860FFF8003F27800E67200
      DC6D00D86B00DA6C00C86300000000000000000000000000F27800FF8A17FF8B
      19FF8A17FF860FFF8003F27800E67200DC6D00D86B00DA6C00C8630000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      008AA0FF8AA3FF90A6FF7993FE4A6BF91A40EF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81FFFFFFFFF0000E007C003C0030000
      C003800180010000800180018001000080018001800100000000800180010000
      0000800180010000000080018001000000008001800100000000800180010000
      000080018001000080018001800100008001800180010000C003800180010000
      E007C003C0030000F81FFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object PrintDialog: TPrintDialog
    Left = 97
    Top = 120
  end
end

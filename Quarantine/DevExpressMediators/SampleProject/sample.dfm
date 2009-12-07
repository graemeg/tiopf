object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 510
  ClientWidth = 1114
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 50
    Width = 117
    Height = 13
    Caption = 'Controls with mediators:'
  end
  object Label2: TLabel
    Left = 768
    Top = 47
    Width = 133
    Height = 13
    Caption = 'Controls without mediators:'
  end
  object cxTextEdit1: TcxTextEdit
    Left = 21
    Top = 69
    TabOrder = 0
    Text = 'cxTextEdit1'
    Width = 121
  end
  object bDebug: TcxButton
    Left = 21
    Top = 8
    Width = 140
    Height = 25
    Caption = 'Debug test object'
    TabOrder = 1
    OnClick = bDebugClick
  end
  object cxCheckBox1: TcxCheckBox
    Left = 21
    Top = 123
    Caption = 'cxCheckBox1'
    ParentBackground = False
    ParentColor = False
    Style.Color = clBtnFace
    TabOrder = 2
    Width = 121
  end
  object cxComboBox1: TcxComboBox
    Left = 21
    Top = 150
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 3
    Text = 'cxComboBox1'
    Width = 121
  end
  object cxItemComboBox1: TcxComboBox
    Left = 21
    Top = 177
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 4
    Text = 'cxItemComboBox1'
    Width = 121
  end
  object cxDynamicComboBox1: TcxComboBox
    Left = 21
    Top = 204
    Properties.Items.Strings = (
      'Value 1'
      'Value 2'
      'Value 3')
    TabOrder = 5
    Text = 'cxDynamicComboBox1'
    Width = 121
  end
  object cxLabel1: TcxLabel
    Left = 21
    Top = 231
    AutoSize = False
    Caption = 'cxLabel1'
    Height = 17
    Width = 121
  end
  object cxTrackBar1: TcxTrackBar
    Left = 21
    Top = 254
    TabOrder = 7
    Height = 20
    Width = 121
  end
  object cxMemo1: TcxMemo
    Left = 21
    Top = 307
    Lines.Strings = (
      'cxMemo')
    TabOrder = 8
    Height = 39
    Width = 121
  end
  object cxDateEdit1: TcxDateEdit
    Left = 21
    Top = 352
    EditValue = 39938.6979166667d
    TabOrder = 9
    Width = 121
  end
  object cxSpinEdit1: TcxSpinEdit
    Left = 21
    Top = 280
    Properties.ValueType = vtInt
    TabOrder = 10
    Width = 121
  end
  object cxMaskEdit1: TcxMaskEdit
    Left = 21
    Top = 96
    TabOrder = 11
    Text = 'cxMaskEdit1'
    Width = 121
  end
  object cxButtonEdit1: TcxButtonEdit
    Left = 21
    Top = 379
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    TabOrder = 12
    Text = 'cxButtonEdit1'
    Width = 121
  end
  object cxImageComboBox1: TcxImageComboBox
    Left = 912
    Top = 150
    Properties.Items = <>
    TabOrder = 13
    Width = 121
  end
  object cxHyperLinkEdit1: TcxHyperLinkEdit
    Left = 21
    Top = 433
    TabOrder = 14
    Text = 'cxHyperLinkEdit1'
    Width = 121
  end
  object cxTimeEdit1: TcxTimeEdit
    Left = 21
    Top = 460
    EditValue = 40178.5993055556d
    TabOrder = 15
    Width = 121
  end
  object cxCurrencyEdit1: TcxCurrencyEdit
    Left = 148
    Top = 69
    Properties.DecimalPlaces = -1
    TabOrder = 16
    Width = 121
  end
  object cxImage1: TcxImage
    Left = 768
    Top = 71
    TabOrder = 17
    Height = 100
    Width = 121
  end
  object cxBlobEdit1: TcxBlobEdit
    Left = 912
    Top = 437
    Properties.BlobEditKind = bekBlob
    TabOrder = 18
    Width = 121
  end
  object cxMRUEdit1: TcxMRUEdit
    Left = 148
    Top = 96
    TabOrder = 19
    Text = 'cxMRUEdit1'
    Width = 121
  end
  object cxPopupEdit1: TcxPopupEdit
    Left = 148
    Top = 123
    Properties.PopupControl = cxMemo1
    TabOrder = 20
    Text = 'cxPopupEdit1'
    Width = 121
  end
  object cxLookupComboBox1: TcxLookupComboBox
    Left = 912
    Top = 464
    Properties.ListColumns = <>
    TabOrder = 21
    Width = 121
  end
  object cxRadioGroup1: TcxRadioGroup
    Left = 148
    Top = 150
    Caption = 'cxRadioGroup1'
    Properties.Items = <
      item
        Caption = 'Item 1'
      end
      item
        Caption = 'Item 2'
      end
      item
        Caption = 'Item 3'
      end
      item
        Caption = 'Item 4'
      end
      item
        Caption = 'Item 5'
      end>
    TabOrder = 22
    Height = 105
    Width = 121
  end
  object cxListBox1: TcxListBox
    Left = 768
    Top = 177
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 23
  end
  object cxProgressBar1: TcxProgressBar
    Left = 148
    Top = 280
    Position = 50.000000000000000000
    Properties.PeakValue = 50.000000000000000000
    TabOrder = 24
    Width = 121
  end
  object cxCheckListBox1: TcxCheckListBox
    Left = 768
    Top = 307
    Width = 121
    Height = 97
    Items = <>
    TabOrder = 25
  end
  object cxColorComboBox1: TcxColorComboBox
    Left = 148
    Top = 307
    ColorValue = clActiveCaption
    Properties.CustomColors = <>
    TabOrder = 26
    Width = 121
  end
  object cxFontNameComboBox1: TcxFontNameComboBox
    Left = 768
    Top = 437
    TabOrder = 27
    Width = 121
  end
  object cxCheckComboBox1: TcxCheckComboBox
    Left = 768
    Top = 464
    Properties.Items = <>
    TabOrder = 28
    Width = 121
  end
  object cxCheckGroup1: TcxCheckGroup
    Left = 912
    Top = 177
    Caption = 'cxCheckGroup1'
    Properties.Items = <>
    TabOrder = 29
    Height = 105
    Width = 185
  end
  object cxRichEdit1: TcxRichEdit
    Left = 148
    Top = 334
    Lines.Strings = (
      'cxRichEdit1')
    TabOrder = 30
    Height = 89
    Width = 121
  end
  object cxShellComboBox1: TcxShellComboBox
    Left = 912
    Top = 383
    TabOrder = 31
    Width = 121
  end
  object cxExtLookupComboBox1: TcxExtLookupComboBox
    Left = 912
    Top = 410
    TabOrder = 32
    Width = 145
  end
  object cxGrid1: TcxGrid
    Left = 313
    Top = 8
    Width = 377
    Height = 266
    TabOrder = 33
    RootLevelOptions.DetailTabsPosition = dtpTop
    object cxgbtvTest: TcxGridBandedTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Bands = <
        item
        end>
    end
    object cxgtvTest: TcxGridTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
    end
    object cxGrid1Level1: TcxGridLevel
      Caption = 'TableView'
      GridView = cxgtvTest
    end
    object cxGrid1Level2: TcxGridLevel
      Caption = 'BandedTable'
      GridView = cxgbtvTest
    end
  end
  object bDebugList: TcxButton
    Left = 167
    Top = 8
    Width = 140
    Height = 25
    Caption = 'Debug test object list'
    TabOrder = 34
    OnClick = bDebugListClick
  end
  object cxCalcEdit1: TcxCalcEdit
    Left = 21
    Top = 406
    EditValue = 0.000000000000000000
    TabOrder = 35
    Width = 121
  end
  object cxvtlTest: TcxVirtualTreeList
    Left = 313
    Top = 280
    Width = 377
    Height = 106
    Bands = <>
    TabOrder = 36
  end
  object cxvtlTest2: TcxVirtualTreeList
    Left = 313
    Top = 392
    Width = 377
    Height = 113
    Bands = <>
    TabOrder = 37
  end
  object tiModelMediator1: TtiModelMediator
    PropertyLinks = <>
    Left = 152
    Top = 40
  end
  object tiModelMediator2: TtiModelMediator
    PropertyLinks = <>
    Left = 184
    Top = 40
  end
  object tiModelMediator3: TtiModelMediator
    PropertyLinks = <>
    Left = 216
    Top = 40
  end
end

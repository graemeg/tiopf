inherited FormPersonEdit: TFormPersonEdit
  Caption = 'Edit a person'
  ClientHeight = 352
  ClientWidth = 511
  PixelsPerInch = 96
  TextHeight = 13
  inherited lblErrors: TLabel
    Top = 267
    Width = 495
  end
  inherited btnOK: TBitBtn
    Left = 351
    Top = 322
    TabOrder = 3
  end
  inherited btnCancel: TBitBtn
    Left = 431
    Top = 322
    TabOrder = 4
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 326
    TabOrder = 7
  end
  object paeTitle: TtiPerAwareEdit
    Left = 8
    Top = 12
    Width = 137
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 0
    LabelLayout = tlTop
    Caption = '&Title'
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
  object paeFirstName: TtiPerAwareEdit
    Left = 8
    Top = 36
    Width = 209
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 1
    LabelLayout = tlTop
    Caption = '&First name'
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
  object paeLastName: TtiPerAwareEdit
    Left = 8
    Top = 60
    Width = 209
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 2
    LabelLayout = tlTop
    Caption = '&Last name'
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
  object LVAdrs: TtiVTListView
    Left = 8
    Top = 96
    Width = 253
    Height = 165
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    EditInlineOnly = False
    Searching = False
    ShowNodeHint = False
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 249
    VT.Height = 111
    VT.Align = alClient
    VT.Colors.UnfocusedColor = clMedGray
    VT.Header.AutoSizeIndex = 0
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'Tahoma'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 1
    VT.TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    VT.Columns = <>
    OnFilterData = LVAdrsFilterData
    OnItemDelete = LVAdrsItemDelete
    OnItemEdit = LVAdrsItemEdit
    OnItemInsert = LVAdrsItemInsert
  end
  object LVEAdrs: TtiVTListView
    Left = 267
    Top = 96
    Width = 236
    Height = 165
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    EditInlineOnly = False
    Searching = False
    ShowNodeHint = False
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 232
    VT.Height = 111
    VT.Align = alClient
    VT.Colors.UnfocusedColor = clMedGray
    VT.Header.AutoSizeIndex = 0
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'Tahoma'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 1
    VT.TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
    VT.Columns = <>
    OnFilterData = LVAdrsFilterData
    OnItemDelete = LVEAdrsItemDelete
    OnItemEdit = LVEAdrsItemEdit
    OnItemInsert = LVEAdrsItemInsert
  end
end

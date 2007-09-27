inherited frmVendorList: TfrmVendorList
  Caption = 'Vendors'
  ClientHeight = 490
  ClientWidth = 586
  ExplicitWidth = 602
  ExplicitHeight = 526
  PixelsPerInch = 96
  TextHeight = 13
  inherited LV: TtiVTListView
    Width = 586
    Height = 272
    VT.Width = 582
    VT.Height = 218
    VT.ExplicitWidth = 582
    VT.ExplicitHeight = 218
    OnItemArrive = LVItemArrive
    OnItemLeave = LVItemLeave
    ExplicitWidth = 586
    ExplicitHeight = 272
  end
  inherited Panel1: TPanel
    Top = 450
    Width = 586
    ExplicitTop = 450
    ExplicitWidth = 586
  end
  object lvParts: TtiVTListView [2]
    Left = 0
    Top = 272
    Width = 586
    Height = 178
    Align = alBottom
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    ShowAlternateRowColor = False
    ShowNodeHint = False
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 582
    VT.Height = 124
    VT.Align = alClient
    VT.Header.AutoSizeIndex = 0
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'MS Sans Serif'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 0
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toFullRowSelect]
    VT.Columns = <>
    OnFilterData = LVFilterData
    OnItemDelete = LVItemDelete
    OnItemEdit = LVItemEdit
    OnItemInsert = LVItemInsert
  end
end

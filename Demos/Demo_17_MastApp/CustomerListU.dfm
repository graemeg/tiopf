inherited frmCustomerList: TfrmCustomerList
  Caption = 'Customers'
  ClientHeight = 458
  PixelsPerInch = 96
  TextHeight = 13
  inherited LV: TtiVTListView
    Height = 233
    Align = alTop
    VT.Height = 179
    OnItemArrive = LVItemArrive
    OnItemLeave = LVItemLeave
  end
  inherited Panel1: TPanel
    Top = 418
  end
  object lvOrders: TtiVTListView [2]
    Left = 0
    Top = 233
    Width = 458
    Height = 185
    Align = alClient
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
    VT.Width = 454
    VT.Height = 131
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
    OnItemDelete = lvOrdersItemDelete
    OnItemEdit = lvOrdersItemEdit
    OnItemInsert = lvOrdersItemInsert
  end
end

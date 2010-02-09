object FormMainOrdinalTypes: TFormMainOrdinalTypes
  Left = 315
  Top = 180
  Caption = 'FormMainOrdinalTypes'
  ClientHeight = 253
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 384
    Top = 188
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 384
    Top = 156
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 384
    Top = 220
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object LV: TtiVTListView
    Left = 5
    Top = 8
    Width = 373
    Height = 237
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
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
    VT.Width = 369
    VT.Height = 183
    VT.Align = alClient
    VT.Colors.UnfocusedColor = clMedGray
    VT.Header.AutoSizeIndex = 0
    VT.Header.DefaultHeight = 17
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'MS Sans Serif'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoShowSortGlyphs, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 1
    VT.TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toFullRowSelect]
    VT.Columns = <>
    OnFilterData = LVFilterData
    OnItemDelete = LVItemDelete
    OnItemEdit = LVItemEdit
    OnItemInsert = LVItemInsert
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 452
    Top = 56
    object aSave: TAction
      Caption = 'Save'
      OnExecute = aSaveExecute
    end
    object aRead: TAction
      Caption = 'Read'
      OnExecute = aReadExecute
    end
  end
end

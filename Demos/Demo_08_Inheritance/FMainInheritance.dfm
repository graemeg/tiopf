object FormMainInheritance: TFormMainInheritance
  Left = 297
  Top = 107
  Caption = 'FormMainInheritance'
  ClientHeight = 344
  ClientWidth = 572
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
  DesignSize = (
    572
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object paeClientCount: TtiPerAwareFloatEdit
    Left = 459
    Top = 24
    Width = 101
    Height = 41
    Anchors = [akTop]
    Constraints.MinHeight = 23
    TabOrder = 0
    LabelStyle = lsTop
    LabelLayout = tlTop
    Caption = 'Client count'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
    ExplicitLeft = 421
  end
  object btnInsert: TButton
    Left = 458
    Top = 71
    Width = 101
    Height = 25
    Anchors = [akTop]
    Caption = 'Insert clients'
    TabOrder = 1
    OnClick = btnInsertClick
    ExplicitLeft = 421
  end
  object Button2: TButton
    Left = 463
    Top = 241
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 2
    OnClick = Button2Click
    ExplicitLeft = 421
  end
  object Button1: TButton
    Left = 461
    Top = 272
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 3
    ExplicitLeft = 420
  end
  object btnRead: TButton
    Left = 461
    Top = 304
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 4
    ExplicitLeft = 420
  end
  object lvClients: TtiVTListView
    Left = 8
    Top = 12
    Width = 437
    Height = 317
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsXPStyle
    ShowAlternateRowColor = False
    ShowNodeHint = False
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 433
    VT.Height = 289
    VT.Align = alClient
    VT.Header.AutoSizeIndex = 0
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'Tahoma'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 0
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toFullRowSelect]
    VT.ExplicitTop = 2
    VT.ExplicitWidth = 181
    VT.ExplicitHeight = 37
    VT.Columns = <>
    OnFilterData = lvClientsFilterData
    OnItemDelete = lvClientsItemDelete
    OnItemEdit = lvClientsItemEdit
    OnItemInsert = lvClientsItemInsert
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 456
    Top = 104
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

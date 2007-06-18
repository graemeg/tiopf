object FormMainOneToMany: TFormMainOneToMany
  Left = 202
  Top = 132
  Caption = 'FormMainOneToMany'
  ClientHeight = 397
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Clients'
  end
  object Label2: TLabel
    Left = 8
    Top = 224
    Width = 74
    Height = 13
    Caption = 'Phone numbers'
  end
  object Button1: TButton
    Left = 408
    Top = 333
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 408
    Top = 301
    Width = 101
    Height = 25
    Action = aShowObjects
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object btnRead: TButton
    Left = 408
    Top = 365
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object paeClientCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 24
    Width = 110
    Height = 41
    Constraints.MinHeight = 23
    TabOrder = 3
    LabelStyle = lsTop
    Caption = 'Client count'
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
  end
  object paePhoneNumberCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 72
    Width = 110
    Height = 41
    Constraints.MinHeight = 23
    TabOrder = 4
    LabelStyle = lsTop
    Caption = 'Phone number count'
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
  end
  object btnInsert: TButton
    Left = 408
    Top = 119
    Width = 102
    Height = 25
    Action = aInsertClients
    TabOrder = 5
  end
  object lvClients: TtiVTListView
    Left = 8
    Top = 27
    Width = 369
    Height = 182
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
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 365
    VT.Height = 154
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
    VT.Columns = <>
    OnFilterData = lvClientsFilterData
    OnItemArrive = lvClientsItemArrive
    OnItemDelete = lvClientsItemDelete
    OnItemEdit = lvClientsItemEdit
    OnItemInsert = lvClientsItemInsert
    OnItemLeave = lvClientsItemLeave
  end
  object lvPhoneNumbers: TtiVTListView
    Left = 12
    Top = 243
    Width = 369
    Height = 146
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
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 365
    VT.Height = 118
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
    VT.Columns = <>
    OnCanInsert = lvPhoneNumbersCanInsert
    OnFilterData = lvClientsFilterData
    OnItemDelete = lvPhoneNumbersItemDelete
    OnItemEdit = lvPhoneNumbersItemEdit
    OnItemInsert = lvPhoneNumbersItemInsert
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 388
    Top = 208
    object aSave: TAction
      Caption = 'Save'
      OnExecute = aSaveExecute
    end
    object aRead: TAction
      Caption = 'Read'
      OnExecute = aReadExecute
    end
    object aInsertClients: TAction
      Caption = 'Insert clients'
      OnExecute = aInsertClientsExecute
    end
    object aShowObjects: TAction
      Caption = 'Show objects'
      OnExecute = aShowObjectsExecute
    end
  end
end

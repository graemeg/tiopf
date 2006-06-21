object FormMainInheritance: TFormMainInheritance
  Left = 297
  Top = 107
  Width = 539
  Height = 378
  Caption = 'FormMainInheritance'
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
    531
    351)
  PixelsPerInch = 96
  TextHeight = 13
  object lvClient: TtiListView
    Left = 20
    Top = 24
    Width = 357
    Height = 313
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    ViewStyle = vsReport
    RowSelect = True
    OnItemEdit = lvClientItemEdit
    OnItemInsert = lvClientItemInsert
    OnItemDelete = lvClientItemDelete
    OnFilterData = lvClientFilterData
    ApplyFilter = True
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
    ButtonStyle = lvbsLargeButtons
    CanStartDrag = False
    DesignSize = (
      357
      313)
  end
  object paeClientCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 24
    Width = 110
    Height = 41
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    LabelStyle = lsTop
    Caption = 'Client count'
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
  end
  object btnInsert: TButton
    Left = 416
    Top = 72
    Width = 102
    Height = 25
    Caption = 'Insert clients'
    TabOrder = 2
    OnClick = btnInsertClick
  end
  object Button2: TButton
    Left = 420
    Top = 240
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 420
    Top = 272
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 4
  end
  object btnRead: TButton
    Left = 420
    Top = 304
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 5
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
  end
end

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
  object Button2: TButton
    Left = 420
    Top = 240
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 420
    Top = 272
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object btnRead: TButton
    Left = 420
    Top = 304
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 3
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

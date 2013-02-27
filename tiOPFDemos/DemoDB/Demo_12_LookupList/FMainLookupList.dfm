object FormMainLookupList: TFormMainLookupList
  Left = 249
  Top = 184
  Width = 498
  Height = 213
  Caption = 'FormMainLookupList'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    490
    186)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 384
    Top = 124
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 384
    Top = 92
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 384
    Top = 156
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object LV: TtiListView
    Left = 4
    Top = 8
    Width = 373
    Height = 173
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    ViewStyle = vsReport
    RowSelect = True
    OnItemEdit = LVItemEdit
    OnItemInsert = LVItemInsert
    OnItemDelete = LVItemDelete
    OnFilterData = LVFilterData
    ApplyFilter = True
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
    ButtonStyle = lvbsLargeButtons
    CanStartDrag = False
    DesignSize = (
      373
      173)
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

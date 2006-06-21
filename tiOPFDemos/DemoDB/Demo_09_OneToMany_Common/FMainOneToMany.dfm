object FormMainOneToMany: TFormMainOneToMany
  Left = 202
  Top = 132
  Width = 531
  Height = 367
  Caption = 'FormMainOneToMany'
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
    523
    340)
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
    Left = 4
    Top = 160
    Width = 74
    Height = 13
    Caption = 'Phone numbers'
  end
  object Button1: TButton
    Left = 417
    Top = 278
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 417
    Top = 246
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Show Objects'
    TabOrder = 1
    OnClick = Button2Click
  end
  object btnRead: TButton
    Left = 417
    Top = 310
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object lvClient: TtiListView
    Left = 20
    Top = 24
    Width = 357
    Height = 129
    ShowFocusRect = True
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
    OnItemArive = lvClientItemArive
    OnItemLeave = lvClientItemLeave
    VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
    ButtonStyle = lvbsLargeButtons
    CanStartDrag = False
    DesignSize = (
      357
      129)
  end
  object lvPhoneNumber: TtiListView
    Left = 20
    Top = 184
    Width = 357
    Height = 149
    ShowFocusRect = True
    MultiSelect = False
    ViewStyle = vsReport
    RowSelect = True
    OnItemEdit = lvPhoneNumberItemEdit
    OnItemInsert = lvPhoneNumberItemInsert
    OnItemDelete = lvPhoneNumberItemDelete
    OnFilterData = lvClientFilterData
    ApplyFilter = True
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
    ButtonStyle = lvbsLargeButtons
    OnCanInsert = lvPhoneNumberCanInsert
    CanStartDrag = False
    DesignSize = (
      357
      149)
  end
  object paeClientCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 24
    Width = 110
    Height = 41
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
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
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 6
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
    Left = 416
    Top = 120
    Width = 102
    Height = 25
    Caption = 'Insert clients'
    TabOrder = 7
    OnClick = btnInsertClick
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

object FormCollectionHardCoded: TFormCollectionHardCoded
  Left = 262
  Top = 204
  Width = 512
  Height = 266
  Caption = 'Collection - hard coded SQL with filter'
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
  object lblCount: TLabel
    Left = 136
    Top = 222
    Width = 34
    Height = 13
    Caption = 'Count: '
  end
  object paeClientName: TtiPerAwareEdit
    Left = 136
    Top = 8
    Width = 281
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Client name starts with:'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 200
    CharCase = ecNormal
    PasswordChar = #0
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 8
    Width = 113
    Height = 89
    TabStop = False
    Lines.Strings = (
      'This demo shows how '
      'to use hard coded SQL '
      'and custom Visitors to '
      'return a filtered list of '
      'objects.')
  end
  object btnSearch: TButton
    Left = 424
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Search'
    Default = True
    TabOrder = 2
    OnClick = btnSearchClick
  end
  object LV: TtiListView
    Left = 136
    Top = 40
    Width = 360
    Height = 177
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    CanStartDrag = False
  end
end

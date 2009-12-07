object FormCollectionHardCoded: TFormCollectionHardCoded
  Left = 262
  Top = 204
  Caption = 'Collection - hard coded SQL with filter'
  ClientHeight = 355
  ClientWidth = 498
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
    Left = 8
    Top = 335
    Width = 34
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Count: '
  end
  object paeClientName: TtiPerAwareEdit
    Left = 9
    Top = 12
    Width = 281
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Client name starting with'
    LabelWidth = 120
    ReadOnly = False
    MaxLength = 200
    CharCase = ecNormal
    PasswordChar = #0
  end
  object btnSearch: TButton
    Left = 296
    Top = 12
    Width = 75
    Height = 25
    Caption = 'Search'
    Default = True
    TabOrder = 1
    OnClick = btnSearchClick
  end
  object LV: TtiVTListView
    Left = 8
    Top = 48
    Width = 482
    Height = 281
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
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VT.Left = 2
    VT.Top = 2
    VT.Width = 478
    VT.Height = 277
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
  end
end

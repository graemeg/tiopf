object FormCollection: TFormCollection
  Left = 262
  Top = 204
  ClientHeight = 394
  ClientWidth = 593
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
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 593
    Height = 105
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paeOID: TtiPerAwareEdit
      Left = 8
      Top = 10
      Width = 321
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'OID'
      ReadOnly = True
      MaxLength = 36
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeClientName: TtiPerAwareEdit
      Left = 8
      Top = 38
      Width = 321
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Client name'
      ReadOnly = True
      MaxLength = 200
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeClientID: TtiPerAwareEdit
      Left = 8
      Top = 67
      Width = 185
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = 'Client ID'
      ReadOnly = True
      MaxLength = 9
      CharCase = ecNormal
      PasswordChar = #0
    end
    object btnInsertRow: TButton
      Left = 335
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Insert object into list'
      TabOrder = 3
      OnClick = btnInsertRowClick
    end
    object btnDeleteRow: TButton
      Left = 335
      Top = 41
      Width = 113
      Height = 25
      Caption = 'Delete object in list'
      TabOrder = 4
      OnClick = btnDeleteRowClick
    end
    object Button2: TButton
      Left = 463
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Show Objects in list'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 463
      Top = 41
      Width = 113
      Height = 25
      Action = aSave
      TabOrder = 6
    end
    object btnReadList: TButton
      Left = 463
      Top = 72
      Width = 113
      Height = 25
      Caption = 'Read list from DB'
      TabOrder = 7
      OnClick = btnReadListClick
    end
  end
  object LV: TtiVTListView
    Left = 0
    Top = 105
    Width = 593
    Height = 289
    Align = alClient
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
    VT.Width = 589
    VT.Height = 285
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
    OnFilterData = LVFilterData
    OnItemArrive = LVItemArrive
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 296
    Top = 68
    object aSave: TAction
      Caption = 'Save'
      OnExecute = aSaveExecute
    end
  end
end

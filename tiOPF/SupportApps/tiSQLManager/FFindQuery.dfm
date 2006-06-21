object FormFindQuery: TFormFindQuery
  Left = 339
  Top = 111
  Width = 416
  Height = 244
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderIcons = [biSystemMenu]
  Caption = 'Find Query'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    408
    210)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Search For'
  end
  object lblCount: TLabel
    Left = 20
    Top = 188
    Width = 52
    Height = 13
    Caption = 'Count: 999'
  end
  object rbQueryName: TRadioButton
    Left = 16
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Name'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object rbQueryText: TRadioButton
    Left = 144
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Query Text'
    Enabled = False
    TabOrder = 4
  end
  object bbOK: TBitBtn
    Left = 245
    Top = 185
    Width = 75
    Height = 25
    Action = aOK
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkOK
  end
  object bbCancel: TBitBtn
    Left = 329
    Top = 185
    Width = 75
    Height = 25
    Action = aCancel
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
  end
  object LVResult: TtiListViewPlus
    Left = 20
    Top = 84
    Width = 381
    Height = 93
    ShowFocusRect = True
    RuntimeGenCols = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    OnDblClick = LVResultDblClick
    OnKeyDown = LVResultKeyDown
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <
      item
        DisplayLabel = 'Query group name'
        FieldName = 'QueryGroupName'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Query name'
        FieldName = 'QueryName'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end>
    SortOrders = <>
    ConfigFeatures = [lvcfSort, lvcfExport]
    ConfigHelpContext = 0
    DesignSize = (
      381
      93)
  end
  object paeSearchText: TtiPerAwareComboBoxHistory
    Left = 20
    Top = 52
    Width = 381
    Height = 23
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight]
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'Enter search text'
    ReadOnly = False
    OnChange = hcSearchTextChange
    DropDownCount = 8
    CharCase = ecNormal
    Items.Strings = (
      '')
    HistoryCount = 5
  end
  object AL: TActionList
    OnUpdate = ALUpdate
    Left = 296
    Top = 8
    object aOK: TAction
      Caption = 'OK'
      OnExecute = aOKExecute
    end
    object aCancel: TAction
      Caption = 'Cancel'
      OnExecute = aCancelExecute
    end
    object aPerformSearch: TAction
      Caption = '!'
    end
  end
  object Tmr: TTimer
    Enabled = False
    OnTimer = TmrTimer
    Left = 332
    Top = 8
  end
end

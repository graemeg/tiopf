object ClientListForm: TClientListForm
  Left = 244
  Top = 202
  BorderStyle = bsNone
  Caption = 'ClientListForm'
  ClientHeight = 513
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ClientListView: TtiListView
    Left = 0
    Top = 64
    Width = 775
    Height = 449
    Align = alClient
    MultiSelect = False
    OnDblClick = ClientListViewDblClick
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <
      item
        DisplayLabel = 'Client No.'
        FieldName = 'ClientNumber'
        DataType = lvtkString
        Derived = False
      end
      item
        DisplayLabel = 'Given names'
        FieldName = 'GivenNames'
        DataType = lvtkString
        Derived = False
      end
      item
        DisplayLabel = 'Family name'
        FieldName = 'FamilyName'
        DataType = lvtkString
        Derived = False
      end
      item
        DisplayLabel = 'Undesirable'
        FieldName = 'Caption'
        DataType = lvtkString
        Derived = True
        OnDeriveColumn = ClientListViewListColumns3DeriveColumn
      end>
    SortOrders = <>
    RuntimeGenCols = False
    CanStartDrag = False
  end
  object SearchResultPanel: TPanel
    Left = 0
    Top = 23
    Width = 775
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    object SearchResultLabel: TLabel
      Left = 8
      Top = 8
      Width = 141
      Height = 23
      Caption = 'Search Results'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object TBDock: TTBDock
    Left = 0
    Top = 0
    Width = 775
    Height = 23
    object ClientListFormToolbar: TTBToolbar
      Left = 0
      Top = 0
      Align = alTop
      Caption = 'ClientListFormToolbar'
      CloseButton = False
      DockRow = 1
      DragHandleStyle = dhDouble
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      DockTextAlign = taLeftJustify
      object TBItem5: TTBItem
        Action = NewClientAction
      end
    end
  end
  object ActionList: TActionList
    Left = 80
    Top = 88
    object NewClientAction: TAction
      Caption = 'New'
      OnExecute = NewClientActionExecute
    end
  end
end

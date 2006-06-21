object ClientIdentityRecordsFrame: TClientIdentityRecordsFrame
  Left = 0
  Top = 0
  Width = 666
  Height = 467
  TabOrder = 0
  object Label4: TLabel
    Left = 12
    Top = 120
    Width = 72
    Height = 13
    Caption = 'Identity records'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 40
    Width = 641
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object Label13: TLabel
    Left = 12
    Top = 16
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object Label8: TLabel
    Left = 12
    Top = 44
    Width = 32
    Height = 13
    Caption = 'Details'
  end
  object AddClientIdentityRecordButton: TBitBtn
    Left = 96
    Top = 71
    Width = 57
    Height = 22
    Action = AddClientIdentityRecordAction
    Caption = '&Add'
    TabOrder = 2
  end
  object ReplaceClientIdentityRecordButton: TBitBtn
    Left = 162
    Top = 71
    Width = 57
    Height = 22
    Action = ReplaceClientIdentityRecordAction
    Caption = '&Replace'
    TabOrder = 3
  end
  object DeleteClientIdentityRecordButton: TBitBtn
    Left = 227
    Top = 71
    Width = 57
    Height = 22
    Action = DeleteClientIdentityRecordAction
    Caption = '&Delete'
    TabOrder = 4
  end
  object IdentityRecordDetailsEdit: TEdit
    Left = 96
    Top = 40
    Width = 273
    Height = 21
    TabOrder = 1
    Text = 'IdentityRecordDetailsEdit'
  end
  object IdentityRecordTypeComboBox: TComboBox
    Left = 96
    Top = 13
    Width = 273
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'IdentityRecordTypeComboBox'
    OnDropDown = IdentityRecordTypeComboBoxDropDown
  end
  object ClientIdentityRecordListView: TtiListViewPlus
    Left = 96
    Top = 120
    Width = 545
    Height = 329
    RuntimeGenCols = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    OnDblClick = ClientIdentityRecordListViewDblClick
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 305
    Top = 64
    object AddClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Add'
      ImageIndex = 13
      ShortCut = 32833
      OnExecute = AddClientIdentityRecordActionExecute
    end
    object ReplaceClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Replace'
      ImageIndex = 27
      ShortCut = 32850
      OnExecute = ReplaceClientIdentityRecordActionExecute
    end
    object DeleteClientIdentityRecordAction: TAction
      Category = 'ClientIdentityRecords'
      Caption = '&Delete'
      ImageIndex = 4
      ShortCut = 32836
      OnExecute = DeleteClientIdentityRecordActionExecute
    end
  end
end

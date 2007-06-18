inherited FormEditPerson: TFormEditPerson
  Left = 363
  Top = 110
  Caption = 'FormEditPerson'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    TabOrder = 1
    inherited btnSave: TBitBtn
      Left = 277
    end
    inherited btnUndo: TBitBtn
      Left = 357
    end
    inherited btnClose: TBitBtn
      Left = 438
    end
  end
  object GroupBox1: TGroupBox [1]
    Left = 4
    Top = 38
    Width = 508
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Name details '
    TabOrder = 0
    object paeLastName: TtiPerAwareEdit
      Left = 268
      Top = 16
      Width = 185
      Height = 39
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 3
      LabelStyle = lsTop
      Caption = '&Last name'
      ReadOnly = False
      MaxLength = 60
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeFirstName: TtiPerAwareEdit
      Left = 72
      Top = 16
      Width = 137
      Height = 39
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      LabelStyle = lsTop
      Caption = '&First name'
      ReadOnly = False
      MaxLength = 60
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeInitials: TtiPerAwareEdit
      Left = 212
      Top = 16
      Width = 53
      Height = 39
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      LabelStyle = lsTop
      Caption = '&Initials'
      ReadOnly = False
      MaxLength = 10
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeTitle: TtiPerAwareComboBoxStatic
      Left = 16
      Top = 16
      Width = 53
      Height = 39
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      LabelStyle = lsTop
      Caption = '&Title'
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        'Ms'
        'Miss'
        'Mrs'
        'Mr'
        'Dr'
        'Prof')
    end
  end
  object tiSplitterPanel1: TtiSplitterPanel [2]
    Left = 4
    Top = 105
    Width = 509
    Height = 311
    Aligned = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColorGrabBar = 16686723
    ColorPanel = clBtnFace
    PanelStyle = spsFramed
    SplitterOrientation = spoHorizontal
    KeepSplitterPosPercent = True
    SplitterPos = 151
    SplitterPosPercent = 49
    Panel1Controls = (
      tiSplitterPanel2)
    Panel2Controls = (
      paeNotes)
    object tiSplitterPanel2: TtiSplitterPanel
      Left = 2
      Top = 2
      Width = 505
      Height = 147
      Aligned = alClient
      ColorGrabBar = 16686723
      ColorPanel = clBtnFace
      BevelInnerSubPanels = bvNone
      BevelOuterSubPanels = bvNone
      PanelStyle = spsNone
      SplitterOrientation = spoVertical
      KeepSplitterPosPercent = True
      SplitterPos = 263
      SplitterPosPercent = 52
      Panel1Controls = (
        Label1
        lvEAddress)
      Panel2Controls = (
        Label2
        lvAddress)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 182
        Height = 13
        Caption = ' Phone, Fax, EMail  && Web Addresses '
      end
      object lvEAddress: TtiListView
        Left = 8
        Top = 28
        Width = 248
        Height = 111
        ShowFocusRect = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnItemEdit = lvEAddressItemEdit
        OnItemInsert = lvEAddressItemInsert
        OnItemDelete = lvEAddressItemDelete
        OnFilterData = LVEAddressFilterData
        ApplyFilter = True
        ApplySort = False
        ListColumns = <
          item
            DisplayLabel = 'Address Type'
            FieldName = 'AdrsTypeAsString'
            DataType = lvtkString
            Derived = False
            Alignment = taLeftJustify
          end
          item
            DisplayLabel = 'Details'
            FieldName = 'Caption'
            DataType = lvtkString
            Derived = False
            Alignment = taLeftJustify
          end>
        SortOrders = <>
        RuntimeGenCols = False
        VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
        ButtonStyle = lvbsNormalButtons
        CanStartDrag = False
      end
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 124
        Height = 13
        Caption = 'Postal && Street Addresses '
      end
      object lvAddress: TtiListView
        Left = 8
        Top = 28
        Width = 219
        Height = 112
        ShowFocusRect = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnItemEdit = lvAddressItemEdit
        OnItemInsert = lvAddressItemInsert
        OnItemDelete = lvEAddressItemDelete
        OnFilterData = LVEAddressFilterData
        ApplyFilter = True
        ApplySort = False
        ListColumns = <
          item
            DisplayLabel = 'Address type'
            FieldName = 'AdrsTypeAsString'
            DataType = lvtkString
            Derived = False
            Alignment = taLeftJustify
          end
          item
            DisplayLabel = 'Details'
            FieldName = 'Caption'
            DataType = lvtkString
            Derived = False
            Alignment = taLeftJustify
          end>
        SortOrders = <>
        RuntimeGenCols = False
        VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
        ButtonStyle = lvbsNormalButtons
        CanStartDrag = False
      end
    end
    object paeNotes: TtiPerAwareMemo
      Left = 12
      Top = 8
      Width = 491
      Height = 136
      ShowFocusRect = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      Constraints.MinHeight = 23
      TabOrder = 0
      LabelStyle = lsTop
      Caption = '&Notes'
      ReadOnly = False
      ScrollBars = ssNone
      WordWrap = True
      MaxLength = 0
    end
  end
end

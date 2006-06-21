object FormEditPerson: TFormEditPerson
  Left = 171
  Top = 112
  Width = 441
  Height = 412
  Caption = 'FormEditPerson'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    433
    385)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 0
    Width = 426
    Height = 85
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Name details '
    TabOrder = 0
    object paeLastName: TtiPerAwareEdit
      Left = 8
      Top = 20
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = '&Last name'
      ReadOnly = False
      OnChange = paeLastNameChange
      MaxLength = 60
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeFirstName: TtiPerAwareEdit
      Left = 8
      Top = 48
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = '&First name'
      ReadOnly = False
      OnChange = paeLastNameChange
      MaxLength = 60
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeInitials: TtiPerAwareEdit
      Left = 216
      Top = 20
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = '&Initials'
      ReadOnly = False
      OnChange = paeLastNameChange
      MaxLength = 10
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeTitle: TtiPerAwareComboBoxStatic
      Left = 216
      Top = 48
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 3
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
  object tiSplitterPanel1: TtiSplitterPanel
    Left = 4
    Top = 92
    Width = 427
    Height = 289
    Aligned = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColorGrabBar = 16686723
    ColorPanel = clBtnFace
    PanelStyle = spsFramed
    SplitterOrientation = spoHorizontal
    KeepSplitterPosPercent = True
    SplitterPos = 191
    SplitterPosPercent = 67
    Panel1Controls = (
      tiSplitterPanel2)
    Panel2Controls = (
      paeNotes)
    object tiSplitterPanel2: TtiSplitterPanel
      Left = 2
      Top = 2
      Width = 423
      Height = 187
      Aligned = alClient
      ColorGrabBar = 16686723
      ColorPanel = clBtnFace
      BevelInnerSubPanels = bvNone
      BevelOuterSubPanels = bvNone
      PanelStyle = spsNone
      SplitterOrientation = spoVertical
      KeepSplitterPosPercent = True
      SplitterPos = 199
      SplitterPosPercent = 47
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
        Width = 184
        Height = 151
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
        CanStartDrag = False
        DesignSize = (
          184
          151)
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
        Width = 201
        Height = 152
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
        CanStartDrag = False
        DesignSize = (
          201
          152)
      end
    end
    object paeNotes: TtiPerAwareMemo
      Left = 12
      Top = 8
      Width = 409
      Height = 77
      ShowFocusRect = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      Constraints.MinHeight = 23
      TabOrder = 0
      LabelStyle = lsTop
      Caption = '&Notes'
      ReadOnly = False
      OnChange = paeLastNameChange
      ScrollBars = ssNone
      WordWrap = True
      MaxLength = 0
    end
  end
end

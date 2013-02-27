inherited FormEditCompany: TFormEditCompany
  Left = 381
  Top = 191
  Caption = 'FormEditCompany'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    TabOrder = 1
    inherited btnSave: TBitBtn
      Left = 278
    end
    inherited btnUndo: TBitBtn
      Left = 358
    end
    inherited btnClose: TBitBtn
      Left = 438
    end
  end
  object gbName: TGroupBox [1]
    Left = 4
    Top = 38
    Width = 509
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Name details '
    TabOrder = 0
    DesignSize = (
      509
      57)
    object paeCompanyName: TtiPerAwareEdit
      Left = 16
      Top = 24
      Width = 481
      Height = 23
      ShowFocusRect = True
      Anchors = [akLeft, akTop, akRight]
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Company &name'
      ReadOnly = False
      MaxLength = 60
      CharCase = ecNormal
      PasswordChar = #0
    end
  end
  object tiSplitterPanel1: TtiSplitterPanel [2]
    Left = 4
    Top = 100
    Width = 507
    Height = 296
    Aligned = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColorGrabBar = 16686723
    ColorPanel = clBtnFace
    PanelStyle = spsFramed
    SplitterOrientation = spoHorizontal
    KeepSplitterPosPercent = True
    SplitterPos = 144
    SplitterPosPercent = 50
    Panel1Controls = (
      tiSplitterPanel2)
    Panel2Controls = (
      paeNotes)
    object tiSplitterPanel2: TtiSplitterPanel
      Left = 2
      Top = 2
      Width = 503
      Height = 140
      Aligned = alClient
      ColorGrabBar = 16686723
      ColorPanel = clBtnFace
      BevelInnerSubPanels = bvNone
      BevelOuterSubPanels = bvNone
      PanelStyle = spsNone
      SplitterOrientation = spoVertical
      KeepSplitterPosPercent = True
      SplitterPos = 257
      SplitterPosPercent = 51
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
        Top = 24
        Width = 243
        Height = 108
        ShowFocusRect = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnItemEdit = lvEAddressItemEdit
        OnItemInsert = lvEAddressItemInsert
        OnItemDelete = lvEAddressItemDelete
        OnFilterData = lvEAddressFilterData
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
        DesignSize = (
          243
          108)
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
        Top = 25
        Width = 221
        Height = 107
        ShowFocusRect = True
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnItemEdit = lvAddressItemEdit
        OnItemInsert = lvAddressItemInsert
        OnItemDelete = lvAddressItemDelete
        OnFilterData = lvEAddressFilterData
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
        DesignSize = (
          221
          107)
      end
    end
    object paeNotes: TtiPerAwareMemo
      Left = 8
      Top = 6
      Width = 491
      Height = 131
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
  inherited RO: TtiReadOnly
    Left = 4
    Top = 36
  end
end

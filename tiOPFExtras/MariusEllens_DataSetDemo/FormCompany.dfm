object FormCompany: TFormCompany
  Left = 318
  Top = 100
  Width = 590
  Height = 453
  Caption = 'Company details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    582
    426)
  PixelsPerInch = 96
  TextHeight = 13
  object gbName: TGroupBox
    Left = 4
    Top = 4
    Width = 571
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Name details '
    TabOrder = 0
    DesignSize = (
      571
      57)
    object EditCompanyName: TDBEdit
      Left = 16
      Top = 24
      Width = 543
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DataField = 'CompanyName'
      DataSource = dsCompany
      MaxLength = 60
      TabOrder = 0
    end
  end
  object tiSplitterPanel1: TtiSplitterPanel
    Left = 4
    Top = 68
    Width = 571
    Height = 354
    Aligned = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColorGrabBar = 16686723
    ColorPanel = clBtnFace
    PanelStyle = spsFramed
    SplitterOrientation = spoHorizontal
    KeepSplitterPosPercent = True
    SplitterPos = 256
    SplitterPosPercent = 73
    Panel1Controls = (
      tiSplitterPanel2)
    Panel2Controls = (
      Label3
      paeNotes)
    object tiSplitterPanel2: TtiSplitterPanel
      Left = 2
      Top = 2
      Width = 567
      Height = 252
      Aligned = alClient
      ColorGrabBar = 16686723
      ColorPanel = clBtnFace
      BevelInnerSubPanels = bvNone
      BevelOuterSubPanels = bvNone
      PanelStyle = spsNone
      SplitterOrientation = spoVertical
      KeepSplitterPosPercent = True
      SplitterPos = 273
      SplitterPosPercent = 48
      Panel1Controls = (
        Label1
        lvContact
        DBNavigator1)
      Panel2Controls = (
        Label2
        lvAddress
        DBNavigator2)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 182
        Height = 13
        Caption = ' Phone, Fax, EMail  && Web Addresses '
      end
      object lvContact: TDBGrid
        Left = 9
        Top = 24
        Width = 261
        Height = 770
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsContact
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = lvContactDblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'AdrsTypeAsString'
            Title.Caption = 'Address Type'
            Width = 74
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Caption'
            Title.Caption = 'Value'
            Width = 60
            Visible = True
          end>
      end
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 227
        Width = 273
        Height = 25
        DataSource = dsContact
        VisibleButtons = [nbInsert, nbDelete, nbEdit]
        Align = alBottom
        TabOrder = 1
      end
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 124
        Height = 13
        Caption = 'Postal && Street Addresses '
      end
      object lvAddress: TDBGrid
        Left = 8
        Top = 22
        Width = 269
        Height = 499
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsAddress
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = lvAddressDblClick
        Columns = <
          item
            Expanded = False
            FieldName = 'AdrsTypeAsString'
            Title.Caption = 'Address type'
            Width = 76
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Caption'
            Title.Caption = 'Value'
            Width = 60
            Visible = True
          end>
      end
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 227
        Width = 286
        Height = 25
        DataSource = dsAddress
        VisibleButtons = [nbInsert, nbDelete, nbEdit]
        Align = alBottom
        TabOrder = 1
      end
    end
    object Label3: TLabel
      Left = 8
      Top = 4
      Width = 28
      Height = 13
      Caption = 'Notes'
    end
    object paeNotes: TDBEdit
      Left = 8
      Top = 20
      Width = 555
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      DataField = 'Notes'
      DataSource = dsCompany
      TabOrder = 0
    end
  end
  object tbCompany: TTiRecordDataset
    StringWidth = 255
    ObjectClassName = 'TCompany'
    ObjectDepth = 0
    AfterPost = tbCompanyAfterPost
    ObjectView = True
    Left = 230
    Top = 142
    object tbCompanyCaption: TStringField
      FieldName = 'Caption'
      Size = 255
    end
    object tbCompanyEAddressList: TDataSetField
      FieldName = 'EAddressList'
    end
    object tbCompanyAddressList: TDataSetField
      FieldName = 'AddressList'
    end
    object tbCompanyNotes: TStringField
      FieldName = 'Notes'
      Size = 255
    end
    object tbCompanyCompanyName: TStringField
      FieldName = 'CompanyName'
      Size = 255
    end
    object tbCompanyPeople: TDataSetField
      FieldName = 'People'
    end
  end
  object dsCompany: TDataSource
    DataSet = tbCompany
    Left = 230
    Top = 190
  end
  object tbAddress: TTiNestedDataset
    StringWidth = 255
    ObjectClassName = 'TAdrs'
    ObjectDepth = 0
    AfterInsert = tbAddressAfterEdit
    AfterEdit = tbAddressAfterEdit
    ObjectView = False
    ShowDeleted = False
    OwnsObjects = False
    DataSetField = tbCompanyAddressList
    Left = 395
    Top = 146
  end
  object tbContact: TTiNestedDataset
    StringWidth = 255
    ObjectClassName = 'TEAdrs'
    ObjectDepth = 0
    AfterInsert = tbContactAfterEdit
    AfterEdit = tbContactAfterEdit
    ObjectView = False
    ShowDeleted = False
    OwnsObjects = False
    DataSetField = tbCompanyEAddressList
    Left = 57
    Top = 146
  end
  object dsContact: TDataSource
    DataSet = tbContact
    Left = 55
    Top = 194
  end
  object dsAddress: TDataSource
    DataSet = tbAddress
    Left = 399
    Top = 198
  end
end

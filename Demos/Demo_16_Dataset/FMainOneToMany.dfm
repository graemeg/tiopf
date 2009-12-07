object FormMainOneToMany: TFormMainOneToMany
  Left = 215
  Top = 121
  Width = 548
  Height = 377
  Caption = 'FormMainOneToMany'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Clients'
  end
  object Label2: TLabel
    Left = 8
    Top = 188
    Width = 74
    Height = 13
    Caption = 'Phone numbers'
  end
  object Button1: TButton
    Left = 408
    Top = 279
    Width = 101
    Height = 25
    Action = aSave
    Anchors = [akRight, akBottom]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 408
    Top = 247
    Width = 101
    Height = 25
    Action = aShowObjects
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object btnRead: TButton
    Left = 408
    Top = 311
    Width = 101
    Height = 25
    Action = aRead
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object paeClientCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 24
    Width = 110
    Height = 41
    Constraints.MinHeight = 23
    TabOrder = 3
    OnChangeDelayInterval = 0
    LabelStyle = lsTop
    LabelLayout = tlTop
    Caption = 'Client count'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
  end
  object paePhoneNumberCount: TtiPerAwareFloatEdit
    Left = 408
    Top = 72
    Width = 110
    Height = 41
    Constraints.MinHeight = 23
    TabOrder = 4
    OnChangeDelayInterval = 0
    LabelStyle = lsTop
    LabelLayout = tlTop
    Caption = 'Phone number count'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesUser
  end
  object btnInsert: TButton
    Left = 408
    Top = 119
    Width = 102
    Height = 25
    Action = aInsertClients
    TabOrder = 5
  end
  object DBGrid1: TDBGrid
    Left = 24
    Top = 24
    Width = 373
    Height = 158
    DataSource = DataSource1
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ClientId'
        Width = 126
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ClientName'
        Width = 209
        Visible = True
      end>
  end
  object DBGrid2: TDBGrid
    Left = 77
    Top = 216
    Width = 320
    Height = 120
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource2
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'NumberType'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NumberText'
        Visible = True
      end>
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 388
    Top = 208
    object aSave: TAction
      Caption = 'Save'
      OnExecute = aSaveExecute
    end
    object aRead: TAction
      Caption = 'Read'
      OnExecute = aReadExecute
    end
    object aInsertClients: TAction
      Caption = 'Insert clients'
      OnExecute = aInsertClientsExecute
    end
    object aShowObjects: TAction
      Caption = 'Show objects'
      OnExecute = aShowObjectsExecute
    end
  end
  object DatasetClients_: TTiDataset
    StringWidth = 255
    ObjectDepth = 0
    ObjectView = True
    ShowDeleted = False
    OwnsObjects = False
    Left = 160
    Top = 96
    object DatasetClients_ClientId: TStringField
      FieldName = 'ClientId'
      Size = 50
    end
    object DatasetClients_ClientName: TStringField
      FieldName = 'ClientName'
      Size = 50
    end
    object DatasetClients_PhoneNumbers: TDataSetField
      FieldName = 'PhoneNumbers'
    end
  end
  object DataSource1: TDataSource
    DataSet = DatasetClients_
    Left = 224
    Top = 96
  end
  object NestedDataset_PhoneNumbers: TTiNestedDataset
    StringWidth = 255
    ObjectDepth = 0
    ObjectView = False
    ShowDeleted = False
    OwnsObjects = False
    Left = 96
    Top = 280
    object NestedDataset_PhoneNumbersNumberType: TStringField
      FieldName = 'NumberType'
    end
    object NestedDataset_PhoneNumbersNumberText: TStringField
      FieldName = 'NumberText'
    end
  end
  object DataSource2: TDataSource
    DataSet = NestedDataset_PhoneNumbers
    Left = 224
    Top = 288
  end
end

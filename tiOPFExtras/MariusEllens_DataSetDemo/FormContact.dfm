object FormContact: TFormContact
  Left = 388
  Top = 263
  Width = 327
  Height = 121
  Caption = 'Address details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    319
    94)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 12
    Width = 61
    Height = 13
    Caption = 'Address &type'
    FocusControl = EditAddrType
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 38
    Height = 13
    Caption = '&Address'
    FocusControl = EditText
  end
  object ButtonOk: TBitBtn
    Left = 162
    Top = 67
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    TabOrder = 0
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 242
    Top = 67
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    TabOrder = 1
    Kind = bkCancel
  end
  object EditAddrType: TDBLookupComboBox
    Left = 100
    Top = 8
    Width = 125
    Height = 21
    DataField = 'AdrsTypeOID'
    DataSource = dsData
    KeyField = 'ID'
    ListField = 'Text'
    ListSource = dsEAddrType
    TabOrder = 2
  end
  object EditText: TDBEdit
    Left = 100
    Top = 36
    Width = 150
    Height = 21
    DataField = 'Text'
    DataSource = dsData
    TabOrder = 3
  end
  object dsEAddrType: TDataSource
    DataSet = tbEAddrType
    Left = 280
    Top = 60
  end
  object tbEAddrType: TTiDataset
    StringWidth = 255
    ObjectClassName = 'TLookupListItem'
    ObjectDepth = 0
    ObjectView = False
    ShowDeleted = False
    OwnsObjects = False
    Left = 280
    Top = 8
  end
  object dsData: TDataSource
    DataSet = tbData
    Left = 217
    Top = 53
  end
  object tbData: TTiRecordDataset
    StringWidth = 255
    ObjectClassName = 'TEAdrs'
    ObjectDepth = 0
    ObjectView = False
    Left = 225
    Top = 3
  end
end

object FormAddress: TFormAddress
  Left = 310
  Top = 177
  Width = 318
  Height = 270
  Caption = 'Address details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    310
    243)
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
    Left = 5
    Top = 40
    Width = 62
    Height = 13
    Caption = 'Address &lines'
    FocusControl = EditLines
  end
  object Label3: TLabel
    Left = 5
    Top = 100
    Width = 34
    Height = 13
    Caption = '&S&uburb'
    FocusControl = DBEdit2
  end
  object Label4: TLabel
    Left = 5
    Top = 128
    Width = 25
    Height = 13
    Caption = '&State'
    FocusControl = DBEdit3
  end
  object Label5: TLabel
    Left = 5
    Top = 148
    Width = 48
    Height = 13
    Caption = '&Post code'
    FocusControl = DBEdit4
  end
  object Label6: TLabel
    Left = 5
    Top = 172
    Width = 36
    Height = 13
    Caption = '&Country'
    FocusControl = DBEdit5
  end
  object ButtonOk: TBitBtn
    Left = 152
    Top = 212
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    TabOrder = 0
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 232
    Top = 212
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
    ListSource = dsAddrType
    TabOrder = 2
  end
  object EditLines: TDBMemo
    Left = 100
    Top = 32
    Width = 125
    Height = 57
    DataField = 'Lines'
    DataSource = dsData
    TabOrder = 3
  end
  object DBEdit2: TDBEdit
    Left = 100
    Top = 96
    Width = 125
    Height = 21
    DataField = 'Suburb'
    DataSource = dsData
    TabOrder = 4
  end
  object DBEdit3: TDBEdit
    Left = 100
    Top = 120
    Width = 125
    Height = 21
    DataField = 'State'
    DataSource = dsData
    TabOrder = 5
  end
  object DBEdit4: TDBEdit
    Left = 100
    Top = 144
    Width = 125
    Height = 21
    DataField = 'PCode'
    DataSource = dsData
    TabOrder = 6
  end
  object DBEdit5: TDBEdit
    Left = 100
    Top = 168
    Width = 125
    Height = 21
    DataField = 'Country'
    DataSource = dsData
    TabOrder = 7
  end
  object dsData: TDataSource
    DataSet = tbData
    Left = 248
    Top = 64
  end
  object tbData: TTiRecordDataset
    StringWidth = 255
    ObjectClassName = 'TAdrs'
    ObjectDepth = 0
    ObjectView = False
    Left = 248
    Top = 8
  end
  object tbAddrType: TTiDataset
    StringWidth = 255
    ObjectClassName = 'TLookupListItem'
    ObjectDepth = 0
    ObjectView = False
    ShowDeleted = False
    OwnsObjects = False
    Left = 248
    Top = 116
  end
  object dsAddrType: TDataSource
    DataSet = tbAddrType
    Left = 248
    Top = 160
  end
end

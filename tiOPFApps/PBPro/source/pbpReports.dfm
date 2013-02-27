object ReportsDataModule: TReportsDataModule
  OldCreateOrder = False
  Left = 168
  Top = 240
  Height = 603
  Width = 838
  object PawnbrokerObjectDataset: TOpfDataset
    OwnsObjects = False
    StringWidth = 255
    ObjectClassName = 'TPawnbroker'
    ShowDeleted = False
    Active = True
    Left = 56
    Top = 32
    object PawnbrokerObjectDatasetName: TStringField
      FieldName = 'Name'
      Size = 255
    end
    object PawnbrokerObjectDatasetContactDetails: TStringField
      FieldName = 'ContactDetails'
      Size = 255
    end
  end
  object PawnbrokerReportDataset: TfrDBDataSet
    CloseDataSource = True
    DataSet = PawnbrokerObjectDataset
    Left = 56
    Top = 80
  end
  object ContractObjectDataset: TOpfDataset
    OwnsObjects = False
    StringWidth = 255
    ObjectClassName = 'TContract'
    ShowDeleted = False
    Active = True
    Left = 240
    Top = 32
    object ContractObjectDatasetCaption: TStringField
      FieldName = 'Caption'
      Size = 255
    end
    object ContractObjectDatasetClientOID: TLargeintField
      FieldName = 'ClientOID'
      MaxValue = 9223372036854775807
      MinValue = -9223372036854775808
    end
    object ContractObjectDatasetClientIdentityRecordProxies: TDataSetField
      FieldName = 'ClientIdentityRecordProxies'
      IncludeObjectField = False
    end
    object ContractObjectDatasetContractFee: TFloatField
      FieldName = 'ContractFee'
    end
    object ContractObjectDatasetContractNumber: TIntegerField
      FieldName = 'ContractNumber'
      MaxValue = 2147483647
      MinValue = -2147483648
    end
    object ContractObjectDatasetEndDate: TDateTimeField
      FieldName = 'EndDate'
    end
    object ContractObjectDatasetExtensionNumber: TIntegerField
      FieldName = 'ExtensionNumber'
      MaxValue = 2147483647
      MinValue = -2147483648
    end
    object ContractObjectDatasetInterestRate: TFloatField
      FieldName = 'InterestRate'
    end
    object ContractObjectDatasetInterestValue: TFloatField
      FieldName = 'InterestValue'
    end
    object ContractObjectDatasetNotes: TStringField
      FieldName = 'Notes'
      Size = 255
    end
    object ContractObjectDatasetRedemptionValue: TFloatField
      FieldName = 'RedemptionValue'
    end
    object ContractObjectDatasetContractState: TWordField
      FieldName = 'ContractState'
      MaxValue = 5
    end
    object ContractObjectDatasetStartDate: TDateTimeField
      FieldName = 'StartDate'
    end
    object ContractObjectDatasetItems: TDataSetField
      FieldName = 'Items'
      IncludeObjectField = False
    end
    object ContractObjectDatasetTransactions: TDataSetField
      FieldName = 'Transactions'
      IncludeObjectField = False
    end
    object ContractObjectDatasetClientGivenNames: TStringField
      FieldName = 'ClientGivenNames'
      Size = 255
    end
    object ContractObjectDatasetClientFamilyName: TStringField
      FieldName = 'ClientFamilyName'
      Size = 255
    end
  end
  object ContractReportDataset: TfrDBDataSet
    CloseDataSource = True
    DataSet = ContractObjectDataset
    Left = 240
    Top = 80
  end
  object ContractReport: TfrReport
    InitialZoom = pzDefault
    PreviewButtons = [pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbHelp, pbExit]
    StoreInDFM = True
    Left = 56
    Top = 168
    ReportForm = {
      17000000F507000017000000000C0043616E6F6E2053323030535000FF090000
      00340800009A0B000000000000000000000000000000000000000000FFFF0100
      000000000000FF09000000340800009A0B000000000000000000000000000000
      000000000000FFFF0100000000000000FF09000000340800009A0B0000000000
      00000000000000000000000000000000FFFF010000000000000002000B005061
      676548656164657231000000000014000000F002000044000000300002000100
      0000000000000000FFFFFF1F00000000000000000000000000FFFF02000B004D
      61737465724461746131000000000080000000F0020000680000003000050001
      000000000000000000FFFFFF1F000000001500436F6E74726163745265706F72
      744461746173657400000000000000FFFF000006004D656D6F31370060020000
      1400000058000000120000000300000001000000000000000000FFFFFF1F2E02
      000000000001000E0050616765202D205B50414745235D00000000FFFF050041
      7269616C0008000000000000000000010000000000020000000000FFFFFF0000
      0000000006004D656D6F31380018020000140000003C00000012000000030000
      0001000000000000000000FFFFFF1F2E020000000000010006005B444154455D
      00000000FFFF0500417269616C00080000000000000000000000000000000200
      00000000FFFFFF00000000000006004D656D6F32300010000000170000005C01
      0000120000000300000001000000000000000000FFFFFF1F2E02000000000001
      0020005B5061776E62726F6B65724F626A656374446174617365742E224E616D
      65225D00000000FFFF14004D6963726F736F66742053616E7320536572696600
      08000000000000000000000000000000020000000000FFFFFF000000000A0B00
      546672526963685669657700000500526963683100100000002C0000005C0100
      00260000000100000001000000000000000000FFFFFF1F2E0200000000000000
      00000000FFFF01D30300007B5C727466315C616E73695C616E73696370673132
      35325C64656666305C6465666C616E67313033337B5C666F6E7474626C7B5C66
      305C666E696C204D532053616E732053657269663B7D7B5C66315C666E696C5C
      666368617273657430204D532053616E732053657269663B7D7D0D0A7B5C636F
      6C6F7274626C203B5C726564305C677265656E305C626C7565303B7D0D0A5C76
      6965776B696E64345C7563315C706172645C6366315C66305C66733136205B50
      61776E62726F6B65724F626A656374446174617365742E225C663120436F6E74
      61637444657461696C735C663020225D0D0A5C706172207D0D0A00000005004D
      656D6F33004400000080000000C0010000120000000300000001000000000000
      000000FFFFFF1F2E020000000000010059003A205B436F6E74726163744F626A
      656374446174617365742E22436C69656E7446616D696C794E616D65225D2C20
      5B436F6E74726163744F626A656374446174617365742E22436C69656E744769
      76656E4E616D6573225D2000000000FFFF0500417269616C0008000000000000
      000000000000000000020000000000FFFFFF00000000000005004D656D6F3400
      100000008000000030000000120000000300000001000000000000000000FFFF
      FF1F2E020000000000010004004E616D6500000000FFFF0500417269616C0008
      000000020000000000000000000000020000000000FFFFFF0000000000000500
      4D656D6F37001302000080000000250000000E00000003000000010000000000
      00000000FFFFFF1F2E020000000000010004004461746500000000FFFF050041
      7269616C0008000000020000000000000000000000020000000000FFFFFF0000
      0000000005004D656D6F380040020000800000009C0000000E00000003000000
      01000000000000000000FFFFFF1F2E020002000000010025003A205B436F6E74
      726163744F626A656374446174617365742E22537461727444617465225D0000
      0000FFFF0500417269616C000800000000000000000000000000000002000000
      0000FFFFFF00000000000006004D656D6F323200140200008E00000025000000
      0F0000000300000001000000000000000000FFFFFF1F2E020000000000010002
      00546F00000000FFFF0500417269616C00080000000200000000000000000000
      00020000000000FFFFFF00000000000006004D656D6F323300410200008E0000
      009C0000000F0000000300000001000000000000000000FFFFFF1F2E02000000
      0000010023003A205B436F6E74726163744F626A656374446174617365742E22
      456E6444617465225D00000000FFFF0500417269616C00080000000000000000
      00000000000000020000000000FFFFFF00000000000006004D656D6F32340095
      0200008E000000480000000F0000000300000001000000000000000000FFFFFF
      1F2E02000000000001000A003C44554520444154453E00000000FFFF05004172
      69616C0008000000020000000000000000000000020000000000FFFFFF000000
      0002010D004D617374657248656164657232000000000018000000F002000028
      0000003000040001000000000000000000FFFFFF1F0000000000000000000000
      0000FFFF02010B004D6173746572446174613200000000007C000000F0020000
      280000003000050001000000000000000000FFFFFF1F00000000000000000000
      000000FFFFFE01000000000000000000000D205469636B65744E756D62657202
      000800436F6E74726163740D0E0020205469636B65744E756D62657200}
  end
  object ClientIdentityRecordObjectDataset: TOpfDataset
    OwnsObjects = False
    StringWidth = 255
    ObjectClassName = 'TContract'
    ShowDeleted = False
    Left = 416
    Top = 32
    object StringField1: TStringField
      FieldName = 'Caption'
      Size = 255
    end
    object LargeintField1: TLargeintField
      FieldName = 'ClientOID'
      MaxValue = 9223372036854775807
      MinValue = -9223372036854775808
    end
    object DataSetField1: TDataSetField
      FieldName = 'ClientIdentityRecordProxies'
      IncludeObjectField = False
    end
    object FloatField1: TFloatField
      FieldName = 'ContractFee'
    end
    object IntegerField1: TIntegerField
      FieldName = 'ContractNumber'
      MaxValue = 2147483647
      MinValue = -2147483648
    end
    object DateTimeField1: TDateTimeField
      FieldName = 'EndDate'
    end
    object IntegerField2: TIntegerField
      FieldName = 'ExtensionNumber'
      MaxValue = 2147483647
      MinValue = -2147483648
    end
    object FloatField2: TFloatField
      FieldName = 'InterestRate'
    end
    object FloatField3: TFloatField
      FieldName = 'InterestValue'
    end
    object StringField2: TStringField
      FieldName = 'Notes'
      Size = 255
    end
    object FloatField4: TFloatField
      FieldName = 'RedemptionValue'
    end
    object WordField1: TWordField
      FieldName = 'ContractState'
      MaxValue = 5
    end
    object DateTimeField2: TDateTimeField
      FieldName = 'StartDate'
    end
    object DataSetField2: TDataSetField
      FieldName = 'Items'
      IncludeObjectField = False
    end
    object DataSetField3: TDataSetField
      FieldName = 'Transactions'
      IncludeObjectField = False
    end
    object StringField3: TStringField
      FieldName = 'ClientGivenNames'
      Size = 255
    end
    object StringField4: TStringField
      FieldName = 'ClientFamilyName'
      Size = 255
    end
  end
  object ClientIdentity: TfrDBDataSet
    DataSet = ClientIdentityRecordObjectDataset
    OpenDataSource = False
    Left = 416
    Top = 80
  end
  object OpfDataset1: TOpfDataset
    OwnsObjects = True
    StringWidth = 255
    ShowDeleted = False
    Left = 288
    Top = 168
  end
end

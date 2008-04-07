inherited FormMainConnectToDBCode: TFormMainConnectToDBCode
  Caption = 'FormMainConnectToDBCode'
  ClientHeight = 251
  OldCreateOrder = True
  ExplicitWidth = 734
  ExplicitHeight = 283
  PixelsPerInch = 96
  TextHeight = 13
  inherited GroupBox1: TGroupBox
    Left = 8
    Width = 712
    ExplicitLeft = 8
    ExplicitWidth = 712
    inherited sbDefaultToPresetValues: TtiSpeedButton
      Left = 551
      ExplicitLeft = 551
    end
    inherited paePersistenceLayer: TtiPerAwareEdit
      Width = 536
      ExplicitWidth = 536
    end
    inherited paeDatabaseName: TtiPerAwareEdit
      Width = 536
      ExplicitWidth = 536
    end
    inherited paeUserName: TtiPerAwareEdit
      Width = 536
      ExplicitWidth = 536
    end
    inherited paePassword: TtiPerAwareEdit
      Width = 536
      ExplicitWidth = 536
    end
  end
  object btnConnectToDatabase: TButton [1]
    Left = 8
    Top = 187
    Width = 241
    Height = 25
    Caption = 'Connect to the database shown above'
    TabOrder = 3
    OnClick = btnConnectToDatabaseClick
  end
  object btnDisconnectFromDatabase: TButton [2]
    Left = 8
    Top = 218
    Width = 241
    Height = 25
    Caption = 'Disconnect from the database shown above'
    TabOrder = 1
    OnClick = btnDisconnectFromDatabaseClick
  end
  object btnShowWhatsConnected: TButton [3]
    Left = 8
    Top = 156
    Width = 241
    Height = 25
    Caption = 'Show what'#39's connected'
    TabOrder = 2
    OnClick = btnShowWhatsConnectedClick
  end
end

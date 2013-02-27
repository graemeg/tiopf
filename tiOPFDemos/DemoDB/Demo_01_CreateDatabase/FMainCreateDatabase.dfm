inherited FormMainCreateDatabase: TFormMainCreateDatabase
  Caption = 'FormMainCreateDatabase'
  PixelsPerInch = 96
  TextHeight = 13
  object btnDatabaseExists: TButton
    Left = 160
    Top = 132
    Width = 121
    Height = 25
    Caption = 'Database exists?'
    TabOrder = 7
    OnClick = btnDatabaseExistsClick
  end
  object btnCreateDatabase: TButton
    Left = 288
    Top = 132
    Width = 115
    Height = 25
    Caption = 'Create database'
    TabOrder = 8
    OnClick = btnCreateDatabaseClick
  end
end

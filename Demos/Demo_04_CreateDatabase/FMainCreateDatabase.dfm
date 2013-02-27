inherited FormMainCreateDatabase: TFormMainCreateDatabase
  Caption = 'FormMainCreateDatabase'
  ClientHeight = 187
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object btnDatabaseExists: TButton [0]
    Left = 476
    Top = 153
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Database exists?'
    TabOrder = 0
    OnClick = btnDatabaseExistsClick
  end
  object btnCreateDatabase: TButton [1]
    Left = 604
    Top = 153
    Width = 115
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create database'
    TabOrder = 1
    OnClick = btnCreateDatabaseClick
  end
  inherited GroupBox1: TGroupBox
    Left = 6
    TabOrder = 2
  end
end

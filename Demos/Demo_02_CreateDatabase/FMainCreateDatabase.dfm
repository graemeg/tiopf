inherited FormMainCreateDatabase: TFormMainCreateDatabase
  Caption = 'FormMainCreateDatabase'
  ClientHeight = 187
  OldCreateOrder = True
  ExplicitHeight = 219
  PixelsPerInch = 96
  TextHeight = 13
  object btnDatabaseExists: TButton [0]
    Left = 291
    Top = 153
    Width = 121
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Database exists?'
    TabOrder = 0
    OnClick = btnDatabaseExistsClick
    ExplicitLeft = 250
  end
  object btnCreateDatabase: TButton [1]
    Left = 419
    Top = 153
    Width = 115
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Create database'
    TabOrder = 1
    OnClick = btnCreateDatabaseClick
    ExplicitLeft = 378
  end
  inherited GroupBox1: TGroupBox
    Left = 6
    TabOrder = 2
    ExplicitLeft = 6
  end
end

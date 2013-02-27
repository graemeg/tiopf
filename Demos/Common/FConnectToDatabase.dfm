inherited FormConnectToDatabase: TFormConnectToDatabase
  Caption = ' Connect to a database'
  ClientHeight = 186
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object btnConnect: TButton [0]
    Left = 378
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Connect'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnCancel: TButton [1]
    Left = 458
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  inherited GroupBox1: TGroupBox
    TabOrder = 2
  end
end

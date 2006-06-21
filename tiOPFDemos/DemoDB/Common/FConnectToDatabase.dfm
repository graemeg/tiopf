inherited FormConnectToDatabase: TFormConnectToDatabase
  Caption = ' Connect to a database'
  ClientHeight = 163
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnSetupForADOAccess: TButton
    TabOrder = 9
  end
  inherited btnSetupForRemote: TButton
    TabOrder = 10
  end
  object btnConnect: TButton
    Left = 248
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Connect'
    Default = True
    TabOrder = 7
    OnClick = Button1Click
  end
  object btnCancel: TButton
    Left = 328
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 8
    OnClick = btnCancelClick
  end
end

inherited FormMainConnectToDBCode: TFormMainConnectToDBCode
  Left = 219
  Top = 222
  Caption = 'FormMainConnectToDBCode'
  ClientHeight = 315
  ClientWidth = 526
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel [0]
    Left = 8
    Top = 128
    Width = 505
    Height = 9
    Shape = bsTopLine
  end
  object Bevel2: TBevel [1]
    Left = 8
    Top = 184
    Width = 505
    Height = 9
    Shape = bsTopLine
  end
  object Bevel3: TBevel [2]
    Left = 8
    Top = 240
    Width = 505
    Height = 9
    Shape = bsTopLine
  end
  inherited btnSetupForADOAccess: TButton
    TabOrder = 9
  end
  inherited btnSetupForRemote: TButton
    TabOrder = 12
  end
  object btnLoadPersistenceLayer: TButton
    Left = 136
    Top = 144
    Width = 131
    Height = 25
    Caption = 'Load persistence layer'
    TabOrder = 7
    OnClick = btnLoadPersistenceLayerClick
  end
  object btnUnLoadPersistenceLayer: TButton
    Left = 272
    Top = 144
    Width = 129
    Height = 25
    Caption = 'Unload persistence layer'
    TabOrder = 8
    OnClick = btnUnLoadPersistenceLayerClick
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 136
    Width = 121
    Height = 33
    TabStop = False
    Lines.Strings = (
      'Load or unload the '
      'persistence layer.')
  end
  object btnLoadDatabase: TButton
    Left = 136
    Top = 200
    Width = 129
    Height = 25
    Caption = 'Load database'
    TabOrder = 10
    OnClick = btnLoadDatabaseClick
  end
  object Button2: TButton
    Left = 272
    Top = 200
    Width = 129
    Height = 25
    Caption = 'Load database'
    TabOrder = 11
    OnClick = Button2Click
  end
  object tiMemoReadOnly2: TtiMemoReadOnly
    Left = 8
    Top = 192
    Width = 121
    Height = 41
    TabStop = False
    Lines.Strings = (
      'Load database '
      'connection pool.')
  end
  object btnLoadBoth: TButton
    Left = 136
    Top = 256
    Width = 129
    Height = 25
    Caption = 'Load both at same time'
    TabOrder = 13
    OnClick = btnLoadBothClick
  end
  object btnUnLoadBoth: TButton
    Left = 272
    Top = 256
    Width = 129
    Height = 25
    Caption = 'Unload both'
    TabOrder = 14
    OnClick = btnUnLoadBothClick
  end
  object tiMemoReadOnly3: TtiMemoReadOnly
    Left = 8
    Top = 248
    Width = 121
    Height = 65
    TabStop = False
    Lines.Strings = (
      'Load persistence '
      'layer and database '
      'connection pool at the '
      'same time.')
  end
  object btnShowLoaded: TButton
    Left = 408
    Top = 144
    Width = 97
    Height = 25
    Caption = 'Show loaded'
    TabOrder = 18
    OnClick = btnShowLoadedClick
  end
end

object FormMainCreateTable: TFormMainCreateTable
  Left = 333
  Top = 210
  Width = 353
  Height = 220
  Caption = 'FormMainCreateTable'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnCreateTable: TButton
    Left = 236
    Top = 8
    Width = 101
    Height = 25
    Caption = 'Create table'
    TabOrder = 0
    OnClick = btnCreateTableClick
  end
  object btnDropTable: TButton
    Left = 236
    Top = 104
    Width = 101
    Height = 25
    Caption = 'Drop table'
    TabOrder = 3
    OnClick = btnDropTableClick
  end
  object btnShowMetaData: TButton
    Left = 236
    Top = 72
    Width = 101
    Height = 25
    Caption = 'Show metadata'
    TabOrder = 2
    OnClick = btnShowMetaDataClick
  end
  object btnTableExists: TButton
    Left = 236
    Top = 40
    Width = 101
    Height = 25
    Caption = 'Table exists?'
    TabOrder = 1
    OnClick = btnTableExistsClick
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 8
    Width = 209
    Height = 177
    TabStop = False
    Lines.Strings = (
      'This demo will:'
      ''
      'a) Create a table called  Client with the '
      'following structure'
      '  OID String( 36 )'
      '  Client_Name String( 200 ) '
      '  Client_ID String( 9 )'
      ''
      'b) Test if a table called Client exists'
      ''
      'c) Show metadata for the Client table'
      ''
      'd) Drop the client table')
  end
end

object FormMainCreateTable: TFormMainCreateTable
  Left = 333
  Top = 210
  Caption = 'Create table demo'
  ClientHeight = 218
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblConnectedTo: TLabel
    Left = 8
    Top = 5
    Width = 90
    Height = 13
    Caption = 'lblConnectedTo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnCreateTable: TButton
    Left = 236
    Top = 24
    Width = 101
    Height = 25
    Caption = 'Create table'
    TabOrder = 0
    OnClick = btnCreateTableClick
  end
  object btnDropTable: TButton
    Left = 236
    Top = 120
    Width = 101
    Height = 25
    Caption = 'Drop table'
    TabOrder = 3
    OnClick = btnDropTableClick
  end
  object btnShowMetaData: TButton
    Left = 236
    Top = 88
    Width = 101
    Height = 25
    Caption = 'Show metadata'
    TabOrder = 2
    OnClick = btnShowMetaDataClick
  end
  object btnTableExists: TButton
    Left = 236
    Top = 56
    Width = 101
    Height = 25
    Caption = 'Table exists?'
    TabOrder = 1
    OnClick = btnTableExistsClick
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 30
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

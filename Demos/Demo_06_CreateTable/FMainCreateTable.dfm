object FormMainCreateTable: TFormMainCreateTable
  Left = 333
  Top = 210
  Caption = 'Create table demo'
  ClientHeight = 218
  ClientWidth = 397
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
  object Info: TLabel
    Left = 8
    Top = 30
    Width = 184
    Height = 156
    Caption = 
      'This demo will:'#13#10 +
      'a) Create a table called Client with the following structure'#13#10 +
      '  OID String( 36 )'#13#10 +
      '  Client_Name String( 200 )'#13#10 +
      '  Client_ID String( 9 )'#13#10 +
      #13#10 +
      'b) Test if a table called Client exists'#13#10 +
      #13#10 +
      'c) Show metadata for the Client table'#13#10 +
      ''#13#10 +
      'd) Drop the client table'
  end
  object btnCreateTable: TButton
    Left = 284
    Top = 28
    Width = 101
    Height = 25
    Caption = 'Create table'
    TabOrder = 0
    OnClick = btnCreateTableClick
  end
  object btnDropTable: TButton
    Left = 284
    Top = 124
    Width = 101
    Height = 25
    Caption = 'Drop table'
    TabOrder = 3
    OnClick = btnDropTableClick
  end
  object btnShowMetaData: TButton
    Left = 284
    Top = 92
    Width = 101
    Height = 25
    Caption = 'Show metadata'
    TabOrder = 2
    OnClick = btnShowMetaDataClick
  end
  object btnTableExists: TButton
    Left = 284
    Top = 60
    Width = 101
    Height = 25
    Caption = 'Table exists?'
    TabOrder = 1
    OnClick = btnTableExistsClick
  end
end

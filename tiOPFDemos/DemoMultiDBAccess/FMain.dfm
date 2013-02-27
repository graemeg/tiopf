object Form2: TForm2
  Left = 76
  Top = 141
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Demo of copying data from one database to another'
  ClientHeight = 379
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnConnectToDB_1: TButton
    Left = 416
    Top = 8
    Width = 213
    Height = 33
    Caption = '1. Connect to the first database'
    TabOrder = 0
    OnClick = btnConnectToDB_1Click
  end
  object btnConnectToDB_2: TButton
    Left = 416
    Top = 52
    Width = 213
    Height = 33
    Caption = '2. Connect to the second database'
    TabOrder = 1
    OnClick = btnConnectToDB_2Click
  end
  object btnUnLoadDBConnections: TButton
    Left = 416
    Top = 312
    Width = 213
    Height = 33
    Caption = '7. Unload DB connections'
    TabOrder = 2
    OnClick = btnUnLoadDBConnectionsClick
  end
  object btnViewFAdrs_1: TButton
    Left = 644
    Top = 112
    Width = 213
    Height = 33
    Caption = 'A. View Address Book #1'
    TabOrder = 3
    OnClick = btnViewFAdrs_1Click
  end
  object btnViewFAdrs_2: TButton
    Left = 644
    Top = 152
    Width = 213
    Height = 33
    Caption = 'B. View Address Book #2'
    TabOrder = 4
    OnClick = btnViewFAdrs_2Click
  end
  object btnReadFromDB_1: TButton
    Left = 416
    Top = 112
    Width = 213
    Height = 33
    Caption = '3. Read from DB #1 to Address Book #1'
    TabOrder = 5
    OnClick = btnReadFromDB_1Click
  end
  object btnReadFromDB_2: TButton
    Left = 416
    Top = 152
    Width = 213
    Height = 33
    Caption = '4. Read from DB #2 to Address Book #2'
    TabOrder = 6
    OnClick = btnReadFromDB_2Click
  end
  object btnClearDB_2: TButton
    Left = 416
    Top = 212
    Width = 213
    Height = 33
    Caption = '5. Clear DB #2'
    TabOrder = 7
    OnClick = btnClearDB_2Click
  end
  object btnSaveAdrsBook_1ToDB_2: TButton
    Left = 416
    Top = 252
    Width = 213
    Height = 33
    Caption = '6. Save from Adrs Book #1 to DB #2'
    TabOrder = 8
    OnClick = btnSaveAdrsBook_1ToDB_2Click
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 4
    Top = 4
    Width = 381
    Height = 365
    TabStop = False
    Lines.Strings = (
      
        'This demonstration will show how to connect to two databases (us' +
        'ing the same'
      
        'persistence layer) with the tiOPF. It will also show how to copy' +
        ' data from one'
      
        'database to another. The object model and database access code u' +
        'sed in'
      'DemoTIPerFramework will be used here.'
      ''
      'Each button executes a single task in the process'
      ''
      '1. Connect to the first database'
      ''
      '2. Connect to the second database'
      ''
      
        '3. Read data from the first database into a variable called FAdr' +
        's1'
      ''
      
        '4. Read data from the second database into variable called FAdrs' +
        '2'
      ''
      ''
      
        'You can now click on button A or button B to view the contents o' +
        'f the object'
      'hierarchy of FAdrs1 or FAdrs2'
      ''
      ''
      
        '5. Clear the contents of the second database (You can re-read th' +
        'e contents'
      'of DB #2 and confirm that it'#39's contents have been deleted.'
      ''
      
        '6. Save the contents of FAdrs1 to database 2 (You can re-read th' +
        'e contents'
      'of DB #2 and confirm that the save worked.'
      ''
      
        '7. Before closing the application, you must unload the two datab' +
        'ases.'
      ' ')
  end
end

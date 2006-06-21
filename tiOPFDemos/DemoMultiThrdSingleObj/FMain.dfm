object Form1: TForm1
  Left = 250
  Top = 239
  Width = 526
  Height = 344
  BorderIcons = [biSystemMenu]
  Caption = ' Thread safe TPerObjList test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    518
    317)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 332
    Top = 96
    Width = 177
    Height = 33
    Caption = 'Single thread test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 332
    Top = 140
    Width = 177
    Height = 33
    Caption = 'Multi thread test'
    TabOrder = 1
    OnClick = Button2Click
  end
  object paeNoOfThreads: TtiPerAwareFloatEdit
    Left = 332
    Top = 180
    Width = 177
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = 'Enter label &name'
    ReadOnly = False
    ValueAsString = '5'
    Value = 5.000000000000000000
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesInteger
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 4
    Top = 8
    Width = 313
    Height = 305
    TabStop = False
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'This demo will test multiple threads accessing a single object, '
      'which descends from TperObjThreadList (a TPerObjList that '
      'contains a semaphore to manage multi threaded access)'
      ''
      'There are two tests:'
      'a)  the first will write to the object, and read it'#39's contents '
      'back (sending it to the log file) in a single thread.'
      ''
      'b)  the second will perform the same write followed by a '
      
        'read, but there will be multiple threads accessing the same obje' +
        'ct. '
      ''
      
        'Two locking strategies will be used. Firstly, the TperObjThreadL' +
        'ist '
      
        'contains locking code in its data access methods like Add( ) and' +
        ' '
      
        'Items[] that will lock the object before the access and unlock i' +
        't '
      'after. Secondly, the object can be explicitly locked by the '
      'programmer before any access.'
      ''
      
        'These strategies can be understood like to work like transaction' +
        ' '
      'management when accessing a relational database with Delphi'#39's '
      
        'TQuery and TDatabase. If you do not start a transaction your sel' +
        'f, '
      'Delphi'#39's data access components will start one for each INSERT '
      
        'statement you run. If you do start one your self, then all INSER' +
        'T '
      'statements will be run inside the same transaction.')
  end
end

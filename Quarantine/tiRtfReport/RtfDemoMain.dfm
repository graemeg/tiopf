object FrmRtfDemoMain: TFrmRtfDemoMain
  Left = 135
  Top = 172
  Width = 312
  Height = 249
  Caption = 'FrmRtfDemoMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 42
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object ButtonParse: TButton
    Left = 5
    Top = 7
    Width = 75
    Height = 25
    Action = ActionParse
    TabOrder = 0
  end
  object ButtonShow: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Action = ActionShow
    TabOrder = 1
  end
  object Button1: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Action = ActionEdit
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 228
    Top = 92
    object ActionParse: TAction
      Caption = '&Parse'
      ShortCut = 120
      OnExecute = ActionParseExecute
    end
    object ActionShow: TAction
      Caption = '&Show'
      OnExecute = ActionShowExecute
    end
    object ActionEdit: TAction
      Caption = '&Edit'
      OnExecute = ActionEditExecute
    end
  end
  object tbBioLife: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'biolife.db'
    Left = 145
    Top = 145
  end
  object DatabaseFishAct: TDatabase
    Connected = True
    DatabaseName = 'DbDemos'
    SessionName = 'Default'
    Left = 145
    Top = 89
  end
  object DatabaseMastSql: TSQLConnection
    ConnectionName = 'IBConnection'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Interbase'
      
        'Database=C:\Program Files\Common Files\Borland Shared\Data\MASTS' +
        'QL.GDB'
      'RoleName=RoleName'
      'User_Name=sysdba'
      'Password=masterkey'
      'ServerCharSet='
      'SQLDialect=1'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'BlobSize=-1'
      'CommitRetain=False'
      'WaitOnLocks=True'
      'Interbase TransIsolation=ReadCommited'
      'Trim Char=False')
    VendorLib = 'gds32.dll'
    Left = 43
    Top = 94
  end
end

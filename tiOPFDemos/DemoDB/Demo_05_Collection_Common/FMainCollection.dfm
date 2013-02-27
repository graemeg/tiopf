object FormCollection: TFormCollection
  Left = 262
  Top = 204
  Width = 592
  Height = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnInsertRow: TButton
    Left = 464
    Top = 48
    Width = 113
    Height = 25
    Caption = 'Insert object into list'
    TabOrder = 0
    OnClick = btnInsertRowClick
  end
  object btnDeleteRow: TButton
    Left = 464
    Top = 77
    Width = 113
    Height = 25
    Caption = 'Delete object in list'
    TabOrder = 1
    OnClick = btnDeleteRowClick
  end
  object btnClear: TButton
    Left = 464
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Clear && New OID'
    TabOrder = 2
    OnClick = btnClearClick
  end
  object paeOID: TtiPerAwareEdit
    Left = 136
    Top = 12
    Width = 321
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 3
    Caption = 'OID'
    ReadOnly = False
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientName: TtiPerAwareEdit
    Left = 136
    Top = 40
    Width = 321
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 4
    Caption = 'Client name'
    ReadOnly = False
    MaxLength = 200
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientID: TtiPerAwareEdit
    Left = 136
    Top = 68
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
    Caption = 'Client ID'
    ReadOnly = False
    MaxLength = 9
    CharCase = ecNormal
    PasswordChar = #0
  end
  object Button1: TButton
    Left = 464
    Top = 152
    Width = 113
    Height = 25
    Action = aSave
    Caption = 'Save list to DB'
    TabOrder = 6
  end
  object Button2: TButton
    Left = 464
    Top = 112
    Width = 113
    Height = 25
    Caption = 'Show Objects in list'
    TabOrder = 7
    OnClick = Button2Click
  end
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 8
    Width = 113
    Height = 89
    TabStop = False
    Lines.Strings = (
      'This demo shows how '
      'to use hard coded SQL '
      'and custom Visitors to '
      'persist a simple list of '
      'objects.')
  end
  object btnReadList: TButton
    Left = 464
    Top = 184
    Width = 113
    Height = 25
    Caption = 'Read list from DB'
    TabOrder = 9
    OnClick = btnReadListClick
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 424
    Top = 108
    object aSave: TAction
      Caption = 'Save'
      OnExecute = aSaveExecute
    end
  end
end

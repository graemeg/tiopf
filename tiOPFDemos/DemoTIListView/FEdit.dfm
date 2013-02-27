object FormEdit: TFormEdit
  Left = 403
  Top = 311
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit'
  ClientHeight = 157
  ClientWidth = 225
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
    Left = 12
    Top = 12
    Width = 27
    Height = 13
    Caption = '&String'
    FocusControl = eString
  end
  object Label2: TLabel
    Left = 12
    Top = 40
    Width = 33
    Height = 13
    Caption = '&Integer'
    FocusControl = seInteger
  end
  object Label3: TLabel
    Left = 12
    Top = 68
    Width = 23
    Height = 13
    Caption = '&Float'
  end
  object Label4: TLabel
    Left = 12
    Top = 96
    Width = 23
    Height = 13
    Caption = '&Date'
    FocusControl = dtpDate
  end
  object bbOK: TBitBtn
    Left = 60
    Top = 124
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = bbOKClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object bbCancel: TBitBtn
    Left = 140
    Top = 124
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
  object eString: TEdit
    Left = 76
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'eString'
  end
  object seInteger: TSpinEdit
    Left = 76
    Top = 36
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
  object dtpDate: TDateTimePicker
    Left = 76
    Top = 92
    Width = 121
    Height = 21
    CalAlignment = dtaLeft
    Date = 36664.3915687384
    Time = 36664.3915687384
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 3
  end
  object eFloat: TEdit
    Left = 76
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'eFloat'
    OnKeyPress = eFloatKeyPress
  end
end

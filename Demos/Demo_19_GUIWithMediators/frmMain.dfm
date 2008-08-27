object Form1: TForm1
  Left = 192
  Top = 107
  Width = 512
  Height = 242
  Caption = 'GUI editing with Mediators'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 22
    Height = 13
    Caption = 'Age:'
  end
  object Label3: TLabel
    Left = 304
    Top = 16
    Width = 56
    Height = 13
    Caption = '(Read-Only)'
  end
  object Label4: TLabel
    Left = 16
    Top = 128
    Width = 38
    Height = 13
    Caption = 'Gender:'
  end
  object edtName: TEdit
    Left = 80
    Top = 16
    Width = 169
    Height = 21
    TabOrder = 0
    Text = 'edtName'
  end
  object ageTrackBar: TTrackBar
    Left = 72
    Top = 48
    Width = 150
    Height = 33
    Frequency = 10
    TabOrder = 1
  end
  object memName: TMemo
    Left = 304
    Top = 32
    Width = 185
    Height = 89
    Lines.Strings = (
      'memName')
    TabOrder = 2
  end
  object cbGender: TComboBox
    Left = 80
    Top = 128
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object btnChgViaCode: TButton
    Left = 16
    Top = 176
    Width = 105
    Height = 25
    Caption = 'Change via Code'
    TabOrder = 4
    OnClick = btnChgViaCodeClick
  end
  object btnShowModel: TButton
    Left = 128
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Show Model'
    TabOrder = 5
    OnClick = btnShowModelClick
  end
  object btnClose: TButton
    Left = 408
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 6
    OnClick = btnCloseClick
  end
  object edtAge: TEdit
    Left = 80
    Top = 80
    Width = 57
    Height = 21
    TabOrder = 7
    Text = 'edtAge'
  end
end

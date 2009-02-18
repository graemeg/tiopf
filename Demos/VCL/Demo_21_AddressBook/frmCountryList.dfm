object CountryListForm: TCountryListForm
  Left = 244
  Top = 174
  Width = 410
  Height = 327
  Caption = 'Country list'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object BAdd: TButton
    Left = 13
    Top = 9
    Width = 75
    Height = 25
    Caption = '&Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BAddClick
  end
  object BEdit: TButton
    Left = 95
    Top = 9
    Width = 75
    Height = 25
    Caption = '&Edit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BEditClick
  end
  object BDelete: TButton
    Left = 176
    Top = 9
    Width = 75
    Height = 25
    Caption = '&Delete'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = BDeleteClick
  end
  object GCountries: TStringGrid
    Left = 12
    Top = 40
    Width = 376
    Height = 224
    FixedCols = 0
    TabOrder = 3
  end
  object BClose: TButton
    Left = 313
    Top = 272
    Width = 75
    Height = 25
    Caption = '&Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 4
  end
end

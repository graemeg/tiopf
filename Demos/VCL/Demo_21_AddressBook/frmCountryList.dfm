object CountryListForm: TCountryListForm
  Left = 244
  Top = 174
  Caption = 'Country list'
  ClientHeight = 388
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    481
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object BAdd: TButton
    Left = 8
    Top = 8
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
    Left = 89
    Top = 8
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
    Left = 170
    Top = 8
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
    Left = 8
    Top = 39
    Width = 465
    Height = 310
    Anchors = [akLeft, akTop, akRight, akBottom]
    FixedCols = 0
    TabOrder = 3
  end
  object BClose: TButton
    Left = 398
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
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

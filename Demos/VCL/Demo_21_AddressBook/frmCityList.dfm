object CityListForm: TCityListForm
  Left = 252
  Top = 180
  BorderIcons = [biSystemMenu]
  Caption = 'CityListForm'
  ClientHeight = 319
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    416
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object GCities: TStringGrid
    Left = 8
    Top = 39
    Width = 400
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    FixedCols = 0
    TabOrder = 0
    ExplicitWidth = 402
    ExplicitHeight = 243
  end
  object BClose: TButton
    Left = 333
    Top = 286
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
    TabOrder = 1
    ExplicitLeft = 335
    ExplicitTop = 288
  end
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
    TabOrder = 2
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
    TabOrder = 3
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
    TabOrder = 4
    OnClick = BDeleteClick
  end
end

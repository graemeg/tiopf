object AddressTypeListForm: TAddressTypeListForm
  Left = 232
  Top = 178
  Caption = 'Address types list'
  ClientHeight = 238
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 0
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    384
    238)
  PixelsPerInch = 96
  TextHeight = 16
  object BAdd: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Add'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
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
    Font.Height = 0
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
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = BDeleteClick
  end
  object GAddressTypes: TStringGrid
    Left = 8
    Top = 39
    Width = 368
    Height = 160
    Anchors = [akLeft, akTop, akRight, akBottom]
    FixedCols = 0
    TabOrder = 3
    ColWidths = (
      64
      64
      64
      64
      64)
  end
  object BClose: TButton
    Left = 301
    Top = 205
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 4
    ExplicitLeft = 317
    ExplicitTop = 193
  end
end

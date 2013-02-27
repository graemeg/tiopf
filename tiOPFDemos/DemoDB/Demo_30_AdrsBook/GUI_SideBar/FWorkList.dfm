inherited FormWorkList: TFormWorkList
  Left = 366
  Top = 213
  Caption = 'FormWorkList'
  ClientHeight = 424
  Color = clBtnHighlight
  Font.Color = clNavy
  Font.Height = -16
  Font.Name = 'Arial'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 18
  inherited pnlCaption: TPanel
    Font.Color = clMenuText
    Font.Height = -12
    Font.Name = 'Microsoft Sans Serif'
    ParentFont = False
  end
  object pnlMain: TPanel [1]
    Left = 28
    Top = 44
    Width = 417
    Height = 373
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 68
      Width = 76
      Height = 18
      Caption = 'Search for:'
    end
    object tiHyperLink1: TtiHyperLink
      Left = 132
      Top = 24
      Width = 99
      Height = 20
      Cursor = crHandPoint
      OnClick = tiHyperLink1Click
      Caption = 'New company'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
    end
    object tiHyperLink2: TtiHyperLink
      Left = 24
      Top = 24
      Width = 84
      Height = 20
      Cursor = crHandPoint
      OnClick = tiHyperLink2Click
      Caption = 'New person'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
    end
    object Bevel1: TBevel
      Left = 16
      Top = 52
      Width = 385
      Height = 5
      Shape = bsTopLine
    end
    object Bevel2: TBevel
      Left = 16
      Top = 100
      Width = 385
      Height = 5
      Shape = bsTopLine
    end
    object editSearch: TEdit
      Left = 132
      Top = 64
      Width = 145
      Height = 26
      CharCase = ecUpperCase
      TabOrder = 0
    end
    object bbSearch: TBitBtn
      Left = 304
      Top = 62
      Width = 85
      Height = 29
      Caption = 'Search'
      Default = True
      TabOrder = 1
      OnClick = bbSearchClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FF6A859A
        58799CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FF63B1E52E97EF5B7DA1FF00FFFF00FFFF00FF1C
        A5D51CA5D51CA5D51CA5D51CA5D51CA5D51CA5D5FF00FFFF00FFFF00FFFF00FF
        51BBFF3094EA677D9EFF00FF1CA5D587E3FD8FDDF978D4F35DC9ED48C2EA40BD
        E93DBBE71CA5D5FF00FFFF00FFFF00FFFF00FF52BAFF3592DF687C9755A4C1B3
        C6CABDC7CC9DC9DB70D1F054CAEF49C2EA43BFE91CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FF72C1F7B5A399E2D4B5FFFBD7FFFCD7F0E1CEA5C0CD60D2F455CC
        F04FC6EE1CA5D5FF00FFFF00FFFF00FFFF00FFFF00FF8DA7B9F3D5ACFFF5C6FF
        FFDCFFFFE7FFFFFFE8DED990B3BD63D5F55DD0F21CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FFD5AC9EF8D6A2F8DBACFFFFDDFFFFEFFFFFF7FAF8DCD2C0B073DA
        F56CDAF71CA5D5FF00FFFF00FFFF00FFFF00FFFF00FFD6AFA3FBDAA5F1C48EFC
        F3CBFFFFE2FFFFE1FCFBD8D4C3B37DE0F879E2FB1CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FF939FA6EFE3C4FCEACCF3CE9DF7DEB1FFF8CCF6E9C5A5C1C481E8
        FE86E9FE1CA5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBEA8A8F4E8DCFC
        E2B0F9D6A0F5D9AFC0C3C189E7FB83EAFE8BEDFF1CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF669FB4CBCFC5DCD4CACFD2D7B6EDFE8DEBFF83E9
        FE8BEDFF1CA5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1CA5D5B3
        F8FFDDFAFFDBF9FFBFF3FF91ECFF87EBFF8DEEFF1CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF1CA5D5B1F3FFBFEAF8AFE3F594DFF573DBF573DE
        F787EAFC1CA5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1CA5D570
        CEEC78CDEA7CD1EC79D1ED6DCFEC58C9E954CAEA1CA5D5FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF1CA5D591D9EFBAF2FDAEF3FFA0F2FF95F0FF8EEE
        FF72DDF31CA5D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1C
        A5D51CA5D51CA5D51CA5D51CA5D51CA5D51CA5D5FF00FFFF00FF}
    end
    object lvSearchResult: TtiListView
      Left = 24
      Top = 116
      Width = 369
      Height = 245
      ShowFocusRect = False
      Anchors = [akLeft, akTop, akBottom]
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = True
      Visible = False
      OnItemEdit = lvSearchResultItemEdit
      OnItemDelete = lvSearchResultItemDelete
      OnFilterData = lvSearchResultFilterData
      ApplyFilter = True
      ApplySort = False
      ListColumns = <>
      SortOrders = <>
      CanStartDrag = False
    end
  end
end

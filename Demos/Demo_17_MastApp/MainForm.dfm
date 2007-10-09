object frmMain: TfrmMain
  Left = 493
  Top = 221
  Width = 341
  Height = 236
  Caption = 'Marine Adventures Order Entry (tiOPF)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 49
    Align = alTop
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object OrderBtn: TSpeedButton
      Left = 1
      Top = 1
      Width = 66
      Height = 45
      Hint = 'Enter new order'
      Caption = '&New Order'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000012000000140000000100
        040000000000F000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333338765F03700000000000000030865F037FFFFFFFFFFFFFF037765F037FF
        FFFFFFFFFFFF03F865F037F444444444444F038765F037FFFFFFFFFFFFFF0308
        65F037F44444444444FF036666C037FFFFFFFFFFFFFF0300000037F444444444
        444F0389888837FFFFFFFFFFFFFF0300066E37F44444444444FF0300000037FF
        FFFFFFFFFFFF0301301937F444444444444F0300008837FFFFFFFFFFFFFF0300
        0AAA37FFFFFFFFFFFFFF03AA2A2037F444FFFFFFFFFF03AA646437FFFFFFFFFF
        FFFF0304040837FFFFFFFFFFFFFF038899003777777777777777730000003333
        33333333333333000000}
      Layout = blGlyphTop
      Spacing = 0
      OnClick = OrderBtnClick
    end
    object BrowseBtn: TSpeedButton
      Left = 66
      Top = 1
      Width = 66
      Height = 45
      Hint = 'Browse and edit orders'
      Caption = '&Browse'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333EEEE33370000000000000003FFFF3337FFFFFFFFFFFFFF0333333337
        FFFFFFFFFFFFFF03CCCC3707FFFFFFFFFFFFFF03FFFF37F7FF4444444444FF03
        333337F7FFFFFFFFFFFFFF03CCCC37F7FF4444444444FF03FFFF37F7FFFFFFFF
        FFFFFF03FFFF37F7FFFFFFFFFFFFFF03FFFF37F7FF4444FF4444FF03FFFF37F7
        FFFFFFFFFFFFFF03FFFF37F7FF4444FF4444FF03FFFF37F7FFFFFFFFFFFFFF03
        FFEE37F7FF4444FF4444FF03091037F7FFFFFFFFFFFFFF0365F037F777777777
        7777777365F037FFFFFFFFFFFFF0333365F03777777777777777333365F03333
        333333333333333365F0}
      Layout = blGlyphTop
      Spacing = 0
      OnClick = ViewOrdersClick
    end
    object PartsBtn: TSpeedButton
      Left = 131
      Top = 1
      Width = 66
      Height = 45
      Hint = 'Browse and edit inventory'
      Caption = 'P&arts'
      Glyph.Data = {
        2A010000424D2A010000000000007600000028000000130000000F0000000100
        040000000000B400000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33000333300033333333333336EEE03030003337000000000E60073730003330
        E6EEEEEEEE03333330003337000000000E600737300033333333333376EEE030
        3000333333333333370007373000333333333333333333333000333333333336
        5550333330003700000000056007333330003056555555550333333330003700
        0000000560073333300033333333337655503333300033333333333700073333
        3000333333333333333333333000}
      Layout = blGlyphTop
      Spacing = 3
      OnClick = PartsBtnClick
    end
    object CloseBtn: TSpeedButton
      Left = 262
      Top = 1
      Width = 66
      Height = 45
      Hint = 'Close the application'
      Caption = 'Cl&ose'
      Glyph.Data = {
        06020000424D0602000000000000760000002800000028000000140000000100
        0400000000009001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00377777777777
        777777773FFFFFFFFFFFF333333F888888888888F7F7F7888888888888883333
        33888888888888877F7F788888888888888F333FF88844444400888FFF444444
        88888888888333888883333334D5007FFF433333333338F888F3338F33333333
        345D50FFFF4333333333388788F3338F3333333334D5D0FFFF433333333338F8
        78F3338F33333333345D50FEFE4333333333388788F3338F3333333334D5D0FF
        FF433333333338F878F3338F33333333345D50FEFE4333333333388788F3338F
        3333333334D5D0FFFF433333333338F878F3338F33333333345D50FEFE433333
        3333388788F3338F3333333334D5D0EFEF433333333338F878F3338F33333333
        345D50FEFE4333333333388788F3338F3333333334D5D0EFEF433333333338F8
        F8FFFF8F33333333344444444443333333333888888888833333333333333333
        3333333333333333FFFFFF333333333333300000033333333333333888888F33
        333333333330AAAA0333333333333338FFFF8F33333333333330000003333333
        33333338888883333333}
      Layout = blGlyphTop
      NumGlyphs = 2
      Spacing = 0
      OnClick = CloseBtnClick
    end
  end
  object MainMenu: TMainMenu
    Left = 36
    Top = 52
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileNewOrder: TMenuItem
        Caption = '&New Order...'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object FilePrintReport: TMenuItem
        Caption = 'Print &Report'
        Enabled = False
        object PrintCustList: TMenuItem
          Caption = '&Customer List'
        end
        object PrintOrders: TMenuItem
          Caption = '&Order History...'
        end
        object PrintInvoice: TMenuItem
          Caption = '&Invoice'
        end
      end
      object FilePrinterSetup: TMenuItem
        Caption = 'Printer Setup...'
        Enabled = False
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object FileExit: TMenuItem
        Caption = 'E&xit'
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      object Vendors1: TMenuItem
        Caption = 'Vendors'
        OnClick = Vendors1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ViewOrders: TMenuItem
        Caption = '&Orders...'
        OnClick = ViewOrdersClick
      end
      object ViewPartsInventory: TMenuItem
        Caption = '&Parts/Inventory...'
        OnClick = PartsBtnClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ViewStayOnTop: TMenuItem
        Caption = '&Stay On Top'
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object HelpAbout: TMenuItem
        Caption = '&About...'
      end
    end
  end
  object PrinterSetup: TPrinterSetupDialog
    Left = 131
    Top = 52
  end
end

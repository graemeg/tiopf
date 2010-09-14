object MainFrm: TMainFrm
  Left = 194
  Top = 185
  ActiveControl = LVContacts
  Caption = 'AddressBook Demo using Model-GUI-Mediator'
  ClientHeight = 293
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LVContacts: TListView
    Left = 12
    Top = 13
    Width = 607
    Height = 275
    Columns = <>
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 70
    Top = 50
    object miFile: TMenuItem
      Caption = '&File'
      object miFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miFileExitClick
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      object miEditInsert: TMenuItem
        Caption = '&Insert'
        OnClick = miEditInsertClick
      end
      object miEditEdit: TMenuItem
        Caption = 'E&dit'
        OnClick = miEditEditClick
      end
      object miEditDelete: TMenuItem
        Caption = '&Delete'
        OnClick = miEditDeleteClick
      end
    end
    object miSystem: TMenuItem
      Caption = '&System'
      object miSystemCountries: TMenuItem
        Caption = 'Country List'
        OnClick = miSystemCountriesClick
      end
      object miSystemCities: TMenuItem
        Caption = 'City List'
        OnClick = miSystemCitiesClick
      end
    end
  end
end

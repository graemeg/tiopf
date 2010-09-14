object frmDemoMain: TfrmDemoMain
  Left = 253
  Top = 351
  Caption = 'VCL Mediators Demo'
  ClientHeight = 349
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    810
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object btnAdd: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 0
    OnClick = AddContactClick
  end
  object btnEdit: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 1
    OnClick = EditContactClick
  end
  object btnDelete: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = DeleteContactClick
  end
  object GContacts: TStringGrid
    Left = 8
    Top = 40
    Width = 794
    Height = 301
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
  object btnShow: TButton
    Left = 249
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Show'
    TabOrder = 3
    OnClick = btnShowClick
  end
  object MainMenu1: TMainMenu
    Left = 176
    Top = 180
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = MIExitClick
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Add1: TMenuItem
        Caption = 'Add'
        OnClick = AddContactClick
      end
      object Edit2: TMenuItem
        Caption = 'Edit'
        OnClick = EditContactClick
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        OnClick = DeleteContactClick
      end
    end
    object System1: TMenuItem
      Caption = 'System'
      object CityList1: TMenuItem
        Caption = 'City List'
        OnClick = CityListClick
      end
      object CountryList1: TMenuItem
        Caption = 'Country List'
        OnClick = CountryListClick
      end
      object AddressTypeList1: TMenuItem
        Caption = 'Address Type List'
        OnClick = AddressTypeListClick
      end
    end
  end
end

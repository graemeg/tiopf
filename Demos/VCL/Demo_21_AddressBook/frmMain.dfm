object frmDemoMain: TfrmDemoMain
  Left = 253
  Top = 351
  Width = 818
  Height = 388
  Caption = 'VCL Mediators Demo'
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
  OnDestroy = FormDestroy
  DesignSize = (
    810
    342)
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
    Width = 791
    Height = 293
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object MainMenu1: TMainMenu
    Left = 292
    Top = 4
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

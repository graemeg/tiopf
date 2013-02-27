inherited frmDoctorMaint: TfrmDoctorMaint
  Top = 216
  Caption = 'Doctor Properties'
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgcDetails: TPageControl
    inherited tabDetails: TTabSheet
      object Label1: TLabel
        Left = 20
        Top = 80
        Width = 28
        Height = 13
        Caption = '&Name'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 20
        Top = 120
        Width = 72
        Height = 13
        Caption = 'N.&H.S. Number'
        FocusControl = edNHSNumber
      end
      object edName: TEdit
        Left = 112
        Top = 76
        Width = 189
        Height = 21
        TabOrder = 0
      end
      object edNHSNumber: TEdit
        Left = 112
        Top = 116
        Width = 121
        Height = 21
        TabOrder = 1
      end
    end
  end
  inherited OvcController2: TOvcController
    EntryCommands.TableList = (
      'Default'
      True
      ()
      'WordStar'
      False
      ()
      'Grid'
      False
      ())
  end
end

inherited frmPatientMaint: TfrmPatientMaint
  Left = 285
  Top = 212
  Caption = 'Patient'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgcDetails: TPageControl
    inherited tabDetails: TTabSheet
      object Label1: TLabel
        Left = 12
        Top = 48
        Width = 28
        Height = 13
        Caption = '&Name'
        FocusControl = edName
      end
      object Label2: TLabel
        Left = 12
        Top = 72
        Width = 38
        Height = 13
        Caption = 'Add&ress'
        FocusControl = meAddress
      end
      object Label3: TLabel
        Left = 12
        Top = 156
        Width = 49
        Height = 13
        Caption = '&Post Code'
        FocusControl = edPostCode
      end
      object Label4: TLabel
        Left = 12
        Top = 184
        Width = 61
        Height = 13
        Caption = 'Date Of &Birth'
        FocusControl = dtpDOB
      end
      object edName: TEdit
        Left = 96
        Top = 44
        Width = 185
        Height = 21
        TabOrder = 0
        Text = 'edName'
      end
      object meAddress: TMemo
        Left = 96
        Top = 72
        Width = 185
        Height = 73
        Lines.Strings = (
          '')
        TabOrder = 1
      end
      object edPostCode: TEdit
        Left = 96
        Top = 152
        Width = 89
        Height = 21
        TabOrder = 2
        Text = 'edPostCode'
      end
      object dtpDOB: TDateTimePicker
        Left = 96
        Top = 180
        Width = 145
        Height = 21
        CalAlignment = dtaLeft
        Date = 37667.958123831
        Time = 37667.958123831
        DateFormat = dfLong
        DateMode = dmComboBox
        Kind = dtkDate
        ParseInput = False
        TabOrder = 3
      end
      object cmbDoctor: TtiPerAwareComboBoxDynamic
        Left = 12
        Top = 208
        Width = 229
        Height = 23
        ShowFocusRect = False
        Constraints.MinHeight = 23
        TabOrder = 4
        Caption = '&Doctor'
        ReadOnly = False
        FieldName = 'Doctor'
        DropDownCount = 8
        CharCase = ecNormal
        FieldNameDisplay = 'Name'
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

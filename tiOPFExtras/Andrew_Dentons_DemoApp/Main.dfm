object frmMain: TfrmMain
  Left = 252
  Top = 221
  Width = 689
  Height = 411
  Caption = 'TechInsite Demonstration Application'
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
  PixelsPerInch = 96
  TextHeight = 13
  object sbMain: TStatusBar
    Left = 0
    Top = 346
    Width = 681
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 681
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object cmbDoctor: TtiPerAwareComboBoxDynamic
      Left = 12
      Top = 8
      Width = 185
      Height = 23
      ShowFocusRect = False
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = '&Doctor'
      LabelWidth = 50
      ReadOnly = False
      FieldName = 'CurrentDoctor'
      OnChange = cmbDoctorChange
      DropDownCount = 8
      CharCase = ecNormal
      FieldNameDisplay = 'Name'
    end
    object dtpCurrentDate: TtiPerAwareDateTimePicker
      Left = 236
      Top = 8
      Width = 185
      Height = 23
      ShowFocusRect = False
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Da&te'
      LabelWidth = 40
      ReadOnly = False
      FieldName = 'CurrentDate'
      OnChange = dtpCurrentDateChange
      Value = 37668.7811282755
      Kind = dtkDate
      DateMode = dmComboBox
    end
  end
  object lvAppointments: TtiListViewPlus
    Left = 0
    Top = 36
    Width = 680
    Height = 272
    ShowFocusRect = False
    RuntimeGenCols = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    MultiSelect = False
    OnDblClick = lvAppointmentsDblClick
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <
      item
        DisplayLabel = 'Time'
        FieldName = 'Scheduled'
        DisplayMask = 'hh:mm'
        DataType = lvtkDateTime
        Derived = False
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Duration'
        FieldName = 'Duration'
        DisplayMask = '###0'
        DataType = lvtkInt
        Derived = False
      end
      item
        DisplayLabel = 'Status'
        FieldName = 'Status'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Patient'
        FieldName = 'Patient'
        DataType = lvtkString
        Derived = True
        OnDeriveColumn = lvAppointmentsListColumns3DeriveColumn
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Comment'
        FieldName = 'Comment'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end>
    SortOrders = <>
  end
  object Panel2: TPanel
    Left = 0
    Top = 305
    Width = 681
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 3
    object spdBook: TSpeedButton
      Left = 0
      Top = 0
      Width = 97
      Height = 41
      Action = actBook
      Flat = True
    end
    object spdCancel: TSpeedButton
      Left = 97
      Top = 0
      Width = 97
      Height = 41
      Action = actCancel
      Flat = True
    end
    object spdArrive: TSpeedButton
      Left = 194
      Top = 0
      Width = 97
      Height = 41
      Action = actArrive
      Flat = True
    end
    object spdStart: TSpeedButton
      Left = 291
      Top = 0
      Width = 97
      Height = 41
      Action = actStart
      Flat = True
    end
    object spdEnd: TSpeedButton
      Left = 388
      Top = 0
      Width = 97
      Height = 41
      Action = actEnd
      Flat = True
    end
    object spdAppDetails: TSpeedButton
      Left = 485
      Top = 0
      Width = 97
      Height = 41
      Action = actAppDetails
      Flat = True
    end
    object spdPatientDetails: TSpeedButton
      Left = 582
      Top = 0
      Width = 97
      Height = 41
      Action = actPatientDetails
      Flat = True
    end
  end
  object MainMenu1: TMainMenu
    Left = 448
    object File1: TMenuItem
      Caption = '&File'
      object Doctors1: TMenuItem
        Action = actDoctors
      end
      object Patients1: TMenuItem
        Action = actPatients
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object New1: TMenuItem
      Caption = '&New'
      object AppointmentSlots1: TMenuItem
        Action = actNewSlots
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object ShowHierarchy1: TMenuItem
        Caption = '&Show Hierarchy'
        OnClick = ShowHierarchy1Click
      end
    end
  end
  object alMain: TActionList
    Left = 480
    object actExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = actExitExecute
    end
    object actDoctors: TAction
      Category = 'File'
      Caption = '&Doctors'
      Hint = 'Shows the Surgery'#39's Doctor List'
      OnExecute = actDoctorsExecute
    end
    object actPatients: TAction
      Category = 'File'
      Caption = '&Patients'
      OnExecute = actPatientsExecute
    end
    object actNewSlots: TAction
      Category = 'New'
      Caption = '&Appointment Slots'
      Hint = 'Creates new appointment schedule for the current doctor.'
      OnExecute = actNewSlotsExecute
      OnUpdate = actNewSlotsUpdate
    end
    object actBook: TAction
      Caption = '&Book'
      Hint = 'Book a slot to a patient'
      ShortCut = 113
      OnExecute = actBookExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      Hint = 'Cancel an appointment freeing it up for another patient'
      ShortCut = 114
      OnExecute = actCancelExecute
    end
    object actArrive: TAction
      Caption = 'Patient Arrive'
      Hint = 'Patient has arrived for their appointment'
      ShortCut = 115
      OnExecute = actArriveExecute
    end
    object actStart: TAction
      Caption = 'Start Consultation'
      Hint = 'Patient has started consultation with doctor'
      ShortCut = 116
      OnExecute = actStartExecute
    end
    object actEnd: TAction
      Caption = 'End Consultation'
      Hint = 'Patient has finished consultation with doctor'
      ShortCut = 117
      OnExecute = actEndExecute
    end
    object actAppDetails: TAction
      Caption = 'Appointment Details'
      Hint = 'Show detailed information about this appointment'
      ShortCut = 120
      OnExecute = actAppDetailsExecute
      OnUpdate = actAppDetailsUpdate
    end
    object actPatientDetails: TAction
      Caption = 'Patient Details'
      Hint = 'Show this patient'#39's details'
      ShortCut = 121
      OnExecute = actPatientDetailsExecute
      OnUpdate = actPatientDetailsUpdate
    end
  end
  object MadExceptionHandler1: TMadExceptionHandler
    Left = 544
    Top = 4
  end
end

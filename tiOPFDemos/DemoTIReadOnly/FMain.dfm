object FormMain: TFormMain
  Left = 201
  Top = 226
  Width = 686
  Height = 362
  Caption = ' TIReadOnly component demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 217
    Height = 281
    Caption = ' Delphi Standard Controls '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 102
      Height = 13
      Caption = 'It will work for these...'
    end
    object Label2: TLabel
      Left = 8
      Top = 124
      Width = 87
      Height = 13
      Caption = 'But not for these...'
    end
    object Edit1: TEdit
      Left = 20
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Memo1: TMemo
      Left = 20
      Top = 68
      Width = 185
      Height = 49
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 20
      Top = 140
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 2
    end
    object RadioButton1: TRadioButton
      Left = 20
      Top = 164
      Width = 113
      Height = 17
      Caption = 'RadioButton1'
      TabOrder = 3
    end
    object ListBox1: TListBox
      Left = 20
      Top = 184
      Width = 77
      Height = 61
      ItemHeight = 13
      Items.Strings = (
        'One'
        'Two'
        'Three')
      TabOrder = 4
    end
    object ComboBox1: TComboBox
      Left = 20
      Top = 252
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 5
      Text = 'ComboBox1'
      Items.Strings = (
        'One'
        'Two'
        'Three')
    end
  end
  object GroupBox2: TGroupBox
    Left = 232
    Top = 48
    Width = 441
    Height = 281
    Caption = ' TechInsite Controls '
    TabOrder = 1
    object tiPerAwareEdit1: TtiPerAwareEdit
      Left = 16
      Top = 20
      Width = 205
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'PerAwareEdit'
      LabelWidth = 100
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object tiPerAwareMemo1: TtiPerAwareMemo
      Left = 16
      Top = 44
      Width = 205
      Height = 41
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'PerAwareMemo'
      LabelWidth = 100
      ReadOnly = False
      ScrollBars = ssNone
      WordWrap = True
      MaxLength = 0
    end
    object tiPerAwareComboBoxStatic1: TtiPerAwareComboBoxStatic
      Left = 16
      Top = 88
      Width = 205
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 2
      Caption = 'PerAwareComboBox'
      LabelWidth = 100
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
      Items.Strings = (
        'One'
        'Two'
        'Three')
    end
    object tiPerAwareDateTimePicker1: TtiPerAwareDateTimePicker
      Left = 228
      Top = 20
      Width = 205
      Height = 21
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 3
      Caption = 'PerAwareDate'
      LabelWidth = 100
      ReadOnly = False
      Value = 37168.682742500000000000
      Kind = dtkDate
      DateMode = dmComboBox
    end
    object tiPerAwareCheckBox1: TtiPerAwareCheckBox
      Left = 228
      Top = 44
      Width = 205
      Height = 17
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 4
      Caption = 'PerAwareCheckBox'
      LabelWidth = 100
      ReadOnly = False
      Value = False
    end
    object tiPerAwareFloatEdit1: TtiPerAwareFloatEdit
      Left = 228
      Top = 64
      Width = 205
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 5
      Caption = 'PerAwareFloat'
      LabelWidth = 100
      ReadOnly = False
      ValueAsString = '0'
      Precision = 0
      UnknownValue = -1.000000000000000000
      IsKnown = True
      Style = fesUser
    end
    object tiListView1: TtiListView
      Left = 16
      Top = 116
      Width = 205
      Height = 158
      ShowFocusRect = True
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = True
      OnItemEdit = tiListView1ItemEdit
      OnItemInsert = tiListView1ItemInsert
      OnItemDelete = tiListView1ItemDelete
      ApplyFilter = False
      ApplySort = False
      ListColumns = <>
      SortOrders = <>
      VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
      CanStartDrag = False
      DesignSize = (
        205
        158)
    end
    object tiTreeView1: TtiTreeView
      Left = 232
      Top = 132
      Width = 185
      Height = 141
      BevelOuter = bvNone
      DataMappings = <
        item
          DataClass = 'TPerson'
          DisplayPropName = 'Name'
          Name = 'tiTVMappingPerson'
        end
        item
          CanDelete = True
          CanInsert = True
          CanEdit = True
          DataClass = 'TAdrs'
          DisplayPropName = 'Address'
          Name = 'tiTVMappingAdrs'
          OnInsert = tiTVMappingAdrsOnInsert
          OnDelete = tiTVMappingAdrsOnDelete
          OnEdit = tiTVMappingAdrsOnEdit
        end>
      SplitterPos = 185
      TreeSortType = stNone
      SplitterVisible = False
    end
  end
  object cbReadOnly: TCheckBox
    Left = 8
    Top = 12
    Width = 97
    Height = 17
    Caption = 'Read only?'
    TabOrder = 2
    OnClick = cbReadOnlyClick
  end
  object RO: TtiReadOnly
    Enabled = True
    Left = 120
    Top = 8
  end
end

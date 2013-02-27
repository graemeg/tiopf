object Form1: TForm1
  Left = 283
  Top = 192
  Width = 638
  Height = 400
  Caption = 'MakeBS for the tiOPF'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 638
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 630
    Height = 336
    ActivePage = TabSheet1
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabWidth = 98
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      BorderWidth = 2
      Caption = '1. Class Name'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 618
        Height = 304
        Align = alClient
        BevelOuter = bvNone
        Color = clCream
        TabOrder = 0
        object Label2: TLabel
          Left = 24
          Top = 12
          Width = 210
          Height = 13
          Caption = 'Class Name (DO NOT include the '#39'T'#39' prefix)::'
        end
        object Label3: TLabel
          Left = 8
          Top = 35
          Width = 15
          Height = 20
          Alignment = taCenter
          AutoSize = False
          Caption = 'T'
          Color = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object Label4: TLabel
          Left = 24
          Top = 144
          Width = 77
          Height = 13
          Caption = 'Output Directory'
        end
        object Label7: TLabel
          Left = 24
          Top = 68
          Width = 58
          Height = 13
          Caption = 'Table Name'
        end
        object edtClassName: TEdit
          Left = 24
          Top = 32
          Width = 229
          Height = 24
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          Text = 'Customers'
          OnChange = edtClassNameChange
          OnKeyPress = DoCheckKeyPress
        end
        object edtOutputDir: TEdit
          Left = 24
          Top = 160
          Width = 193
          Height = 21
          TabOrder = 3
        end
        object btnBrowse: TButton
          Left = 224
          Top = 159
          Width = 27
          Height = 22
          Hint = 'Browse'
          Caption = '...'
          TabOrder = 4
          OnClick = btnBrowseClick
        end
        object edtTableName: TEdit
          Left = 24
          Top = 84
          Width = 229
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          Text = 'Customers'
          OnKeyPress = DoCheckKeyPress
        end
        object chkUseClassName: TCheckBox
          Left = 128
          Top = 64
          Width = 125
          Height = 17
          Caption = 'Sa&me as ClassName'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = chkUseClassNameClick
        end
        object Memo3: TMemo
          Left = 304
          Top = 20
          Width = 301
          Height = 269
          TabStop = False
          Lines.Strings = (
            'Step by Step'
            ''
            '1. Enter the name of your class. If your database table '
            'name is different from your class name, uncheck the '#39'Same '
            'as ClassName'#39' checkbox and enter the table name.'
            ''
            '2. Select your Output directory.'
            ''
            '3. Go to the Field Roster page and enter your class'#39's fields.'
            ''
            '4. Click the Generate button at any time to create and '
            'Preview your BOM and SVR source files.'
            ''
            '5. Click the Save button to write them to disk,'
            ''
            '6. Use the '#39'Export to CSV'#39' button to save the field list only.')
          ScrollBars = ssVertical
          TabOrder = 5
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = '2. Field Roster'
      ImageIndex = 3
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 622
        Height = 308
        Align = alClient
        Color = clCream
        TabOrder = 0
        object Label5: TLabel
          Left = 44
          Top = 224
          Width = 24
          Height = 13
          Caption = 'Type'
        end
        object Label6: TLabel
          Left = 16
          Top = 196
          Width = 53
          Height = 13
          Caption = 'Field Name'
        end
        object cmbFieldType: TComboBox
          Left = 80
          Top = 220
          Width = 125
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
        end
        object rgrVisibility: TRadioGroup
          Left = 220
          Top = 184
          Width = 118
          Height = 111
          Caption = 'Visibility'
          ItemIndex = 3
          Items.Strings = (
            'Private'
            'Protected'
            'Public'
            'Published')
          TabOrder = 2
        end
        object edtFieldName: TEdit
          Left = 80
          Top = 192
          Width = 125
          Height = 21
          TabOrder = 3
          Text = 'Field_Name'
          OnChange = edtFieldNameChange
          OnKeyPress = edtFieldNameKeyPress
        end
        object btnAddField: TBitBtn
          Left = 520
          Top = 190
          Width = 85
          Height = 42
          Caption = '&Add Field'
          Default = True
          TabOrder = 4
          OnClick = btnAddFieldClick
        end
        object btnRemove: TBitBtn
          Left = 521
          Top = 26
          Width = 87
          Height = 25
          Caption = 'Remove Field'
          Enabled = False
          TabOrder = 5
          OnClick = btnRemoveClick
        end
        object btnRemoveAll: TBitBtn
          Left = 521
          Top = 67
          Width = 87
          Height = 25
          Caption = 'Remove All'
          Enabled = False
          TabOrder = 6
          OnClick = btnRemoveAllClick
        end
        object stgFields: TStringGrid
          Left = 12
          Top = 16
          Width = 489
          Height = 153
          ColCount = 7
          Ctl3D = False
          DefaultRowHeight = 16
          FixedColor = clCream
          FixedCols = 0
          RowCount = 2
          Options = [goFixedHorzLine, goThumbTracking]
          ParentCtl3D = False
          TabOrder = 0
          OnClick = stgFieldsClick
        end
        object grpDirectives: TGroupBox
          Left = 356
          Top = 184
          Width = 118
          Height = 111
          Caption = 'Directives'
          TabOrder = 7
          object chkUseGetter: TCheckBox
            Left = 18
            Top = 20
            Width = 87
            Height = 17
            Caption = 'Use Getter'
            TabOrder = 0
          end
          object chkUseSetter: TCheckBox
            Left = 18
            Top = 42
            Width = 87
            Height = 17
            Caption = 'Use Setter'
            TabOrder = 1
          end
          object chkOverride: TCheckBox
            Left = 18
            Top = 64
            Width = 87
            Height = 17
            Caption = 'Override'
            TabOrder = 2
          end
          object chkReintroduce: TCheckBox
            Left = 18
            Top = 86
            Width = 87
            Height = 17
            Caption = 'Reintroduce'
            TabOrder = 3
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = '3. BOM Output'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 622
        Height = 308
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Andale Mono'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
      end
    end
    object TabSheet3: TTabSheet
      Caption = '4. SVR Output'
      ImageIndex = 2
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 622
        Height = 308
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Andale Mono'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 336
    Width = 630
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnPreview: TButton
      Left = 32
      Top = 6
      Width = 137
      Height = 25
      Caption = '&Generate && Preview'
      Enabled = False
      TabOrder = 0
      OnClick = btnPreviewClick
    end
    object btnSave: TButton
      Left = 200
      Top = 6
      Width = 97
      Height = 25
      Caption = '&Save Files'
      Enabled = False
      TabOrder = 1
      OnClick = btnGenerateClick
    end
    object btnExportCSV: TButton
      Left = 460
      Top = 6
      Width = 101
      Height = 25
      Caption = 'E&xport CSV'
      TabOrder = 2
      OnClick = btnExportCSVClick
    end
    object btnImportCSV: TButton
      Left = 332
      Top = 6
      Width = 101
      Height = 25
      Caption = 'Import CSV'
      TabOrder = 3
      OnClick = btnImportCSVClick
    end
  end
end

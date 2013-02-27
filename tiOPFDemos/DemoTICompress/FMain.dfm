object FormMain: TFormMain
  Left = 449
  Top = 159
  Width = 452
  Height = 387
  BorderIcons = [biSystemMenu]
  Caption = ' Test the TtiCompress classes'
  Color = clMenu
  Constraints.MinHeight = 387
  Constraints.MinWidth = 452
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 122
    Height = 13
    Caption = 'Compression &class to use:'
  end
  object cbCompressionType: TComboBox
    Left = 148
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object PC: TPageControl
    Left = 8
    Top = 40
    Width = 429
    Height = 313
    ActivePage = TabSheet2
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = '&File compression'
      object Label2: TLabel
        Left = 220
        Top = 20
        Width = 99
        Height = 13
        Caption = 'File to compress &from'
      end
      object Label3: TLabel
        Left = 220
        Top = 72
        Width = 88
        Height = 13
        Caption = 'File to compress &to'
      end
      object Label4: TLabel
        Left = 216
        Top = 124
        Width = 100
        Height = 13
        Caption = 'File to &decompress to'
      end
      object eFileNameBefore: TEdit
        Left = 228
        Top = 40
        Width = 169
        Height = 21
        TabOrder = 0
        Text = 'Before.txt'
      end
      object eFileNameCompress: TEdit
        Left = 228
        Top = 92
        Width = 169
        Height = 21
        TabOrder = 1
        Text = 'Compress.txt'
      end
      object eFileNameAfter: TEdit
        Left = 228
        Top = 144
        Width = 169
        Height = 21
        TabOrder = 2
        Text = 'After.txt'
      end
      object bbTestFileCompression: TBitBtn
        Left = 272
        Top = 248
        Width = 139
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Test file compression'
        Default = True
        TabOrder = 3
        OnClick = bbTestFileCompressionClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF7770777777
          77FFFF777807777777FFFF777700777777FFFF77770B077777FFFF770000B077
          77FFFF770BBBBB0777FFFF7770FB000077FFFF7770BBB07777FFFF00000BFB07
          77FFFF0FBBBFBBB077FFFF70FBFB000007FFFF70BFBFB07777FFFF770BFBFB07
          77FFFF770FFFFFF077FFFF7770FBFFBF07FFFF777000000000FF}
      end
      object Memo1: TMemo
        Left = 8
        Top = 20
        Width = 201
        Height = 254
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'This will test file compression by creating '
          'a text file with random characters '
          '<compress from> and '
          'compressing it to <compress to>. '
          ''
          'The compressed file shall then be '
          'decompressed to <decompress to>. '
          ''
          'The <from> and <to> files shall be loaded '
          'into TStringLists and the Text properties '
          'shall be compared. If they are the same, '
          'the test shall be deemed to have passed.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 4
      end
    end
    object TabSheet2: TTabSheet
      Caption = '&String compression'
      ImageIndex = 1
      object Label5: TLabel
        Left = 220
        Top = 16
        Width = 93
        Height = 13
        Caption = '&Before compression'
      end
      object Label6: TLabel
        Left = 220
        Top = 132
        Width = 84
        Height = 13
        Caption = '&After compression'
      end
      object bbTestStringCompression: TBitBtn
        Left = 272
        Top = 248
        Width = 139
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Test string compression'
        Default = True
        TabOrder = 0
        OnClick = bbTestStringCompressionClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF7770777777
          77FFFF777807777777FFFF777700777777FFFF77770B077777FFFF770000B077
          77FFFF770BBBBB0777FFFF7770FB000077FFFF7770BBB07777FFFF00000BFB07
          77FFFF0FBBBFBBB077FFFF70FBFB000007FFFF70BFBFB07777FFFF770BFBFB07
          77FFFF770FFFFFF077FFFF7770FBFFBF07FFFF777000000000FF}
      end
      object mBefore: TMemo
        Left = 228
        Top = 36
        Width = 185
        Height = 89
        Anchors = [akLeft, akTop, akRight]
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object mAfter: TMemo
        Left = 228
        Top = 152
        Width = 185
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object Memo2: TMemo
        Left = 8
        Top = 20
        Width = 201
        Height = 254
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'This will test string compression by '
          'compressing the text in <before '
          'compression> to a temporary string '
          'variable. '
          ''
          'The string will then be decompressed to '
          '<after compression> and the text '
          'properties of each of the TMemos will be '
          'compared.'
          ''
          'If the text is the same, the test will be '
          'deemed to have passed.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 3
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'S&tream compression'
      ImageIndex = 2
      object Label7: TLabel
        Left = 220
        Top = 20
        Width = 99
        Height = 13
        Caption = 'File to compress &from'
      end
      object Label8: TLabel
        Left = 220
        Top = 72
        Width = 88
        Height = 13
        Caption = 'File to compress &to'
      end
      object Label9: TLabel
        Left = 216
        Top = 124
        Width = 100
        Height = 13
        Caption = 'File to &decompress to'
      end
      object sbFileOpen: TSpeedButton
        Left = 396
        Top = 40
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Glyph.Data = {
          5E000000424D5E000000000000003E0000002800000008000000080000000100
          010000000000200000000000000000000000020000000000000000000000FFFF
          FF00FF000000FF000000EF000000C70000008300000001000000FF000000FF00
          0000}
        OnClick = sbFileOpenClick
      end
      object Memo3: TMemo
        Left = 8
        Top = 20
        Width = 201
        Height = 254
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'This will test stream compression by '
          'reading the file named in <compress '
          'from> and compressing it to <compress '
          'to>. '
          ''
          'The compressed file shall then be '
          'decompressed to <decompress to>. '
          ''
          'The <decompress to> file shall be loaded '
          'with the default editor for that file type '
          'using ShellExecute( )'
          ''
          'Note: I have been having problems with '
          'ShellExeucte( ) locking the file when '
          'running in the Delphi IDE. This may '
          'cause the error '#39'Can not create file'#39'. Try '
          'running outside Delphi and if you know a '
          'fix to this problem, please let me know.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object bbTestStreamCompression: TBitBtn
        Left = 272
        Top = 248
        Width = 139
        Height = 29
        Anchors = [akRight, akBottom]
        Caption = 'Test stream compression'
        Default = True
        TabOrder = 1
        OnClick = bbTestStreamCompressionClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FF7770777777
          77FFFF777807777777FFFF777700777777FFFF77770B077777FFFF770000B077
          77FFFF770BBBBB0777FFFF7770FB000077FFFF7770BBB07777FFFF00000BFB07
          77FFFF0FBBBFBBB077FFFF70FBFB000007FFFF70BFBFB07777FFFF770BFBFB07
          77FFFF770FFFFFF077FFFF7770FBFFBF07FFFF777000000000FF}
      end
      object eStreamBefore: TEdit
        Left = 228
        Top = 40
        Width = 165
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = eStreamBeforeChange
      end
      object eStreamCompress: TEdit
        Left = 228
        Top = 92
        Width = 165
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object eStreamAfter: TEdit
        Left = 228
        Top = 144
        Width = 165
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
      object cbLaunch: TCheckBox
        Left = 228
        Top = 180
        Width = 165
        Height = 17
        Caption = 'Launch after decompression?'
        TabOrder = 5
      end
    end
    object TabSheet4: TTabSheet
      Caption = '&Buffer compression'
      ImageIndex = 3
      object Memo4: TMemo
        Left = 8
        Top = 20
        Width = 201
        Height = 254
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          'Sorry, I have implemented this '
          'functionality yet.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object OD: TOpenDialog
    Filter = 
      'Program files|*.exe|Image files|*.gif|Word documents|*.doc|Excel' +
      ' documents|*.xls|All files|*.*'
    Left = 396
    Top = 68
  end
end

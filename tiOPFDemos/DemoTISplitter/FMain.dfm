object FormMain: TFormMain
  Left = 189
  Top = 135
  Width = 560
  Height = 443
  Caption = '  TISplitterPanel demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object tiSplitterPanel1: TtiSplitterPanel
    Left = 8
    Top = 8
    Width = 539
    Height = 384
    Aligned = alNone
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColorGrabBar = 16686723
    ColorPanel = clBtnFace
    PanelStyle = spsFramed
    SplitterOrientation = spoVertical
    KeepSplitterPosPercent = True
    SplitterPos = 269
    SplitterPosPercent = 50
    Panel1Controls = (
      Panel1)
    Panel2Controls = (
      tiSplitterPanel2)
    object Panel1: TPanel
      Left = 8
      Top = 8
      Width = 253
      Height = 368
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      Color = 13828095
      TabOrder = 0
      object Label1: TLabel
        Left = 12
        Top = 12
        Width = 190
        Height = 13
        Caption = 'This is pane 1 of tiSplitterPanel 1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbKeepSplitterPosPercent1: TCheckBox
        Left = 12
        Top = 32
        Width = 125
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Keep splitter percent?'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = cbKeepSplitterPosPercent1Click
      end
      object rgSplitterOrientation1: TRadioGroup
        Left = 12
        Top = 56
        Width = 222
        Height = 65
        Caption = ' Splitter orientation '
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Vertical'
          'Horizontal')
        TabOrder = 1
        OnClick = rgSplitterOrientation1Click
      end
      object rgPanelStyle1: TRadioGroup
        Left = 12
        Top = 128
        Width = 223
        Height = 105
        Caption = ' Panel style '
        Columns = 2
        ItemIndex = 4
        Items.Strings = (
          'None'
          'User'
          'Lowered'
          'Raised'
          'Framed'
          'Shadow'
          'Bump')
        TabOrder = 2
        OnClick = rgPanelStyle1Click
      end
      object Memo1: TMemo
        Left = 12
        Top = 240
        Width = 233
        Height = 124
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        Lines.Strings = (
          '* Adjust the splitter position with the mouse at '
          'design time.'
          ''
          '* Double click the TtiSplitterPanel at design '
          'time for a property editor.'
          ''
          '* Use the PanelStyle property for easy access '
          'to panel'#39's border style.')
        ParentColor = True
        ReadOnly = True
        TabOrder = 3
      end
    end
    object tiSplitterPanel2: TtiSplitterPanel
      Left = 2
      Top = 2
      Width = 258
      Height = 380
      Aligned = alNone
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColorGrabBar = 16686723
      ColorPanel = clBtnFace
      PanelStyle = spsFramed
      SplitterOrientation = spoHorizontal
      KeepSplitterPosPercent = True
      SplitterPos = 260
      SplitterPosPercent = 69
      Panel1Controls = (
        Panel2)
      Panel2Controls = (
        Panel3)
      object Panel2: TPanel
        Left = 8
        Top = 8
        Width = 242
        Height = 243
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        Color = 16777170
        TabOrder = 0
        object Label2: TLabel
          Left = 12
          Top = 8
          Width = 190
          Height = 13
          Caption = 'This is pane 1 of tiSplitterPanel 2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object rgSplitterOrientation2: TRadioGroup
          Left = 12
          Top = 54
          Width = 220
          Height = 65
          Caption = ' Splitter orientation '
          Columns = 2
          ItemIndex = 1
          Items.Strings = (
            'Vertical'
            'Horizontal')
          TabOrder = 0
          OnClick = rgSplitterOrientation2Click
        end
        object cbKeepSplitterPosPercent2: TCheckBox
          Left = 12
          Top = 32
          Width = 125
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Keep splitter percent?'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbKeepSplitterPosPercent2Click
        end
        object rgPanelStyle2: TRadioGroup
          Left = 12
          Top = 128
          Width = 220
          Height = 101
          Caption = ' Panel style '
          Columns = 2
          ItemIndex = 4
          Items.Strings = (
            'None'
            'User'
            'Lowered'
            'Raised'
            'Framed'
            'Shadow'
            'Bump')
          TabOrder = 2
          OnClick = rgPanelStyle2Click
        end
      end
      object Panel3: TPanel
        Left = 8
        Top = 7
        Width = 243
        Height = 99
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        Color = 13028863
        TabOrder = 0
        object Label3: TLabel
          Left = 8
          Top = 8
          Width = 186
          Height = 13
          Caption = 'This is pane 2 of tiSplitterPanel2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Memo2: TMemo
          Left = 8
          Top = 26
          Width = 227
          Height = 67
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          Lines.Strings = (
            '(c) TechInsite Pty. Ltd.'
            'http://www.techiniste.com.au'
            'mailto:opensource@techinsite.com.au')
          ParentColor = True
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 394
    Width = 552
    Height = 22
    Panels = <>
    SimplePanel = False
  end
end

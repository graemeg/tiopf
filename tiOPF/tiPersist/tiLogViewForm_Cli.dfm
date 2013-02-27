object LogViewForm: TLogViewForm
  Left = 50
  Top = 224
  Width = 806
  Height = 220
  Caption = 'Application event log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MemoLog: TMemo
    Left = 0
    Top = 29
    Width = 798
    Height = 164
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenu
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 798
    Height = 29
    AutoSize = True
    Caption = 'ToolBar'
    Flat = True
    ShowCaptions = True
    TabOrder = 1
  end
  object PopupMenu: TPopupMenu
    Left = 396
    Top = 44
    object Viewlogfile1: TMenuItem
      Caption = '&View log file'
      OnClick = Viewlogfile1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ClearMenuItem: TMenuItem
      Caption = '&Clear'
      ShortCut = 16460
      OnClick = ClearMenuItemClick
    end
    object WordWrapMenuItem: TMenuItem
      Caption = '&Word wrap'
      ShortCut = 16471
      OnClick = WordWrapMenuItemClick
    end
    object LogMenuItem: TMenuItem
      Caption = '&Log'
      OnClick = LogMenuItemClick
    end
  end
end

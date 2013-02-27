object FormMain: TFormMain
  Left = 461
  Top = 209
  Caption = ' TechInsite SQL Editor'
  ClientHeight = 223
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SB: TStatusBar
    Left = 0
    Top = 199
    Width = 326
    Height = 24
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end>
  end
  object TB: TToolBar
    Left = 0
    Top = 0
    Width = 326
    Height = 25
    Caption = 'TB'
    Images = ImageList1
    TabOrder = 1
    ExplicitTop = 4
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = aFileSave
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Action = aFileSaveAs
    end
    object ToolButton3: TToolButton
      Left = 46
      Top = 0
      Action = aFileOpen
    end
    object ToolButton4: TToolButton
      Left = 69
      Top = 0
      Action = aNew
    end
    object ToolButton5: TToolButton
      Left = 92
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton6: TToolButton
      Left = 100
      Top = 0
      Action = aRun
    end
    object ToolButton7: TToolButton
      Left = 123
      Top = 0
      Action = aRunScript
    end
    object ToolButton8: TToolButton
      Left = 146
      Top = 0
      Width = 8
      Caption = 'ToolButton8'
      ImageIndex = 6
      Style = tbsSeparator
    end
    object ToolButton10: TToolButton
      Left = 154
      Top = 0
      Action = aDatabaseConnectionDetails
    end
    object ToolButton11: TToolButton
      Left = 177
      Top = 0
      Action = aClose
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 36
    Top = 44
    object File1: TMenuItem
      Caption = '&File'
      object Fileopen1: TMenuItem
        Action = aFileOpen
      end
      object Filesave1: TMenuItem
        Action = aFileSave
      end
      object Saveas1: TMenuItem
        Action = aFileSaveAs
      end
      object New1: TMenuItem
        Action = aNew
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = aClose
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
    end
    object Query1: TMenuItem
      Caption = '&Query'
      object Run1: TMenuItem
        Action = aRun
      end
      object Runasscript1: TMenuItem
        Action = aRunScript
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Viewdatabaseconnectiondetails1: TMenuItem
        Action = aDatabaseConnectionDetails
      end
    end
  end
  object ActionList1: TActionList
    Images = ImageList1
    OnUpdate = ActionList1OnUpdate
    Left = 72
    Top = 44
    object aFileOpen: TAction
      Caption = 'File &open'
      ImageIndex = 0
      ShortCut = 16463
      OnExecute = aFileOpenExecute
    end
    object aFileSave: TAction
      Caption = 'File &save'
      Enabled = False
      ImageIndex = 1
      ShortCut = 16467
      OnExecute = aFileSaveExecute
    end
    object aFileSaveAs: TAction
      Caption = 'Save &as'
      OnExecute = aFileSaveAsExecute
    end
    object aNew: TAction
      Caption = '&New'
      ImageIndex = 2
      ShortCut = 16462
      OnExecute = aNewExecute
    end
    object aRun: TAction
      Caption = 'Run'
      Enabled = False
      ImageIndex = 3
      ShortCut = 119
      OnExecute = aRunExecute
    end
    object aRunScript: TAction
      Caption = 'Run as &script'
      ShortCut = 8311
      OnExecute = aRunScriptExecute
    end
    object aClose: TAction
      Caption = '&Close'
      ImageIndex = 4
      ShortCut = 16499
      OnExecute = aCloseExecute
    end
    object aDatabaseConnectionDetails: TAction
      Caption = 'View database connection details'
      OnExecute = aDatabaseConnectionDetailsExecute
    end
  end
  object ImageList1: TImageList
    Left = 104
    Top = 44
  end
  object pmRun: TPopupMenu
    Left = 140
    Top = 44
    object Runasscript2: TMenuItem
      Action = aRunScript
    end
  end
end

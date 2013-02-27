object FormSQLMgrBrowse: TFormSQLMgrBrowse
  Left = 428
  Top = 262
  Caption = 'FormSQLMgrBrowse'
  ClientHeight = 241
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object sb: TStatusBar
    Left = 0
    Top = 217
    Width = 473
    Height = 24
    Panels = <
      item
        Width = 120
      end
      item
        Width = 200
      end
      item
        Width = 200
      end
      item
        Width = 120
      end>
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 473
    Height = 26
    Caption = 'ToolBar1'
    Images = ImageList1
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = aStructure
    end
    object ToolButton4: TToolButton
      Left = 23
      Top = 0
      Action = aExport
    end
    object ToolButton5: TToolButton
      Left = 46
      Top = 0
      Action = aShowRecord
    end
    object ToolButton2: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 77
      Top = 0
      Action = aClose
    end
  end
  object LV: TListView
    Left = 23
    Top = 32
    Width = 250
    Height = 150
    Columns = <>
    PopupMenu = PopupMenu1
    TabOrder = 2
    ViewStyle = vsReport
  end
  object PopupMenu1: TPopupMenu
    Images = ImageList1
    Left = 64
    Top = 56
    object Structure1: TMenuItem
      Action = aStructure
    end
    object ExporttoCSVfile1: TMenuItem
      Action = aExport
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Close1: TMenuItem
      Action = aClose
    end
  end
  object ImageList1: TImageList
    Left = 96
    Top = 6
  end
  object ActionList1: TActionList
    Left = 128
    Top = 8
    object aStructure: TAction
      Caption = 'Structure'
      ImageIndex = 0
      ShortCut = 16467
      OnExecute = aStructureExecute
    end
    object aClose: TAction
      Caption = 'Close'
      ImageIndex = 1
      ShortCut = 27
      OnExecute = aCloseExecute
    end
    object aExport: TAction
      Caption = '&Export to CSV file'
      ImageIndex = 2
      OnExecute = aExportExecute
    end
    object aSQLCreate: TAction
      Caption = '&Create'
      OnExecute = aSQLCreateExecute
    end
    object aSQLUpdate: TAction
      Caption = '&Update'
      OnExecute = aSQLUpdateExecute
    end
    object aSQLDelete: TAction
      Caption = '&Delete'
      OnExecute = aSQLDeleteExecute
    end
    object aSQLRead: TAction
      Caption = '&Read'
      OnExecute = aSQLReadExecute
    end
    object aSQLAsMapRowToObject: TAction
      Caption = '&MapRowToObject'
      OnExecute = aSQLAsMapRowToObjectExecute
    end
    object aSQLAsSetupParams: TAction
      Caption = '&SetupParams'
      OnExecute = aSQLAsSetupParamsExecute
    end
    object aSQLAsClassInterface: TAction
      Caption = 'As &class interface'
      OnExecute = aSQLAsClassInterfaceExecute
    end
    object aShowRecord: TAction
      Caption = 'Show this record'
      Hint = 'Show this record in a popup window'
      ImageIndex = 3
      OnExecute = aShowRecordExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 160
    Top = 6
    object File1: TMenuItem
      Caption = '&File'
      object Close2: TMenuItem
        Action = aClose
      end
    end
    object SQL1: TMenuItem
      Caption = '&SQL'
      object aSQLCreate1: TMenuItem
        Action = aSQLRead
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object aSQLCreate2: TMenuItem
        Action = aSQLCreate
      end
      object aSQLUpdate1: TMenuItem
        Action = aSQLUpdate
      end
      object aSQLDelete1: TMenuItem
        Action = aSQLDelete
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Asclassinterface1: TMenuItem
        Action = aSQLAsClassInterface
      end
      object SQLAsSetupParams1: TMenuItem
        Action = aSQLAsSetupParams
      end
      object aSQLAsMapRowToObject1: TMenuItem
        Action = aSQLAsMapRowToObject
      end
    end
  end
end

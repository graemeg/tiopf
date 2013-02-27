object FormSQLEditor: TFormSQLEditor
  Left = 439
  Top = 165
  BorderStyle = bsNone
  Caption = 'FormSQLEditor'
  ClientHeight = 317
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pmSQL: TPopupMenu
    Left = 36
    Top = 244
    object mnuRunQuery: TMenuItem
      Action = aRunQuery
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object FindinSQL1: TMenuItem
      Action = aFindInSQL
    end
    object FindinSQLagain1: TMenuItem
      Action = aFindInSQLAgain
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Copytoclipboard1: TMenuItem
      Action = aCopyToClip
    end
    object mnuCopytoclipboard: TMenuItem
      Action = aCopyToClipAsQuoted
    end
  end
  object alMain: TActionList
    OnUpdate = alMainUpdate
    Left = 108
    Top = 244
    object aCopyToClipAsQuoted: TAction
      Caption = 'Copy query to &clipboard as quoted string'
      OnExecute = aCopyToClipAsQuotedExecute
    end
    object aRunQuery: TAction
      Caption = '&Run queryy'
      Hint = 'Run query'
      ShortCut = 119
      OnExecute = aRunQueryExecute
    end
    object aDefaultParams: TAction
      Caption = 'aDefaultParams'
      Hint = 'Default params'
    end
    object aFindInSQL: TAction
      Caption = 'F&ind in SQL'
      ShortCut = 114
      OnExecute = aFindInSQLExecute
    end
    object aFindInSQLAgain: TAction
      Caption = 'Fi&nd in SQL again'
      ShortCut = 8306
      OnExecute = aFindInSQLAgainExecute
      OnUpdate = aFindInSQLAgainUpdate
    end
    object aCopyToClip: TAction
      Caption = 'Copy to clipboard'
      OnExecute = aCopyToClipExecute
    end
  end
end

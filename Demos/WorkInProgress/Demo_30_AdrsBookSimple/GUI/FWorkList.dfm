inherited FormWorkList: TFormWorkList
  Caption = 'FormWorkList'
  ClientHeight = 326
  ClientWidth = 526
  Color = clWhite
  Font.Color = clNavy
  Font.Height = -16
  PixelsPerInch = 96
  TextHeight = 20
  object imgTop: TImage [0]
    Left = 0
    Top = 0
    Width = 526
    Height = 50
    Align = alTop
  end
  object imgBottom: TImage [1]
    Left = 0
    Top = 277
    Width = 526
    Height = 49
    Align = alBottom
  end
  object imgRight: TImage [2]
    Left = 413
    Top = 50
    Width = 113
    Height = 227
    Align = alRight
  end
  inherited pnlCaption: TPanel
    Top = 50
    Width = 526
  end
  object tiHyperlinkWithImage1: TtiHyperlinkWithImage
    Left = 8
    Top = 82
    Width = 209
    Height = 33
    Cursor = crHandPoint
    Hint = 
      'View a list of names. Select a name to edit. Enter a new name or' +
      ' delete a name.'
    OnHint = tiHyperlinkWithImage1Hint
    Margin = 16
  end
  object memoHint: TMemo
    Left = 240
    Top = 75
    Width = 167
    Height = 185
    TabStop = False
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
end

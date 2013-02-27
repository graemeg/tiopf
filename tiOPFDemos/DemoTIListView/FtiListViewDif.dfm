object FormTIListViewDif: TFormTIListViewDif
  Left = 295
  Top = 187
  Width = 516
  Height = 460
  Caption = ' TtiListViewDif demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblLHS: TLabel
    Left = 8
    Top = 8
    Width = 21
    Height = 13
    Caption = 'LHS'
  end
  object lblRHS: TLabel
    Left = 268
    Top = 8
    Width = 23
    Height = 13
    Caption = 'RHS'
  end
  object LVD: TtiListViewDif
    Left = 8
    Top = 32
    Width = 493
    Height = 393
    OnMoveSplitter = LVDMoveSplitter
    SplitterPos = 222
  end
end

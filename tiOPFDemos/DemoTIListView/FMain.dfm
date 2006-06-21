object FormMain: TFormMain
  Left = 399
  Top = 161
  Width = 239
  Height = 367
  Caption = ' tiListView Demo'
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
  object Bevel1: TBevel
    Left = 12
    Top = 284
    Width = 201
    Height = 13
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 198
    Height = 13
    Caption = 'How many values do you want to display?'
  end
  object Bevel2: TBevel
    Left = 16
    Top = 64
    Width = 201
    Height = 13
    Shape = bsTopLine
  end
  object seNoOfItems: TSpinEdit
    Left = 80
    Top = 32
    Width = 69
    Height = 22
    Increment = 100
    MaxValue = 100000
    MinValue = 1
    TabOrder = 3
    Value = 1000
  end
  object btnListView: TButton
    Left = 28
    Top = 84
    Width = 173
    Height = 29
    Caption = 'tiList&View Demo'
    TabOrder = 0
    OnClick = btnListViewClick
  end
  object btnListViewPlus: TButton
    Left = 28
    Top = 124
    Width = 173
    Height = 29
    Caption = 'tiListView&Plus Demo'
    TabOrder = 1
    OnClick = btnListViewPlusClick
  end
  object btnClose: TButton
    Left = 24
    Top = 300
    Width = 173
    Height = 29
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object btnListViewCtrls: TButton
    Left = 28
    Top = 164
    Width = 173
    Height = 29
    Caption = 'tiListView&Ctrls Demo'
    TabOrder = 4
    OnClick = btnListViewCtrlsClick
  end
  object btnListViewMultiSelect: TButton
    Left = 28
    Top = 204
    Width = 173
    Height = 29
    Caption = 'tiListViewMultiSelect Demo'
    TabOrder = 5
    OnClick = btnListViewMultiSelectClick
  end
  object btnListViewDIf: TButton
    Left = 28
    Top = 244
    Width = 173
    Height = 29
    Caption = 'tiListViewDif Demo'
    TabOrder = 6
    OnClick = btnListViewDIfClick
  end
end

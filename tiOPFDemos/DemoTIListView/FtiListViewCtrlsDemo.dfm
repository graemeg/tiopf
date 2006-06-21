object FormListViewCtrlsDemo: TFormListViewCtrlsDemo
  Left = 384
  Top = 231
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' TtiListViewCtrls demo'
  ClientHeight = 420
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 136
    Height = 26
    Caption = 'TtiListViewListBox used with radio buttons'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 176
    Top = 8
    Width = 136
    Height = 26
    Caption = 'TtiListViewListBox used with check boxes'
    WordWrap = True
  end
  object LVListBoxRB: TtiListViewListBox
    Left = 16
    Top = 40
    Width = 149
    Height = 209
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    OnGetChecked = LVListBoxRBGetChecked
    OnCheck = LVListBoxRBCheck
  end
  object LVListBoxCB: TtiListViewListBox
    Left = 180
    Top = 40
    Width = 149
    Height = 209
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <>
    SortOrders = <>
    Style = lvlbStyleCheckBox
    OnGetChecked = LVListBoxCBGetChecked
    OnCheck = LVListBoxCBCheck
  end
  object memoSelected: TMemo
    Left = 16
    Top = 280
    Width = 313
    Height = 129
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  object cbRBReadOnly: TCheckBox
    Left = 20
    Top = 256
    Width = 97
    Height = 17
    Caption = 'Read only'
    TabOrder = 3
    OnClick = cbRBReadOnlyClick
  end
  object cbCBReadOnly: TCheckBox
    Left = 180
    Top = 256
    Width = 97
    Height = 17
    Caption = 'Read only'
    TabOrder = 4
    OnClick = cbCBReadOnlyClick
  end
end

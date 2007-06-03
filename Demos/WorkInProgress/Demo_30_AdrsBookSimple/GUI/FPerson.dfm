inherited FormPerson: TFormPerson
  Caption = 'FormPerson'
  ClientHeight = 325
  ClientWidth = 433
  ExplicitWidth = 439
  ExplicitHeight = 357
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlCaption: TPanel
    Width = 433
    ExplicitWidth = 433
  end
  object paeFirstName: TtiPerAwareEdit
    Left = 8
    Top = 8
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 1
    LabelLayout = tlTop
    Caption = '&First name'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeLastName: TtiPerAwareEdit
    Left = 8
    Top = 40
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 2
    LabelLayout = tlTop
    Caption = '&Last name'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeTitle: TtiPerAwareEdit
    Left = 8
    Top = 66
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 3
    LabelLayout = tlTop
    Caption = '&Title'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeInitials: TtiPerAwareEdit
    Left = 8
    Top = 96
    Width = 185
    Height = 24
    Constraints.MinHeight = 24
    TabOrder = 4
    LabelLayout = tlTop
    Caption = '&Initials'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeNotes: TtiPerAwareMemo
    Left = 8
    Top = 126
    Width = 185
    Height = 41
    Constraints.MinHeight = 24
    TabOrder = 5
    LabelLayout = tlTop
    Caption = '&Notes'
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clBlack
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    LabelParentFont = False
    ReadOnly = False
    ScrollBars = ssNone
    WordWrap = True
    MaxLength = 0
  end
  object LV: TtiVTListView
    Left = 8
    Top = 173
    Width = 297
    Height = 133
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsXPStyle
    ShowNodeHint = False
    SortOrders.GroupColumnCount = 0
    SortOrders = <>
    VisibleButtons = [tiLVBtnVisEdit, tiLVBtnVisNew, tiLVBtnVisDelete]
    VT.Left = 2
    VT.Top = 26
    VT.Width = 293
    VT.Height = 105
    VT.Align = alClient
    VT.Header.AutoSizeIndex = 0
    VT.Header.Font.Charset = DEFAULT_CHARSET
    VT.Header.Font.Color = clWindowText
    VT.Header.Font.Height = -11
    VT.Header.Font.Name = 'Tahoma'
    VT.Header.Font.Style = []
    VT.Header.MainColumn = -1
    VT.Header.Options = [hoColumnResize, hoDrag, hoVisible]
    VT.Header.Style = hsXPStyle
    VT.NodeDataSize = 4
    VT.TabOrder = 0
    VT.TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    VT.TreeOptions.SelectionOptions = [toFullRowSelect]
    VT.ExplicitTop = 2
    VT.ExplicitWidth = 181
    VT.ExplicitHeight = 37
    VT.Columns = <>
  end
end

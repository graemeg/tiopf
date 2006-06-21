object frmLookup: TfrmLookup
  Left = 348
  Top = 230
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsToolWindow
  Caption = 'Lookup'
  ClientHeight = 372
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbCriteria: TGroupBox
    Left = 0
    Top = 0
    Width = 296
    Height = 49
    Align = alTop
    Caption = ' Selection Criteria '
    TabOrder = 0
    object edCriteria: TEdit
      Left = 8
      Top = 16
      Width = 185
      Height = 21
      TabOrder = 0
    end
    object btnSearch: TButton
      Left = 208
      Top = 12
      Width = 75
      Height = 25
      Caption = '&Search'
      TabOrder = 1
      OnClick = btnSearchClick
    end
  end
  object gbResults: TGroupBox
    Left = 0
    Top = 52
    Width = 296
    Height = 285
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Search Results '
    TabOrder = 1
    object lvMain: TtiListViewPlus
      Left = 2
      Top = 15
      Width = 292
      Height = 268
      ShowFocusRect = True
      RuntimeGenCols = False
      Align = alClient
      MultiSelect = False
      OnDblClick = lvMainDblClick
      OnKeyPress = FormKeyPress
      ViewStyle = vsReport
      RowSelect = True
      ApplyFilter = False
      ApplySort = False
      ListColumns = <
        item
          DisplayLabel = 'Name'
          FieldName = 'Name'
          DataType = lvtkString
          Derived = False
          Alignment = taLeftJustify
        end
        item
          DisplayLabel = 'Post Code'
          FieldName = 'PostCode'
          DataType = lvtkString
          Derived = False
          Alignment = taLeftJustify
        end>
      SortOrders = <>
    end
  end
  object btnOK: TButton
    Left = 136
    Top = 344
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 220
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnCancelClick
  end
end

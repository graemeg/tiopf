object frmLVFilter: TfrmLVFilter
  Left = 474
  Top = 186
  BorderStyle = bsDialog
  Caption = 'Filter Options'
  ClientHeight = 414
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    376
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 363
    Width = 375
    Height = 10
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object lblCaption: TLabel
    Left = 0
    Top = 0
    Width = 376
    Height = 25
    Align = alTop
    AutoSize = False
    Caption = 'Show items that match the following conditions :-'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object lblProperties: TLabel
    Left = 80
    Top = 208
    Width = 39
    Height = 13
    Caption = '&Property'
    FocusControl = cmbProperties
  end
  object Label1: TLabel
    Left = 80
    Top = 244
    Width = 41
    Height = 13
    Caption = 'Ope&rator'
    FocusControl = cmbOperator
  end
  object Label2: TLabel
    Left = 80
    Top = 276
    Width = 27
    Height = 13
    Caption = '&Value'
    FocusControl = edValue
  end
  object Label3: TLabel
    Left = 80
    Top = 312
    Width = 35
    Height = 13
    Caption = 'And/Or'
    FocusControl = cmbConj
  end
  object btnOK: TButton
    Left = 213
    Top = 383
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 297
    Top = 383
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object lbCriteria: TListBox
    Left = 0
    Top = 28
    Width = 377
    Height = 131
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnClear: TButton
    Left = 8
    Top = 164
    Width = 75
    Height = 25
    Action = actClearAll
    TabOrder = 3
  end
  object btnDelete: TButton
    Left = 103
    Top = 164
    Width = 75
    Height = 25
    Action = actDelete
    TabOrder = 4
  end
  object btnNew: TButton
    Left = 197
    Top = 164
    Width = 75
    Height = 25
    Action = actNew
    TabOrder = 5
  end
  object btnEdit: TButton
    Left = 292
    Top = 164
    Width = 75
    Height = 25
    Action = actEdit
    TabOrder = 6
  end
  object cmbProperties: TComboBox
    Left = 152
    Top = 204
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 7
  end
  object cmbOperator: TComboBox
    Left = 152
    Top = 239
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 8
  end
  object edValue: TEdit
    Left = 152
    Top = 273
    Width = 145
    Height = 21
    TabOrder = 9
  end
  object cmbConj: TComboBox
    Left = 152
    Top = 308
    Width = 65
    Height = 21
    Style = csDropDownList
    TabOrder = 10
  end
  object btnAdd: TButton
    Left = 140
    Top = 340
    Width = 75
    Height = 25
    Action = actAdd
    TabOrder = 11
  end
  object alFilter: TActionList
    Top = 196
    object actClearAll: TAction
      Caption = 'C&lear All'
      Hint = 'Clears all currently set filters.'
      OnExecute = actClearAllExecute
      OnUpdate = actClearAllUpdate
    end
    object actDelete: TAction
      Caption = '&Delete'
      Hint = 'Deletes the selected filter item.'
      ShortCut = 46
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actNew: TAction
      Caption = '&New'
      Hint = 'Add a new filter item.'
      ShortCut = 45
      OnExecute = actNewExecute
    end
    object actEdit: TAction
      Caption = '&Edit'
      Hint = 'Edit the current filter line'
      OnExecute = actEditExecute
      OnUpdate = actDeleteUpdate
    end
    object actAdd: TAction
      Caption = '&Add'
      Hint = 'Add this filter to the list.'
      OnExecute = actAddExecute
      OnUpdate = actAddUpdate
    end
  end
end

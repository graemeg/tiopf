object SystemOptionsForm: TSystemOptionsForm
  Left = 514
  Top = 226
  BorderStyle = bsDialog
  Caption = 'System Options'
  ClientHeight = 355
  ClientWidth = 445
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
  object ButtonPanel: TPanel
    Left = 0
    Top = 323
    Width = 445
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object OKButton: TButton
      Left = 272
      Top = 3
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 360
      Top = 3
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object tiTreeView1: TtiTreeView
    Left = 4
    Top = 4
    Width = 433
    Height = 312
    BevelOuter = bvNone
    DataMappings = <
      item
        DataClass = 'TPersistent'
        DisplayPropName = 'Caption'
        Name = 'tiTVMappingPersistent'
      end>
    SplitterPos = 216
    HasChildForms = True
    TreeSortType = stNone
  end
end

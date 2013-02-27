object FormViewFiles: TFormViewFiles
  Left = 389
  Top = 183
  Width = 490
  Height = 322
  Caption = ' View files'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    482
    295)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSource: TLabel
    Left = 12
    Top = 4
    Width = 44
    Height = 13
    Caption = 'lblSource'
  end
  object lblTarget: TLabel
    Left = 12
    Top = 20
    Width = 41
    Height = 13
    Caption = 'lblTarget'
  end
  object pnlAdvanced: TPanel
    Left = 8
    Top = 40
    Width = 469
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      469
      249)
    object PC: TPageControl
      Left = 4
      Top = 6
      Width = 457
      Height = 239
      ActivePage = tsSource
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnChange = PCChange
      object tsSource: TTabSheet
        Caption = 'Source'
        DesignSize = (
          449
          211)
        object lv: TtiListViewPlus
          Left = 4
          Top = 4
          Width = 445
          Height = 207
          ShowFocusRect = True
          RuntimeGenCols = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          MultiSelect = False
          ViewStyle = vsReport
          RowSelect = True
          ApplyFilter = False
          ApplySort = False
          ListColumns = <
            item
              DisplayLabel = 'File name'
              FieldName = 'PathAndName'
              DataType = lvtkString
              Derived = False
              Alignment = taLeftJustify
            end
            item
              DisplayLabel = 'Date'
              FieldName = 'Date'
              DisplayMask = 'dd/mm/yyyy hh:mm:ss'
              DataType = lvtkDateTime
              Derived = False
              Alignment = taLeftJustify
            end
            item
              DisplayLabel = 'Size'
              FieldName = 'Size'
              DisplayMask = '#,##0'
              DataType = lvtkInt
              Derived = False
            end>
          SortOrders = <>
          ConfigHelpContext = 0
          DesignSize = (
            445
            207)
        end
      end
      object tsTarget: TTabSheet
        Caption = 'Target'
        ImageIndex = 1
      end
      object tsCopy: TTabSheet
        Caption = 'Copy'
        ImageIndex = 2
      end
      object tsUpdate: TTabSheet
        Caption = 'Update'
        ImageIndex = 3
      end
      object tsDelete: TTabSheet
        Caption = 'Delete'
        ImageIndex = 4
      end
    end
  end
end

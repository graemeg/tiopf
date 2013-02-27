object FormTIListViewMultiSelect: TFormTIListViewMultiSelect
  Left = 441
  Top = 167
  Width = 302
  Height = 282
  Caption = ' Test TtiPerAwareMultiSelect'
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
  object MS: TtiPerAwareMultiSelect
    Left = 4
    Top = 5
    Width = 289
    Height = 245
    Caption = ' tiMultiSelect test '
    Anchors = [akLeft, akTop, akRight, akBottom]
    Border = True
    CaptionAvailable = '&Available'
    CaptionSelected = '&Selected'
    ListColumns = <
      item
        DisplayLabel = 'Caption'
        FieldName = 'Caption'
        DataType = lvtkString
        Derived = False
      end>
    RunTimeSelectedCols = True
  end
end

object FormTIEmbeddedDataForm: TFormTIEmbeddedDataForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'FormTIEmbeddedDataForm'
  ClientHeight = 234
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 319
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblCaption: TLabel
      Left = 8
      Top = 6
      Width = 43
      Height = 13
      Caption = 'Caption'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object AL: TActionList
    OnUpdate = ALUpdate
    Left = 68
    Top = 80
  end
end

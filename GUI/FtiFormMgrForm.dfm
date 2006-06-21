object FormTIFormMgrForm: TFormTIFormMgrForm
  Left = 342
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'FormTIFormMgrForm'
  ClientHeight = 239
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 0
    Align = alTop
    BevelOuter = bvNone
    Color = clNavy
    TabOrder = 0
    object lblCaption: TLabel
      Left = 8
      Top = 8
      Width = 72
      Height = 18
      Caption = 'lblCaption'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindow
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end

object tiPopupDataForm: TtiPopupDataForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'tiPopupDataForm'
  ClientHeight = 270
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBorder: TtiRoundedPanel
    Left = 0
    Top = 0
    Width = 412
    Height = 270
    CornerRadius = 5
    BorderColor = clBlack
    BorderThickness = 1
    Align = alClient
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 6
      Top = 234
      Width = 400
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        400
        30)
      object btnOK: TtiSpeedButton
        Left = 237
        Top = 4
        Width = 75
        Height = 22
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        ImageRes = tiRINone
        ExplicitLeft = 447
      end
      object btnCancel: TtiSpeedButton
        Left = 318
        Top = 4
        Width = 75
        Height = 22
        Cursor = crHandPoint
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        ImageRes = tiRINone
        ExplicitLeft = 528
      end
    end
    object pnlMain: TPanel
      Left = 6
      Top = 6
      Width = 400
      Height = 228
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
end

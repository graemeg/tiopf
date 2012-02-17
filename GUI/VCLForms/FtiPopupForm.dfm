object FormTIPopupData: TFormTIPopupData
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'FormTIPopupData'
  ClientHeight = 260
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBorder: TtiRoundedPanel
    Left = 0
    Top = 0
    Width = 420
    Height = 260
    CornerRadius = 5
    BorderColor = clBlack
    BorderThickness = 1
    Align = alClient
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 6
      Top = 224
      Width = 408
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        408
        30)
      object btnOK: TBitBtn
        Left = 239
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ExplicitLeft = 231
      end
      object btnCancel: TBitBtn
        Left = 320
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Cancel'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ExplicitLeft = 312
      end
    end
    object pnlMain: TPanel
      Left = 6
      Top = 6
      Width = 408
      Height = 218
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 400
      ExplicitHeight = 228
    end
  end
end

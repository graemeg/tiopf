object FormTIDeployChild_Launch: TFormTIDeployChild_Launch
  Left = 331
  Top = 189
  Width = 532
  Height = 394
  Caption = 'FormTIDeployChild_Launch'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlForm: TPanel
    Left = 4
    Top = 8
    Width = 461
    Height = 309
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    DesignSize = (
      461
      309)
    object SRB: TScrollBox
      Left = 8
      Top = 8
      Width = 262
      Height = 297
      HorzScrollBar.Visible = False
      Anchors = [akLeft, akTop, akBottom]
      Color = clWhite
      ParentColor = False
      TabOrder = 0
    end
    object memoNotes: TMemo
      Left = 275
      Top = 45
      Width = 182
      Height = 230
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object tmrMouse: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrMouseTimer
    Left = 404
    Top = 20
  end
end

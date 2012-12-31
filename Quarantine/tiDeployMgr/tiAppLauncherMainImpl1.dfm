object tiAppLauncherMain: TtiAppLauncherMain
  Left = 403
  Top = 191
  Width = 494
  Height = 408
  AxBorderStyle = afbNone
  Caption = 'tiAppLauncherMain'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = ActiveFormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlReading: TPanel
    Left = 4
    Top = 8
    Width = 221
    Height = 185
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    Visible = False
    DesignSize = (
      221
      185)
    object lblText: TLabel
      Left = 12
      Top = 16
      Width = 197
      Height = 157
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 'Reading available programs...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object tmrLoad: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrLoadTimer
    Left = 228
    Top = 4
  end
end

object FormMain: TFormMain
  Left = 463
  Top = 339
  Width = 386
  Height = 224
  Caption = '  TechInsite File Sync'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    378
    190)
  PixelsPerInch = 96
  TextHeight = 13
  object sbSyncSourceToTarget: TSpeedButton
    Left = 92
    Top = 162
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '>>'
    OnClick = sbSyncSourceToTargetClick
  end
  object sbSyncTargetToSource: TSpeedButton
    Left = 184
    Top = 162
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '<<'
    OnClick = sbSyncTargetToSourceClick
  end
  object btnShowFiles: TButton
    Left = 8
    Top = 162
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Show &files...'
    TabOrder = 0
    OnClick = btnShowFilesClick
  end
  object bbClose: TBitBtn
    Left = 296
    Top = 161
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = bbCloseClick
    Kind = bkCancel
  end
  object PC: TPageControl
    Left = 8
    Top = 4
    Width = 365
    Height = 149
    ActivePage = tsDirToSync
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    OnChange = PCChange
    object tsDirToSync: TTabSheet
      Caption = 'Directories to synchronise'
    end
    object tsFilters: TTabSheet
      Caption = 'Filters'
      ImageIndex = 1
    end
  end
  object tmrRun: TTimer
    OnTimer = tmrRunTimer
    Left = 332
    Top = 36
  end
end

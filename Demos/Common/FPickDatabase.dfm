object FormPickDatabase: TFormPickDatabase
  Left = 371
  Top = 234
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Enter database connection details'
  ClientHeight = 196
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 4
    Top = 8
    Width = 716
    Height = 137
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Database connection details '
    TabOrder = 0
    object sbDefaultToPresetValues: TSpeedButton
      Left = 555
      Top = 20
      Width = 146
      Height = 22
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Caption = 'Default to preset values'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = sbDefaultToPresetValuesClick
    end
    object PersistenceLayerLabel: TLabel
      Left = 8
      Top = 20
      Width = 109
      Height = 13
      Caption = 'P&ersistence layer name'
    end
    object DatabaseNameLabel: TLabel
      Left = 8
      Top = 48
      Width = 75
      Height = 13
      Caption = '&Database name'
    end
    object UserNameLabel: TLabel
      Left = 8
      Top = 76
      Width = 51
      Height = 13
      Caption = '&User name'
    end
    object PasswordLabel: TLabel
      Left = 8
      Top = 104
      Width = 46
      Height = 13
      Caption = '&Password'
    end
    object PersistenceLayerEdit: TEdit
      Left = 136
      Top = 20
      Width = 404
      Height = 21
      TabOrder = 0
    end
    object DatabaseNameEdit: TEdit
      Left = 136
      Top = 48
      Width = 404
      Height = 21
      TabOrder = 1
    end
    object UserNameEdit: TEdit
      Left = 136
      Top = 76
      Width = 404
      Height = 21
      TabOrder = 2
    end
    object PasswordEdit: TEdit
      Left = 136
      Top = 104
      Width = 404
      Height = 21
      TabOrder = 3
    end
  end
  object PM: TPopupMenu
  end
  object AL: TActionList
    Left = 28
    object Action1: TAction
      Caption = 'Action1'
    end
  end
end

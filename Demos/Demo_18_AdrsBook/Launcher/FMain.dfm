object Form1: TForm1
  Left = 413
  Top = 264
  BorderIcons = [biSystemMenu]
  Caption = 'Address book deployment manager'
  ClientHeight = 311
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bvlBottom: TBevel
    Left = 8
    Top = 271
    Width = 275
    Height = 2
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object lblProgressMajor: TLabel
    Left = 8
    Top = 188
    Width = 224
    Height = 13
    Caption = 'Checking for the latest version of Address Book'
    Visible = False
  end
  object lblProgressMinor: TLabel
    Left = 8
    Top = 228
    Width = 77
    Height = 13
    Caption = 'lblProgressMinor'
    Visible = False
  end
  object memoLog: TtiMemoReadOnly
    Left = 8
    Top = 74
    Width = 272
    Height = 109
    TabStop = False
    Lines.Strings = (
      'Checking for the latest version of Address Book'
      '')
  end
  object pbMajor: TProgressBar
    Left = 8
    Top = 205
    Width = 272
    Height = 17
    Smooth = True
    TabOrder = 1
    Visible = False
  end
  object btnClose: TButton
    Left = 206
    Top = 279
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    Default = True
    TabOrder = 2
    Visible = False
    OnClick = btnCloseClick
  end
  object pbMinor: TProgressBar
    Left = 8
    Top = 245
    Width = 272
    Height = 17
    Smooth = True
    TabOrder = 3
    Visible = False
  end
  object Animate: TAnimate
    Left = 8
    Top = 8
    Width = 272
    Height = 60
    CommonAVI = aviCopyFiles
    StopFrame = 31
  end
  object btnCancel: TButton
    Left = 123
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object XPManifest1: TXPManifest
    Left = 8
    Top = 278
  end
end

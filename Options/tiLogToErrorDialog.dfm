object LogErrorForm: TLogErrorForm
  Left = 66
  Top = 225
  Width = 813
  Height = 546
  BorderIcons = [biSystemMenu]
  Caption = 'Application error log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    805
    512)
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 4
    Top = 204
    Width = 32
    Height = 32
  end
  object CopyButton: TSpeedButton
    Left = 8
    Top = 8
    Width = 24
    Height = 24
    Hint = 'Copy this error message to the clipboard'
    Flat = True
    ParentShowHint = False
    ShowHint = True
    OnClick = CopyButtonClick
  end
  object MemoLog: TMemo
    Left = 48
    Top = 4
    Width = 751
    Height = 469
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object OKButton: TButton
    Left = 348
    Top = 480
    Width = 113
    Height = 25
    Cancel = True
    Caption = 'Try to &continue'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKButtonClick
  end
end

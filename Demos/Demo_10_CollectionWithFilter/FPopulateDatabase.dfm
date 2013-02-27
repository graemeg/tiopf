object FormPopulateDatabase: TFormPopulateDatabase
  Left = 404
  Top = 276
  Caption = 'Populate database'
  ClientHeight = 132
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tiMemoReadOnly1: TtiMemoReadOnly
    Left = 8
    Top = 8
    Width = 185
    Height = 89
    TabStop = False
    Lines.Strings = (
      'This demo requires the database to be '
      'populated with a large number of client '
      'records. How many client records '
      '(made from random strings) would you '
      'like?')
  end
  object paeCount: TtiPerAwareFloatEdit
    Left = 208
    Top = 40
    Width = 185
    Height = 23
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = 'Count of client records'
    LabelWidth = 110
    ReadOnly = False
    ValueAsString = '0'
    Precision = 0
    UnknownValue = -1.000000000000000000
    IsKnown = True
    Style = fesInteger
  end
  object tiButtonPanel1: TtiButtonPanel
    Left = 0
    Top = 101
    Width = 404
    Height = 31
    OnBtn1Click = tiButtonPanel1Btn1Click
    OnBtn2Click = tiButtonPanel1Btn2Click
    Btn1Enabled = True
    Btn2Enabled = True
  end
end

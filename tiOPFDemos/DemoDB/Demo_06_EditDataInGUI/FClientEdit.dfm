inherited FormClientEdit: TFormClientEdit
  Left = 128
  Top = 127
  Caption = 'FormClientEdit'
  ClientHeight = 208
  ClientWidth = 371
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 211
    Top = 178
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Left = 291
    Top = 178
    TabOrder = 5
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 182
    TabOrder = 3
  end
  object paeOID: TtiPerAwareEdit [3]
    Left = 10
    Top = 12
    Width = 357
    Height = 23
    ShowFocusRect = False
    Constraints.MinHeight = 23
    TabOrder = 0
    Caption = 'OID'
    ReadOnly = True
    MaxLength = 36
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientName: TtiPerAwareEdit [4]
    Left = 10
    Top = 40
    Width = 357
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 1
    Caption = 'Client name'
    ReadOnly = False
    MaxLength = 200
    CharCase = ecNormal
    PasswordChar = #0
  end
  object paeClientID: TtiPerAwareEdit [5]
    Left = 10
    Top = 68
    Width = 185
    Height = 23
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 2
    Caption = 'Client ID'
    ReadOnly = False
    MaxLength = 9
    CharCase = ecNormal
    PasswordChar = #0
  end
  object memoErrors: TtiMemoReadOnly [6]
    Left = 8
    Top = 100
    Width = 357
    Height = 69
    TabStop = False
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  inherited RO: TtiReadOnly
    Left = 172
    Top = 172
  end
end

inherited FormClientEdit: TFormClientEdit
  Left = 190
  Top = 45
  Caption = ' Edit a client, and it'#39's owned address'
  ClientHeight = 327
  ClientWidth = 393
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 233
    Top = 297
    TabOrder = 3
  end
  inherited btnCancel: TBitBtn
    Left = 313
    Top = 297
    TabOrder = 4
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 301
    TabOrder = 2
  end
  object gbClient: TGroupBox
    Left = 8
    Top = 4
    Width = 377
    Height = 69
    Caption = ' Client '
    TabOrder = 0
    object paeOID: TtiPerAwareEdit
      Left = 8
      Top = 18
      Width = 357
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 0
      LabelLayout = tlTop
      Caption = 'OID'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = True
      MaxLength = 36
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeClientName: TtiPerAwareEdit
      Left = 8
      Top = 42
      Width = 357
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 1
      LabelLayout = tlTop
      Caption = 'Client name'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      MaxLength = 200
      CharCase = ecUpperCase
      PasswordChar = #0
    end
  end
  object gbAddress: TGroupBox
    Left = 8
    Top = 80
    Width = 377
    Height = 129
    Caption = ' Address '
    TabOrder = 1
    object paeAdrsText: TtiPerAwareMemo
      Left = 10
      Top = 20
      Width = 353
      Height = 41
      Constraints.MinHeight = 23
      TabOrder = 0
      LabelLayout = tlTop
      Caption = 'Address'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      ScrollBars = ssNone
      WordWrap = True
      MaxLength = 240
      OnKeyPress = paeAdrsTextKeyPress
    end
    object paeLocality: TtiPerAwareEdit
      Left = 10
      Top = 68
      Width = 235
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 1
      LabelLayout = tlTop
      Caption = 'Locality'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      MaxLength = 46
      CharCase = ecUpperCase
      PasswordChar = #0
    end
    object paeState: TtiPerAwareEdit
      Left = 10
      Top = 96
      Width = 131
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 2
      LabelLayout = tlTop
      Caption = '&State'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      MaxLength = 3
      CharCase = ecUpperCase
      PasswordChar = #0
    end
    object paePostCode: TtiPerAwareEdit
      Left = 156
      Top = 96
      Width = 125
      Height = 23
      Constraints.MinHeight = 23
      TabOrder = 3
      LabelLayout = tlTop
      Caption = 'Post code'
      LabelFont.Charset = DEFAULT_CHARSET
      LabelFont.Color = clBlack
      LabelFont.Height = -11
      LabelFont.Name = 'MS Sans Serif'
      LabelFont.Style = []
      LabelParentFont = False
      ReadOnly = False
      MaxLength = 4
      CharCase = ecUpperCase
      PasswordChar = #0
    end
  end
  object memoErrors: TtiMemoReadOnly
    Left = 12
    Top = 217
    Width = 373
    Height = 69
    TabStop = False
    Lines.Strings = (
      'memoErrors')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
end

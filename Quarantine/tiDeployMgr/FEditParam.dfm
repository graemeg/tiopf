inherited FormEditParams: TFormEditParams
  Left = 404
  Top = 347
  Caption = ' Edt command line parameters'
  ClientHeight = 270
  ClientWidth = 349
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 189
    Top = 240
  end
  inherited btnCancel: TBitBtn
    Left = 269
    Top = 240
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 244
  end
  object paeParams: TtiPerAwareMemo [3]
    Left = 8
    Top = 8
    Width = 335
    Height = 54
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight]
    Constraints.MinHeight = 23
    TabOrder = 3
    LabelStyle = lsTopLeft
    Caption = '&Parameters'
    ReadOnly = False
    ScrollBars = ssNone
    WordWrap = True
    MaxLength = 256
  end
  object paeDescription: TtiPerAwareMemo [4]
    Left = 8
    Top = 124
    Width = 335
    Height = 106
    ShowFocusRect = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 23
    TabOrder = 4
    LabelStyle = lsTopLeft
    Caption = '&Description'
    ReadOnly = False
    ScrollBars = ssNone
    WordWrap = True
    MaxLength = 0
  end
  object paeDisplayText: TtiPerAwareEdit [5]
    Left = 4
    Top = 72
    Width = 185
    Height = 37
    ShowFocusRect = True
    Constraints.MinHeight = 23
    TabOrder = 5
    LabelStyle = lsTopLeft
    Caption = '&Display text'
    ReadOnly = False
    MaxLength = 0
    CharCase = ecNormal
    PasswordChar = #0
  end
end

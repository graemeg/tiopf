inherited FormFileSyncSetupEdit: TFormFileSyncSetupEdit
  Left = 338
  Top = 323
  Caption = ' Edit file synchronisation setup'
  ClientHeight = 184
  ClientWidth = 377
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 217
    Top = 154
  end
  inherited btnCancel: TBitBtn
    Left = 297
    Top = 154
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 158
  end
  object GroupBox1: TGroupBox [3]
    Left = 4
    Top = 4
    Width = 369
    Height = 69
    Caption = ' Source '
    TabOrder = 3
    object paeLocalDir: TtiPerAwareEdit
      Left = 12
      Top = 12
      Width = 349
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Location'
      LabelWidth = 60
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeSourceReader: TtiPerAwareComboBoxStatic
      Left = 12
      Top = 40
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Reader'
      LabelWidth = 60
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
    end
  end
  object gbTarget: TGroupBox [4]
    Left = 4
    Top = 76
    Width = 369
    Height = 69
    Caption = ' Target '
    TabOrder = 4
    object paeTargetLocation: TtiPerAwareEdit
      Left = 12
      Top = 12
      Width = 349
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Locatoin'
      LabelWidth = 60
      ReadOnly = False
      MaxLength = 0
      CharCase = ecNormal
      PasswordChar = #0
    end
    object paeTargetReader: TtiPerAwareComboBoxStatic
      Left = 12
      Top = 40
      Width = 185
      Height = 23
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = 'Reader'
      LabelWidth = 60
      ReadOnly = False
      DropDownCount = 8
      CharCase = ecNormal
    end
  end
  inherited RO: TtiReadOnly
    Left = 28
    Top = 244
  end
end

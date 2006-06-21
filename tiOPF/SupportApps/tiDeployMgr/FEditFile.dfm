inherited FormEditFile: TFormEditFile
  Left = 350
  Top = 305
  Caption = ' Edit a file to deploy'
  ClientHeight = 198
  ClientWidth = 419
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOK: TBitBtn
    Left = 259
    Top = 168
  end
  inherited btnCancel: TBitBtn
    Left = 339
    Top = 168
  end
  inherited cbEnterAsTab: TCheckBox
    Top = 172
  end
  object GroupBox1: TGroupBox [3]
    Left = 4
    Top = 4
    Width = 411
    Height = 53
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Deploy from '
    TabOrder = 3
    DesignSize = (
      411
      53)
    object paeFileName: TtiPerAwarePickFile
      Left = 8
      Top = 20
      Width = 395
      Height = 21
      ShowFocusRect = True
      Anchors = [akLeft, akTop, akRight]
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'File &name'
      LabelWidth = 100
      ReadOnly = False
      OnChange = paeFileNameChange
      Filter = 
        'Program files|*.exe|Delphi packages|*.bpl|Database connection de' +
        'tails|*.dcd|Images|*.gif|All files|*.*'
      FilterIndex = 0
    end
  end
  object GroupBox2: TGroupBox [4]
    Left = 4
    Top = 64
    Width = 411
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Deploy to '
    TabOrder = 4
    DesignSize = (
      411
      97)
    object Label1: TLabel
      Left = 8
      Top = 52
      Width = 236
      Height = 13
      Caption = 'Type $WINSYS for the Windows System directory'
    end
    object paeDeployTo: TtiPerAwarePickDirectory
      Left = 8
      Top = 20
      Width = 395
      Height = 21
      ShowFocusRect = True
      Anchors = [akLeft, akTop, akRight]
      Constraints.MinHeight = 23
      TabOrder = 0
      Caption = 'Deploy to &directory'
      LabelWidth = 100
      ReadOnly = False
    end
    object paeLaunch: TtiPerAwareCheckBox
      Left = 8
      Top = 72
      Width = 185
      Height = 17
      ShowFocusRect = True
      Constraints.MinHeight = 23
      TabOrder = 1
      Caption = '&Launch?'
      LabelWidth = 100
      ReadOnly = False
      Value = False
    end
  end
end

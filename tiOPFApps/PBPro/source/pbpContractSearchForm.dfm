object ContractSearchForm: TContractSearchForm
  Left = 278
  Top = 219
  BorderStyle = bsNone
  Caption = 'ContractSearchForm'
  ClientHeight = 513
  ClientWidth = 203
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SearchCriteriaPanel: TPanel
    Left = 0
    Top = 0
    Width = 203
    Height = 513
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 19
      Width = 203
      Height = 30
      Align = alTop
      Shape = bsBottomLine
    end
    object Label1: TLabel
      Left = 30
      Top = 26
      Width = 118
      Height = 13
      Caption = 'Search for Contracts'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 117
      Width = 83
      Height = 13
      Caption = 'By contract state:'
    end
    object Label4: TLabel
      Left = 8
      Top = 64
      Width = 120
      Height = 13
      Caption = 'Search for clients named:'
    end
    object SearchNowButton: TSpeedButton
      Left = 8
      Top = 256
      Width = 89
      Height = 25
      Action = SearchNowAction
      Transparent = False
    end
    object SearchHeaderPanel: TPanel
      Left = 0
      Top = 0
      Width = 203
      Height = 19
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 4
      Caption = 'Search'
      TabOrder = 2
      object CloseButton: TSpeedButton
        Left = 184
        Top = 0
        Width = 18
        Height = 19
        Action = CloseAction
        Anchors = [akTop, akRight]
        Flat = True
        Glyph.Data = {
          BE000000424DBE0000000000000076000000280000000A000000090000000100
          0400000000004800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDD00
          0000D00DDDD00D000000DD00DD00DD000000DDD0000DDD000000DDDD00DDDD00
          0000DDD0000DDD000000DD00DD00DD000000D00DDDD00D000000DDDDDDDDDD00
          0000}
      end
    end
    object SearchAnimate: TAnimate
      Left = 8
      Top = 24
      Width = 16
      Height = 16
      Active = False
      CommonAVI = aviFindFile
      StopFrame = 8
    end
    object ContractStatesPanel: TPanel
      Left = 8
      Top = 133
      Width = 166
      Height = 108
      BevelInner = bvRaised
      BevelOuter = bvLowered
      ParentColor = True
      TabOrder = 1
      object RedeemedCheckBox: TCheckBox
        Left = 11
        Top = 28
        Width = 97
        Height = 17
        Caption = 'Redeemed'
        TabOrder = 1
      end
      object ActiveStateCheckBox: TCheckBox
        Left = 11
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Active'
        TabOrder = 0
      end
      object ExpiredCheckBox: TCheckBox
        Left = 11
        Top = 48
        Width = 97
        Height = 17
        Caption = 'Expired'
        TabOrder = 2
      end
      object DeletedCheckBox: TCheckBox
        Left = 11
        Top = 70
        Width = 97
        Height = 17
        Caption = 'Deleted'
        TabOrder = 3
      end
    end
    object ClientNameEdit: TEdit
      Left = 8
      Top = 82
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'ClientNameEdit'
      OnKeyPress = ClientNameEditKeyPress
    end
  end
  object ActionList: TActionList
    Left = 100
    Top = 200
    object SearchNowAction: TAction
      Caption = '&Search Now'
      ShortCut = 32851
      OnExecute = SearchNowActionExecute
    end
    object CloseAction: TAction
      OnExecute = CloseActionExecute
    end
  end
end

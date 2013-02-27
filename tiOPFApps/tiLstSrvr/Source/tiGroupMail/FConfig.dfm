object FormConfig: TFormConfig
  Left = 393
  Top = 169
  Width = 552
  Height = 436
  BorderIcons = [biSystemMenu]
  Caption = ' TechInsite List Server setup'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    544
    409)
  PixelsPerInch = 96
  TextHeight = 13
  object pcConfig: TPageControl
    Left = 8
    Top = 8
    Width = 531
    Height = 397
    ActivePage = tsListMembers
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = pcConfigChange
    object tsGeneral: TTabSheet
      Caption = 'General settings'
      object paeListName: TtiPerAwareEdit
        Left = 20
        Top = 24
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 0
        LabelStyle = lsTopLeft
        Caption = 'List &name'
        ReadOnly = False
        OnChange = paeListNameChange
        Value = 'tiOPF'
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paeListEMailAdrs: TtiPerAwareEdit
        Left = 20
        Top = 72
        Width = 221
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 1
        LabelStyle = lsTopLeft
        Caption = 'List &EMail address'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paeListMasterEMailAdrs: TtiPerAwareEdit
        Left = 20
        Top = 116
        Width = 221
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 2
        LabelStyle = lsTopLeft
        Caption = 'List &master EMail address'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paeMaxMessageSize: TtiPerAwareFloatEdit
        Left = 20
        Top = 160
        Width = 137
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 3
        LabelStyle = lsTopLeft
        Caption = 'Maximum message &size (kb)'
        ReadOnly = False
        ValueAsString = '0'
        Precision = 0
        UnknownValue = -1.000000000000000000
        IsKnown = True
        Style = fesInteger
      end
      object paeArchiveDir: TtiPerAwarePickDirectory
        Left = 20
        Top = 212
        Width = 261
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 4
        LabelStyle = lsTopLeft
        Caption = 'EMail &archive directory'
        ReadOnly = False
      end
      object paeListNameLong: TtiPerAwareEdit
        Left = 20
        Top = 256
        Width = 261
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 5
        LabelStyle = lsTopLeft
        Caption = 'Long list name'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paeListMasterName: TtiPerAwareEdit
        Left = 24
        Top = 300
        Width = 257
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 6
        LabelStyle = lsTopLeft
        Caption = 'List master'#39's name'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paeSenderAdrsType: TtiPerAwareComboBoxStatic
        Left = 280
        Top = 116
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 7
        LabelStyle = lsTopLeft
        Caption = 'Address shown in <sender> field of messages'
        ReadOnly = False
        DropDownCount = 8
        CharCase = ecNormal
      end
      object paeAcceptPostsFrom: TtiPerAwareComboBoxStatic
        Left = 280
        Top = 164
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 8
        LabelStyle = lsTopLeft
        Caption = 'Accept posts from'
        ReadOnly = False
        DropDownCount = 8
        CharCase = ecNormal
      end
      object paeActive: TtiPerAwareCheckBox
        Left = 280
        Top = 40
        Width = 185
        Height = 22
        ShowFocusRect = True
        Constraints.MinHeight = 17
        TabOrder = 9
        Caption = 'Is list active?'
        ReadOnly = False
        OnChange = paeListNameChange
        Value = False
      end
    end
    object tsMailServer: TTabSheet
      Caption = 'Mail server settings'
      ImageIndex = 1
      object paePOPHost: TtiPerAwareEdit
        Left = 20
        Top = 24
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 0
        LabelStyle = lsTopLeft
        Caption = 'POP &Host'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paePOPUserID: TtiPerAwareEdit
        Left = 20
        Top = 72
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 1
        LabelStyle = lsTopLeft
        Caption = 'POP &User name'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = #0
      end
      object paePopPassword: TtiPerAwareEdit
        Left = 20
        Top = 116
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 2
        LabelStyle = lsTopLeft
        Caption = 'POP &Password'
        ReadOnly = False
        MaxLength = 0
        CharCase = ecNormal
        PasswordChar = '*'
      end
      object paeSMTPHost: TtiPerAwareComboBoxHistory
        Left = 20
        Top = 168
        Width = 225
        Height = 39
        ShowFocusRect = True
        Constraints.MinHeight = 23
        TabOrder = 3
        LabelStyle = lsTopLeft
        Caption = '&SMTP Host'
        ReadOnly = False
        DropDownCount = 8
        CharCase = ecNormal
        Items.Strings = (
          '')
        HistoryCount = 5
      end
    end
    object tsMessageText: TTabSheet
      Caption = 'Message text'
      ImageIndex = 3
      DesignSize = (
        523
        369)
      object pcMessageText: TPageControl
        Left = 6
        Top = 12
        Width = 513
        Height = 353
        ActivePage = tsFooterMessage
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object tsJoinMessage: TTabSheet
          Caption = 'Join'
          DesignSize = (
            505
            325)
          object paeJoinMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = 'Enter label &name'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
        object tsLeaveMessage: TTabSheet
          Caption = 'Leave'
          ImageIndex = 1
          DesignSize = (
            505
            325)
          object paeLeaveMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = 'Enter label &name'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
        object tsFooterMessage: TTabSheet
          Caption = 'Footer'
          ImageIndex = 5
          DesignSize = (
            505
            325)
          object paeFooterMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = '&Footer'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
        object tsRejectMessageSizeMessage: TTabSheet
          Caption = 'Message to big'
          ImageIndex = 2
          DesignSize = (
            505
            325)
          object paeRejectMessageSizeMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = 'Enter label &name'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
        object tsRejectNonListMemberMessage: TTabSheet
          Caption = 'Non list member'
          ImageIndex = 3
          DesignSize = (
            505
            325)
          object paeNonListMemberMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = 'Enter label &name'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
        object tsRejectAttachmentMessage: TTabSheet
          Caption = 'Attachment'
          ImageIndex = 4
          DesignSize = (
            505
            325)
          object paeRejectAttachmentMessage: TtiPerAwareMemo
            Left = 4
            Top = 8
            Width = 495
            Height = 311
            ShowFocusRect = True
            Anchors = [akLeft, akTop, akRight, akBottom]
            Constraints.MinHeight = 23
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Fixedsys'
            Font.Style = []
            TabOrder = 0
            LabelStyle = lsNone
            Caption = 'Enter label &name'
            ReadOnly = False
            ScrollBars = ssVertical
            WordWrap = True
            MaxLength = 0
          end
        end
      end
    end
    object tsListMembers: TTabSheet
      Caption = 'List members'
      ImageIndex = 4
      DesignSize = (
        523
        369)
      object lblCount: TLabel
        Left = 8
        Top = 354
        Width = 122
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'No. of members of the list:'
      end
      object mbImport: TtiMicroButton
        Left = 0
        Top = 28
        Width = 13
        Height = 12
        Flat = True
        OnClick = mbImportClick
      end
      object LVMembers: TtiListView
        Left = 15
        Top = 8
        Width = 503
        Height = 343
        ShowFocusRect = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        MultiSelect = False
        ViewStyle = vsReport
        RowSelect = True
        OnItemEdit = LVMembersItemEdit
        OnItemInsert = LVMembersItemInsert
        OnItemDelete = LVMembersItemDelete
        OnFilterData = LVMembersFilterData
        OnGetFont = LVMembersGetFont
        ApplyFilter = True
        ApplySort = False
        ListColumns = <>
        SortOrders = <>
        AfterRefreshData = LVMembersAfterRefreshData
        RuntimeGenCols = False
        VisibleButtons = [tiLVBtnVisNew, tiLVBtnVisEdit, tiLVBtnVisDelete]
        ButtonStyle = lvbsNormalButtons
        CanStartDrag = False
        DesignSize = (
          503
          343)
      end
    end
  end
end

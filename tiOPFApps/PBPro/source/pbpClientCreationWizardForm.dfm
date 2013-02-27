object ClientCreationWizardForm: TClientCreationWizardForm
  Left = 158
  Top = 201
  ActiveControl = ClientIdentityRecordsFrame.AddClientIdentityRecordButton
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsNone
  Caption = 'Create Client'
  ClientHeight = 601
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Wizard: TKWizard
    Left = 0
    Top = 0
    Width = 770
    Height = 601
    ActivePage = KWizardInteriorPage2
    ButtonStart.Caption = 'To &Start Page'
    ButtonStart.NumGlyphs = 1
    ButtonStart.Layout = blGlyphLeft
    ButtonStart.ModalResult = 0
    ButtonStart.Width = 85
    ButtonLast.Caption = 'To &Last Page'
    ButtonLast.NumGlyphs = 1
    ButtonLast.Layout = blGlyphLeft
    ButtonLast.ModalResult = 0
    ButtonLast.Width = 85
    ButtonBack.Caption = '< &Back'
    ButtonBack.NumGlyphs = 1
    ButtonBack.Layout = blGlyphLeft
    ButtonBack.ModalResult = 0
    ButtonBack.Width = 75
    ButtonNext.Caption = '&Next >'
    ButtonNext.NumGlyphs = 1
    ButtonNext.Layout = blGlyphLeft
    ButtonNext.ModalResult = 0
    ButtonNext.Width = 75
    ButtonFinish.Caption = '&Finish'
    ButtonFinish.NumGlyphs = 1
    ButtonFinish.Layout = blGlyphLeft
    ButtonFinish.ModalResult = 0
    ButtonFinish.Width = 75
    ButtonCancel.Caption = 'Cancel'
    ButtonCancel.NumGlyphs = 1
    ButtonCancel.Layout = blGlyphLeft
    ButtonCancel.ModalResult = 2
    ButtonCancel.Width = 75
    ButtonHelp.Caption = '&Help'
    ButtonHelp.NumGlyphs = 1
    ButtonHelp.Layout = blGlyphLeft
    ButtonHelp.ModalResult = 0
    ButtonHelp.Width = 75
    ShowRouteMap = True
    OnCancelButtonClick = WizardCancelButtonClick
    object ClientPersonalDetailsPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Personal details'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 2
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 'Enter client personal details'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 2
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      inline ClientFrame: TClientFrame
        Top = 70
        Width = 617
        Height = 489
        Align = alClient
        inherited Bevel4: TBevel
          Width = 598
        end
        inherited Bevel3: TBevel
          Width = 593
        end
        inherited UndesirableRichEdit: TRichEdit
          Width = 502
        end
      end
    end
    object KWizardInteriorPage2: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Identity records'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 
        'Add or edit existing identity records. Note: once a record is in' +
        ' use in a contract it can no longer be modified or deleted.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      Caption = 'KWizardInteriorPage2'
      inline ClientIdentityRecordsFrame: TClientIdentityRecordsFrame
        Top = 70
        Width = 617
        Height = 489
        Align = alClient
        inherited Bevel1: TBevel
          Width = 592
        end
        inherited ClientIdentityRecordListView: TtiListViewPlus
          Width = 496
          Height = 351
        end
      end
    end
    object CreateWizardPage: TKWizardInteriorPage
      Header.Color = clWindow
      Header.Visible = True
      Header.ImageIndex = -1
      Header.ImageOffset = 0
      Header.ImageAlignment = iaRight
      Header.Height = 70
      Header.ParentFont = True
      Header.Title.Color = clNone
      Header.Title.Visible = True
      Header.Title.Text = 'Finish'
      Header.Title.Anchors = [akLeft, akTop, akRight]
      Header.Title.AnchorPlacement = 4
      Header.Title.Indent = 0
      Header.Title.Alignment = taLeftJustify
      Header.Title.Font.Charset = DEFAULT_CHARSET
      Header.Title.Font.Color = clWindowText
      Header.Title.Font.Height = -16
      Header.Title.Font.Name = 'MS Sans Serif'
      Header.Title.Font.Style = [fsBold]
      Header.Subtitle.Color = clNone
      Header.Subtitle.Visible = True
      Header.Subtitle.Text = 
        'You are about to commit changes made to this client. If you are ' +
        'satisfied with the details below click on the '#39'Finish'#39' button.'
      Header.Subtitle.Anchors = [akLeft, akTop, akRight, akBottom]
      Header.Subtitle.AnchorPlacement = 4
      Header.Subtitle.Indent = 0
      Header.Subtitle.Alignment = taLeftJustify
      Header.Subtitle.Font.Charset = DEFAULT_CHARSET
      Header.Subtitle.Font.Color = clWindowText
      Header.Subtitle.Font.Height = -11
      Header.Subtitle.Font.Name = 'MS Sans Serif'
      Header.Subtitle.Font.Style = []
      Header.ShowDivider = True
      Image.Alignment = iaStretch
      Image.Layout = ilStretch
      Image.Transparent = False
      Panel.Color = clBtnFace
      Panel.Visible = False
      Panel.BorderWidth = 7
      VisibleButtons = [bkBack, bkFinish, bkCancel]
      OnFinishButtonClick = CreateWizardPageFinishButtonClick
      object Label1: TLabel
        Left = 12
        Top = 392
        Width = 28
        Height = 13
        Caption = 'N&otes'
        FocusControl = NotesRichEdit
      end
      object NotesRichEdit: TRichEdit
        Left = 96
        Top = 392
        Width = 507
        Height = 145
        Lines.Strings = (
          'NotesRichEdit')
        TabOrder = 0
      end
    end
    object KWizardRouteMapNodes1: TKWizardRouteMapNodes
      Left = 0
      Top = 0
      Width = 153
      Height = 559
      ItemHeight = 20
      Color = 7850482
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Indent = 8
      NodeColors.Selected = clGray
      NodeColors.Unselected = clWhite
      NodeColors.Line = clBtnShadow
      NodeColors.Disabled = clBtnFace
      UsePageTitle = True
    end
  end
  object IdleTimer: TTimer
    Interval = 500
    OnTimer = IdleTimerTimer
    Left = 17
    Top = 80
  end
end

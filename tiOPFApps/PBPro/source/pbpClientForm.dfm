object ClientForm: TClientForm
  Left = 189
  Top = 137
  BorderStyle = bsNone
  Caption = 'Client'
  ClientHeight = 618
  ClientWidth = 697
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 5
    Top = 64
    Width = 609
    Height = 3
    Shape = bsBottomLine
  end
  object Label13: TLabel
    Left = 560
    Top = 80
    Width = 28
    Height = 13
    Caption = 'Photo'
  end
  object ClientDetailsPanel: TPanel
    Left = 0
    Top = 23
    Width = 697
    Height = 87
    Align = alTop
    BevelOuter = bvNone
    Color = 12240841
    TabOrder = 0
    object Label3: TLabel
      Left = 204
      Top = 7
      Width = 55
      Height = 13
      Caption = 'Client name'
    end
    object Label15: TLabel
      Left = 7
      Top = 34
      Width = 58
      Height = 13
      Caption = 'Date of birth'
    end
    object Label1: TLabel
      Left = 7
      Top = 10
      Width = 46
      Height = 13
      Caption = 'Client No.'
    end
    object ClientNameEdit: TEdit
      Left = 272
      Top = 4
      Width = 401
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
      Text = 'ClientNameEdit'
    end
    object ClientNumberEdit: TEdit
      Left = 88
      Top = 4
      Width = 97
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      Text = 'ClientNumberEdit'
    end
    object DateOfBirthEdit: TEdit
      Left = 86
      Top = 33
      Width = 99
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'DateOfBirthEdit'
    end
  end
  object AddressPanel: TPanel
    Left = 0
    Top = 180
    Width = 697
    Height = 134
    Align = alTop
    BevelOuter = bvNone
    Color = 12240841
    TabOrder = 1
    object Label10: TLabel
      Left = 319
      Top = 43
      Width = 66
      Height = 13
      Caption = 'Phone (Work)'
    end
    object Label9: TLabel
      Left = 319
      Top = 19
      Width = 68
      Height = 13
      Caption = 'Phone (Home)'
    end
    object Label12: TLabel
      Left = 319
      Top = 99
      Width = 28
      Height = 13
      Caption = 'Email '
    end
    object Label11: TLabel
      Left = 319
      Top = 75
      Width = 71
      Height = 13
      Caption = 'Phone (Mobile)'
    end
    object Label21: TLabel
      Left = 7
      Top = 84
      Width = 48
      Height = 13
      Caption = 'Post code'
    end
    object Label20: TLabel
      Left = 7
      Top = 60
      Width = 25
      Height = 13
      Caption = 'State'
    end
    object Label6: TLabel
      Left = 7
      Top = 41
      Width = 34
      Height = 13
      Caption = 'Suburb'
    end
    object Label7: TLabel
      Left = 7
      Top = 20
      Width = 28
      Height = 13
      Caption = 'Street'
    end
    object PhoneWorkEdit: TEdit
      Left = 400
      Top = 40
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      Text = 'PhoneWorkEdit'
    end
    object PhoneHomeEdit: TEdit
      Left = 400
      Top = 16
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'PhoneHomeEdit'
    end
    object EmailAddressEdit: TEdit
      Left = 400
      Top = 99
      Width = 273
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
      Text = 'EmailAddressEdit'
    end
    object PhoneMobileEdit: TEdit
      Left = 400
      Top = 72
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
      Text = 'PhoneMobileEdit'
    end
    object StreetEdit: TEdit
      Left = 91
      Top = 17
      Width = 209
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      Text = 'StreetEdit'
    end
    object SuburbEdit: TEdit
      Left = 91
      Top = 40
      Width = 209
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 5
      Text = 'SuburbEdit'
    end
    object StateEdit: TEdit
      Left = 91
      Top = 63
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
      Text = 'StateEdit'
    end
    object PostCodeEdit: TEdit
      Left = 91
      Top = 87
      Width = 161
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 7
      Text = 'PostCodeEdit'
    end
  end
  object IdentityRecordPanel: TPanel
    Left = 0
    Top = 314
    Width = 697
    Height = 170
    Align = alClient
    BevelOuter = bvNone
    Color = 9609633
    TabOrder = 2
    object Label2: TLabel
      Left = 7
      Top = 16
      Width = 72
      Height = 13
      Caption = 'Identity records'
    end
    object ClientIdentityRecordListView: TtiListViewPlus
      Left = 91
      Top = 16
      Width = 583
      Height = 131
      RuntimeGenCols = False
      Anchors = [akLeft, akTop, akRight, akBottom]
      MultiSelect = False
      ViewStyle = vsReport
      RowSelect = False
      ApplyFilter = False
      ApplySort = False
      ListColumns = <
        item
          DisplayLabel = 'Type'
          FieldName = 'Type'
          DataType = lvtkString
          Derived = True
          OnDeriveColumn = ClientIdentityRecordListViewListColumns0DeriveColumn
        end
        item
          DisplayLabel = 'Details'
          FieldName = 'Details'
          DataType = lvtkString
          Derived = False
        end
        item
          DisplayLabel = 'In use'
          FieldName = 'InUse'
          DataType = lvtkString
          Derived = False
        end>
      SortOrders = <>
      SelectFirstRow = False
    end
  end
  object UndesirablePanel: TPanel
    Left = 0
    Top = 110
    Width = 697
    Height = 70
    Align = alTop
    BevelOuter = bvNone
    Color = 9609633
    TabOrder = 3
    object Label14: TLabel
      Left = 8
      Top = 37
      Width = 25
      Height = 13
      Caption = 'Code'
    end
    object Label17: TLabel
      Left = 264
      Top = 13
      Width = 37
      Height = 13
      Caption = 'Reason'
    end
    object Label5: TLabel
      Left = 7
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Undesirable'
    end
    object UndesirableReasonRichEdit: TRichEdit
      Left = 312
      Top = 5
      Width = 361
      Height = 46
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'UndesirableReasonRichEdit')
      ParentColor = True
      PlainText = True
      ReadOnly = True
      TabOrder = 2
    end
    object UndesirableCodeEdit: TEdit
      Left = 88
      Top = 33
      Width = 97
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
      Text = 'UndesirableCodeEdit'
    end
    object UndesirableEdit: TEdit
      Left = 88
      Top = 4
      Width = 97
      Height = 21
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
      Text = 'UndesirableEdit'
    end
  end
  object TBDock: TTBDock
    Left = 0
    Top = 0
    Width = 697
    Height = 23
    object ClientFormToolbar: TTBToolbar
      Left = 0
      Top = 0
      Align = alTop
      Caption = 'ClientFormToolbar'
      CloseButton = False
      DockRow = 1
      DragHandleStyle = dhDouble
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 0
      DockTextAlign = taLeftJustify
      object TBItem5: TTBItem
        Action = NewClientAction
      end
      object TBItem1: TTBItem
        Action = EditAction
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 484
    Width = 697
    Height = 134
    Align = alBottom
    BevelOuter = bvNone
    Color = 12240841
    TabOrder = 5
    object Label8: TLabel
      Left = 7
      Top = 16
      Width = 28
      Height = 13
      Caption = 'Notes'
    end
    object NotesRichEdit: TRichEdit
      Left = 91
      Top = 13
      Width = 583
      Height = 108
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'NotesRichEdit')
      ParentColor = True
      PlainText = True
      ReadOnly = True
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Left = 88
    object NewClientAction: TAction
      Category = 'Client'
      Caption = '&New'
      OnExecute = NewClientActionExecute
    end
    object EditAction: TAction
      Category = 'Client'
      Caption = 'Edit'
      OnExecute = EditActionExecute
    end
  end
end

object ClientFrame: TClientFrame
  Left = 0
  Top = 0
  Width = 638
  Height = 529
  TabOrder = 0
  object Bevel4: TBevel
    Left = 8
    Top = 256
    Width = 619
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 48
    Width = 614
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object Label2: TLabel
    Left = 12
    Top = 14
    Width = 64
    Height = 13
    Caption = 'Client number'
  end
  object Name: TLabel
    Left = 12
    Top = 40
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label1: TLabel
    Left = 12
    Top = 108
    Width = 28
    Height = 13
    Caption = 'Street'
  end
  object Label21: TLabel
    Left = 12
    Top = 172
    Width = 48
    Height = 13
    Caption = 'Post code'
  end
  object Label20: TLabel
    Left = 12
    Top = 148
    Width = 25
    Height = 13
    Caption = 'State'
  end
  object Label14: TLabel
    Left = 12
    Top = 129
    Width = 34
    Height = 13
    Caption = 'Suburb'
  end
  object Label9: TLabel
    Left = 12
    Top = 212
    Width = 68
    Height = 13
    Caption = 'Phone (Home)'
  end
  object Label3: TLabel
    Left = 12
    Top = 235
    Width = 66
    Height = 13
    Caption = 'Phone (Work)'
  end
  object Label11: TLabel
    Left = 12
    Top = 258
    Width = 71
    Height = 13
    Caption = 'Phone (Mobile)'
  end
  object Label40: TLabel
    Left = 12
    Top = 342
    Width = 25
    Height = 13
    Caption = 'Code'
  end
  object Label29: TLabel
    Left = 12
    Top = 368
    Width = 37
    Height = 13
    Caption = 'Reason'
  end
  object Label15: TLabel
    Left = 12
    Top = 65
    Width = 58
    Height = 13
    Caption = 'Date of birth'
  end
  object Label4: TLabel
    Left = 12
    Top = 280
    Width = 65
    Height = 13
    Caption = 'Email address'
  end
  object UndesirableNoticeLabel: TLabel
    Left = 114
    Top = 318
    Width = 261
    Height = 13
    Caption = 'Note: this client is marked as UNDESIRABLE.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ClientNameEdit: TEdit
    Left = 96
    Top = 37
    Width = 411
    Height = 21
    TabOrder = 1
    Text = 'ClientNameEdit'
    OnExit = ClientNameEditExit
  end
  object ClientNumberEdit: TEdit
    Left = 96
    Top = 11
    Width = 81
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object StreetEdit: TEdit
    Left = 96
    Top = 105
    Width = 209
    Height = 21
    TabOrder = 3
    Text = 'StreetEdit'
  end
  object SuburbEdit: TEdit
    Left = 96
    Top = 128
    Width = 209
    Height = 21
    TabOrder = 4
    Text = 'SuburbEdit'
  end
  object StateEdit: TEdit
    Left = 96
    Top = 151
    Width = 161
    Height = 21
    TabOrder = 5
    Text = 'StateEdit'
  end
  object PostCodeEdit: TEdit
    Left = 96
    Top = 175
    Width = 161
    Height = 21
    TabOrder = 6
    Text = 'PostCodeEdit'
  end
  object PhoneHomeEdit: TEdit
    Left = 96
    Top = 209
    Width = 161
    Height = 21
    TabOrder = 7
    Text = 'PhoneHomeEdit'
  end
  object PhoneWorkEdit: TEdit
    Left = 96
    Top = 232
    Width = 161
    Height = 21
    TabOrder = 8
    Text = 'PhoneWorkEdit'
  end
  object EmailAddressEdit: TEdit
    Left = 96
    Top = 279
    Width = 225
    Height = 21
    TabOrder = 10
    Text = 'EmailAddressEdit'
  end
  object PhoneMobileEdit: TEdit
    Left = 96
    Top = 255
    Width = 161
    Height = 21
    TabOrder = 9
    Text = 'PhoneMobileEdit'
  end
  object UndesirableCheckBox: TCheckBox
    Left = 12
    Top = 316
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Undesirable'
    TabOrder = 11
    OnClick = UndesirableCheckBoxClick
  end
  object UndesirableRichEdit: TRichEdit
    Left = 96
    Top = 364
    Width = 523
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'UndesirableRichEdit')
    ScrollBars = ssVertical
    TabOrder = 13
  end
  object UndesirableCodeEdit: TEdit
    Left = 96
    Top = 339
    Width = 161
    Height = 21
    TabOrder = 12
    Text = 'UndesirableCodeEdit'
  end
  object DateOfBirthDateTimePicker: TDateTimePicker
    Left = 96
    Top = 63
    Width = 113
    Height = 21
    CalAlignment = dtaLeft
    Date = 37443.7999578472
    Time = 37443.7999578472
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 2
  end
end

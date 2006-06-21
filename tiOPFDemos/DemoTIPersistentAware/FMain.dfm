object FormMain: TFormMain
  Left = 242
  Top = 178
  Width = 684
  Height = 409
  Caption = ' tiPersistentAware constrols demonstration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object memoNotes: TMemo
    Left = 0
    Top = 0
    Width = 225
    Height = 341
    Align = alLeft
    Color = clBtnFace
    Lines.Strings = (
      'The TListView is browsing a TObjectList of '
      'TAnimal(s).'
      ''
      'The TAnimal class descends from TPersistent, '
      'and has the additional methods:'
      '* Assign( )'
      '* Clone'
      '* Equals( )'
      ''
      'These are used to make a copy to use as an '
      'edit buffer in the edit dialog.'
      ''
      'Double click on an item in the TListView to '
      'popup an edit dialog.'
      ''
      'The edit dialog uses TtiPerAware controls and '
      'shows:'
      '* How to assign the data and property name to '
      'a control'
      '* How to change the label alignment'
      '* How to change the label width'
      '* How to detect a change'
      '* How to save changes')
    ReadOnly = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 341
    Width = 676
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnClose: TBitBtn
      Left = 595
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
      Kind = bkCancel
    end
  end
  object lvAnimals: TListView
    Left = 225
    Top = 0
    Width = 451
    Height = 341
    Align = alClient
    Columns = <
      item
        Caption = 'Animal name'
        Width = 75
      end
      item
        Alignment = taRightJustify
        Caption = 'Weight'
        Width = 65
      end
      item
        Alignment = taRightJustify
        Caption = 'No of legs'
        Width = 65
      end
      item
        Caption = 'Last visited'
        Width = 75
      end
      item
        Caption = 'Vaccinated'
        Width = 70
      end
      item
        Caption = 'Animal type'
      end
      item
        Caption = 'Notes'
        Width = 100
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnData = lvAnimalsData
    OnDblClick = lvAnimalsDblClick
  end
end

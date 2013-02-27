inherited frmDoctorList: TfrmDoctorList
  Caption = 'Doctors'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lvMaint: TtiListView
    OnItemEdit = lvMaintItemEdit
    OnItemInsert = lvMaintItemInsert
    OnItemDelete = lvMaintItemDelete
    ListColumns = <
      item
        DisplayLabel = 'Name'
        FieldName = 'Name'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'NHS Number'
        FieldName = 'NHSNumber'
        DataType = lvtkString
        Derived = False
        Alignment = taLeftJustify
      end>
    RuntimeGenCols = False
  end
end

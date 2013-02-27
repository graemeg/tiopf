inherited frmPatientList: TfrmPatientList
  Caption = 'Patients'
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
      end
      item
        DisplayLabel = 'Post Code'
        FieldName = 'PostCode'
        DataType = lvtkString
        Derived = False
      end>
    RuntimeGenCols = False
  end
end

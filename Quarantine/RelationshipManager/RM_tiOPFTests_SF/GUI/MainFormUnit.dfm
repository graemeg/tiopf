object frmMainForm: TfrmMainForm
  Left = 0
  Top = 0
  Caption = 'frmMainForm'
  ClientHeight = 556
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblCustomers: TLabel
    Left = 24
    Top = 16
    Width = 58
    Height = 13
    Caption = 'Customers :'
  end
  object lblOrders: TLabel
    Left = 24
    Top = 232
    Width = 40
    Height = 13
    Caption = 'Orders :'
  end
  object btnConnect: TButton
    Left = 112
    Top = 443
    Width = 161
    Height = 33
    Caption = 'Connect to database'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 112
    Top = 488
    Width = 161
    Height = 33
    Caption = 'Disconnect from database'
    TabOrder = 1
    OnClick = btnDisconnectClick
  end
  object btnReadCustomers: TButton
    Left = 560
    Top = 30
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 2
    OnClick = btnReadCustomersClick
  end
  object btnInsertCustomer: TButton
    Left = 560
    Top = 61
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 3
    OnClick = btnInsertCustomerClick
  end
  object btnSaveCustomers: TButton
    Left = 560
    Top = 131
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveCustomersClick
  end
  object btnInsertOrder: TButton
    Left = 560
    Top = 275
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 5
    OnClick = btnInsertOrderClick
  end
  object btnDeleteCustomer: TButton
    Left = 560
    Top = 92
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = btnDeleteCustomerClick
  end
  object btnDeleteOrder: TButton
    Left = 560
    Top = 306
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 7
    OnClick = btnDeleteOrderClick
  end
  object lstCustomers: TListBox
    Left = 112
    Top = 30
    Width = 401
    Height = 169
    ItemHeight = 13
    TabOrder = 8
    OnClick = lstCustomersClick
  end
  object lstOrders: TListBox
    Left = 112
    Top = 244
    Width = 401
    Height = 169
    ItemHeight = 13
    TabOrder = 9
  end
end

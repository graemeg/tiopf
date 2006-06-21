object frmIBSQLMonitor: TfrmIBSQLMonitor
  Left = 65
  Top = 169
  Width = 642
  Height = 540
  Caption = 'IB SQL Monitor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tiSplitter: TtiSplitter
    Left = 0
    Top = 466
    Width = 634
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object SQLLabel: TLabel
    Left = 0
    Top = 474
    Width = 634
    Height = 32
    Align = alBottom
    Constraints.MinHeight = 32
    WordWrap = True
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 634
    Height = 23
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 49
    Caption = 'ToolBar'
    Flat = True
    ShowCaptions = True
    TabOrder = 0
    object ConnectButton: TToolButton
      Left = 0
      Top = 0
      AllowAllUp = True
      Caption = 'Connect'
      ImageIndex = 0
      Style = tbsCheck
      OnClick = ConnectButtonClick
    end
    object ToolButton2: TToolButton
      Left = 49
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object ClearButton: TToolButton
      Left = 57
      Top = 0
      Caption = 'Clear'
      ImageIndex = 11
      OnClick = ClearButtonClick
    end
    object ToolButton13: TToolButton
      Left = 106
      Top = 0
      Width = 8
      Caption = 'ToolButton13'
      ImageIndex = 11
      Style = tbsSeparator
    end
    object TraceQPrepareButton: TToolButton
      Left = 114
      Top = 0
      Caption = 'Prepare'
      ImageIndex = 1
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceQExecuteButton: TToolButton
      Tag = 1
      Left = 163
      Top = 0
      Caption = 'Execute'
      ImageIndex = 2
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceQFetchButton: TToolButton
      Tag = 2
      Left = 212
      Top = 0
      Caption = 'Fetch'
      ImageIndex = 3
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceErrorButton: TToolButton
      Tag = 3
      Left = 261
      Top = 0
      Caption = 'Error'
      ImageIndex = 4
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceStmtButton: TToolButton
      Tag = 4
      Left = 310
      Top = 0
      Caption = 'Stmt'
      ImageIndex = 5
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceConnectButton: TToolButton
      Tag = 5
      Left = 359
      Top = 0
      Caption = 'Connect'
      ImageIndex = 6
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceTransactButton: TToolButton
      Tag = 6
      Left = 408
      Top = 0
      Caption = 'Transact'
      ImageIndex = 7
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceBlobButton: TToolButton
      Tag = 7
      Left = 457
      Top = 0
      Caption = 'Blob'
      ImageIndex = 8
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceServiceButton: TToolButton
      Tag = 8
      Left = 506
      Top = 0
      Caption = 'Service'
      ImageIndex = 9
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
    object TraceMiscButton: TToolButton
      Tag = 9
      Left = 555
      Top = 0
      Caption = 'Misc'
      ImageIndex = 10
      Style = tbsCheck
      OnClick = TraceQPrepareButtonClick
    end
  end
  object EventListView: TtiListView
    Left = 0
    Top = 23
    Width = 634
    Height = 443
    ShowFocusRect = True
    Align = alClient
    MultiSelect = False
    ViewStyle = vsReport
    RowSelect = True
    ApplyFilter = False
    ApplySort = False
    ListColumns = <
      item
        DisplayLabel = 'Time'
        FieldName = 'EventTimeCaption'
        DataType = lvtkString
        Derived = False
        Width = 85
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Application'
        FieldName = 'ApplicationName'
        DataType = lvtkString
        Derived = False
        Width = 72
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Type'
        FieldName = 'EventTypeName'
        DataType = lvtkString
        Derived = False
        Width = 72
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Repeats'
        FieldName = 'Repeats'
        DisplayMask = '#'
        DataType = lvtkInt
        Derived = False
        Width = 60
      end
      item
        DisplayLabel = 'Rep. Time'
        FieldName = 'RepeatTimeCaption'
        DataType = lvtkString
        Derived = False
        Width = 85
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Table'
        FieldName = 'TableName'
        DataType = lvtkString
        Derived = False
        Width = 80
        Alignment = taLeftJustify
      end
      item
        DisplayLabel = 'Description'
        FieldName = 'Description'
        DataType = lvtkString
        Derived = False
        Width = 300
        Alignment = taLeftJustify
      end>
    SortOrders = <>
    SortOnHeadingClick = False
    OnItemArive = EventListViewItemArive
    OnItemLeave = EventListViewItemLeave
    RuntimeGenCols = False
    CanStartDrag = False
    DesignSize = (
      634
      443)
  end
  object IBSQLMonitor: TIBSQLMonitor
    OnSQL = IBSQLMonitorSQL
    TraceFlags = [tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect, tfTransact, tfBlob, tfService, tfMisc]
    Enabled = False
    Left = 12
    Top = 36
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 52
    Top = 39
  end
end

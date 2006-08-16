{$I tiDefines.inc}

unit tiOPFControlsReg;

interface

procedure Register;

implementation
uses
  Classes
  ,ActnList
   {$IFNDEF VER130}
     ,DesignIntf
     ,DesignEditors
   {$ELSE}
     ,DsgnIntf
   {$ENDIF}
  ,DtiDefaultActionValues
  ,tiPerAwareCtrls
  ,tiPerAwareFileCombos
  ,tiPerAwareDirectoryCombos
  ,tiMemoReadOnly
  ,tiPerAwareMultiSelect
  ,tiReadOnly
  ,tiHyperLink
  ,tiSpeedButton
  ,tiButtons
  ,tiRoundedPanel
  ,tiPerAwareDateRange
  ,tiListView
  ,tiListViewCtrls
  ,tiListViewPlus
  ,tiListViewDif
  ,tiSplitter
  ,tiSplitterEditor
  ,tiTreeView
  ,tiTreeViewChildForm
  ,tiVTListView
  ,tiVTTreeView
  ,tiTreeviewEditor
  ;

{$R tiOPFControls.dcr}

procedure Register;
begin
  RegisterComponents( 'TechInsite',
                      [   TtiPerAwareEdit
                         ,TtiPerAwareMemo
                         ,TtiPerAwareComboBoxStatic
                         ,TtiPerAwareComboBoxDynamic
                         ,TtiPerAwareComboBoxHistory
                         ,TtiPerAwareDateTimePicker
                         ,TtiPerAwareCheckBox
                         ,TtiPerAwareFloatEdit
                         ,TtiPerAwareImageEdit
                         ,TtiUserDefinedPicker
                         ,TtiPerAwarePickFile
                         ,TtiPerAwarePickDirectory
                         ,TtiMemoReadOnly
                         ,TtiPerAwareMultiSelect
                         ,TtiReadOnly
                         ,TtiHyperLink
                         ,TtiSpeedButton
                         ,TtiRoundedPanel
                         ,TtiDateRange
                         ,TtiSplitter
                         ,TtiSplitterPanel
                         ,TtiToolBar
                         ,TtiButtonPanel
                         ,TtiMicroButton

                         ,TtiListView      // Depreciated.  Use TtiVTListView in future
                         ,TtiListViewListBox
                         ,TtiListViewPlus
                         ,TtiListViewDif

                         ,TtiTreeView      // Depreciated.  Use TtiVTTreeView in future
                         ,TtiTreeViewChildForm

                         ,TtiVTListView
                         ,TtiVTTreeView

                      ]) ;


  RegisterActions( 'TechInsite',
                   [
                     TtiImageLoadAction
                    ,TtiImageSaveAction
                    ,TtiImagePasteFromClipboardAction
                    ,TtiImageCopyToClipboardAction
                    ,TtiImageEditAction
                    ,TtiImageClearAction
                    ,TtiImageStretchAction
                    ,TtiImageViewAction
                    ,TtiImageNewAction
                    ,TtiImageExportAction
                   ],
                   TtidmDefaultActionValues );

  RegisterComponentEditor( TtiSplitterPanel, TtiSplitterPanelEditor );

  RegisterPropertyEditor( TypeInfo( TtiTVNodeEvent ),            // TypeInfo of property
                          TtiTVDataMapping,                      // ClassRef of component containing property
                          '',                                    // Name of property
                          TtiTVNodeEventPropertyEditor ) ;       // ClassRef of property editor

  RegisterPropertyEditor( TypeInfo( TtiTVNodeConfirmEvent ),     // TypeInfo of property
                          nil,                                   // ClassRef of component containing property
                          '',                                    // Name of property
                          TtiTVNodeEventPropertyEditor ) ;       // ClassRef of property editor


  RegisterPropertyEditor( TypeInfo( TtiTVDragDropEvent ),        // TypeInfo of property
                          nil,                                   // ClassRef of component containing property
                          '',                                    // Name of property
                          TtiTVNodeEventPropertyEditor ) ;       // ClassRef of property editor

  RegisterPropertyEditor( TypeInfo( TtiTVDragDropConfirmEvent ), // TypeInfo of property
                          nil,                                   // ClassRef of component containing property
                          '',                                    // Name of property
                          TtiTVNodeEventPropertyEditor ) ;       // ClassRef of property editor

end;

end.

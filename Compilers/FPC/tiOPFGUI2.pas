{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit tiOPFGUI2; 

interface

uses
  cTIPerAwareCtrls, tiButtons, tiCtrlButtonPanel, tiFocusPanel, tiHyperlink, 
    tiImageMgr, tiListView, tiListViewCtrls, tiListViewDif, tiListViewPlus, 
    tiMemoReadOnly, tiPerAwareCombosAbs, tiPerAwareCtrls, tiPerAwareDateRange, 
    tiPerAwareDirectoryCombos, tiPerAwareFileCombos, tiPerAwareMultiSelect, 
    tiReadOnly, tiResources, tiRoundedPanel, tiSpeedButton, tiSplitter, 
    tiThreadProgress, tiTreeView, tiTreeViewChildForm, tiDialogs, 
    DtiDefaultActionValues, tiOPFControlsReg, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('tiOPFControlsReg', @tiOPFControlsReg.Register); 
end; 

initialization
  RegisterPackage('tiOPFGUI2', @Register); 
end.

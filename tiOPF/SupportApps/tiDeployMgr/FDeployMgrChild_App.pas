{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FDeployMgrChild_App;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiDeployMgr_BOM, ComCtrls, ExtCtrls, tiPerAwareCtrls, tiListView, ImgList,
  StdCtrls, Buttons, tiFocusPanel ;

type
  TFormTIDeployChild_App = class(TForm)
    PC: TPageControl;
    tsDescription: TTabSheet;
    tsFiles: TTabSheet;
    paeAppName: TtiPerAwareEdit;
    paeAppDescription: TtiPerAwareMemo;
    LVFiles: TtiListView;
    ilListView: TImageList;
    paeCompression: TtiPerAwareComboBoxStatic;
    lblFileCount: TLabel;
    tsParams: TTabSheet;
    LVParams: TtiListView;
    paeIcon: TtiPerAwareImageEdit;
    paeDisplayText: TtiPerAwareEdit;
    sbSaveOne: TSpeedButton;
    procedure paeAppNameChange(Sender: TObject);
    procedure paeAppNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure LVFilesFilterData(pData: TPersistent; var pbInclude: Boolean);
    procedure LVFilesGetFont(pLV: TtiCustomListView; pCanvas: TCanvas;
      pItem: TListItem; pData: TPersistent);
    procedure LVFilesGetImageIndex(pData: TPersistent;
      var pImageIndex: Integer);
    procedure LVFilesItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVFilesItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVFilesItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure paeCompressionChange(Sender: TObject);
    procedure LVFilesAfterRefreshData(pLV: TtiCustomListView);
    procedure LVParamsItemEdit(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure LVParamsItemInsert(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure PCChange(Sender: TObject);
    procedure sbSaveOneClick(Sender: TObject);
    procedure LVFilesItemArive(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVFilesItemLeave(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
  private
    FData: TtiDeployApp;
    FTreeNode: TTreeNode;
    procedure SetData(const Value: TtiDeployApp);
    function GetValid: boolean;
    { Private declarations }
  public
    procedure RefreshData ;
  published
    property Data : TtiDeployApp read FData write SetData ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid : boolean read GetValid ;
  end;


implementation
uses
  tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,FEditFile
  ,FEditParam
  ,tiCompressAbs
  ,tiDeployMgr_Cli
  ,tiLog
  ,tiPersist
  ,ctiDeployMgr
  ,tiDialogs
  ;

{$R *.DFM}

{ TForm1 }

function TFormTIDeployChild_App.GetValid: boolean;
begin
  result := true ;
end;

procedure TFormTIDeployChild_App.SetData(const Value: TtiDeployApp);
begin
  FData := Value;
  paeAppName.LinkToData(        FData, 'AppName'     ) ;
  paeAppDescription.LinkToData( FData, 'Description' ) ;
  paeDisplayText.LinkToData(    FData, 'DisplayText' ) ;
  paeCompression.LinkToData(    FData, 'Compression' ) ;
  paeIcon.LinkToData(           FData, 'Image'       ) ;

  if FData <> nil then
  begin
    LVFiles.Data := FData.List;
    LVParams.Data := FData.DeployParams.List ;
  end
  else
  begin
    LVFiles.Data  := nil ;
    LVParams.Data := nil ;
  end ;
end;

procedure TFormTIDeployChild_App.paeAppNameChange(Sender: TObject);
begin
  FData.Dirty := true ;
  TreeNode.Text := FData.AppName ;
end;

procedure TFormTIDeployChild_App.paeCompressionChange(Sender: TObject);
begin
  FData.MarkListItemsDirty ;
  paeAppNameChange( nil );
end;

procedure TFormTIDeployChild_App.paeAppNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not ( Key in [ 'a'..'z', 'A'..'Z', '_', Char( VK_BACK ), '0'..'9'  ]) then
    Key := '_' ;
end;

procedure TFormTIDeployChild_App.FormShow(Sender: TObject);
begin
  if ( FData.ObjectState = posCreate ) then
  begin
    PC.ActivePage := tsDescription ;
    paeAppName.SetFocus ;
  end ;
end;

procedure TFormTIDeployChild_App.LVFilesFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  pbInclude := ( not ( pData as TPerObjAbs ).Deleted ) ;
end;

procedure TFormTIDeployChild_App.LVFilesGetFont(pLV: TtiCustomListView;
  pCanvas: TCanvas; pItem: TListItem; pData: TPersistent);
begin
  if TtiDeployFile( pData ).SourceFileNotFound then
    pCanvas.Font.Color := clGray
  else if TtiDeployFile( pData ).Dirty or TtiDeployFile( pData ).DeployFromDirty then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack ;
end;

procedure TFormTIDeployChild_App.LVFilesGetImageIndex(pData: TPersistent;
  var pImageIndex: Integer);
begin
  if pData = nil then
    Exit ; //==>

  if TtiDeployFile( pData ).Launch then
    pImageIndex := 0
  else
    pImageIndex := -1 ;

end;

procedure TFormTIDeployChild_App.LVFilesItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete( TPerObjAbs( pData )) then
    ReadDeployFromIcon( FData ) ;
end;

procedure TFormTIDeployChild_App.LVFilesItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEditFile.Execute( pData as TtiDeployFile ) then
  begin
    ReadDeployFromDirty( TPerObjAbs( pData )) ;
    ReadDeployFromIcon( FData ) ;
  end ;
end;

procedure TFormTIDeployChild_App.LVFilesItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TtiDeployFile ;
begin
  lData := TtiDeployFile.CreateNew ;
  if TFormEditFile.Execute( lData ) then
  begin
    FData.Add( lData ) ;
    ReadDeployFromDirty( lData ) ;
    ReadDeployFromIcon( FData ) ;
  end
  else
    lData.Free ;
end;

procedure TFormTIDeployChild_App.FormCreate(Sender: TObject);
begin
  gCompressFactory.AssignCompressionTypes( paeCompression.Items ) ;
end;

procedure TFormTIDeployChild_App.LVFilesAfterRefreshData(
  pLV: TtiCustomListView);
begin
  if FData.Count <> 1 then
    lblFileCount.Caption := Format( ' %d files to be deployed.', [FData.CountNotDeleted] )
  else
    lblFileCount.Caption := Format( ' %d file to be deployed.',  [FData.CountNotDeleted] )
end;

procedure TFormTIDeployChild_App.LVParamsItemEdit(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  TFormEditParams.Execute( pData as TtiDeployParam ) ;
end;

procedure TFormTIDeployChild_App.LVParamsItemInsert(
  pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
var
  lData : TtiDeployParam ;
begin
  lData := TtiDeployParam.CreateNew ;
  if TFormEditParams.Execute( lData ) then
  begin
    FData.DeployParams.Add( lData ) ;
    pLV.Refresh
  end 
  else
    lData.Free ;
end;

procedure TFormTIDeployChild_App.PCChange(Sender: TObject);
begin
  if PC.ActivePage = tsDescription then
    paeIcon.Refresh ;
end;

procedure TFormTIDeployChild_App.sbSaveOneClick(Sender: TObject);
var
  lFile : TtiDeployFile ;
begin
  if LVFiles.Selected = nil then
    Exit ; //==>

  lFile := ( LVFiles.SelectedData as TtiDeployFile ) ;
  if not tiAppConfirmation( 'Are you sure you want to save <' +
                            lFile.DeployFromPathAndName + '>?' ) then
    Exit ; //==>
    
  ReadDeployFromFileDetails( lFile ) ;
  gTIPerMgr.VisMgr.Execute( cgsDeployMgrSave, lFile ) ;
  ReadDeployFromDirty( lFile ) ;

end;

procedure TFormTIDeployChild_App.LVFilesItemArive(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  sbSaveOne.Enabled := TtiDeployFile( pData ).DeployFromDirty ;
  sbSaveOne.Hint := 'Save <' + TtiDeployFile( pData ).DeployFromPathAndName + '>' ;
end;

procedure TFormTIDeployChild_App.LVFilesItemLeave(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  sbSaveOne.Enabled := false ;
end;


procedure TFormTIDeployChild_App.RefreshData;
var
  lData : TtiDeployApp ;
begin
  lData := FData ;
  SetData( nil ) ;
  SetData( lData ) ;
end;

end.

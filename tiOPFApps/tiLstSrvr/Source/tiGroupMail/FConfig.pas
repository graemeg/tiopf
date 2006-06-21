unit FConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, tiPerAwareCtrls, ExtCtrls, tiLstSrvrCfg_BOM,
  tiPerAwareFileCombos, tiListView, tiButtons, tiFocusPanel,
  tiPerAwareCombosAbs, tiPerAwareDirectoryCombos ;

type                             
  TFormConfig = class(TForm)
    pcConfig: TPageControl;
    tsGeneral: TTabSheet;
    tsMailServer: TTabSheet;
    tsMessageText: TTabSheet;
    tsListMembers: TTabSheet;
    pcMessageText: TPageControl;
    tsJoinMessage: TTabSheet;
    tsLeaveMessage: TTabSheet;
    tsRejectMessageSizeMessage: TTabSheet;
    tsRejectNonListMemberMessage: TTabSheet;
    tsRejectAttachmentMessage: TTabSheet;
    paeListName: TtiPerAwareEdit;
    paeListEMailAdrs: TtiPerAwareEdit;
    paeListMasterEMailAdrs: TtiPerAwareEdit;
    paeMaxMessageSize: TtiPerAwareFloatEdit;
    paePOPHost: TtiPerAwareEdit;
    paePOPUserID: TtiPerAwareEdit;
    paePopPassword: TtiPerAwareEdit;
    paeJoinMessage: TtiPerAwareMemo;
    paeLeaveMessage: TtiPerAwareMemo;
    paeRejectMessageSizeMessage: TtiPerAwareMemo;
    paeNonListMemberMessage: TtiPerAwareMemo;
    paeRejectAttachmentMessage: TtiPerAwareMemo;
    tsFooterMessage: TTabSheet;
    paeFooterMessage: TtiPerAwareMemo;
    lblCount: TLabel;
    paeArchiveDir: TtiPerAwarePickDirectory;
    LVMembers: TtiListView;
    mbImport: TtiMicroButton;
    paeListNameLong: TtiPerAwareEdit;
    paeListMasterName: TtiPerAwareEdit;
    paeSMTPHost: TtiPerAwareComboBoxHistory;
    paeSenderAdrsType: TtiPerAwareComboBoxStatic;
    paeAcceptPostsFrom: TtiPerAwareComboBoxStatic;
    paeActive: TtiPerAwareCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure paeListNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LVMembersItemDelete(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVMembersItemEdit(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVMembersItemInsert(pLV: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVMembersFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure LVMembersGetFont(pLV: TtiCustomListView; pCanvas: TCanvas;
      pItem: TListItem; pData: TPersistent);
    procedure LVMembersAfterRefreshData(pLV: TtiCustomListView);
    procedure mbImportClick(Sender: TObject);
    procedure pcConfigChange(Sender: TObject);
  private
    FData : TtiLstSrvrCfg ;
    FTreeNode: TTreeNode;
    function  GetValid: boolean;
    procedure SetData(const Value: TtiLstSrvrCfg);
    procedure DoOnChange( Sender : TObject ) ;
  public
  published
    property  Data : TtiLstSrvrCfg read FData write SetData ;
    property  Valid : boolean read GetValid ;
    property  TreeNode : TTreeNode read FTreeNode write FTreeNode ;
  end;

var
  FormConfig: TFormConfig;

implementation
uses
  tiUtils
  ,FEditListMember
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiDialogs
  ,tiRegINI
  ;

{$R *.DFM}

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( Self ) ;
end;

procedure TFormConfig.paeListNameChange(Sender: TObject);
begin
  FData.Dirty := true ;
  FTreeNode.Text := FData.Caption ;
end;

function TFormConfig.GetValid: boolean;
begin
  result := true ;
end;

procedure TFormConfig.SetData(const Value: TtiLstSrvrCfg);
begin
  FData := Value;
  if FData = nil then
    Exit ; //==>

  paeListName.LinkToData(            FData, 'ListName' ) ;
  paeListEMailAdrs.LinkToData(       FData, 'ListEMailAdrs' ) ;
  paeListMasterEMailAdrs.LinkToData( FData, 'ListMasterEMailAdrs' ) ;
  paeMaxMessageSize.LinkToData(      FData, 'MaxMessageSize' ) ;
  paeListNameLong.LinkToData(        FData, 'ListNameLong' ) ;
  paeListMasterName.LinkToData(      FData, 'ListMasterName' ) ;
  paeSenderAdrsType.LinkToData(      FData,  'SenderAdrsTypeAsString' );
  paeAcceptPostsFrom.LinkToData(     FData,  'AcceptPostsFromAsString' );
  paeActive.LinkToData(              FData,  'Active' ) ;

  paePopHost.LinkToData(     FData, 'POPHost'     ) ;
  paePOPUserID.LinkToData(   FData, 'POPUserID'   ) ;
  paePOPPassword.LinkToData( FData, 'POPPassword' ) ;
  paeSMTPHost.LinkToData(    FData, 'SMTPHost'    ) ;

  paeArchiveDir.LinkToData( FData, 'ArchiveDir'   ) ;

  paeJoinMessage.LinkToData( FData,              'JoinMessage' ) ;
  paeLeaveMessage.LinkToData( FData,             'LeaveMessage' ) ;
  paeRejectMessageSizeMessage.LinkToData( FData, 'RejectMessageSizeMessage' ) ;
  paeNonListMemberMessage.LinkToData( FData,     'NonListMemberMessage' ) ;
  paeRejectAttachmentMessage.LinkToData( FData,  'RejectAttachmentMessage' ) ;
  paeFooterMessage.LinkToData( FData,            'FooterMessage' ) ;

  LVMembers.Data := FData.ListMembers.List ;
  LVMembersAfterRefreshData(nil);

  if FData.ObjectState = posCreate then
  begin
    pcConfig.ActivePage := tsGeneral ;
    pcMessageText.ActivePage := tsJoinMessage ;
  end ;

end;

procedure TFormConfig.FormShow(Sender: TObject);
begin
  if pcConfig.ActivePage = tsGeneral then
    paeListName.SetFocus ;
end;

procedure TFormConfig.FormCreate(Sender: TObject);
var
  i : integer ;
  lSortOrder : TlvSortOrder;
begin
  AssignSenderAdrsTypes(paeSenderAdrsType.Items);
  AssignAcceptPostsFrom(paeAcceptPostsFrom.Items);
  for i := 0 to ComponentCount - 1 do
    if ( Components[i] is TtiPerAwareAbs ) and
       ( not Assigned( TtiPerAwareAbs( Components[i] ).OnChange )) then
      TtiPerAwareAbs( Components[i] ).OnChange := DoOnChange ;
  pcConfig.ActivePage := tsGeneral ;
  pcMessageText.ActivePage := tsJoinMessage ;
  LVMembers.AddColumn('EMailAddress', lvtkString, 'EMail Address');
  lSortOrder := LVMembers.SortOrders.Add;
  lSortOrder.FieldName := 'EMailAddress' ;
  lSortOrder.SortDirection := lvsdAscending;
  LVMembers.ApplySort := true ;
end;

procedure TFormConfig.DoOnChange(Sender: TObject);
begin
  FData.Dirty := true ;
end;

procedure TFormConfig.LVMembersItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete( pData as TPerObjAbs ) then
    FData.Save ;
end;

procedure TFormConfig.LVMembersItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if TFormEditListMember.Execute( pData as TPerObjAbs ) then
    FData.Save ;
end;

procedure TFormConfig.LVMembersItemInsert(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  lData : TListMember ;
begin
  lData := TListMember.CreateNew ;
  if TFormEditListMember.Execute( lData ) then
  begin
    FData.ListMembers.Add( lData );
    FData.Save ;
  end else
    lData.Free ;
end;

procedure TFormConfig.LVMembersFilterData(pData: TPersistent; var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;

procedure TFormConfig.LVMembersGetFont(pLV: TtiCustomListView;
  pCanvas: TCanvas; pItem: TListItem; pData: TPersistent);
begin
  tiPerObjAbsFormatCanvas( pCanvas, TPerObjAbs( pData )) ;
end;

procedure TFormConfig.LVMembersAfterRefreshData(pLV: TtiCustomListView);
begin
  if FData <> nil then
    lblCount.Caption := 'Number of members in list: ' + IntToStr( FData.ListMembers.CountNotDeleted )
  else
    lblCount.Caption := '' ;
end;

procedure TFormConfig.mbImportClick(Sender: TObject);
var
  lOD : TOpenDialog ;
  lAction : string ;
  lCount : integer ;
  i : integer ;
begin
  lOD := TOpenDialog.Create( nil ) ;
  try
    lOD.FileName := gReg.ReadString( Name, 'ImportFileName', '' ) ;
    if lOD.Execute then
    begin
      lAction := tiMessageDlg( 'Do you want to append or overwrite the addresses?',
                               ['Append', 'Overwrite', 'Cancel'] );
      if lAction = 'Cancel' then
        Exit ; //==>
      gReg.WriteString( Name, 'ImportFileName', lOD.FileName ) ;
      lCount := FData.ListMembers.Count ;
      FData.ListMembers.LoadFromFile( lOD.FileName ) ;
      if lAction = 'Overwrite' then
        for i := lCount - 1 downto 0 do
          FData.ListMembers.Items[i].Deleted := true ;
      LVMembers.Refresh ;
    end ;
  finally
    lOD.Free ;
  end ;
end;

procedure TFormConfig.pcConfigChange(Sender: TObject);
begin
  LVMembers.Refresh;
end;

end.

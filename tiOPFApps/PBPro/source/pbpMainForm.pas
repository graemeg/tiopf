{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, Menus, ActnList, ImgList, Buttons,
  StdCtrls, Contnrs,

  jclStrings,

  tiTreeView,

  TB2Item, TB2MDI, TB2Dock, TB2Toolbar, TB2ExtItems, TBSkinPlus,

  pbpTypes, pbpClasses, pbpBusinessClasses, pbpClientListForm, pbpClientForm, pbpUtils,
  pbpContractListForm, pbpContractForm, pbpContractSearchForm, pbpTasksForm,
  pbpPawnbrokerForm, pbpAboutForm, pbpResources;

type
  TtiTreeViewAdapter = class;

  TMainForm = class(TForm)
    GlyfxFree16x16ImageList: TImageList;
    MainActionList: TActionList;
    BackAction: TAction;
    ForwardAction: TAction;
    StatusBar1: TStatusBar;
    ExitAction: TAction;
    SearchAction: TAction;
    TBDock1: TTBDock;
    MenuToolbar: TTBToolbar;
    File1: TTBSubmenuItem;
    Exit1: TTBItem;
    Edit1: TTBItem;
    Favorites1: TTBItem;
    Tools1: TTBItem;
    Help1: TTBSubmenuItem;
    AboutTBItem: TTBItem;
    StandardButtonsToolbar: TTBToolbar;
    BackSubmenuItem: TTBSubmenuItem;
    TBItem3: TTBItem;
    ForwardSubmenuItem: TTBSubmenuItem;
    SearchTBItem: TTBItem;
    FoldersTBItem: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    FoldersAction: TAction;
    TBToolbar3: TTBToolbar;
    QuickFindTBEditItem: TTBEditItem;
    TBSkin: TTBSkin;
    TBControlItem1: TTBControlItem;
    Label1: TLabel;
    TBItem1: TTBItem;
    HomeAction: TAction;
    TBSeparatorItem2: TTBSeparatorItem;
    TreeView: TtiTreeView;
    TBSubmenuItem1: TTBSubmenuItem;
    TBItem2: TTBItem;
    TBItem4: TTBItem;
    AboutAction: TAction;
    procedure BackActionExecute(Sender: TObject);
    procedure ForwardActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure FoldersActionExecute(Sender: TObject);
    procedure MainActionListUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure HomeActionExecute(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure TreeViewGetImageIndex(pData: TObject;
      var piImageIndex: Integer);
    procedure TreeViewAfterGetChildForm(pChildForm: TForm; pData: TObject;
      pNode: TTreeNode);
    procedure ForwardSubmenuItemPopup(Sender: TTBCustomItem;
      FromLink: Boolean);
    procedure AboutActionExecute(Sender: TObject);
    procedure QuickFindTBEditItemAcceptText(Sender: TObject;
      var NewText: String; var Accept: Boolean);
  private
    FTreeViewAdapter: TtiTreeViewAdapter;
  protected
    procedure Read;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property TreeViewAdapter: TtiTreeViewAdapter read FTreeViewAdapter;
  end;

  TSidebarStyle = (ssNone, ssDefault, ssFolders, ssSearch, ssTasks);

  TtiTreeViewAdapter = class(TObject)
  private
    FContractSearchForm: TContractSearchForm;
    FCurrentChildForm: TForm;
    FChildFormGoBackStack: TFormStack;
    FChildFormGoForwardStack: TFormStack;
    FTasksForm: TTasksForm;
    FInternalSplitter: TtiTVSplitter;
    FInternalTreeView: TTreeView;
    FTreeView: TtiTreeView;
    FSidebarStyle: TSidebarStyle;
    procedure InternalSetCurrentChildForm(const Value: TForm);
    procedure HandleTreeViewOnClick(Sender: TObject);
    procedure SetTreeView(const Value: TtiTreeView);
    procedure SetSidebarStyle(const Value: TSidebarStyle);
    function GetCurrentChildForm: TForm;
    procedure SetCurrentChildForm(const Value: TForm);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure GoBack;
    procedure GoForward;
    property CurrentChildForm: TForm read GetCurrentChildForm write SetCurrentChildForm;
    property BackStack: TFormStack read FChildFormGoBackStack;
    property ForwardStack: TFormStack read FChildFormGoForwardStack;
    property TreeView: TtiTreeView read FTreeView write SetTreeView;
    property SidebarStyle: TSidebarStyle read FSidebarStyle write SetSidebarStyle;
  end;

var
  MainForm: TMainForm;

implementation

uses
  pbpContractCreationWizardForm;

{$R *.DFM}

{ TExplorerMain }

procedure TMainForm.AfterConstruction;
begin
  inherited;

  // Setup action list
  MainActionList.Images := GetResourceImageList24x24;
  BackAction.ImageIndex    := GLYFX_24_BACK;
  ForwardAction.ImageIndex := GLYFX_24_FORWARD;
  HomeAction.ImageIndex    := GLYFX_24_HOME;
  SearchAction.ImageIndex  := GLYFX_24_SEARCH;
  FoldersAction.ImageIndex := GLYFX_24_FOLDERS;
  
  StandardButtonsToolbar.Images := GetResourceImageList24x24;

  FTreeViewAdapter := TtiTreeViewAdapter.Create;
  FTreeViewAdapter.TreeView := TreeView;
  FTreeViewAdapter.SidebarStyle := ssFolders;

  TreeView.RegisterChildForm(TClientList, TClientListForm, False);
  TreeView.RegisterChildForm(TContractList, TContractListForm, False);
  TreeView.RegisterChildForm(TPawnBroker, TPawnbrokerForm, False);

  TreeView.SplitterPos := 120;

  Read;
  TreeView.Data := Pawnbroker;

  Caption := Format('PB Pro - %s', [PawnBroker.Name]);
end;

{ TExplorerMain - actions }

procedure TMainForm.BackActionExecute(Sender: TObject);
begin
  FTreeViewAdapter.GoBack;
end;

procedure TMainForm.ForwardActionExecute(Sender: TObject);
begin
  FTreeViewAdapter.GoForward;
end;

procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FoldersActionExecute(Sender: TObject);
begin
  if FTreeViewAdapter.SidebarStyle <> ssFolders then
    FTreeViewAdapter.SidebarStyle := ssFolders
  else
    FTreeViewAdapter.SidebarStyle := ssDefault;
end;

procedure TMainForm.SearchActionExecute(Sender: TObject);
begin
  if FTreeViewAdapter.SidebarStyle <> ssSearch then
    FTreeViewAdapter.SidebarStyle := ssSearch
  else
    FTreeViewAdapter.SidebarStyle := ssDefault;
end;

procedure TMainForm.Read;
begin
  PawnBroker.SelfIterate := False;
  PawnBroker.Read;

  PawnBroker.ContractPaymentTypes.Read;
  PawnBroker.SystemValues.Read;
  PawnBroker.ClientIdentityRecordTypes.Read;
  PawnBroker.ContractItemCategories.Read;
  PawnBroker.Manufacturers.Read;

  PawnBroker.Clients.ReadPK;

{ TODO -oTT -cAssumption :
This assumes that contract oids and numbers are in the same order. It
would be better to use a 'sortbycontractnumber' algorithm here. }
  PawnBroker.Contracts.SortByOID;
end;

procedure TMainForm.MainActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  case CaseObject(Action, [BackAction, ForwardAction]) of
    0: BackAction.Enabled := FTreeViewAdapter.BackStack.Count > 0;
    1: ForwardAction.Enabled := FTreeViewAdapter.ForwardStack.Count > 0;
  end;

  FoldersAction.Checked := FTreeViewAdapter.SidebarStyle = ssFolders;
  SearchAction.Checked  := FTreeViewAdapter.SidebarStyle = ssSearch;
end;

procedure TMainForm.HomeActionExecute(Sender: TObject);
begin
  TreeView.SelectedData := Pawnbroker;
end;

{ TtiTreeViewAdapter }

procedure TtiTreeViewAdapter.AfterConstruction;
begin
  inherited;
  FChildFormGoBackStack := TFormStack.Create;
  FChildFormGoForwardStack := TFormStack.Create;

  FContractSearchForm := TContractSearchForm.Create(nil);
  FContractSearchForm.Align := alLeft;

  FTasksForm := TTasksForm.Create(nil);
  FTasksForm.Align := alLeft;
end;

procedure TtiTreeViewAdapter.BeforeDestruction;
begin
  FContractSearchForm.Visible := False;
  FContractSearchForm.Parent := nil;
  FContractSearchForm.Free;

  FTasksForm.Visible := False;
  FTasksForm.Parent := nil;
  FTasksForm.Free;

  FChildFormGoBackStack.Free;
  FChildFormGoForwardStack.Free;
  inherited;
end;

function TtiTreeViewAdapter.GetCurrentChildForm: TForm;
begin
  if FCurrentChildForm <> nil then
    Result := FCurrentChildForm
  else
    Result := FTreeView.CurrentChildForm;
end;

procedure TtiTreeViewAdapter.GoBack;
var
  ChildForm: TForm;
begin
  if FChildFormGoBackStack.Count > 0 then
  begin
    ChildForm := FChildFormGoBackStack.Pop;
    if Assigned(ChildForm) then
    begin
      if Assigned(FCurrentChildForm) then
        FChildFormGoForwardStack.Push(FCurrentChildForm);
      InternalSetCurrentChildForm(ChildForm);
    end;
  end;
end;

procedure TtiTreeViewAdapter.GoForward;
var
  ChildForm: TForm;
begin
  ChildForm := FChildFormGoForwardStack.Pop;
  if Assigned(ChildForm) then
  begin
    if Assigned(FCurrentChildForm) then
      FChildFormGoBackStack.Push(FCurrentChildForm);
    InternalSetCurrentChildForm(ChildForm);
  end;
end;

procedure TtiTreeViewAdapter.HandleTreeViewOnClick(Sender: TObject);
begin
  if FTreeView.CurrentChildForm <> nil then
  begin
    Self.CurrentChildForm := FTreeView.CurrentChildForm;
  end;
end;


procedure TtiTreeViewAdapter.InternalSetCurrentChildForm(
  const Value: TForm);
var
  SupportsToolbar2000: ISupportsToolbar2000;
begin
  if FCurrentChildForm <> nil then
  begin
    FCurrentChildForm.Visible := False;

    if Supports( FCurrentChildForm, IID_SupportsToolbar2000, SupportsToolbar2000) then
    begin
      SupportsToolbar2000.SetToolbarParent(nil);
    end;
  end;

  FCurrentChildForm := Value;

  if FCurrentChildForm <> nil then
  begin
    FCurrentChildForm.Parent := FTreeView;
    FCurrentChildForm.Visible := True;
    FCurrentChildForm.Align := alClient;
    FCurrentChildForm.BringToFront;

    if Supports(FCurrentChildForm, IID_SupportsToolbar2000, SupportsToolbar2000) then
    begin
      SupportsToolbar2000.SetToolbarParent(MainForm.StandardButtonsToolbar);
    end;
  end;
end;

procedure TtiTreeViewAdapter.SetCurrentChildForm(const Value: TForm);
var
  Counter: Integer;
begin
  if FCurrentChildForm <> Value then
  begin
    if Assigned(FCurrentChildForm) then
    begin
{ TODO -oTT -cReview : 
Again, this could probably done with a test for a particular interface
(e.g. IStackableForm or similar) }
      if not (FCurrentChildForm is TContractCreationWizardForm) then
      begin
        FChildFormGoBackStack.Push(FCurrentChildForm);
      end;
    end;

    InternalSetCurrentChildForm(Value);

    // Clear GoForwardStack
    for Counter := 1 to FChildFormGoForwardStack.Count do
      FChildFormGoForwardStack.Pop;
  end
  else
  begin
    if FCurrentChildForm <> nil then
      FCurrentChildForm.Refresh;
  end;
end;

procedure TtiTreeViewAdapter.SetSidebarStyle(const Value: TSidebarStyle);
begin
  FSidebarStyle := Value;
  case FSidebarStyle of
    ssNone:
      begin
        FContractSearchForm.Visible := False;
        FTreeView.SplitterVisible := False;
        FInternalTreeView.Visible := False;
      end;

    ssDefault:
      begin
{ TODO -oTT -cFixThis : Dodge, just to get this running }      
        SetSidebarStyle(ssTasks);
      end;

    ssFolders:
      begin
        FInternalTreeView.Visible := True;
        FTreeView.SplitterVisible := True;

        FContractSearchForm.Visible := False;
        FTasksForm.Visible := False;
      end;

    ssSearch:
      begin
        FInternalSplitter.Align := alRight;
        FContractSearchForm.Visible := True;
        FTreeView.SplitterVisible := True;

        FInternalTreeView.Visible := False;
        FTasksForm.Visible := False;
        FInternalSplitter.Align := alLeft;
      end;

    ssTasks:
      begin
        FInternalSplitter.Align := alRight;
        FTasksForm.Visible := True;
        FTreeView.SplitterVisible := True;

        FContractSearchForm.Visible := False;
        FInternalTreeView.Visible := False;
        
        FInternalSplitter.Align := alLeft;
      end;
  end;
end;

procedure TtiTreeViewAdapter.SetTreeView(const Value: TtiTreeView);
var
  Counter: Integer;
begin
  FTreeView := Value;
  if FTreeView <> nil then
  begin
    // Hijack internal TTreeView
    FInternalTreeView := TTreeView(FTreeView.FindComponent(FTreeView.Name + '_TV'));
    if FInternalTreeView <> nil then
      FInternalTreeView.BorderStyle := bsNone;
    FInternalTreeView.OnClick := HandleTreeViewOnClick;

    // Hijack internal TtiSplitter
    for Counter := 0 to FTreeView.ComponentCount-1 do
    begin
      if FTreeView.Components[Counter].InheritsFrom(TtiTVSplitter) then
      begin
        FInternalSplitter := FTreeView.Components[Counter] as TtiTVSplitter;
        Break;
      end;
    end;

    Assert(FInternalSplitter <> nil);

    // Change Z-order of controls
 //   FInternalSplitter.Visible := False;
//    FInternalSplitter.Parent := nil;
    FContractSearchForm.Parent := FTreeView;
    FTasksForm.Parent := FTreeView;
  end
  else
  begin
    FInternalTreeView := nil;
  end;
end;

procedure TMainForm.BeforeDestruction;
begin
  FTreeViewAdapter.Free;
  inherited;
end;

procedure TMainForm.TreeViewGetImageIndex(pData: TObject;
  var piImageIndex: Integer);
begin
  piImageIndex := 0;
end;

procedure TMainForm.TreeViewAfterGetChildForm(pChildForm: TForm;
  pData: TObject; pNode: TTreeNode);
begin
  FTreeViewAdapter.CurrentChildForm := pChildForm;
end;

procedure TMainForm.ForwardSubmenuItemPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  Counter: Integer;
  Item: TTBCustomItem;
  Form: TForm;
begin
  Sender.Clear;
  for Counter := 0 to Pred(FTreeViewAdapter.ForwardStack.Count) do
  begin
    Item := TTBCustomItem.Create(nil);
    Sender.Add(TTBCustomItem.Create(Sender));
    Form := FTreeViewAdapter.ForwardStack.Items[Counter];
    Item.DisplayMode := nbdmTextOnly;
    Item.Caption := Form.Caption;
  end;  
end;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
  begin
    try
      Execute;
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.QuickFindTBEditItemAcceptText(Sender: TObject;
  var NewText: String; var Accept: Boolean);
var
  Contract: TContract;
  ContractNumber: Integer;
  ContractForm: TContractForm;
begin
  

  if StrIsDigit(NewText) then
  begin
    ContractNumber := StrToInt(NewText);
    Contract := PawnBroker.Contracts.FindByProps(['ContractNumber'], [ContractNumber]);
{ TODO -oPBPRO -cSMELL : 
This kind of behaviour should be moved to a common controller. The
same sort of code appears in pbpContractListForm }    
    if Assigned(Contract) then
    begin
      ContractForm := TContractForm.Create(Application);
      TreeViewAdapter.CurrentChildForm := ContractForm;
      ContractForm.Data := Contract;
    end
    else
    begin
      MessageDlg('Sorry, there is no Contract # ' + NewText + ' found in the database',
        mtError, [mbOK], 0);
    end;
  end;
end;

end.

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
unit pbpContractListForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Comctrls, StdCtrls, ImgList, ActnList,

  jclStrings,

  tiListView, tiPtnVisPerObj, tiPersist,

  TB2Item, TB2Dock, TB2Toolbar,

  pbpTypes, pbpBusinessClasses, pbpContractForm, pbpContractCreationWizardForm;


type
  TContractListForm = class(TForm, IUnknown, ISupportsToolbar2000, IPerObjListListener)
    ContractListView: TtiListView;
    SearchResultPanel: TPanel;
    SearchResultLabel: TLabel;
    TBDock: TTBDock;
    ContractListFormToolbar: TTBToolbar;
    TBItem5: TTBItem;
    ActionList1: TActionList;
    NewContractAction: TAction;
    TBImageList1: TTBImageList;
    procedure ContractListViewDblClick(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure ContractListViewFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure ContractListViewKeyPress(Sender: TObject; var Key: Char);
    procedure NewContractActionExecute(Sender: TObject);
  private
    FData: TPersistent;
    FFilter_States: TContractStates;
    FFilter_ClientName: string;
    FTreeNode: TTreeNode;
    procedure SetData(const Value: TPersistent);
    function GetFiltered: Boolean;
    procedure SetFiltered(const Value: Boolean);
    function GetValid: boolean;
    procedure SetSelectedContract(const Value: TContract);
  protected
    { IPerObjListListener }
    procedure ItemAdded(AObject: TPerObjAbs);
    procedure ItemDeleted(AObject: TPerObjAbs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetToolbarParent(Parent: TWinControl);
    property Filtered: Boolean read GetFiltered write SetFiltered;
{ TODO -oTT -cReview : These should be moved out to a general purpose 'filter' object at some stage }
    property Filter_States: TContractStates read FFilter_States write FFilter_States;
    property Filter_ClientName: string read FFilter_ClientName write FFilter_ClientName;
    property SelectedContract: TContract write SetSelectedContract;
  published
    property Data: TPersistent read FData write SetData;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid    : boolean   read GetValid ;
  end;

var
  ContractListForm: TContractListForm;

implementation

uses pbpMainForm;

{$R *.DFM}

{ TContractListForm }

procedure TContractListForm.AfterConstruction;
begin
  inherited;
  Filtered := False;
end;

procedure TContractListForm.SetData(const Value: TPersistent);
begin
  if FData <> nil then
  begin
    (FData as TContractList).RemoveListener(Self);
  end;

  FData := Value;
  if FData <> nil then
  begin
    Assert(Value is TContractList);
    ContractListView.Data := (FData as TContractList).List;
    (FData as TContractList).AddListener(Self);
  end
  else
    ContractListView.Data := nil;
end;

function TContractListForm.GetFiltered: Boolean;
begin
  Result := ContractListView.ApplyFilter;
end;

procedure TContractListForm.SetFiltered(const Value: Boolean);
begin
  ContractListView.ApplyFilter := True;
  ContractListView.Refresh;
  SearchResultPanel.Visible := Value;
end;

procedure TContractListForm.ContractListViewDblClick(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  Assert(pData is TContract);
  SelectedContract := pData as TContract;
end;

procedure TContractListForm.ContractListViewFilterData(pData: TPersistent;
  var pbInclude: Boolean);
var
  Contract: TContract;
  Counter: Integer;
  ClientNameStr: string;
  ClientNameStrings: TStrings;
begin
  Contract := pData as TContract;
  pbInclude := True;

  // Filter on ContractStates
  if pbInclude then
  begin
    pbInclude := pbInclude and ((FFilter_States = []) or (Contract.ContractState in FFilter_States));
  end;

  // Filter on ClientName
  ClientNameStr := Uppercase(FFilter_ClientName);
  if pbInclude and (ClientNameStr <> '') then
  begin
    if Contract.ObjectState = posPK then
      gTiPerMgr.VisMgr.Execute('read.contractClient', Contract);
    if Contract.Client <> nil then
    begin
      ClientNameStrings := TStringList.Create;
      try
        StrToStrings(ClientNameStr, AnsiSpace, ClientNameStrings);
        for Counter := 0 to Pred(ClientNameStrings.Count) do
        begin
          pbInclude := pbInclude and
            ( (Pos(ClientNameStrings[Counter], Uppercase(Contract.Client.FamilyName)) = 1) or
              (Pos(ClientNameStrings[Counter], Uppercase(Contract.Client.GivenNames)) = 1)
            );
          if pbInclude then
            Break;  
        end;
      finally
        ClientNameStrings.Free;
      end;
//
//      pbInclude := pbInclude and
//        ( (Pos(ClientNameStr, Uppercase(Contract.Client.FamilyName)) = 1) or
//          (Pos(ClientNameStr, Uppercase(Contract.Client.GivenNames)) = 1)
//        );
    end
    else
      pbInclude := False;
  end;
end;

function TContractListForm.GetValid: boolean;
begin
  Result := True;
end;

procedure TContractListForm.ContractListViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = AnsiCarriageReturn then
  begin
    SelectedContract := ContractListView.SelectedData as TContract;
  end;
end;

procedure TContractListForm.SetSelectedContract(const Value: TContract);
var
  ContractForm: TContractForm;
begin
  ContractForm := TContractForm.Create(Application);
  MainForm.TreeViewAdapter.CurrentChildForm := ContractForm;
  ContractForm.Data := Value;
end;

procedure TContractListForm.SetToolbarParent(Parent: TWinControl);
begin
  if Parent <> nil then
    ContractListFormToolbar.Parent := Parent
  else
    ContractListFormToolbar.Parent := TBDock;
end;

procedure TContractListForm.NewContractActionExecute(Sender: TObject);
var
  WizardForm: TContractCreationWizardForm;
begin
  MainForm.TreeViewAdapter.BackStack.Push(Self);

  WizardForm := TContractCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;

procedure TContractListForm.ItemAdded(AObject: TPerObjAbs);
begin
  ContractListView.Refresh(True);
end;

procedure TContractListForm.ItemDeleted(AObject: TPerObjAbs);
begin
  ContractListView.Refresh(True);
end;

procedure TContractListForm.BeforeDestruction;
begin
  Data := nil;
  inherited;
end;

end.

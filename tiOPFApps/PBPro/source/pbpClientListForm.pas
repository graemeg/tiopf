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
unit pbpClientListForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Comctrls, ImgList, StdCtrls, ActnList,

  jclStrings,

  tiListView, tiPtnVisPerObj,

  TB2Item, TB2Dock, TB2Toolbar,

  pbpTypes, pbpBusinessClasses, pbpClientForm, pbpClientCreationWizardForm, pbpResources;

type
  TClientListForm = class(TForm, IUnknown, ISupportsToolbar2000, IPerObjListListener)
    ClientListView: TtiListView;
    SearchResultPanel: TPanel;
    SearchResultLabel: TLabel;
    TBDock: TTBDock;
    ClientListFormToolbar: TTBToolbar;
    TBItem5: TTBItem;
    ActionList: TActionList;
    NewClientAction: TAction;
    procedure ClientListViewListColumns3DeriveColumn(
      const pLV: TtiCustomListView; const pData: TPersistent;
      const ptiListColumn: TtiListColumn; var pResult: String);
    procedure ClientListViewDblClick(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure NewClientActionExecute(Sender: TObject);
  private
    FData: TPersistent;
    FTreeNode: TTreeNode;
    procedure SetData(const Value: TPersistent);
    function GetFiltered: Boolean;
    function GetValid: boolean;
    procedure SetFiltered(const Value: Boolean);
  protected
    { IPerObjListListener }
    procedure ItemAdded(AObject: TPerObjAbs);
    procedure ItemDeleted(AObject: TPerObjAbs);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetToolbarParent(Parent: TWinControl);
    property Filtered: Boolean read GetFiltered write SetFiltered;
  published
    property Data: TPersistent read FData write SetData;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid    : boolean   read GetValid ;
  end;

var
  ClientListForm: TClientListForm;

implementation

uses pbpMainForm;

{$R *.DFM}

procedure TClientListForm.BeforeDestruction;
begin
  Data := nil;
  inherited;
end;

procedure TClientListForm.ItemAdded(AObject: TPerObjAbs);
begin
  ClientListView.Refresh;
end;

procedure TClientListForm.ItemDeleted(AObject: TPerObjAbs);
begin
  ClientListView.Refresh;
end;

procedure TClientListForm.ClientListViewListColumns3DeriveColumn(
  const pLV: TtiCustomListView; const pData: TPersistent;
  const ptiListColumn: TtiListColumn; var pResult: String);
var
  Client: TClient;
begin
  Client := pData as TClient;
  pResult := BooleanToStr(Client.Undesirable);
end;

procedure TClientListForm.SetData(const Value: TPersistent);
begin
  if Assigned(FData) then
  begin
    (FData as TClientList).RemoveListener(Self);
  end;

  FData := Value;
  if FData <> nil then
  begin
    ClientListView.Data := (FData as TClientList).List;
    (FData as TClientList).AddListener(Self);
  end
  else
  begin
    ClientListView.Data := nil;
  end;
end;

procedure TClientListForm.ClientListViewDblClick(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  ClientForm: TClientForm;
  Client: TClient;
begin
  Assert(pData is TClient);

  Client := pData as TClient;
{ TODO -oTT -cReview : 
There should be a factory in here to create client forms where appropriate
(or reuse a singleton client form) }
  ClientForm := TClientForm.Create(Application);
  MainForm.TreeViewAdapter.CurrentChildForm := ClientForm;
  ClientForm.Data := Client;
end;

function TClientListForm.GetFiltered: Boolean;
begin
{ TODO -oTT -cFixThis : Dodge, just to get this running }
  Result := SearchResultPanel.Visible;
end;

function TClientListForm.GetValid: boolean;
begin
  Result := True;
end;

procedure TClientListForm.SetFiltered(const Value: Boolean);
begin
  SearchResultPanel.Visible := Value;
end;

procedure TClientListForm.AfterConstruction;
begin
  inherited;
  // Setup action list
  ActionList.Images := GetResourceImageList24x24;
  NewClientAction.ImageIndex := GLYFX_24_NEW;
  // Setup toolbar
  ClientListFormToolbar.Images := GetResourceImageList24x24;

  Filtered := False;
end;

procedure TClientListForm.SetToolbarParent(Parent: TWinControl);
begin
  if Parent <> nil then
    ClientListFormToolbar.Parent := Parent
  else
    ClientListFormToolbar.Parent := TBDock;
end;

{ TClientListForm - events - actions }

procedure TClientListForm.NewClientActionExecute(Sender: TObject);
var
  WizardForm: TClientCreationWizardForm;
begin
  WizardForm := TClientCreationWizardForm.Create(Application);
  WizardForm.Execute;
end;





end.

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
unit pbpContractSearchForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, ComCtrls, Buttons, ExtCtrls,

  jclStrings,

  pbpContractListForm, pbpBusinessClasses;

type
  TContractSearchForm = class(TForm)
    SearchCriteriaPanel: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SearchNowButton: TSpeedButton;
    SearchHeaderPanel: TPanel;
    CloseButton: TSpeedButton;
    SearchAnimate: TAnimate;
    ContractStatesPanel: TPanel;
    RedeemedCheckBox: TCheckBox;
    ActiveStateCheckBox: TCheckBox;
    ExpiredCheckBox: TCheckBox;
    DeletedCheckBox: TCheckBox;
    ActionList: TActionList;
    SearchNowAction: TAction;
    CloseAction: TAction;
    ClientNameEdit: TEdit;
    procedure SearchNowActionExecute(Sender: TObject);
    procedure ClientNameEditKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
  private
  public
    procedure AfterConstruction; override;
  end;

var
  ContractSearchForm: TContractSearchForm;

implementation

uses pbpMainForm;

{$R *.DFM}

procedure TContractSearchForm.AfterConstruction;
begin
  inherited;
  ClientNameEdit.Text := '';
end;

procedure TContractSearchForm.SearchNowActionExecute(Sender: TObject);
var
  SearchResultsForm: TContractListForm;
begin
  SearchAnimate.Active := True;
  Screen.Cursor := crHourglass;
  try
    SearchResultsForm := TContractListForm.Create(Self);
    SearchResultsForm.Name := '';
    SearchResultsForm.Visible := False;
    SearchResultsForm.Parent := nil;
    SearchResultsForm.Align := alClient;

    // Set ClientName
    SearchResultsForm.Filter_ClientName := ClientNameEdit.Text;

    // Set ContractStates
    SearchResultsForm.Filter_States := [];
    if ActiveStateCheckBox.Checked then
      SearchResultsForm.Filter_States := [csActive];
    if RedeemedCheckBox.Checked then
      SearchResultsForm.Filter_States := SearchResultsForm.Filter_States + [csRedeemed];
    if ExpiredCheckBox.Checked then
      SearchResultsForm.Filter_States := SearchResultsForm.Filter_States + [csExpired];
    if DeletedCheckBox.Checked then
      SearchResultsForm.Filter_States := SearchResultsForm.Filter_States + [csDeleted];

    SearchResultsForm.Data := PawnBroker.Contracts;
    SearchResultsForm.Filtered := True;

    MainForm.TreeViewAdapter.CurrentChildForm := SearchResultsForm;
  finally
    SearchAnimate.Active := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TContractSearchForm.ClientNameEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = AnsiCarriageReturn then
  begin
    SearchNowActionExecute(Sender);
    Abort;
  end;
end;

procedure TContractSearchForm.FormShow(Sender: TObject);
begin
  ClientNameEdit.SetFocus;
end;

procedure TContractSearchForm.CloseActionExecute(Sender: TObject);
begin
{ TODO -oTT -cReview : Again, I'm concerned that the sidebars need to know about the main form }
  MainForm.TreeViewAdapter.SidebarStyle := ssDefault
end;

end.

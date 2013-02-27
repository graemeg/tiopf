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
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    Edit a SQLManager group name

  Classes:
    TFormChildGroup

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FChildGroup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiPtnVisPerObj,
  tiSQLMgr_BOM, Buttons, Spin, ComCtrls ;

type
  TFormChildGroup = class(TForm)
    eGroupName: TEdit;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure eGroupNameChange(Sender: TObject);
  private
    FData: TSQLMgrGroup;
    FTreeNode: TTreeNode;
    function GetValid: boolean;
    procedure SetData(const Value: TSQLMgrGroup);
  published
    property Data : TSQLMgrGroup read FData write SetData ;
    property Valid : boolean    read GetValid ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
  public
  end;

implementation
uses
  FMainSQLManager
  ,tiUtils
  ,tiGUISupport
  ;

{$R *.DFM}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormChildGroup
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TFormChildGroup.FormShow(Sender: TObject);
begin
  inherited;
  if FData.ObjectState = posCreate then begin
    eGroupName.SetFocus ;
    eGroupName.SelectAll ;
  end ;
end;

procedure TFormChildGroup.eGroupNameChange(Sender: TObject);
begin
  inherited;
  FData.GroupName := eGroupName.Text ;
  FTreeNode.Text  := eGroupName.Text ;
  FData.Dirty     := true ;
end;

function TFormChildGroup.GetValid: boolean;
begin
  result := false ;

  if not tiMustHaveVal( eGroupName, 'Please enter a group name' ) then
    Exit ; //==>

  if not FData.Owner.IsGroupNameUnique( FData ) then
    Exit ; //==>

  result := true ;

end;

procedure TFormChildGroup.SetData(const Value: TSQLMgrGroup);
begin
  FData := Value;
  if FData = nil then
    Exit ; //==>

  eGroupName.OnChange := nil ;
  try
    eGroupName.Text   := FData.GroupName ;
  finally
    eGroupName.OnChange := eGroupNameChange ;
  end ;

end;

end.


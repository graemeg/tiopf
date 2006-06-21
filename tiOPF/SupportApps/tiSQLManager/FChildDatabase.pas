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
    Select the database and persistence package to use

  Classes:
    TFormChildDatabase - The form

  ToDo:
    This functionality has not been implemented yet.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FChildDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiPtnVisPerObj,
  tiSQLMgr_BOM, ComCtrls ;

type
  TFormChildDatabase = class(TForm)
    eDatabaseName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eUserName: TEdit;
    ePassword: TEdit;
    Label3: TLabel;
  private
    FData: TSQLMgr;
    FTreeNode: TTreeNode;
    function GetValid: boolean;
    procedure SetData(const Value: TSQLMgr);
  published
    property Data : TSQLMgr read FData write SetData ;
    property Valid : boolean    read GetValid ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
  public
  end;

implementation
uses
  FMainSQLManager
  ;

{$R *.DFM}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormChildDatabase
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TFormChildDatabase.GetValid: boolean;
begin
  result := true ;
end;

//function TFormChildDatabase.LocalData: TSQLMgr;
//begin
//  result := TSQLMgr( Data ) ;
//end;

{
procedure TFormChildDatabase.SaveForm ;
begin
  inherited ;
  LocalData.DatabaseName := eDatabaseName.Text ;
  LocalData.UserName     := eUserName.Text     ;
  LocalData.Password     := ePassword.Text     ;
end ;
}
procedure TFormChildDatabase.SetData(const Value: TSQLMgr);
begin
  FData := Value;
  if Value = nil then
    Exit ; //==>
//  eDatabaseName.Text := FData.DatabaseName ;
//  eUserName.Text     := FData.UserName ;
//  ePassword.Text     := FData.Password ;
end;

end.


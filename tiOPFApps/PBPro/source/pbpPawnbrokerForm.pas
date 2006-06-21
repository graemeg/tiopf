{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd

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
unit pbpPawnbrokerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Comctrls,

  pbpBusinessClasses;

type
  TPawnbrokerForm = class(TForm)
  private
    FData: TPawnBroker;
    FTreeNode: TTreeNode;
    function GetData: TPawnBroker;
    function GetValid: boolean;
    procedure SetData(const Value: TPawnBroker);
  public
  published
    // These published properties are required by the TtiTreeViewPlus
    property Data : TPawnBroker read GetData write SetData ;
    property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    property Valid    : boolean   read GetValid ;
  end;

var
  PawnbrokerForm: TPawnbrokerForm;

implementation

{$R *.DFM}

{ TPawnbrokerForm }

function TPawnbrokerForm.GetData: TPawnBroker;
begin
  Result := FData;
end;

function TPawnbrokerForm.GetValid: boolean;
begin
  Result := True;
end;

procedure TPawnbrokerForm.SetData(const Value: TPawnBroker);
begin
  FData := Value;
end;

end.

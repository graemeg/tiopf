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
unit pbpClientFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, ComCtrls, ExtCtrls,

  jclStrings,

  tiPtnVisPerObj,

  pbpBusinessClasses, pbpUtils;

type
  TClientFrame = class(TFrame)
    ClientNameEdit: TEdit;
    ClientNumberEdit: TEdit;
    Label2: TLabel;
    Name: TLabel;
    Label1: TLabel;
    StreetEdit: TEdit;
    SuburbEdit: TEdit;
    StateEdit: TEdit;
    PostCodeEdit: TEdit;
    Label21: TLabel;
    Label20: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    PhoneHomeEdit: TEdit;
    PhoneWorkEdit: TEdit;
    Label3: TLabel;
    Label11: TLabel;
    EmailAddressEdit: TEdit;
    PhoneMobileEdit: TEdit;
    UndesirableCheckBox: TCheckBox;
    Bevel4: TBevel;
    Label40: TLabel;
    Label29: TLabel;
    UndesirableRichEdit: TRichEdit;
    UndesirableCodeEdit: TEdit;
    Bevel3: TBevel;
    Label15: TLabel;
    DateOfBirthDateTimePicker: TDateTimePicker;
    Label4: TLabel;
    UndesirableNoticeLabel: TLabel;
    procedure ClientNameEditExit(Sender: TObject);
    procedure UndesirableCheckBoxClick(Sender: TObject);
  private
    FData: TClient;
    function GetData: TClient;
    procedure SetData(const Value: TClient);
    procedure UpdateClientName;
    procedure UpdateClientUndesirableStatus;
    procedure UpdateClientAddress;
    procedure UpdateClientMiscellaneousDetails;
    function GetValid: Boolean;
  public
    procedure UpdateData;
    property Data: TClient read GetData write SetData;
    property Valid: Boolean read GetValid;
  end;

implementation

{$R *.DFM}

{ TClientFrame }

function TClientFrame.GetData: TClient;
begin
  UpdateData;
  Result := FData;
end;

function TClientFrame.GetValid: Boolean;
begin
  if Assigned(FData) then
  begin
    UpdateClientName;
    Result := FData.FamilyName <> '';
  end
  else
  begin
    Result := False;
  end;
end;

procedure TClientFrame.SetData(const Value: TClient);

  procedure ClearAddressControls;
  begin
    StreetEdit.Clear;
    SuburbEdit.Clear;
    StateEdit.Clear;
    PostCodeEdit.Clear;
  end;

begin
  FData := Value;

  if Assigned(FData) then
  begin
    if FData.ObjectState = posPK then
      FData.Read;

    ClientNumberEdit.Text := IntToStr(FData.ClientNumber);
    ClientNameEdit.Text := FData.Caption;
    if FData.ObjectState = posCreate then
    begin
      ClientNameEdit.ReadOnly := False;
      ClientNameEdit.Color := clWindow;
      ClientNameEdit.TabStop := True;
    end
    else
    begin
      ClientNameEdit.ReadOnly := True;
      ClientNameEdit.ParentColor := True;
      ClientNameEdit.TabStop := False;
    end;

    DateOfBirthDateTimePicker.Date := FData.DateOfBirth;

    PhoneHomeEdit.Text := FData.PhoneHome;
    PhoneMobileEdit.Text := FData.PhoneMobile;
    PhoneWorkEdit.Text := FData.PhoneWork;
    EmailAddressEdit.Text := FData.EmailAddress;

    if Assigned(FData.CurrentAddress) then
    begin
      StreetEdit.Text := FData.CurrentAddress.Street;
      SuburbEdit.Text := FData.CurrentAddress.Suburb;
      StateEdit.Text := FData.CurrentAddress.State;
      PostCodeEdit.Text := FData.CurrentAddress.PostCode;
    end
    else
    begin
      ClearAddressControls;
    end;
{ TODO -oPBPRO -cSMELL :
Turn off UndesirableCheckBox.OnClick event to prevent FData.UndesirableCode and .UndesirableReason
being updated with the UndesirableCodeEdit. and UndesirableRichEdit.Text values as a side-effect. }
    UndesirableCheckBox.OnClick := nil;
    try
      UndesirableCheckBox.Checked := FData.Undesirable;
      UndesirableNoticeLabel.Visible := FData.Undesirable;
      UndesirableCodeEdit.Text := FData.UndesirableCode;
      UndesirableRichEdit.Text := FData.UndesirableReason;
    finally
      UndesirableCheckBox.OnClick := UndesirableCheckBoxClick;
    end;

    if FData.ObjectState = posCreate then
      ClientNameEdit.SetFocus
    else
      DateOfBirthDateTimePicker.SetFocus;
  end
  else
  begin
    ClientNumberEdit.Clear;
    ClientNameEdit.Clear;

    ClearAddressControls;

    PhoneHomeEdit.Clear;
    PhoneWorkEdit.Clear;
    PhoneMobileEdit.Clear;
    EmailAddressEdit.Clear;

    UndesirableCodeEdit.Clear;
    UndesirableRichEdit.Lines.Clear;

    UndesirableNoticeLabel.Visible := False;
  end;
end;

procedure TClientFrame.UpdateClientAddress;
var
  Counter: Integer;
  Address: TClientAddress;
begin
  FData.CurrentAddress := nil;
  // Search for pre-exisiting address
  for Counter := FData.Addresses.Count-1 downto 0 do
  begin
    Address := FData.Addresses.Items[Counter];
    if (AnsiCompareText(Address.Street,   StreetEdit.Text) = 0) and
       (AnsiCompareText(Address.Suburb,   SuburbEdit.Text) = 0) and
       (AnsiCompareText(Address.State,    StateEdit.Text) = 0) and
       (AnsiCompareText(Address.PostCode, PostCodeEdit.Text) = 0) then
    begin
      FData.CurrentAddress := Address;
      FData.Dirty := True;
      Break;
    end;
  end;
  // If no pre-exisiting address, create a new address
  if not Assigned(FData.CurrentAddress) then
  begin
    // If address fields are not blank
    if not ( (AnsiCompareText('',   StreetEdit.Text) = 0) and
             (AnsiCompareText('',   SuburbEdit.Text) = 0) and
             (AnsiCompareText('',    StateEdit.Text) = 0) and
             (AnsiCompareText('', PostCodeEdit.Text) = 0) ) then
    begin
      Address := TClientAddress.CreateNew;
      Address.Street := StreetEdit.Text;
      Address.Suburb := SuburbEdit.Text;
      Address.State  := StateEdit.Text;
      Address.PostCode := PostCodeEdit.Text;
      FData.Addresses.Add(Address);
      FData.Addresses.Dirty := True;
      FData.CurrentAddress := Address;
      FData.Dirty := True;
    end
    else
    begin
      FData.CurrentAddress := nil;
    end;
  end
  else
  begin
    FData.CurrentAddress := nil;
  end;
end;


procedure TClientFrame.UpdateClientMiscellaneousDetails;
begin
  FData.DateOfBirth := Int(DateOfBirthDateTimePicker.Date); 

  FData.PhoneHome := PhoneHomeEdit.Text;
  FData.PhoneMobile := PhoneMobileEdit.Text;
  FData.PhoneWork := PhoneWorkEdit.Text;
  FData.EmailAddress := EmailAddressEdit.Text;
end;

procedure TClientFrame.UpdateClientName;
begin
  if Pos(',', ClientNameEdit.Text) <> 0 then
  begin
    FData.FamilyName := Trim(StrLeft(ClientNameEdit.Text, Pos(',', ClientNameEdit.Text)-1));
    FData.GivenNames := Trim(StrRestOf(ClientNameEdit.Text, Pos(',', ClientNameEdit.Text)+1));
  end
  else
  begin
    if StrLastPos(AnsiSpace, ClientNameEdit.Text) <> 0 then
    begin
      FData.FamilyName := Trim(StrRestOf(ClientNameEdit.Text, StrLastPos(AnsiSpace, ClientNameEdit.Text)+1));
      FData.GivenNames := Trim(StrLeft(ClientNameEdit.Text, StrLastPos(AnsiSpace, ClientNameEdit.Text)-1));
    end
    else
    begin
      // if there is only one name in the edit control, assume it is a family name
      FData.FamilyName := Trim(ClientNameEdit.Text);
      FData.GivenNames := '';
    end;
  end;
end;

procedure TClientFrame.UpdateClientUndesirableStatus;
begin
  FData.Undesirable := UndesirableCheckBox.Checked;
  FData.UndesirableCode := UndesirableCodeEdit.Text;
  FData.UndesirableReason := UndesirableRichEdit.Text;
end;

procedure TClientFrame.UpdateData;
begin
  UpdateClientAddress;
  UpdateClientName;
  UpdateClientUndesirableStatus;
  UpdateClientMiscellaneousDetails;
  FData.Dirty := True;
end;

{ TClientFrame - events }

procedure TClientFrame.ClientNameEditExit(Sender: TObject);
begin
  UpdateClientName;
  ClientNameEdit.Text := FData.Caption;
end;

procedure TClientFrame.UndesirableCheckBoxClick(Sender: TObject);
begin
  UpdateClientUndesirableStatus;
  UndesirableNoticeLabel.Visible := FData.Undesirable;
end;






end.

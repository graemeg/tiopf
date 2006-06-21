{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: An Open Tools API Wizard for Delphi to assist in the creation
  of a unit defining a TPerObjList which owns TPerObjAbs items.
  This form is used for creating/editing property values.

  Revision History:
    March 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFPropertyFrm;

interface

uses
  Windows, Messages, SysUtils,
  {$IFDEF VER140}
  Variants,
  {$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, tiOPFProperty;

type
  TfrmProperty = class(TForm)
    Label1: TLabel;
    edName: TEdit;
    rgVisibility: TRadioGroup;
    gbType: TGroupBox;
    rbInteger: TRadioButton;
    rbBoolean: TRadioButton;
    rbByte: TRadioButton;
    rgRead: TRadioGroup;
    rgWrite: TRadioGroup;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    rbOther: TRadioButton;
    edType: TEdit;
    rbWord: TRadioButton;
    rbLongWord: TRadioButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    procedure rbTypeClick(Sender: TObject);
    procedure rbOtherClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    ThisProperty: TProperty;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AProperty: TProperty); reintroduce;
  end;

implementation

uses
  tiOPFWizardsGlobal, INIFiles;

{$R *.dfm}

procedure TfrmProperty.rbTypeClick(Sender: TObject);
begin
  edType.Text := (Sender as TRadioButton).Caption;
  edType.Enabled := False;
end;

procedure TfrmProperty.rbOtherClick(Sender: TObject);
begin
  edType.Enabled := True;
  if Self.Visible then
    edType.SetFocus;
end;

constructor TfrmProperty.Create(AOwner: TComponent; AProperty: TProperty);
var
  i: Integer;
  Found: Boolean;
begin
  inherited Create(AOwner);
  ThisProperty := AProperty;
  edName.Text := AProperty.Name;
  rgVisibility.ItemIndex := Ord(AProperty.Visibility);
  i := 0;
  Found := False;
  while (i < gbType.ControlCount) and not Found do
  begin
    if gbType.Controls[i] is TRadioButton then
      if TRadioButton(gbType.Controls[i]).Caption = AProperty.DataType then
      begin
        TRadioButton(gbType.Controls[i]).Checked := True;
        Found := True;
      end;
    Inc(i);
  end;
  if not Found then
  begin
    rbOther.Checked := True;
    edType.Text := AProperty.DataType;
  end;
  rgRead.ItemIndex := Ord(AProperty.ReadAccess);
  rgWrite.ItemIndex := Ord(AProperty.WriteAccess);
end;

procedure TfrmProperty.btnOKClick(Sender: TObject);
var
  ErrMsg: string;
begin
  ErrMsg := '';
  if edName.Text = '' then
    ErrMsg := ErrMsg + 'The property must have a name'#13;
  if edType.Text = '' then
    ErrMsg := ErrMsg + 'The property must have a type'#13;
  if ErrMsg = '' then
  begin
    ThisProperty.Name := edName.Text;
    ThisProperty.VisibilityAsString := rgVisibility.Items[rgVisibility.ItemIndex];
    ThisProperty.DataType := edType.Text;
    ThisProperty.ReadAccessAsString := rgRead.Items[rgRead.ItemIndex];
    ThisProperty.WriteAccessAsString := rgWrite.Items[rgWrite.ItemIndex];
    ModalResult := mrOk;
  end
  else
    MessageDlg('Error in Property creation:'#13#13 + ErrMsg, mtError, [mbOK], 0);
end;

procedure TfrmProperty.FormCreate(Sender: TObject);
var
  i: Integer;
  sDefaultType: string;
  iniWiz: TINIFile;

  procedure SetItemIndex(RadioGroup: TRadioGroup; Item: string);
  var
    i, iDefaultIndex: Integer;
    sDefault: string;
  begin
    // Case insensitive search for an item in the RadioGroup
    sDefault := UpperCase(iniWiz.ReadString('Property', Item, ''));
    iDefaultIndex := -1;
    for i := 0 to RadioGroup.Items.Count-1 do
      if UpperCase(RadioGroup.Items[i]) = sDefault then
      begin
        iDefaultIndex := i;
        Break;
      end;
    if iDefaultIndex <> -1 then
      RadioGroup.ItemIndex := iDefaultIndex;
  end;

begin
  // Load the defaults from the INI file
  iniWiz := TIniFile.Create(cgWizardINIFile);
  SetItemIndex(rgVisibility, 'DefaultPropertyVisibility');
  SetItemIndex(rgRead, 'DefaultReadAccess');
  SetItemIndex(rgWrite, 'DefaultWriteAccess');
  // The type is a group box
  sDefaultType := UpperCase(iniWiz.ReadString('Property', 'DefaultType', 'Other'));
  for i := 0 to gbType.ControlCount-1 do
    if (gbType.Controls[i] is TRadioButton) then
      if UpperCase(TRadioButton(gbType.Controls[i]).Caption) = sDefaultType then
        TRadioButton(gbType.Controls[i]).Checked := True;

  iniWiz.Free;
end;

end.


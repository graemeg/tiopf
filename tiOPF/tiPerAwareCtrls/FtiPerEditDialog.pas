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
    An abstract form for editing a tiPerAware business object in a modal dialog.

  Classes:
    TFormTIPerEditDialog - The form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiPerEditDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiPtnVisPerObj, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly ;

type

// Paste this into the concrete class
//  protected
//    procedure SetData(const Value: TPerObjAbs); override ;
//    function  FormIsValid : boolean ; override ;

  TFormTIPerEditDialog = class( TFormTiDialogAbs )
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbEnterAsTab: TCheckBox;
    RO: TtiReadOnly;
    procedure aCancelExecute(Sender: TObject);
    procedure alUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    FAL         : TActionList ;
    FaOK        : TAction ;
    FaCancel    : TAction ;
    FData       : TPerObjAbs ;
    FDataBuffer : TPerObjAbs ;

  protected

    procedure aOKExecute(Sender: TObject);virtual;

    property  Databuffer : TPerObjAbs read FDataBuffer write FDataBuffer ;

    // Implement these in the concrete...
    procedure SetData(const Value: TPerObjAbs); virtual ;
    function  FormIsValid : boolean ; virtual ;
    function  FormIsEdited : boolean ; virtual ;
    procedure SetupButtons ;

  public
    property Data : TPerObjAbs read FData write SetData ;
    class function Execute( const pData : TPerObjAbs ; pReadOnly : boolean = false ) : boolean ; virtual ;
  end;

implementation
uses
  tiUtils
  ,tiRegINI
  ;

{$R *.DFM}

// -----------------------------------------------------------------------------
class function TFormTIPerEditDialog.Execute( const pData: TPerObjAbs ; pReadOnly : boolean = false ): boolean;
var
  lForm : TFormTIPerEditDialog ;
begin
  lForm := Create( nil ) ;
  try
    lForm.RO.ReadOnly := pReadOnly ;
    lForm.Data := pData ;
    result := lForm.ShowModal = mrOK ;
  finally
    lForm.Free ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.FormCreate(Sender: TObject);
begin
  inherited;

  FAL := TActionList.Create( Self ) ;
  FAL.OnUpdate := ALUpdate ;

  FaOK := TAction.Create( FAL ) ;
  FaOK.ActionList := FAL ;
  FaOK.Caption   := '&OK' ;
  FaOK.OnExecute := aOKExecute ;
  btnOK.Action := FaOK ;

  FaCancel := TAction.Create( FAL ) ;
  FaCancel.ActionList := FAL ;
  FaCancel.Caption   := '&Cancel' ;
  FaCancel.OnExecute := aCancelExecute ;
  btnCancel.Action := FaCancel ;

  cbEnterAsTab.Checked := gReg.ReadBool( Name, 'EnterAsTab', False ) ;

end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataBuffer.Free ;
  gReg.WriteBool( Name, 'EnterAsTab', cbEnterAsTab.Checked ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.aOKExecute(Sender: TObject);
begin
  Assert( FData <> nil, 'FData not assigned' ) ;
  Assert( FDataBuffer <> nil, 'FDataBuffer not assigned' ) ;
  FData.Assign( FDataBuffer ) ;
  FData.Dirty := true ;
  ModalResult := mrOK ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.SetData(const Value: TPerObjAbs);
begin
  FData := Value;
  FreeAndNil(FDataBuffer) ;
  if FData <> nil then
    FDataBuffer := FData.Clone ;
  SetupButtons ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.alUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  FaOK.Enabled := FormIsValid and FormIsEdited ;
  btnCancel.Default := ( not cbEnterAsTab.Checked ) and ( not FaOK.Enabled ) ;
  Handled := true ;
end;

// -----------------------------------------------------------------------------
function TFormTIPerEditDialog.FormIsValid: boolean;
begin
  result := true ;
end;


// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIPerEditDialog.SetupButtons ;
begin
  // No data
  if ( FData = nil ) or
     ( FDataBuffer = nil ) then
  begin
    FaOK.Enabled := false ;
    btnCancel.Default := true ;
    Exit ; //==>
  end ;

  btnCancel.Default := false ;
  btnOK.Default := not cbEnterAsTab.Checked ;
  KeyPreview := cbEnterAsTab.Checked ;

end ;

procedure TFormTIPerEditDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0; // Eat the Beep
    SelectNext(ActiveControl AS TWinControl, True, True) // Forward
  end
  else
  if Key = #2 then
    SelectNext(ActiveControl AS TWinControl, False, True) // Backward
end;

procedure TFormTIPerEditDialog.FormShow(Sender: TObject);
begin
  SelectFirst ;       
end;

function TFormTIPerEditDialog.FormIsEdited: boolean;
begin
  result := not FData.Equals( FDataBuffer ) ;
end;

end.




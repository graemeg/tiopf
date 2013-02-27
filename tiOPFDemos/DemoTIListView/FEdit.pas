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
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    Popup dialog for editing a row in the TtiListView demo.

  ToDo:
    Change control for editing a float from a TEdit to a TMaskEdit, or
    somthing which gives better control over what is entered.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DemoData, StdCtrls, Buttons, ComCtrls, Mask, Spin ;

type
  TFormEdit = class(TForm)
    bbOK: TBitBtn;
    bbCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eString: TEdit;
    seInteger: TSpinEdit;
    dtpDate: TDateTimePicker;
    eFloat: TEdit;
    procedure bbOKClick(Sender: TObject);
    procedure eFloatKeyPress(Sender: TObject; var Key: Char);
  private
    FData: TDataRow;
    procedure SetData(const Value: TDataRow);
    { Private declarations }
  public
    property Data : TDataRow read FData write SetData ;
  end;

implementation

{$R *.DFM}


// -----------------------------------------------------------------------------
procedure TFormEdit.SetData(const Value: TDataRow);
begin
  FData           := Value ;
  eString.Text    := FData.StringData ;
  seInteger.Value := FData.IntegerData ;
  eFloat.Text    := FloatToStr( FData.FloatData ) ;
  dtpDate.Date    := FData.DateData ;
end;

// -----------------------------------------------------------------------------
procedure TFormEdit.bbOKClick(Sender: TObject);
begin
  FData.StringData  := eString.Text    ;
  FData.IntegerData := seInteger.Value ;
  FData.FloatData   := StrToFloat( eFloat.Text ) ;
  FData.DateData    := dtpDate.Date    ;
  ModalResult       := mrOK ;
end;

// -----------------------------------------------------------------------------
procedure TFormEdit.eFloatKeyPress(Sender: TObject; var Key: Char);
begin
  if not ( Key in [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.' ]) then
    Key := #0 ;
end;

end.

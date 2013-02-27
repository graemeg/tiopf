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
    TtiListView & TtiListViewPlus demo main form

  ToDo:
    Nothing.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, Spin;

type
  TFormMain = class(TForm)
    Bevel1: TBevel;
    seNoOfItems: TSpinEdit;
    Label1: TLabel;
    Bevel2: TBevel;
    btnListView: TButton;
    btnListViewPlus: TButton;
    btnClose: TButton;
    btnListViewCtrls: TButton;
    btnListViewMultiSelect: TButton;
    btnListViewDIf: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnListViewClick(Sender: TObject);
    procedure btnListViewPlusClick(Sender: TObject);
    procedure btnListViewCtrlsClick(Sender: TObject);
    procedure btnListViewMultiSelectClick(Sender: TObject);
    procedure btnListViewDIfClick(Sender: TObject);
  private
    function GetNoOfItems: integer;
    { Private declarations }
  public
    property NoOfItems : integer read GetNoOfItems ;
  end;

var
  FormMain: TFormMain;

implementation
uses
  FtiListViewDemo
  ,FtiListViewPlusDemo, FtiListViewCtrlsDemo, FtiListViewMultiSelect,
  FtiListViewDif
  ;

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.btnListViewClick(Sender: TObject);
begin
  Application.CreateForm( TFormListViewDemo, FormListViewDemo ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.btnListViewPlusClick(Sender: TObject);
begin
  Application.CreateForm( TFormListViewPlusDemo, FormListViewPlusDemo ) ;
end;

// -----------------------------------------------------------------------------
function TFormMain.GetNoOfItems: integer;
begin
  result := seNoOfItems.Value ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.btnListViewCtrlsClick(Sender: TObject);
begin
  Application.CreateForm( TFormListViewCtrlsDemo, FormListViewCtrlsDemo ) ;
end;

procedure TFormMain.btnListViewMultiSelectClick(Sender: TObject);
begin
  Application.CreateForm( TFormTIListViewMultiSelect, FormTIListViewMultiSelect ) ;
end;

procedure TFormMain.btnListViewDIfClick(Sender: TObject);
begin
  Application.CreateForm( TFormTIListViewDif, FormTIListViewDif ) ;
end;

end.

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
    Dec, 2000, Peter Hinrichsen, Made open source

  Purpose:
    To demonstrate the use of the TtiPerAware controls.

  Classes:
    TFormMain - The application's main form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, uAnimal, tiPerAwareCtrls ;

type

  //----------------------------------------------------------------------------
  TFormMain = class(TForm)
    memoNotes: TMemo;
    Panel1: TPanel;
    lvAnimals: TListView;
    btnClose: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure lvAnimalsData(Sender: TObject; Item: TListItem);
    procedure btnCloseClick(Sender: TObject);
    procedure lvAnimalsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAnimals : TAnimals ;
  public
  end;

var
  FormMain: TFormMain;

implementation
uses
  FEdit
  ;

{$R *.DFM}

// The form's onCreate event. Create an instance of TAnimals and attacht to
// the TListView
//------------------------------------------------------------------------------
procedure TFormMain.FormCreate(Sender: TObject);
begin
  FAnimals := TAnimals.Create ;
  lvAnimals.Items.Count := FAnimals.Count ;
end;

// The form's onDestroy event. Destroy the instance of TAnimals
//------------------------------------------------------------------------------
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FAnimals.Free ;
end;

// The TListView's OnData event. If you have not used a TListView in this way
// before, then read the help text. This event gets called for each item in
// FAnimals and is used to assing the properties of FAnimals to the TListItem(s).
//------------------------------------------------------------------------------
procedure TFormMain.lvAnimalsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := FAnimals.Items[Item.Index].AnimalName ;
  Item.SubItems.Add( FloatToStr( FAnimals.Items[Item.Index].Weight )) ;
  Item.SubItems.Add( IntToStr( FAnimals.Items[Item.Index].NoOfLegs )) ;
  Item.SubItems.Add( DateToStr( FAnimals.Items[Item.Index].LastVisited )) ;
  Item.SubItems.Add( IntToStr( Ord( FAnimals.Items[Item.Index].Vaccinated ))) ;
  Item.SubItems.Add( FAnimals.Items[Item.Index].AnimalType ) ;
  Item.SubItems.Add( FAnimals.Items[Item.Index].Notes ) ;
end;

// The TListView's OnDblClick event handler
//------------------------------------------------------------------------------
procedure TFormMain.lvAnimalsDblClick(Sender: TObject);
var
  lForm : TFormEdit ;
begin

  // Check a list item was selected, and exit if not.
  if lvAnimals.Selected = nil then
    Exit ; //==>

//ShowMessage( TAnimal( FAnimals.Items[lvAnimals.Selected.Index]).FavouriteFood.TextDisplay ) ;

  // Create an instance of TFormEdit
  lForm := TFormEdit.Create( nil ) ;
  try
    // Assing the form's data property to the TAnimal that corresponds
    // to the selected TListItem.
    lForm.Data := TAnimal( FAnimals.Items[lvAnimals.Selected.Index]) ;

    // Show the form modally
    if lForm.ShowModal = mrOK then
      // If a change was made, then refresh the TListView
      lvAnimals.Refresh ;
  finally
    lForm.Free ;
  end;

end;

//------------------------------------------------------------------------------
procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close ;
end;

end.

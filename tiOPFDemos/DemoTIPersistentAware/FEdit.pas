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
    Dec 2000, Peter Hinrichsen, Made open source

  Purpose:
    Demonstrate the use of the TtiPerAware controls

  Classes:
    TFormEdit - The modal edit form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Spin, ComCtrls, 
  uAnimal, Buttons, ActnList, tiPerAwareCtrls, tiFocusPanel ;

type

  //----------------------------------------------------------------------------
  TFormEdit = class(TForm)
    Panel1: TPanel;
    bbCancel: TBitBtn;
    bbOK: TBitBtn;
    ActionList1: TActionList;
    aOK: TAction;
    aCancel: TAction;
    GroupBox1: TGroupBox;
    paeAnimalName: TtiPerAwareEdit;
    GroupBox2: TGroupBox;
    paeNoOfLegs: TtiPerAwareFloatEdit;
    paeWeight: TtiPerAwareFloatEdit;
    GroupBox3: TGroupBox;
    paeVaccinated: TtiPerAwareCheckBox;
    paeLastVisited: TtiPerAwareDateTimePicker;
    GroupBox4: TGroupBox;
    paeNotes: TtiPerAwareMemo;
    GroupBox5: TGroupBox;
    seLabelWidth: TSpinEdit;
    Label1: TLabel;
    rgLabelStyle: TRadioGroup;
    Label2: TLabel;
    Label3: TLabel;
    paeImage: TtiPerAwareImageEdit;
    cbEnable: TCheckBox;
    paeAnimalType: TtiPerAwareComboBoxStatic;
    cbReadOnly: TCheckBox;
    paeFavouriteFood: TtiPerAwareComboBoxDynamic;
    procedure FormDestroy(Sender: TObject);
    procedure aOKExecute(Sender: TObject);
    procedure aCancelExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure seLabelWidthChange(Sender: TObject);
    procedure rgLabelStyleClick(Sender: TObject);
    procedure cbEnableClick(Sender: TObject);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FData       : TAnimal ;
    FEditBuffer : TAnimal ;
    procedure SetData(const Value: TAnimal);
  public
    property Data : TAnimal read FData write SetData ;
  end;

implementation
uses
  TypInfo
  ;

{$R *.DFM}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* If using the TtiPerAware controls in a modal edit dialog, the methods below
//* should be implemented.
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Setup the TtiPerAwareComboboxDynamic with some data
procedure TFormEdit.FormCreate(Sender: TObject);
var
  i : TLabelStyle ;
begin
  paeFavouriteFood.FieldNameDisplay := 'TextDisplay' ;
  paeFavouriteFood.List := gFavouriteFoods ;
  rgLabelStyle.Items.Clear ;
  for i := Low(TLabelStyle) to High(TLabelStyle) do
    rgLabelStyle.Items.Add( GetEnumName( TypeInfo( TLabelStyle ), Ord( i ))) ;
end;

// Clean up the EditBuffer if one was created
//------------------------------------------------------------------------------
procedure TFormEdit.FormDestroy(Sender: TObject);
begin
  FEditBuffer.Free ;
end;

// Assign the data object
//------------------------------------------------------------------------------
procedure TFormEdit.SetData(const Value: TAnimal);
begin
  // Setup an internal var to point to the data object
  FData := Value ;

  // Make a clone of the data to be edited, for buffering
  FEditBuffer := FData.Clone ;

  // Link the EditBuffer to each of the TtiPerAware controls
  paeAnimalName.LinkToData(    FEditBuffer, 'AnimalName'  ) ;
  paeWeight.LinkToData(        FEditBuffer, 'Weight'      ) ;
  paeNoOfLegs.LinkToData(      FEditBuffer, 'NoOfLegs'    ) ;
  paeLastVisited.LinkToData(   FEditBuffer, 'LastVisited' ) ;
  paeVaccinated.LinkToData(    FEditBuffer, 'Vaccinated'  ) ;
  paeNotes.LinkToData(         FEditBuffer, 'Notes'       ) ;
  paeAnimalType.LinkToData(    FEditBuffer, 'AnimalType'  ) ;
  paeFavouriteFood.LinkToData( FEditBuffer, 'FavouriteFood' ) ;
  paeImage.LinkToData(         FEditBuffer, 'Graphic'     ) ;

end;

// The dialog's <OK> button
//------------------------------------------------------------------------------
procedure TFormEdit.aOKExecute(Sender: TObject);
begin
  FData.Assign( FEditBuffer ) ;
  modalResult := mrOK ;
end;

// The dialog's <Cancel> button
//------------------------------------------------------------------------------
procedure TFormEdit.aCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

// The ActionList's onUpdate method.
// Check to see if a change has been made to the EditBuffer, and
// enable the <OK> button if necessary.
//------------------------------------------------------------------------------
procedure TFormEdit.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  aOK.Enabled := not FData.Equals( FEditBuffer ) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* The methods below do not have to be implemented when using the TtiPerAware
//* controls. They are only necessary for the demonstration to show how the
//* label width and labelStyle properties can be changed.
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Change the labelWidth property
//------------------------------------------------------------------------------
procedure TFormEdit.seLabelWidthChange(Sender: TObject);
var
  i : integer ;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TtiPerAwareAbs then
      TtiPerAwareAbs( Components[i] ).LabelWidth := seLabelWidth.Value ;
end;

// Change the labelStyle property
//------------------------------------------------------------------------------
procedure TFormEdit.rgLabelStyleClick(Sender: TObject);
var
  i : integer ;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TtiPerAwareAbs then
    begin
      if not ( Components[i] is TtiPerAwareMemo ) and
         not ( Components[i] is TtiPerAwareImageEdit ) then
        if rgLabelStyle.ItemIndex in [1, 3 ] then
          TtiPerAwareAbs( Components[i] ).Height := 42
        else
          TtiPerAwareAbs( Components[i] ).Height := 23 ;
      TtiPerAwareAbs( Components[i] ).LabelStyle := TLabelStyle( rgLabelStyle.ItemIndex ) ;
    end ;
end;

procedure TFormEdit.cbEnableClick(Sender: TObject);
var
  i : integer ;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TtiPerAwareAbs then
      TtiPerAwareAbs( Components[i] ).Enabled := cbEnable.Checked ;
end;

procedure TFormEdit.cbReadOnlyClick(Sender: TObject);
var
  i : integer ;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TtiPerAwareAbs then
      TtiPerAwareAbs( Components[i] ).ReadOnly := cbReadOnly.Checked ;
end;

end.

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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit uAnimal;

interface
uses
  Classes,
  Contnrs,
  Graphics
  ;

type

  TFavouriteFood = class ;
  TFavouriteFoods = class( TList )
  public
    constructor create ; virtual ;
    function FindByTextDatabase( const pTextDatabase : string ) : TFavouriteFood ;
  end ;

  TFavouriteFood = class( TPersistent )
  private
    FsTextDisplay: string;
    FsTextDatabase: string;
  published
    property TextDisplay  : string read FsTextDisplay write FsTextDisplay ;
    property TextDatabase : string read FsTextDatabase write FsTextDatabase ;
  end ;

  TAnimal = class ;
  TAnimal = class( TPersistent )
  private
    FsAnimalName : string;
    FrWeight: real;
    FdtLastVisited: TDateTime;
    FbVaccinated: boolean;
    FsNotes: string;
    FiNoOfLegs: integer;
    FGraphic : TGraphic ;
    FsAnimalType: string;
    FFavouriteFood: TFavouriteFood;
  public
    Constructor Create ;
    Destructor  Destroy ; override ;
    procedure Assign( pValue : TAnimal ) ; reintroduce ;
    function  Clone : TAnimal ;
    function  Equals( pValue : TAnimal ) : boolean ;
  published
    property AnimalName  : string    read FsAnimalName   write FsAnimalName ;
    property Weight      : real      read FrWeight       write FrWeight ;
    property LastVisited : TDateTime read FdtLastVisited write FdtLastVisited ;
    property Vaccinated   : boolean  read FbVaccinated   write FbVaccinated ;
    property NoOfLegs    : integer   read FiNoOfLegs     write FiNoOfLegs ;
    property Notes       : string    read FsNotes        write FsNotes ;
    property Graphic     : TGraphic  read FGraphic       write FGraphic ;
    property AnimalType  : string    read FsAnimalType   write FsAnimalType ;
    property FavouriteFood : TFavouriteFood read FFavouriteFood write FFavouriteFood ;
  end ;

  TAnimals = class( TObjectList )
  protected
    function GetItem(i: integer): TAnimal;
  public
    constructor Create ;
    property    Items[ i : integer ] : TAnimal read GetItem ;
  end ;

function gFavouriteFoods : TFavouriteFoods ;

implementation
uses
  SysUtils
  ,Forms
  ;

var
  uFavouriteFoods : TFavouriteFoods ;

function gFavouriteFoods : TFavouriteFoods ;
begin
  if uFavouriteFoods = nil then
    uFavouriteFoods := TFavouriteFoods.Create ;
  result := uFavouriteFoods ;
end ;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TAnimals
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TAnimals.Create;
var
  lAnimal : TAnimal ;
begin
  inherited;
  lAnimal := TAnimal.Create ;
  lAnimal.AnimalName         := 'Tigger' ;
  lAnimal.Weight             := 5.5 ;
  lAnimal.LastVisited        := Date - 10 ;
  lAnimal.Vaccinated          := false ;
  lAnimal.Notes              := 'Tiggers don''t like honey' ;
  lAnimal.NoOfLegs           := 4 ;
  lAnimal.AnimalType         := 'Tigger' ;
  lAnimal.FavouriteFood      := gFavouriteFoods.FindByTextDatabase( 'UNKNOWN' ) ;
  // Must make this a resource
  //lAnimal.Graphic.LoadFromFile( 'tigger.bmp' ) ;
  Add( lAnimal ) ;

  lAnimal := TAnimal.Create ;
  lAnimal.AnimalName         := 'Pooh' ;
  lAnimal.Weight             := 1.2 ;
  lAnimal.LastVisited        := Date - 1 ;
  lAnimal.Vaccinated          := true ;
  lAnimal.Notes              := 'Isn''t if funny how a bear likes honey. Buz, buz, buz. I wonder why he duz?' ;
  lAnimal.NoOfLegs           := 4 ;
  lAnimal.AnimalType         := 'Bear' ;
  lAnimal.FavouriteFood      := gFavouriteFoods.FindByTextDatabase( 'HONEY' ) ;
  // Must make this a resource
  //lAnimal.Graphic.LoadFromFile( 'pooh.bmp' ) ;
  Add( lAnimal ) ;

  lAnimal := TAnimal.Create ;
  lAnimal.AnimalName         := 'Eor' ;
  lAnimal.Weight             := 12.4 ;
  lAnimal.LastVisited        := Date ;
  lAnimal.Vaccinated          := true ;
  lAnimal.Notes              := 'I''m so depressed...' ;
  lAnimal.NoOfLegs           := 4 ;
  lAnimal.AnimalType         := 'Donkey' ;
  lAnimal.FavouriteFood      := gFavouriteFoods.FindByTextDatabase( 'THISTLES' ) ;
  // Must make this a resource
  //lAnimal.Graphic.LoadFromFile( 'eor.bmp' ) ;
  Add( lAnimal ) ;

end;

// -----------------------------------------------------------------------------
function TAnimals.GetItem(i: integer): TAnimal;
begin
  result := TAnimal( inherited GetItem( i )) ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TAnimal
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TAnimal.Create;
begin
  inherited ;
  FGraphic := TBitMap.Create ;
end;

// -----------------------------------------------------------------------------
destructor TAnimal.Destroy;
begin
  FGraphic.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TAnimal.Assign(pValue: TAnimal);
begin
  AnimalName    := pValue.AnimalName  ;
  Weight        := pValue.Weight      ;
  LastVisited   := pValue.LastVisited ;
  Vaccinated    := pValue.Vaccinated  ;
  NoOfLegs      := pValue.NoOfLegs    ;
  AnimalType    := pValue.AnimalType  ;
  Notes         := pValue.Notes       ;
  // Bit of a trick here, FavouriteFood is a pointer into a list of objects
  // that is shared between all the TAnimals. Somtimes we may want to clone
  // the value of a object which is 'pointed to' but in this case we do not
  // want a clone, just to copy the pointer.
  FavouriteFood := pValue.FavouriteFood ;
  Graphic.Assign( pValue.Graphic ) ;
end;

// -----------------------------------------------------------------------------
function TAnimal.Clone: TAnimal;
begin
  result := TAnimal.Create ;
  result.Assign( self ) ;
end;

// -----------------------------------------------------------------------------
function TAnimal.Equals(pValue: TAnimal): boolean;
begin
  result := true ;
  result := result and ( AnimalName    = pValue.AnimalName    ) ;
  result := result and ( Weight        = pValue.Weight        ) ;
  result := result and ( LastVisited   = pValue.LastVisited   ) ;
  result := result and ( Vaccinated    = pValue.Vaccinated    ) ;
  result := result and ( NoOfLegs      = pValue.NoOfLegs      ) ;
  result := result and ( AnimalType    = pValue.AnimalType    ) ;
  result := result and ( Notes         = pValue.Notes         ) ;
  result := result and ( FavouriteFood = pValue.FavouriteFood ) ;
  result := result and ( Not Graphic.Modified ) ;
end;

{ TFavouriteFoods }

constructor TFavouriteFoods.create;
var
  lData : TFavouriteFood ;
begin
  inherited;
  lData := TFavouriteFood.Create ;
  lData.TextDisplay  := 'Honey' ;
  lData.TextDatabase := 'HONEY' ;
  Add( lData ) ;

  lData := TFavouriteFood.Create ;
  lData.TextDisplay  := 'Thistles' ;
  lData.TextDatabase := 'THISTLES' ;
  Add( lData ) ;

  lData := TFavouriteFood.Create ;
  lData.TextDisplay  := 'Unknown' ;
  lData.TextDatabase := 'UNKNOWN' ;
  Add( lData ) ;

end;

function TFavouriteFoods.FindByTextDatabase( const pTextDatabase: string): TFavouriteFood;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( TFavouriteFood( Items[i] ).TextDatabase, pTextDatabase ) then
    begin
      result := TFavouriteFood( Items[i] ) ;
      Break ; //==>
    end ;
end;

end.

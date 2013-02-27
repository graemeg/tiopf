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
    September 2000, Peter Hinrichsen, Made open source

  Purpose:
    For to demonstrate the features of the TtiListViewListBox

  ToDo:
    Nothing

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiListViewCtrlsDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, tiListView, tiListViewCtrls, StdCtrls, ExtCtrls;

type

  // A class to browse in the list box
  // ---------------------------------------------------------------------------
  TAnimal = class( TPersistent )
  private
    FbSelected: boolean;
    FsName: string;
  published
    property Name : string read FsName write FsName ;
  public
    constructor Create( const psName : string ) ;
    property Selected : boolean read FbSelected write FbSelected ;
  end ;

  // A list of animals to browse in the list box
  // ---------------------------------------------------------------------------
  TAnimals = class( TList )
  public
    constructor Create ;
    destructor  Destroy ; override ;
  end ;

  // The demp form
  // ---------------------------------------------------------------------------
  TFormListViewCtrlsDemo = class(TForm)
    Label1: TLabel;
    LVListBoxRB: TtiListViewListBox;
    LVListBoxCB: TtiListViewListBox;
    Label2: TLabel;
    memoSelected: TMemo;
    cbRBReadOnly: TCheckBox;
    cbCBReadOnly: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVListBoxRBGetChecked(pData: TPersistent;
      var pbChecked: Boolean);
    procedure LVListBoxRBCheck(Sender: TtiCustomListView; pData: TPersistent;
      pItem: TListItem);
    procedure LVListBoxCBGetChecked(pData: TPersistent;
      var pbChecked: Boolean);
    procedure LVListBoxCBCheck(Sender: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure cbRBReadOnlyClick(Sender: TObject);
    procedure cbCBReadOnlyClick(Sender: TObject);
  private
    FAnimalsRB : TAnimals ;
    FAnimalsCB : TAnimals ;
    procedure SelectedToMemo( pAnimals : TAnimals ; pListView : TtiCustomListView ) ;
  public
    { Public declarations }
  end;

var
  FormListViewCtrlsDemo : TFormListViewCtrlsDemo ;

implementation

{$R *.DFM}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAnimal
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TAnimal.Create(const psName: string);
begin
  inherited Create ;
  Name := psName ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAnimals
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TAnimals.Create;
begin
  inherited;
  Add( TAnimal.Create( 'Dog'   )) ;
  Add( TAnimal.Create( 'Cat'   )) ;
  Add( TAnimal.Create( 'Bird'  )) ;
  Add( TAnimal.Create( 'Fish'  )) ;
  Add( TAnimal.Create( 'Cow'   )) ;
  Add( TAnimal.Create( 'Horse' )) ;
  Add( TAnimal.Create( 'Sheep' )) ;
end;

// -----------------------------------------------------------------------------
destructor TAnimals.Destroy;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    TObject( Items[i] ).Free ;
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormListViewCtrlsDemo
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TFormListViewCtrlsDemo.FormCreate(Sender: TObject);
begin
  FAnimalsRB := TAnimals.Create ;
  LVListBoxRB.Data := FAnimalsRB ;
  FAnimalsCB := TAnimals.Create ;
  LVListBoxCB.Data := FAnimalsCB ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.FormDestroy(Sender: TObject);
begin
  FAnimalsRB.Free ;
  FAnimalsCB.Free ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.LVListBoxRBGetChecked(
  pData: TPersistent; var pbChecked: Boolean);
begin
  pbChecked := TAnimal( pData ).Selected ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.LVListBoxRBCheck(Sender: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
var
  i : integer ;
begin
  // Set all 
  for i := 0 to FAnimalsRB.Count - 1 do
    TAnimal( FAnimalsRB.Items[i] ).Selected := false ;
  TAnimal( pData ).Selected := true ;
  SelectedToMemo( TAnimals( Sender.Data ), Sender  );
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.LVListBoxCBGetChecked(pData: TPersistent;
  var pbChecked: Boolean);
begin
  pbChecked := TAnimal( pData ).Selected ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.LVListBoxCBCheck( Sender: TtiCustomListView;
                                                   pData: TPersistent;
                                                   pItem: TListItem);
begin
  TAnimal( pData ).Selected := not TAnimal( pData ).Selected ;
  SelectedToMemo( TAnimals( Sender.Data ), Sender  );
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.SelectedToMemo( pAnimals: TAnimals ;
                                                 pListView : TtiCustomListView );
var
  i : integer ;
begin
  with MemoSelected.Lines do
  begin
    Clear ;
    Add( 'Selected items in ' + pListView.Name ) ;
    Add( '' ) ;
    for i := 0 to pAnimals.Count - 1 do
      if TAnimal( pAnimals.Items[i] ).Selected then
        Add( TAnimal( pAnimals.Items[i] ).Name ) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.cbRBReadOnlyClick(Sender: TObject);
begin
  lvListBoxRB.ReadOnly := cbRBReadOnly.checked ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewCtrlsDemo.cbCBReadOnlyClick(Sender: TObject);
begin
  lvListBoxCB.ReadOnly := cbCBReadOnly.Checked;
end;

end.

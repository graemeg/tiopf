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

unit FtiListViewMultiSelect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiPerAwareMultiSelect, ExtCtrls, Contnrs, tiListView,
  tiTreeView, ComCtrls;

type

  TPerAwareMultiSelectTestObj = class( TPersistent )
  private
    FCaption: string;
  published
    property Caption : string read FCaption write FCaption ;
  public
    constructor Create( const pCaption : string ) ;
  end ;

  TFormTIListViewMultiSelect = class(TForm)
    MS: TtiPerAwareMultiSelect;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAvailable : TObjectList ;
    FSelected  : TList ;
  public
    { Public declarations }
  end;

var
  FormTIListViewMultiSelect: TFormTIListViewMultiSelect;

implementation

{$R *.DFM}

procedure TFormTIListViewMultiSelect.FormCreate(Sender: TObject);
var
  i : integer ;
begin
  FAvailable := TObjectList.Create ;
  FSelected  := TList.Create ;
  for i := 1 to 10 do
    FAvailable.Add(
      TPerAwareMultiSelectTestObj.Create( 'Test Obj #' +
        IntToStr( i ))) ;
  MS.Selected  := FSelected ;
  MS.Available := FAvailable ;
end;

{ TPerAwareMultiSelectTestObj }

constructor TPerAwareMultiSelectTestObj.Create(const pCaption: string);
begin
  inherited Create ;
  FCaption := pCaption ;
end;

procedure TFormTIListViewMultiSelect.FormDestroy(Sender: TObject);
begin
  FAvailable.Free ;
  FSelected.Free ;
end;

end.

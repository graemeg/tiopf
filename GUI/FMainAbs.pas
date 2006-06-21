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

unit FMainAbs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, Menus, StdCtrls, ExtCtrls, tiPerAwareCtrls,
  tiPerAwareFileCombos, ImgList, ActnList;

type
  TFormMainAbs = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Windows1: TMenuItem;
    ActionList: TActionList;
    ImageList: TImageList;
    ToolBar: TToolBar;
    aExit: TAction;
    tbExit: TToolButton;
    aTileVertically: TAction;
    aTileHorizontally: TAction;
    aCloseAll: TAction;
    Tilevertically1: TMenuItem;
    Tilehorizontally1: TMenuItem;
    N1: TMenuItem;
    Closeall1: TMenuItem;
    aCascade: TAction;
    Cascade1: TMenuItem;
    aAbout: TAction;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure aTileVerticallyExecute(Sender: TObject);
    procedure aTileHorizontallyExecute(Sender: TObject);
    procedure aCloseAllExecute(Sender: TObject);
    procedure aCascadeExecute(Sender: TObject);
    procedure aAboutExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMainAbs: TFormMainAbs;
  gFormAboutClass : TFormClass ;

implementation
uses
  tiUtils,
  tiGUISupport,
  tiRegINI
  ;

{$R *.DFM}

procedure TFormMainAbs.FormCreate(Sender: TObject);
begin
  gReg.ReadFormState( Self ) ;
  gGUIPackageMgr.MainForm := self ;
end;

procedure TFormMainAbs.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( Self ) ;
end;

procedure TFormMainAbs.aExitExecute(Sender: TObject);
begin
  Close ;
end;

procedure TFormMainAbs.aTileVerticallyExecute(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;

procedure TFormMainAbs.aTileHorizontallyExecute(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;

procedure TFormMainAbs.aCloseAllExecute(Sender: TObject);
var
  i : integer ;
begin
  for I := MDIChildCount - 1 downto 0 do
    MDIChildren[I].Close ;
end;

procedure TFormMainAbs.aCascadeExecute(Sender: TObject);
begin
  Cascade ;
end;

procedure TFormMainAbs.aAboutExecute(Sender: TObject);
var
  lForm : TForm ;
begin
  Assert( gFormAboutClass <> nil,
          'gFormAboutClass not assigned.' ) ;
  lForm := gFormAboutClass.Create( nil ) ;
  try
    lForm.ShowModal ;
  finally
    lForm.Free ;
  end ;
end;

end.




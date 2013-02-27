{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpResources;

interface

uses
  Controls, Forms, Imglist, graphics, windows;

const
  GLYFX_16_FOLDER  : Integer = -1;

  GLYFX_24_BACK    : Integer = -1;
  GLYFX_24_FORWARD : Integer = -1;
  GLYFX_24_HOME    : Integer = -1;
  GLYFX_24_SEARCH  : Integer = -1;
  GLYFX_24_FOLDERS : Integer = -1;
  GLYFX_24_NEW     : Integer = -1;
  GLYFX_24_EDIT    : Integer = -1;
  GLYFX_24_PRINT   : Integer = -1;
  GLYFX_24_PREVIEW : Integer = -1;

resourcestring
  LicenseText =
    'The PBPro Pawnbroking System is free software; you can redistribute it' + #13 +
    'and/or modify it under the terms of the GNU General Public License' + #13 +
    'as published by the Free Software Foundation; either version 2 of' + #13 +
    'the License, or (at your option) any later version.' + #13 +
    '' + #13 +
    'The PBPro Pawnbroking System is distributed in the hope that it will be' + #13 +
    'useful, but WITHOUT ANY WARRANTY; without even the implied warranty' + #13 +
    'of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + #13 +
    'See the GNU General Public License for more details.' + #13 +
    '' + #13 +
    'You should have received a copy of the GNU General Public License' + #13 +
    'along with the PBPro Pawnbroking System; if not, write to the Free' + #13 +
    'Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA' + #13 +
    '02111-1307 USA';

  CreditsText =
    'Eventide would like to thank the following:' + #13 +
    '' + #13 +
    'TechInsite (www.techinsite.com.au) for the tiOPF Object Persistence Framework.' + #13 +
    '' + #13 +
    'FastReport (www.fast-report.com) for the FreeReport reporting system.' + #13 +
    '' + #13 +
    'Jordan Russell (www.jrsoftware.org) for the Toolbar2000 components.' + #13 +
    '' + #13 +
    'glyFX (www.glyfx.com) for toolbar images.' + #13 +
    '' + #13 +

    'Please note: commercial distribution of PBPro may require licensing ' +
    'of third-party content. Please contact individual vendors for licensing details.';


function GetResourceImageList24x24: TImageList;

{$IFDEF BINARY_RELEASE}
{$R pbpImages.res}
{$ELSE}
{$R pbpImagesFree.res}
{$ENDIF}

implementation

var
  ResourceImageList24x24: TImageList;

const
  TransparentColor: TColor = clFuchsia;

procedure LoadResourceIntoImageList(Name: string; ImageList: TImageList);
var
  Bitmap: Graphics.TBitmap;
begin
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Handle := LoadBitmap(hInstance, PChar(Name));
    ImageList.AddMasked(Bitmap, TransparentColor);
  finally
    Bitmap.Free;
  end;
end;

function GetResourceImageList24x24: TImageList;
begin
  if not Assigned(ResourceImageList24x24) then
  begin
    ResourceImageList24x24 := TImageList.Create(Application);
    ResourceImageList24x24.Height := 24;
    ResourceImageList24x24.Width := 24;

    LoadResourceIntoImageList('GLYFX_24_BACK',    ResourceImageList24x24);  // 0
    LoadResourceIntoImageList('GLYFX_24_FORWARD', ResourceImageList24x24);  // 1
    LoadResourceIntoImageList('GLYFX_24_HOME',    ResourceImageList24x24);  // 2
    LoadResourceIntoImageList('GLYFX_24_SEARCH',  ResourceImageList24x24);  // 3
    LoadResourceIntoImageList('GLYFX_24_FOLDERS', ResourceImageList24x24);  // 4
    LoadResourceIntoImageList('GLYFX_24_NEW',     ResourceImageList24x24);  // 5
    LoadResourceIntoImageList('GLYFX_24_EDIT',    ResourceImageList24x24);  // 6
    LoadResourceIntoImageList('GLYFX_24_PRINT',   ResourceImageList24x24);  // 7
    LoadResourceIntoImageList('GLYFX_24_PREVIEW', ResourceImageList24x24);  // 8

    GLYFX_24_BACK    := 0;
    GLYFX_24_FORWARD := 1;
    GLYFX_24_HOME    := 2;
    GLYFX_24_SEARCH  := 3;
    GLYFX_24_FOLDERS := 4;
    GLYFX_24_NEW     := 5;
    GLYFX_24_EDIT    := 6;
    GLYFX_24_PRINT   := 7;
    GLYFX_24_PREVIEW := 8;
  end;

  Result := ResourceImageList24x24;
end;


end.

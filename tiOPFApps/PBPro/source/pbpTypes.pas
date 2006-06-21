{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd

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

unit pbpTypes;

interface

uses
  ActiveX, Controls, Imglist, Menus, Classes, DB, Sysutils, Graphics,

  TB2Toolbar,

  tiPtnVisPerObj,

  pbpEvents;

type
  IWorkspaceView = interface
    ['{0BB54805-850E-4594-992F-BE9F5FD90064}']
    procedure Reparent(Parent: TWinControl);
  end;

  IToolbarControl = interface
    ['{097ED900-21C1-4028-9276-A98B93278B53}']
    
  end;

  ISupportsToolbar2000 = interface
    ['{95B98674-EBBF-4ADB-B515-D23E96BDB42E}']
    procedure SetToolbarParent(Parent: TWinControl);
  end;

  IPerObjAbsListener = interface(IEventListener)
    ['{ED621406-E1A4-48D2-85D3-D7E8981D730F}']
    procedure Changed(Sender: TObject);
  end;


  IPerObjListListener = interface(IEventListener)
    ['{DD48AA24-4DDB-4BDD-976B-D443F6DE48DF}']
    procedure ItemAdded(AObject: TPerObjAbs);
    procedure ItemDeleted(AObject: TPerObjAbs);
  end;



const
  XP_OUTLOOKBAR_GRADIENT_START : TColor = $00E7A27B;
  XP_OUTLOOKBAR_GRADIENT_END   : TColor = $00D67563;

  IID_PerObjAbsListener: TGUID = '{ED621406-E1A4-48D2-85D3-D7E8981D730F}';
  IID_PerObjListListener: TGUID = '{DD48AA24-4DDB-4BDD-976B-D443F6DE48DF}';
  IID_SupportsToolbar2000: TGUID = '{95B98674-EBBF-4ADB-B515-D23E96BDB42E}';

implementation

end.

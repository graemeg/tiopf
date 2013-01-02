unit TB2Version;

{
  Toolbar2000
  Copyright (C) 1998-2003 by Jordan Russell
  All rights reserved.
  For conditions of distribution and use, see LICENSE.TXT.

  $jrsoftware: tb2k/Source/TB2Version.pas,v 1.57 2003/11/23 23:16:38 jr Exp $
}

interface

{$I TB2Ver.inc}

const
  Toolbar2000Version = '2.1.3';
  Toolbar2000VersionPropText = 'Toolbar2000 version ' + Toolbar2000Version;

type
  TToolbar2000Version = type string;

const
  Sig: PChar = '- ' + Toolbar2000VersionPropText +
    {$IFDEF VER90}  '/D2'+ {$ENDIF} {$IFDEF VER93}  '/CB1'+ {$ENDIF}
    {$IFDEF VER100} '/D3'+ {$ENDIF} {$IFDEF VER110} '/CB3'+ {$ENDIF}
    {$IFDEF VER120} '/D4'+ {$ENDIF} {$IFDEF VER125} '/CB4'+ {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER130} '/D5'+ {$ENDIF} {$ELSE} {$IFDEF VER130} '/CB5'+ {$ENDIF} {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER140} '/D6'+ {$ENDIF} {$ELSE} {$IFDEF VER140} '/CB6'+ {$ENDIF} {$ENDIF}
    {$IFNDEF BCB} {$IFDEF VER150} '/D7'+ {$ENDIF} {$ELSE} {$IFDEF VER150} '/CB7'+ {$ENDIF} {$ENDIF}
    ', Copyright (C) 1998-2003 by Jordan Russell -';

implementation

initialization
  Sig := Sig;
end.

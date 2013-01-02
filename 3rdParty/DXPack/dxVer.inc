{$B-} { Complete Boolean Evaluation }
{$R-} { Range-Checking }
{$T-} { Typed @ operator }
{$X+} { Extended syntax }
{$P+} { Open string params }
{$J+} { Writeable structured consts }
{$H+} { Use long strings by default }

{$DEFINE VCL}

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE DELPHI3} { Borland Delphi 3.0 }
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE CBUILDER3}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER120} { Borland Delphi 4.0 }
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFDEF VER125} { Borland C++Builder 4.0 }
  {$DEFINE CBUILDER3}
  {$DEFINE DELPHI4}
  {$DEFINE CBUILDER4}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130} { Borland Delphi 5.0 }
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$IFDEF BCB}  { Borland C++Builder 5.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}


{$IFDEF VER140} { Borland Delphi 6.0 }
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$IFDEF BCB}  { Borland C++Builder 6.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER6}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150} { Borland Delphi 7.0 }
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7}
{$ENDIF}

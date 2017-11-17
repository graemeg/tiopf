// 08/02/2010, PH, Added to reduce compiler warnings

//Delphi 2009 or later
{$IFNDEF VER200}
 {$IFNDEF VER210}
  {$IFNDEF VER220}
   {$IFNDEF VER230}  // XE2
   {$IFNDEF VER240}  // XE3
   Defines to suppress compiler warnings are out of date with the compiler version
   {$ENDIF}
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}


// 26/10/2010, PH, Added to reduce compiler warnings

//Delphi 2009 or later
{$IFNDEF VER200}
 {$IFNDEF VER210}
  {$IFNDEF VER220}
   {$IFNDEF VER230}
   Defines to suppress compiler warnings are out of date with the compiler version
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN WIDECHAR_REDUCED OFF}


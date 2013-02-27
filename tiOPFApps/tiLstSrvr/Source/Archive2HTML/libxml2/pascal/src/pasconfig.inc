{
  This file configures capabilities specific to various Pascal compilers.
  All compiler differences should be expressed here.
  2001-2002 (C) Petr Kozelka <pkozelka@email.cz>
}

{$MINENUMSIZE 4}
{$ASSERTIONS OFF}
{$DEFINE ALWAYS_TRUE}
{$UNDEF ALWAYS_FALSE}

// Memory alignment
{$IFDEF VER130} {$ALIGN ON} {$ENDIF}
{$IFDEF VER140} {$ALIGN 4} {$ENDIF}
{$IFDEF FPC} {+$A} {$ENDIF}

// Do we have deprecation directive ?
{$IFDEF VER140} {$DEFINE HAS_DEPR} {$ENDIF}

// Do we have varargs directive for procedures and functions ?
{$IFDEF VER140} {$DEFINE HAS_VA} {$ENDIF}

// Do we have varargs directive for functional types ?
{$IFDEF VER_NOT_YET} {$DEFINE HAS_TVA} {$ENDIF}

// Can we use global variables from shared modules ?
{$IFDEF VER_NOT_YET} {$DEFINE HAS_SO_VARS} {$ENDIF}

// Do we have C-like enumerations, i.e. with the option to assign direct values to individual constants ?
{$IFDEF VER140} {$DEFINE HAS_CENUM} {$ENDIF}

// Do we have support for StrNextChar function ?
{$IFDEF VER140} {$DEFINE HAS_STRNEXTCHAR_SUPPORT} {$ENDIF}


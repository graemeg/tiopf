unit iconv;
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
interface
{ Copyright (C) 1999-2001 Free Software Foundation, Inc.
  This file is part of the GNU LIBICONV Library.

  The GNU LIBICONV Library is free software; you can redistribute it
  and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  The GNU LIBICONV Library is distributed in the hope that it will be
  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with the GNU LIBICONV Library; see the file COPYING.LIB.
  If not, write to the Free Software Foundation, Inc., 59 Temple Place -
  Suite 330, Boston, MA 02111-1307, USA.   }

const
{$IFDEF WIN32}
  LIBICONV_SO = 'libiconv.dll';
{$ENDIF}
{$IFDEF LINUX}
  LIBICONV_SO = 'libiconv.so';
{$ENDIF}


const
  LIBICONV_VERSION = $0107; { version number: (major<<8) + minor  }

function _libiconv_version(): integer;

{ We would like to #include any system header file which could define
  iconv_t, 1. in order to eliminate the risk that the user gets compilation
  errors because some other system header file includes /usr/include/iconv.h
  which defines iconv_t or declares iconv after this file, 2. when compiling
  for LIBICONV_PLUG, we need the proper iconv_t type in order to produce
  binary compatible code.
  But gcc's #include_next is not portable. Thus, once libiconv's iconv.h
  has been installed in /usr/local/include, there is no way any more to
  include the original /usr/include/iconv.h. We simply have to get away
  without it.
  Ad 1. The risk that a system header file does
  #include "iconv.h"  or  #include_next "iconv.h"
  is small. They all do #include <iconv.h>.
  Ad 2. The iconv_t type is a pointer type in all cases I have seen. (It
  has to be a scalar type because (iconv_t)(-1) is a possible return value
  from iconv_open().)  }

type
  { Define iconv_t ourselves.  }
  iconv_t = Pointer;
  { Get size_t declaration.  }
  size_t = Cardinal;

{ Get errno declaration and values.  }
{todo: #include <errno.h>}
{ Some systems, like SunOS 4, don't have EILSEQ. On these systems, define
  EILSEQ ourselves, but don't define it as EINVAL, because iconv() callers
  want to distinguish EINVAL and EILSEQ.  }
{todo
const
  EILSEQ = ENOENT;
}

{ Allocates descriptor for code conversion from encoding `fromcode' to
  encoding `tocode'.  }
function iconv_open (tocode: pchar; fromcode: pchar): iconv_t; cdecl; external LIBICONV_SO
{$IFNDEF LIBICONV_PLUG}
  name 'libiconv_open'
{$ENDIF};

{ Converts, using conversion descriptor `cd', at most ` inbytesleft' bytes
  starting at ` inbuf', writing at most ` outbytesleft' bytes starting at
  ` outbuf'.
  Decrements ` inbytesleft' and increments ` inbuf' by the same amount.
  Decrements ` outbytesleft' and increments ` outbuf' by the same amount.  }
function _iconv (cd: iconv_t; var inbuf: pchar; var inbytesleft: size_t; var outbuf: pchar; var outbytesleft: size_t): size_t; cdecl; external LIBICONV_SO
{$IFNDEF LIBICONV_PLUG}
  name 'libiconv'
{$ENDIF};

{ Frees resources allocated for conversion descriptor `cd'.  }
function iconv_close(cd: iconv_t): integer; cdecl; external LIBICONV_SO
{$IFNDEF LIBICONV_PLUG}
  name 'libiconv_close'
{$ENDIF};

{$IFNDEF LIBICONV_PLUG}
{ Nonstandard extensions.  }

{ Control of attributes.  }
function iconvctl (cd: iconv_t; request: integer; argument: pointer): integer; cdecl; external LIBICONV_SO
  name 'libiconvctl';

{ Requests for iconvctl.  }
const
  { int  argument  }
  ICONV_TRIVIALP = 0;
  { int  argument  }
  ICONV_GET_TRANSLITERATE = 1;
  { const int  argument  }
  ICONV_SET_TRANSLITERATE = 2;
{$ENDIF}

implementation

uses
{$IFDEF WIN32}
  Windows;
{$ENDIF}
{$IFDEF LINUX}
  libc;
{$ENDIF}

{ This is the exported variable really.  }
function _libiconv_version(): integer;
{$IFDEF WIN32}
var
  hLib: Windows.THandle;
  ptr: ^integer;
{$ENDIF}
begin
  result := LIBICONV_VERSION;
{$IFDEF WIN32}
  hLib := Windows.LoadLibrary(LIBICONV_SO);
  if (0= hLib) then begin
    exit;
  end;
  try
    ptr := Windows.GetProcAddress(hLib, '_libiconv_version');
  finally
    Windows.FreeLibrary(hLib);
  end;
  if (not assigned(ptr)) then begin
    exit;
  end;
  result := ptr^;
{$ENDIF}
end;

end.

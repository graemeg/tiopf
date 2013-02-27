// CVS-REV:gnome-xml/include/libxml/encoding.h:1.31:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * encoding.h : interface for the encoding conversion functions needed for
 *              XML
 *
 * Related specs:
 * rfc2044        (UTF-8 and UTF-16) F. Yergeau Alis Technologies
 * [ISO-10646]    UTF-8 and UTF-16 in Annexes
 * [ISO-8859-1]   ISO Latin-1 characters codes.
 * [UNICODE]      The Unicode Consortium, "The Unicode Standard --
 *                Worldwide Character Encoding -- Version 1.0", Addison-
 *                Wesley, Volume 1, 1991, Volume 2, 1992.  UTF-8 is
 *                described in Unicode Technical Report #4.
 * [US-ASCII]     Coded Character Set--7-bit American Standard Code for
 *                Information Interchange, ANSI X3.4-1986.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  xmlCharEncoding = Longint;
const
  (**
   * xmlCharEncoding:
   *
   * Predefined values for some standard encodings.
   * Libxml don't do beforehand translation on UTF8, ISOLatinX.
   * It also support UTF16 (LE and BE) by default.
   *
   * Anything else would have to be translated to UTF8 before being
   * given to the parser itself. The BOM for UTF16 and the encoding
   * declaration are looked at and a converter is looked for at that
   * point. If not found the parser stops here as asked by the XML REC
   * Converter can be registered by the user using xmlRegisterCharEncodingHandler
   * but the current form doesn't allow stateful transcoding (a serious
   * problem agreed !). If iconv has been found it will be used
   * automatically and allow stateful transcoding, the simplest is then
   * to be sure to enable icon and to provide iconv libs for the encoding
   * support needed.
   *)
  XML_CHAR_ENCODING_ERROR     = -1; { No char encoding detected  }
  XML_CHAR_ENCODING_NONE      =  0; { No char encoding detected  }
  XML_CHAR_ENCODING_UTF8      =  1; { UTF-8  }
  XML_CHAR_ENCODING_UTF16LE   =  2; { UTF-16 little endian  }
  XML_CHAR_ENCODING_UTF16BE   =  3; { UTF-16 big endian  }
  XML_CHAR_ENCODING_UCS4LE    =  4; { UCS-4 little endian  }
  XML_CHAR_ENCODING_UCS4BE    =  5; { UCS-4 big endian  }
  XML_CHAR_ENCODING_EBCDIC    =  6; { EBCDIC uh!  }
  XML_CHAR_ENCODING_UCS4_2143 =  7; { UCS-4 unusual ordering  }
  XML_CHAR_ENCODING_UCS4_3412 =  8; { UCS-4 unusual ordering  }
  XML_CHAR_ENCODING_UCS2      =  9; { UCS-2  }
  XML_CHAR_ENCODING_8859_1    = 10; { ISO-8859-1 ISO Latin 1  }
  XML_CHAR_ENCODING_8859_2    = 11; { ISO-8859-2 ISO Latin 2  }
  XML_CHAR_ENCODING_8859_3    = 12; { ISO-8859-3  }
  XML_CHAR_ENCODING_8859_4    = 13; { ISO-8859-4  }
  XML_CHAR_ENCODING_8859_5    = 14; { ISO-8859-5  }
  XML_CHAR_ENCODING_8859_6    = 15; { ISO-8859-6  }
  XML_CHAR_ENCODING_8859_7    = 16; { ISO-8859-7  }
  XML_CHAR_ENCODING_8859_8    = 17; { ISO-8859-8  }
  XML_CHAR_ENCODING_8859_9    = 18; { ISO-8859-9  }
  XML_CHAR_ENCODING_2022_JP   = 19; { ISO-2022-JP  }
  XML_CHAR_ENCODING_SHIFT_JIS = 20; { Shift_JIS  }
  XML_CHAR_ENCODING_EUC_JP    = 21; { EUC-JP  }
  XML_CHAR_ENCODING_ASCII     = 22; { pure ASCII  }

type
  (**
   * xmlCharEncodingInputFunc:
   * @out:  a pointer to an array of bytes to store the UTF-8 result
   * @outlen:  the length of @out
   * @in:  a pointer to an array of chars in the original encoding
   * @inlen:  the length of @in
   *
   * Take a block of chars in the original encoding and try to convert
   * it to an UTF-8 block of chars out.
   *
   * Returns the number of byte written, or -1 by lack of space, or -2
   *     if the transcoding failed.
   * The value of @inlen after return is the number of octets consumed
   *     as the return value is positive, else unpredictiable.
   * The value of @outlen after return is the number of octets consumed.
   *)
  xmlCharEncodingInputFunc = function(aOut: PByte; outlen: Integer; aIn: PByte; inlen: Integer): Integer; cdecl;

  (**
   * xmlCharEncodingOutputFunc:
   * @out:  a pointer to an array of bytes to store the result
   * @outlen:  the length of @out
   * @in:  a pointer to an array of UTF-8 chars
   * @inlen:  the length of @in
   *
   * Take a block of UTF-8 chars in and try to convert it to an other
   * encoding.
   * Note: a first call designed to produce heading info is called with
   * in = NULL. If stateful this should also initialize the encoder state.
   *
   * Returns the number of byte written, or -1 by lack of space, or -2
   *     if the transcoding failed.
   * The value of @inlen after return is the number of octets consumed
   *     as the return value is positive, else unpredictiable.
   * The value of @outlen after return is the number of ocetes consumed.
   *)
  xmlCharEncodingOutputFunc = function(aOut: PByte; outlen: Integer; aIn: PByte; inlen: Integer): Integer; cdecl;

  (*
   * Block defining the handlers for non UTF-8 encodings.
   * If iconv is supported, there is two extra fields.
   *)
  xmlCharEncodingHandlerPtr = ^xmlCharEncodingHandler;
  xmlCharEncodingHandler = record
    name: PChar;
    input: xmlCharEncodingInputFunc;
    output: xmlCharEncodingOutputFunc;
{$IFDEF LIBXML_ICONV_ENABLED}
    iconv_in: iconv_t;
    iconv_out: iconv_t;
{$ENDIF} //LIBXML_ICONV_ENABLED
  end;

(*
 * Interfaces for encoding handlers.
 *)
procedure xmlInitCharEncodingHandlers; cdecl; external LIBXML2_SO;
procedure xmlCleanupCharEncodingHandlers; cdecl; external LIBXML2_SO;
procedure xmlRegisterCharEncodingHandler(handler: xmlCharEncodingHandlerPtr); cdecl; external LIBXML2_SO;
function  xmlGetCharEncodingHandler(enc: xmlCharEncoding): xmlCharEncodingHandlerPtr; cdecl; external LIBXML2_SO;
function  xmlFindCharEncodingHandler(name: PChar): xmlCharEncodingHandlerPtr; cdecl; external LIBXML2_SO;
function  xmlNewCharEncodingHandler(name: PChar; aInput: xmlCharEncodingInputFunc; aOutput: xmlCharEncodingOutputFunc): xmlCharEncodingHandlerPtr; cdecl; external LIBXML2_SO;

(*
 * Interfaces for encoding names and aliases.
 *)
function  xmlAddEncodingAlias(name: PChar; alias: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlDelEncodingAlias(alias: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlGetEncodingAlias(alias: PChar): PChar; cdecl; external LIBXML2_SO;
procedure xmlCleanupEncodingAliases; cdecl; external LIBXML2_SO;
function  xmlParseCharEncoding(name: PChar): xmlCharEncoding; cdecl; external LIBXML2_SO;
function  xmlGetCharEncodingName(enc: xmlCharEncoding): PChar; cdecl; external LIBXML2_SO;

(*
 * Interfaces directly used by the parsers.
 *)
function  xmlDetectCharEncoding(aIn: PByte; len: Longint): xmlCharEncoding; cdecl; external LIBXML2_SO;
function  xmlCharEncOutFunc(handler: xmlCharEncodingHandlerPtr; aOut: xmlBufferPtr; aIn: xmlBufferPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlCharEncInFunc(handler: xmlCharEncodingHandlerPtr; aOut: xmlBufferPtr; aIn: xmlBufferPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlCharEncFirstLine(handler: xmlCharEncodingHandlerPtr; aOut: xmlBufferPtr; aIn: xmlBufferPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlCharEncCloseFunc(handler: xmlCharEncodingHandlerPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Export a few useful functions.
 *)
function  UTF8Toisolat1(aOut: PByte; outlen: PLongint; aIn: PByte; inlen: PLongint): Longint; cdecl; external LIBXML2_SO;
function  isolat1ToUTF8(aOut: PByte; outlen: PLongint; aIn: PByte; inlen: PLongint): Longint; cdecl; external LIBXML2_SO;
function  xmlGetUTF8Char(utf: PChar; len: PLongint): Longint; cdecl; external LIBXML2_SO;

(*
 * exports additional "UTF-8 aware" string routines which are.
 *)
function  xmlCheckUTF8(utf: PByte): Longint; cdecl; external LIBXML2_SO;
function  xmlUTF8Strsize(utf: PxmlChar; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlUTF8Strndup(utf: PxmlChar; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlUTF8Strpos(utf: PxmlChar; pos: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlUTF8Strloc(utf: PxmlChar; utfchar: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlUTF8Strsub(utf: PxmlChar; start: Longint; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlUTF8Strlen(utf: PxmlChar): Longint; cdecl; external LIBXML2_SO;


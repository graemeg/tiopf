// CVS-REV:gnome-xml/include/libxml/xmlerror.h:1.12:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

type
  xmlParserErrors = Longint;
const
  XML_ERR_OK                        = 0;
  XML_ERR_INTERNAL_ERROR            = 1;
  XML_ERR_NO_MEMORY                 = 2;
  XML_ERR_DOCUMENT_START            = 3;
  XML_ERR_DOCUMENT_EMPTY            = 4;
  XML_ERR_DOCUMENT_END              = 5;
  XML_ERR_INVALID_HEX_CHARREF       = 6;
  XML_ERR_INVALID_DEC_CHARREF       = 7;
  XML_ERR_INVALID_CHARREF           = 8;
  XML_ERR_INVALID_CHAR              = 9;
  XML_ERR_CHARREF_AT_EOF            = 10;
  XML_ERR_CHARREF_IN_PROLOG         = 11;
  XML_ERR_CHARREF_IN_EPILOG         = 12;
  XML_ERR_CHARREF_IN_DTD            = 13;
  XML_ERR_ENTITYREF_AT_EOF          = 14;
  XML_ERR_ENTITYREF_IN_PROLOG       = 15;
  XML_ERR_ENTITYREF_IN_EPILOG       = 16;
  XML_ERR_ENTITYREF_IN_DTD          = 17;
  XML_ERR_PEREF_AT_EOF              = 18;
  XML_ERR_PEREF_IN_PROLOG           = 19;
  XML_ERR_PEREF_IN_EPILOG           = 20;
  XML_ERR_PEREF_IN_INT_SUBSET       = 21;
  XML_ERR_ENTITYREF_NO_NAME         = 22;
  XML_ERR_ENTITYREF_SEMICOL_MISSING = 23;
  XML_ERR_PEREF_NO_NAME             = 24;
  XML_ERR_PEREF_SEMICOL_MISSING     = 25;
  XML_ERR_UNDECLARED_ENTITY         = 26;
  XML_WAR_UNDECLARED_ENTITY         = 27;
  XML_ERR_UNPARSED_ENTITY           = 28;
  XML_ERR_ENTITY_IS_EXTERNAL        = 29;
  XML_ERR_ENTITY_IS_PARAMETER       = 30;
  XML_ERR_UNKNOWN_ENCODING          = 31;
  XML_ERR_UNSUPPORTED_ENCODING      = 32;
  XML_ERR_STRING_NOT_STARTED        = 33;
  XML_ERR_STRING_NOT_CLOSED         = 34;
  XML_ERR_NS_DECL_ERROR             = 35;
  XML_ERR_ENTITY_NOT_STARTED        = 36;
  XML_ERR_ENTITY_NOT_FINISHED       = 37;
  XML_ERR_LT_IN_ATTRIBUTE           = 38;
  XML_ERR_ATTRIBUTE_NOT_STARTED     = 39;
  XML_ERR_ATTRIBUTE_NOT_FINISHED    = 40;
  XML_ERR_ATTRIBUTE_WITHOUT_VALUE   = 41;
  XML_ERR_ATTRIBUTE_REDEFINED       = 42;
  XML_ERR_LITERAL_NOT_STARTED       = 43;
  XML_ERR_LITERAL_NOT_FINISHED      = 44;
  XML_ERR_COMMENT_NOT_FINISHED      = 45;
  XML_ERR_PI_NOT_STARTED            = 46;
  XML_ERR_PI_NOT_FINISHED           = 47;
  XML_ERR_NOTATION_NOT_STARTED      = 48;
  XML_ERR_NOTATION_NOT_FINISHED     = 49;
  XML_ERR_ATTLIST_NOT_STARTED       = 50;
  XML_ERR_ATTLIST_NOT_FINISHED      = 51;
  XML_ERR_MIXED_NOT_STARTED         = 52;
  XML_ERR_MIXED_NOT_FINISHED        = 53;
  XML_ERR_ELEMCONTENT_NOT_STARTED   = 54;
  XML_ERR_ELEMCONTENT_NOT_FINISHED  = 55;
  XML_ERR_XMLDECL_NOT_STARTED       = 56;
  XML_ERR_XMLDECL_NOT_FINISHED      = 57;
  XML_ERR_CONDSEC_NOT_STARTED       = 58;
  XML_ERR_CONDSEC_NOT_FINISHED      = 59;
  XML_ERR_EXT_SUBSET_NOT_FINISHED   = 60;
  XML_ERR_DOCTYPE_NOT_FINISHED      = 61;
  XML_ERR_MISPLACED_CDATA_END       = 62;
  XML_ERR_CDATA_NOT_FINISHED        = 63;
  XML_ERR_RESERVED_XML_NAME         = 64;
  XML_ERR_SPACE_REQUIRED            = 65;
  XML_ERR_SEPARATOR_REQUIRED        = 66;
  XML_ERR_NMTOKEN_REQUIRED          = 67;
  XML_ERR_NAME_REQUIRED             = 68;
  XML_ERR_PCDATA_REQUIRED           = 69;
  XML_ERR_URI_REQUIRED              = 70;
  XML_ERR_PUBID_REQUIRED            = 71;
  XML_ERR_LT_REQUIRED               = 72;
  XML_ERR_GT_REQUIRED               = 73;
  XML_ERR_LTSLASH_REQUIRED          = 74;
  XML_ERR_EQUAL_REQUIRED            = 75;
  XML_ERR_TAG_NAME_MISMATCH         = 76;
  XML_ERR_TAG_NOT_FINISED           = 77;
  XML_ERR_STANDALONE_VALUE          = 78;
  XML_ERR_ENCODING_NAME             = 79;
  XML_ERR_HYPHEN_IN_COMMENT         = 80;
  XML_ERR_INVALID_ENCODING          = 81;
  XML_ERR_EXT_ENTITY_STANDALONE     = 82;
  XML_ERR_CONDSEC_INVALID           = 83;
  XML_ERR_VALUE_REQUIRED            = 84;
  XML_ERR_NOT_WELL_BALANCED         = 85;
  XML_ERR_EXTRA_CONTENT             = 86;
  XML_ERR_ENTITY_CHAR_ERROR         = 87;
  XML_ERR_ENTITY_PE_INTERNAL        = 88;
  XML_ERR_ENTITY_LOOP               = 89;
  XML_ERR_ENTITY_BOUNDARY           = 90;
  XML_ERR_INVALID_URI               = 91;
  XML_ERR_URI_FRAGMENT              = 92;
  XML_WAR_CATALOG_PI                = 93;
  XML_ERR_NO_DTD                    = 94;

type
  (**
   * xmlGenericErrorFunc:
   * @ctx:  a parsing context
   * @msg:  the message
   * @...:  the extra arguments of the varags to format the message
   *
   * Signature of the function to use when there is an error and
   * no parsing or validity context available.
   *)
  xmlGenericErrorFunc = procedure(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_TVA} varargs; {$ENDIF}
  // note: the above modifier is commented because this syntax is not supported.
  //TODO: report it to Borland!
(*
 * Use the following function to reset the two global variables
 * xmlGenericError and xmlGenericErrorContext.
 *)
procedure xmlSetGenericErrorFunc(ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXML2_SO;
procedure xmlInitGenericErrorDefaultFunc(var handler: xmlGenericErrorFunc); cdecl; external LIBXML2_SO;

(*
 * Default message routines used by SAX and Valid context for error
 * and warning reporting.
 *)
procedure xmlParserError(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;
procedure xmlParserWarning(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;
procedure xmlParserValidityError(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;
procedure xmlParserValidityWarning(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;
procedure xmlParserPrintFileInfo(input: xmlParserInputPtr); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;
procedure xmlParserPrintFileContext(input: xmlParserInputPtr); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;


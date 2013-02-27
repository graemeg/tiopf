// CVS-REV:gnome-xml/include/libxml/xinclude.h:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * xinclude.c : API to handle XInclude processing
 *
 * World Wide Web Consortium Working Draft 26 October 2000
 * http://www.w3.org/TR/2000/WD-xinclude-20001026
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)
function  xmlXIncludeProcess(doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;


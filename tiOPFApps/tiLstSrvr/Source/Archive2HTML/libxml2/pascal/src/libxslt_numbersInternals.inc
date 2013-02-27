// CVS-REV:libxslt/libxslt/numbersInternals.h:1.12:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * numbers.h: Implementation of the XSLT number functions
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 * Bjorn Reese <breese@users.sourceforge.net>
 *)

(**
 * xsltNumberData:
 *
 * This data structure is just a wrapper to pass xsl: number data in.
 *)
type
  xsltNumberDataPtr = ^xsltNumberData;
  xsltNumberData = record
    level: PxmlChar;
    count: PxmlChar;
    from: PxmlChar;
    value: PxmlChar;
    format: PxmlChar;
    has_format: Longint;
    digitsPerGroup: Longint;
    groupingCharacter: Longint;
    groupingCharacterLen: Longint;
    doc: xmlDocPtr;
    node: xmlNodePtr;
  end;

  (**
   * xsltFormatNumberInfo,:
   *
   * This data structure lists the various parameters needed to format numbers.
   *)
  xsltFormatNumberInfoPtr = ^xsltFormatNumberInfo;
  xsltFormatNumberInfo = record
    integer_hash: Longint;                       (* Number of '#' in integer part *)
    integer_digits: Longint;                     (* Number of '0' in integer part *)    frac_digits: Longint;                        (* Number of '0' in fractional part *)    frac_hash: Longint;                          (* Number of '#' in fractional part *)    group: Longint;                              (* Number of chars per display 'group' *)    multiplier: Longint;                         (* Scaling for percent or permille *)    add_decimal: char;                           (* Flag for whether decimal point appears in pattern *)    is_multiplier_set: char;                     (* Flag to catch multiple occurences of percent/permille *)    is_negative_pattern: char;                   (* Flag for processing -ve prefix/suffix *)  end;


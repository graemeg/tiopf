unit libxslt;
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

interface

uses libxml2;

const
{$IFDEF WIN32}
  LIBXSLT_SO = 'libxslt.dll';
  LIBEXSLT_SO = 'libexslt.dll';
{$ENDIF}
{$IFDEF LINUX}
  LIBXSLT_SO = 'libxslt.so';
  LIBEXSLT_SO = 'libexslt.so';
{$ENDIF}

{$I pasconfig.inc}

type
  xsltRuntimeExtraPtr = Pointer;

{$I libxslt_xslt.inc}
{$I libxslt_xsltwin32config.inc}
{$I libxslt_numbersInternals.inc}
{$I libxslt_xsltInternals.inc}
{$I libxslt_transform.inc}
{$I libxslt_xsltutils.inc}
{$I libxslt_attributes.inc}
{$I libxslt_documents.inc}

{$I libxslt_extensions.inc}
{$I libxslt_extra.inc}
{$I libxslt_functions.inc}
{$I libxslt_keys.inc}
{$I libxslt_namespaces.inc}
{$I libxslt_pattern.inc}
{$I libxslt_preproc.inc}
{$I libxslt_templates.inc}
{$I libxslt_imports.inc}
{$I libxslt_variables.inc}
{$I libxslt_security.inc}

{TODO: $I  libxslt_xsltconfig.inc}

{$I libexslt_exslt.inc}
{$I libexslt_exsltconfig.inc}

implementation

end.


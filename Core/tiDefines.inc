// Delphi 5
{$IFDEF VER130}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//Delphi 6
{$IFDEF VER140}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI6}
{$ENDIF}

//Delphi 7
{$IFDEF VER150}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI7}
{$ENDIF}

//Delphi 8
{$IFDEF VER160}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI8}
  {$DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//Delphi 9 (2005)
{$IFDEF VER170}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI9}
  {. $DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//Delphi 10 (2006)
{$IFDEF VER180}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI10ORABOVE}
  {$DEFINE DELPHI10}
  {. $DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
{$ENDIF}

// Free Pascal
{$IFDEF FPC}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}

  {$MODE DELPHI}{$H+}
  {$ASMMODE intel}
{$ENDIF}

// Cloned from IndySoap

{=============================================================================
  Defines Relating to Object Tracking
 =============================================================================}

{.$DEFINE CLASS_TRACKING}
  { Allows basic leak tracking. A count of each type of class is
    kept and a supressed exception is raised if any descendent objects are
    still alive when the application terminates. See tiBaseObject.pas for details.}

{.$DEFINE OBJECT_TRACKING}
  { Complete Object Tracking. A full list of Objects is maintained in
    a separate list. Objects are cross-compared with the list. Not only
    does this provide leak tracking on a class per class basis, but it
    allows for authoritative checking that the object still points to
    an actual live instance that it is meant to. In addition, a watch can
    be requested on an object so that an break point will be called when
    the object is freed. See tiObjAbs.pas for details. }

{$IFDEF OBJECT_TRACKING}
  {$DEFINE CLASS_TRACKING}
{$ENDIF}

// If Delphi 7, turn off UNSAFE_* Warnings (just until we have fixed them all)
// We want the platform warnings as we are working towards Linux support.
{$IFDEF DELPHI7ORABOVE}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

// Turn this on if you have SynEdit installed and want colour syntax hilighting in
// the tiSQLManager and tiSQLEditor, SynEdit is available from synedit.sourceforge.net
{.$DEFINE SYNEDIT}

// For old tiOPF applications where TOID was an Int64, not an object
{.$DEFINE OID_AS_INT64}

// Determine format for date/time to string conversion
// This is currently only referenced in tiDateTimeAsXMLString
// and should be replaced by a more suitable solution
// cf: Support NG thread starting 15/08/2006
{.$DEFINE DATEFORMAT_YYYYMMDD}

// Determine format for boolean to string conversion
// This is currently only referenced in TtiQueryParamBoolean.GetValueAsString
// and should be replaced by a more suitable solution
// (along with BOOLEAN_CHAR_1)
// Note: If enabled, this value overrides BOOLEAN_CHAR_1
// It is only used for setting parameters because when converting from string 
// back to boolean, a multi-test is generally performed (ie. if 'TRUE', 'T', '1')
// cf: Support NG thread starting 15/08/2006
{.$DEFINE BOOLEAN_NUM_1}

// Some databases do not have a native boolean type, so you must roll it yourself 
// from a CHAR or VARCHAR. The tiOPF will use 'T' & 'F' or 'TRUE ' & 'FALSE'. This
// can be controled with the compiler directive below:
{$DEFINE BOOLEAN_CHAR_1}

// tiPerAwareCtrls will use either "Courier New" or "MS Sans Serif"
// for GUI controls based on this directive.
// Enable for Courier New
{$DEFINE GUI_FIXED_FONT}

// The MSXML and XMLLight persistence layers use a custom XML format that mapps
// roughly into a database schema. If this OPTIMISE_XMLDB_SIZE is not $DEFINEd,
// then the XML will include tags like <table_name> and <field_name>. IF
// OPTIMISE_XMLDB_SIZE is $DEFINEd, then these tags will be replaced with smaller
// equivalents like <tn> and <fn>.
{.$DEFINE OPTIMISE_XMLDB_SIZE}

// Turn ThirdPartyExceptionHandling on to change how the tiOPF handles exceptions.
// If ThridPartyExceptionHandling is on, then an exception will usually cause
// a scrolling exception window to display, but will not cause the application
// to stop running.
// A couple of signature changes were made between madExcept v2 and v3.
// Refer routine tiExcept.tiMailBugReport
{$IFDEF MadExcept3}
  {$DEFINE MadExcept}
{$ENDIF}
{$IFDEF MadExcept}
  {$DEFINE ThirdPartyExceptionHandling}
{$ENDIF}

// Use these defines, along with tiCompileTimeLinkPerLayers.pas to force static linking
// at compile time of a persistence layer. These defines must be kept in
// sync with tiCompileTimeLinkPerLayers.pas
// Note: When adding another persistence layer, compiler directive must also
//       be added to tiOPFManager.pas
{$IFDEF LINK_XML}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_IBX}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_BDEPARADOX}  {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ADOACCESS}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ADOSQLSERVER}{$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_CSV}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_TAB}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_XMLLIGHT}    {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_DOA}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_REMOTE}      {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_SQLDB_IB}    {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_FBL}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}

{$IFDEF DELPHI6ORABOVE}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$IFDEF DELPHI7ORABOVE}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

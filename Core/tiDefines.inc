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

//Delphi 10 (2006) & 11 (2007)
{$IFDEF VER180}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI10ORABOVE}
  {$DEFINE DELPHI2006ORABOVE}

  //Delphi 10 (2006)
  {$IFNDEF VER185}
    {$DEFINE DELPHI10}
    {$DEFINE DELPHI2006}
  //Delphi 11 (2007)
  {$ELSE}
    {$DEFINE DELPHI11ORABOVE}
    {$DEFINE DELPHI11}
    {$DEFINE DELPHI2007ORABOVE}
    {$DEFINE DELPHI2007}
  {$ENDIF}
  {. $DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//Delphi 12 (2009)
{$IFDEF VER200}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI10ORABOVE}
  {$DEFINE DELPHI2006ORABOVE}
  {$DEFINE DELPHI11ORABOVE}
  {$DEFINE DELPHI12ORABOVE}
  {$DEFINE DELPHI12}
  {$DEFINE DELPHI2007ORABOVE}
  {$DEFINE DELPHI2009ORABOVE}
  {$DEFINE DELPHI2009}
  {. $DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
  {$DEFINE GENERICS}
{$ENDIF}

//Delphi 14 (2010)
{$IFDEF VER210}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI10ORABOVE}
  {$DEFINE DELPHI2006ORABOVE}
  {$DEFINE DELPHI11ORABOVE}
  {$DEFINE DELPHI12ORABOVE}
  {$DEFINE DELPHI13ORABOVE}
  {$DEFINE DELPHI13}
  {$DEFINE DELPHI14}
  {$DEFINE DELPHI2007ORABOVE}
  {$DEFINE DELPHI2009ORABOVE}
  {$DEFINE DELPHI2010ORABOVE}
  {$DEFINE DELPHI2010}
  {. $DEFINE DOTNET}
  {$DEFINE MSWINDOWS}
  {$DEFINE GENERICS}
{$ENDIF}

//Delphi 15 (XE)
{$IFDEF VER220}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}
  {$DEFINE DELPHI8ORABOVE}
  {$DEFINE DELPHI9ORABOVE}
  {$DEFINE DELPHI10ORABOVE}
  {$DEFINE DELPHI2006ORABOVE}
  {$DEFINE DELPHI11ORABOVE}
  {$DEFINE DELPHI12ORABOVE}
  {$DEFINE DELPHI13ORABOVE}
  {$DEFINE DELPHI2007ORABOVE}
  {$DEFINE DELPHI2009ORABOVE}
  {$DEFINE DELPHI2010ORABOVE}
  {$DEFINE DELPHIXEORABOVE}
  {$DEFINE DELPHIXE}
  {$DEFINE MSWINDOWS}
  {$DEFINE GENERICS}
{$ENDIF}

// Free Pascal
{$IFDEF FPC}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}

  {$MODE DELPHI}{$H+}
  {$ASMMODE intel}
  // FPC includes this unit with the compiler
  {$define USE_BASE64_UNIT}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}
{$IFDEF DUNIT2}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}

{$IFDEF DELPHI2009ORABOVE}
  tiOPF2 does not support Delphi 2009 or above. Please use tiOPF3.
{$ENDIF}

{: Enable the conditional define REFERENCE_COUNTING to enable reference counting
   on TtiBaseObject}
{$DEFINE REFERENCE_COUNTING}

// If Delphi 7, turn off UNSAFE_* Warnings (just until we have fixed them all)
// We want the platform warnings as we are working towards Linux support.
{$IFDEF DELPHI7ORABOVE}
  {$IFNDEF FPC}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN DUPLICATE_CTOR_DTOR OFF}
  {$ENDIF}
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
// If both BOOLEAN_NUM_1 and BOOLEAN_CHAR_1 are undefined, then 'TRUE ' & 'FALSE'
// is used.
{$DEFINE BOOLEAN_CHAR_1}
{$IFDEF BOOLEAN_NUM_1}
  {$UNDEF BOOLEAN_CHAR_1}
{$ENDIF}

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
{$IFDEF MadExcept}
  {$DEFINE ThirdPartyExceptionHandling}
{$ENDIF}

// Do we have a 3rd party Regex (regular expressions) unit available. If so,
// then enable this feature. Currently used in tiIteratorIntf unit.
{$IFDEF FPC}
  {$DEFINE ThirdPartyRegex}   // FPC comes standard with a regex unit
{$ELSE}
  {.$DEFINE ThirdPartyRegex}
{$ENDIF}


{$IFDEF DELPHI6ORABOVE}
  {$IFNDEF FPC}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_LIBRARY OFF}
    {$WARN UNIT_LIBRARY OFF}
    {$WARN UNIT_PLATFORM OFF}
    {$IFDEF DELPHI7ORABOVE}
      {$WARN UNSAFE_TYPE OFF}
      {$WARN UNSAFE_CODE OFF}
      {$WARN UNSAFE_CAST OFF}
      {$IFDEF DELPHI2007ORABOVE}
        // Turn off warning: [DCC Warning] tiOPFCore.dpk(67):
        //   W1029 Duplicate constructor 'TtiLogToFile.CreateWithDateInFileName'
        //   with identical parameters will be inacessible from C++
        // ToDo: This compiles in D2007. Check other compiler versions.
        {$WARN DUPLICATE_CTOR_DTOR OFF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF FPC}
{$ENDIF}


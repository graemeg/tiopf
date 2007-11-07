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

// Free Pascal
{$IFDEF FPC}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE DELPHI6ORABOVE}
  {$DEFINE DELPHI7ORABOVE}

  {$MODE DELPHI}{$H+}
  {$ASMMODE intel}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}
{$IFDEF DUNIT2}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}


// Cloned from IndySoap

{=============================================================================
  Defines Relating to Object Tracking
 =============================================================================}

{.$DEFINE OBJECT_TRACKING}
  { Allows basic leak tracking. A count of each type of class is
    kept and a supressed exception is raised if any descendent objects are
    still alive when the application terminates. See tiBaseObject.pas for details.

    A full list of Objects is maintained in a separate list.
    Objects are cross-compared with the list. Not only does this provide leak
    tracking on a class per class basis, but it allows for authoritative
    checking that the object still points to an actual live instance that it is
    meant to. In addition, a watch can be requested on an object so that an
    break point will be called when the object is freed. See tiBaseObject.pas
    for details. }

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
{$IFDEF LINK_ZEOS_FB10}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ZEOS_FB15}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}



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
    {$IFDEF DELPHI2007ORABOVE}
      // Turn off warning: [DCC Warning] tiOPFCore.dpk(67):
      //   W1029 Duplicate constructor 'TtiLogToFile.CreateWithDateInFileName'
      //   with identical parameters will be inacessible from C++
      // ToDo: This compiles in D2007. Check other compiler versions.
      {$WARN DUPLICATE_CTOR_DTOR OFF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

(*
// Here's a full list of BDS2006 compiler directives
// Might be useful to include them down the track.
{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE ON}
{$WARN UNSAFE_CODE ON}
{$WARN UNSAFE_CAST ON}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
*)

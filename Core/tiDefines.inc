{$IFNDEF FPC} // Delphi compiler
  // allow compiler to use old IFEND
  {$IF CompilerVersion >= 24.0 }
    {$LEGACYIFEND ON}
  {$IFEND}
  
  {$IF CompilerVersion >= 20}
    {$DEFINE DELPHI2009ORABOVE}  // RAD Studio 2009 (14; BDS 6)
    {$DEFINE DELPHI2009}
    {$DEFINE GENERICS}
    {$IF CompilerVersion >= 21}
      {$DEFINE DELPHI2010ORABOVE}  // RAD Studio 2010 (15; BDS 7)
      {$UNDEF DELPHI2009}
      {$DEFINE DELPHI2010}
      {$IF CompilerVersion >= 22}
        {$DEFINE DELPHIXEORABOVE}  // RAD Studio XE 1 (16; BDS 8)
        {$UNDEF DELPHI2010}
        {$DEFINE DELPHIXE}
        {$IF CompilerVersion >= 23}
          {$DEFINE DELPHIXE2ORABOVE}  // RAD Studio XE 2 (17; BDS 9)
          {$UNDEF DELPHIXE}
          {$DEFINE DELPHIXE2}
          {$IF CompilerVersion >= 24}
            {$DEFINE DELPHIXE3ORABOVE}  // RAD Studio XE 3 (18; BDS 10)
            {$UNDEF DELPHIXE2}
            {$DEFINE DELPHIXE3}
            {$IF CompilerVersion >= 25}
              {$DEFINE DELPHIXE4ORABOVE}  // RAD Studio XE 4 (19; BDS 11)
              {$UNDEF DELPHIXE3}
              {$DEFINE DELPHIXE4}
              {$IF CompilerVersion >= 26}
                {$DEFINE DELPHIXE5ORABOVE}  // RAD Studio XE 5 (20; BDS 12)
                {$UNDEF DELPHIXE4}
                {$DEFINE DELPHIXE5}
                {$IF CompilerVersion >= 27}
                  {$DEFINE DELPHIXE6ORABOVE}  // RAD Studio XE 6 (21; BDS 14)
                  {$UNDEF DELPHIXE5}
                  {$DEFINE DELPHIXE6}
                  {$IF CompilerVersion >= 28}
                    {$DEFINE DELPHIXE7ORABOVE}  // RAD Studio XE 7 (22; BDS 15)
                    {$UNDEF DELPHIXE6}
                    {$DEFINE DELPHIXE7}
                    {$IF CompilerVersion >= 29}
                      {$DEFINE DELPHIXE8ORABOVE}  // RAD Studio XE 8 (23; BDS 16)
                      {$UNDEF DELPHIXE7}
                      {$DEFINE DELPHIXE8}
                      {$IF CompilerVersion >= 30}
                        {$DEFINE DELPHIXE10ORABOVE}  // RAD Studio 10 Seattle (24; BDS 17)
                        {$UNDEF DELPHIXE8}
                        {$DEFINE DELPHIXE10}
                        {$IF CompilerVersion >= 31}
                          {$DEFINE DELPHIXE101ORABOVE}  // RAD Studio 10.1 Berlin (25; BDS 18)
                          {$UNDEF DELPHIXE10}
                          {$DEFINE DELPHIXE101}
                          {$IF CompilerVersion >= 32}
                            {$DEFINE DELPHIXE102ORABOVE}  // RAD Studio 10.2 Tokyo (26; BDS 19)
                            {$UNDEF DELPHIXE101}
                            {$DEFINE DELPHIXE102}
                            {$IF CompilerVersion >= 33}
Unsupported Delphi version.
If you get here, you are probably trying to compile tiOPF with a new Delphi release.
Please notify the tiOPF developers or if you can, supply a patch to support the
new release.
                            {$IFEND}
                          {$IFEND} // XE 10.2 Tokyo
                        {$IFEND} // XE 10.1 Berlin
                      {$IFEND} // XE10 Seattle
                    {$IFEND} // XE8
                  {$IFEND} // XE7
                {$IFEND} // XE6
              {$IFEND} // XE5
            {$IFEND} // XE4
          {$IFEND} // XE3
        {$IFEND} //XE2
      {$IFEND} // XE
    {$IFEND} // 2010
  {$IFEND} // 2009
{$ENDIF} // Delphi versions

// Free Pascal
{$IFDEF FPC}
  {$DEFINE DELPHI2010ORABOVE}
  {$MODE DELPHIUNICODE}{//$H+}
  {$ASMMODE intel}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}

{$IFDEF DUNIT2}
  {$DEFINE DUNIT2ORFPC}
{$ENDIF}

// MAC OS
{$IFDEF POSIX}
  {$DEFINE UNIX}
{$ENDIF}


{: Enable the conditional define REFERENCE_COUNTING to enable reference counting
   on TtiBaseObject}
{$DEFINE REFERENCE_COUNTING}

// Turn this on if you have SynEdit installed and want colour syntax hilighting in
// the tiSQLManager and tiSQLEditor, SynEdit is available from synedit.sourceforge.net
{.$DEFINE SYNEDIT}

// Turn this on if you have VirtualTrees installed and want virtual treeview support
// in TtiVT... controls and mediators
{.$DEFINE VIRTUAL_TREEVIEW}

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

{: The TtiTestCase class adds the method UnderConstruction, that can be called
   to force the test to fail. This can be useful while the test is being
   written. The Fail('Under construction') call can be turned off with this
   conditional define.}
{.$DEFINE IGNORE_UNDER_CONSTRUCTION}

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

// Enable if you have Delphi and JCL installed. Used by tiOPF test suite.
{.$DEFINE USE_JEDI_JCL}

// Use these defines, along with tiCompileTimeLinkPerLayers.pas to force static linking
// at compile time of a persistence layer. These defines must be kept in
// sync with tiCompileTimeLinkPerLayers.pas
// Note: When adding another persistence layer, compiler directive must also
//       be added to tiOPFManager.pas
{$IFDEF LINK_XML}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_IBX}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_FIBP}        {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_BDEPARADOX}  {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ADOACCESS}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ADOSQLSERVER}{$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_CSV}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_TAB}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_UIB_FB}      {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_XMLLIGHT}    {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_DOA}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_REMOTE}      {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_SQLDB_IB}    {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_FBL}         {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ZEOS_FB10}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_ZEOS_FB15}   {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}

{$IFDEF DELPHI2009ORABOVE}
  // These warnings must remain off as there will always be unsafe code
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  // This warning should remain off as the duplicate constructor is convenient.
  // W1029 Duplicate constructor 'TtiLogToCacheAbs.CreateSynchronized',
  // 'TtiLogToFile.CreateOverwriteOld' & 'TtiLogToFile.CreateWithDateInFileName'
  // with identical parameters will be inacessible from C++
  {$WARN DUPLICATE_CTOR_DTOR OFF}
  // These warnings might be turned back on as we fix the Unicode related warnings
  {$WARN EXPLICIT_STRING_CAST OFF}
  {$WARN EXPLICIT_STRING_CAST_LOSS OFF}
  {$WARN HRESULT_COMPAT OFF}
{$ENDIF}

(*
// Here's a full list of Delphi 2010 compiler directives
// Might be useful to include them down the track.

  {$WARN SYMBOL_EXPERIMENTAL OFF}
  {$WARN XML_CREF_NO_RESOLVE OFF}
  {$WARN CVT_WCHAR_TO_ACHAR OFF}
  {$WARN XML_WHITESPACE_NOT_ALLOWED OFF}
  {$WARN CVT_NARROWING_STRING_LOST OFF}
  {$WARN XML_NO_MATCHING_PARM OFF}
  {$WARN IMAGEBASE_MULTIPLE OFF}
  {$WARN DUPLICATES_IGNORED OFF}
  {$WARN SUSPICIOUS_TYPECAST OFF}
  {$WARN IMPLICIT_VARIANTS OFF}
  {$WARN RLINK_WARNING OFF}
  {$WARN BAD_GLOBAL_SYMBOL OFF}
  {$WARN LOCAL_PINVOKE OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN USE_BEFORE_DEF OFF}
  {$WARN XML_EXPECTED_CHARACTER OFF}
  {$WARN FILE_OPEN OFF}
  {$WARN ZERO_NIL_COMPAT OFF}
  {$WARN CVT_ACHAR_TO_WCHAR OFF}
  {$WARN COMBINING_SIGNED_UNSIGNED OFF}
  {$WARN COMPARING_SIGNED_UNSIGNED OFF}
  {$WARN FOR_LOOP_VAR_VARPAR OFF}
  {$WARN HRESULT_COMPAT OFF}
  {$WARN PACKAGED_THREADVAR OFF}
  {$WARN XML_INVALID_NAME OFF}
  {$WARN OPTION_TRUNCATED OFF}
  {$WARN ASG_TO_TYPED_CONST OFF}
  {$WARN WIDECHAR_REDUCED OFF}
  {$WARN FOR_LOOP_VAR_UNDEF OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN HIDDEN_VIRTUAL OFF}
  {$WARN PRIVATE_PROPACCESSOR OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN DUPLICATE_CTOR_DTOR OFF}
  {$WARN COMPARISON_FALSE OFF}
  {$WARN CONSTRUCTING_ABSTRACT OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN UNSUPPORTED_CONSTRUCT OFF}
  {$WARN STRING_CONST_TRUNCED OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN NO_RETVAL OFF}
  {$WARN NO_CFG_FILE_FOUND OFF}
  {$WARN UNIT_NAME_MISMATCH OFF}
  {$WARN XML_NO_PARM OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN XML_INVALID_NAME_START OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN CASE_LABEL_RANGE OFF}
  {$WARN COMPARISON_TRUE OFF}
  {$WARN TYPED_CONST_VARPAR OFF}
  {$WARN FILE_OPEN_UNITSRC OFF}
  {$WARN UNIT_INIT_SEQ OFF}
  {$WARN UNIT_EXPERIMENTAL OFF}
  {$WARN LOCALE_TO_UNICODE OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN XML_UNKNOWN_ENTITY OFF}
  {$WARN CVT_WIDENING_STRING_LOST OFF}
  {$WARN PACKAGE_NO_LINK OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN TYPEINFO_IMPLICITLY_ADDED OFF}
  {$WARN BOUNDS_ERROR OFF}
  {$WARN FOR_VARIABLE OFF}
  {$WARN MESSAGE_DIRECTIVE OFF}
  {$WARN HIDING_MEMBER OFF}
  {$WARN HPPEMIT_IGNORED OFF}
  {$WARN UNICODE_TO_LOCALE OFF}
  {$WARN INVALID_DIRECTIVE OFF}
  {$WARN IMPLICIT_IMPORT OFF}
  {$WARN GARBAGE OFF}
  {$WARN UNIT_DEPRECATED OFF}

*)

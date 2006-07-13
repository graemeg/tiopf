// Delphi 5
{$IFDEF VER130}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI5ORABOVE}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//Delphi 6 & Kylix 1-3
{$IFDEF VER140}
  {$IFDEF MSWINDOWS}
    {$DEFINE DELPHI5ORABOVE}
    {$DEFINE DELPHI6ORABOVE}
    {$DEFINE DELPHI6}
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
    // Kylix is compatible with Delphi 6, so we include them here as well.
    // Might change this later.
    {$DEFINE DELPHI5ORABOVE}
    {$DEFINE DELPHI6ORABOVE}
    {$DEFINE DELPHI6}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IFDEF CompilerVersion}
         // Important:  Don't use CompilerVersion here as 
         // $IF's are evaluated before $IFDEF's 
         // and Kylix 1 does not have CompilerVersion defined at all. 
         {$IF Declared(RTLVersion) and (RTLVersion = 14.20)} 
           {$DEFINE KYLIX2} 
           {$DEFINE KYLIX1ORABOVE} 
           {$DEFINE KYLIX2ORABOVE} 
         {$IFEND} 
         {$IF Declared(RTLVersion) and (RTLVersion = 14.50)} 
           {$DEFINE KYLIX3} 
           {$DEFINE KYLIX1ORABOVE} 
           {$DEFINE KYLIX2ORABOVE} 
           {$DEFINE KYLIX3ORABOVE} 
         {$IFEND} 
      {$ELSE} 
         //CompilerVersion is not defined under Kylix 1 
         {$DEFINE KYLIX1} 
         {$DEFINE KYLIX1ORABOVE} 
      {$ENDIF} 
    {$ENDIF}
  {$ENDIF}
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
  {$DEFINE KYLIX1ORABOVE}
  {$DEFINE KYLIX2ORABOVE}
  {$DEFINE KYLIX3ORABOVE}
  {.$MODE OBJFPC}{$MODE DELPHI}{$H+}
  {$ASMMODE intel}

  { Due to certain bugs in 2.0.2 this is a workaround IFDEF that should only
    be used in the unit tests. }
  {$if defined(ver1) or (defined(ver2_0) and (fpc_patch<2))}
    {$fatal tiOPF2 requires at least FPC 2.0.2}
  {$ELSEIF (defined(ver2_0) and (fpc_patch=2))}
    {$DEFINE FPC202}
  {$ELSE}
    {$DEFINE FPC202OrAbove}
  {$IFEND}

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

// Some databases do not have a native boolean type, so you must roll it yourself 
// from a CHAR or VARCHAR. The tiOPF will use 'T' & 'F' or 'TRUE ' & 'FALSE'. This
// can be controled with the compiler directive below:
{$DEFINE BOOLEAN_CHAR_1}

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

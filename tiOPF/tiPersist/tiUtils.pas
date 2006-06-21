{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
                                             
  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    General purpose routines to supplement SysUtils.pas

  ToDo:
    Make string params const

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiUtils ;

interface
uses
  SysUtils
  ,Classes
  {$IFDEF MSWINDOWS}
  ,WinProcs
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,Buttons
  ,shellAPI
  ,ExtCtrls
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QGraphics
  ,QControls
  ,QForms
  ,QDialogs
  ,QStdCtrls
  ,QButtons
  ,QExtCtrls
  {$ENDIF LINUX}
  ,Math
  ,TypInfo
  {$IFDEF DELPHI6ORABOVE}
   ,Variants
   ,Types
  {$ENDIF}
  ,tiDialogs
  ;

const
  csWinDateFormat     = 'dd/MM/yyyy' ;
  csWinTimeFormat     = 'hh:mm:ss' ;
  csWinDateTimeFormat = 'dd/MM/YYYY hh:mm:ss' ;
  {$IFDEF LINUX}
  AllFilesWildCard = '*';
  {$ELSE}
  AllFilesWildCard = '*.*';
  {$ENDIF LINUX}

  // Error messages
  cErrorDecodeNumError = 'tiDecodeNum: <%s> does not represent a number in base %d.';

{$IFDEF VER130}
type
  TVarType = Word;
{$ENDIF}

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * String manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  { ToDo 5 -cUtils: Convert params to const }
  // Scan the string pStrValue, and replace any characters pStrDel with pStrIns (Case sensitive)
  function  tiStrTran(          pStrValue, pStrDel, pStrIns : string ) : string ;
  function  tiStrTran1( aStrValue, aStrDel, aStrIns : string ) : string ;
  // Scan the string pStrValue, and replace any characters pStrDel with pStrIns (Case insensitive)
  function  tiCIStrTran(        pStrValue, pStrDel, pStrIns : string ) : string ;
  // Count the number of blocks of text in pStrValue seperated by pStrToken
  function  tiNumToken(         const pStrValue, pStrToken : string ) : integer ;
  // Extract the pIntNum(th) block of text in pStrValue, seperated by pStrToken
  function  tiToken(            const pStrValue, pStrToken : string; const pIntNum : integer ) : string ;
  // Return a string of spaces pIntLen long
  function  tiSpace(            pIntLen : integer ) : string ;
  // Pad pStrValue with spaces on the RHS to make it pIntLen long
  function  tiPadR( const psValue : string; piLen : integer ) : string ;
  // Pad pStrValue with spaces on the LHS to make it pIntLen long
  function  tiPadL(             pStrValue : string; pIntLen : integer ) : string ;
  // Pad pStrValue with spaces on both sides to make it pIntLen long
  function  tiPadC(             pStrValue : string; pIntLen : integer ) : string ;
  // Pad pStrValue with zeros on the LHS to make it pIntLen long
  function  tiPad0(             pStrValue : string; pIntLen : integer ) : string ;
  // Remove any leading zeros from a string
  function  tiRemoveLeading0(   pStrValue : string ) : string ;
  // Convert a string into mized case using a simple algorithm
  // Note: There is a more complete version of this (that takes care of works like
  //       McCrae or King Charles IV
  //       It will also work with characters other than #32 to sep words
  function  tiMixedCase(        pStrValue : string ) : string ;
  // Replicate the string pStrValue to return a string pIntLength long
  function  tiReplicate(        const pStrValue : string ; pRepCount : Word ) : string ;
  // If pStrLine is not '', add the trailing value pStrValue
  function  tiAddTrailingValue( const pLine, pValue : string ; pDuplicates : boolean = true ) : string ;
  // If the last character of pStrLine is pStrValue, then remove it
  function  tiRemoveTrailingValue( pStrLine, pStrValue : string ) : string ;
  // If pStrValue is not '', add a trailing ', '
  function  tiAddTrailingComma( pStrValue : string ) : string ;
  // If pStrValue is not '', add a trailing ' and '
  function  tiAddTrailingAnd(   pStrValue : string ) : string ;
  // If pStrValue is not '', add a trailing ' or '
  function  tiAddTrailingOr(    pStrValue : string ) : string ;
  // If pStr is not '', add a trailing ' '
  function  tiAddTrailingSpace( pStrValue : string ) : string ;
  // Return the first position of pStrValue in pStrTarget from the right.
  // Same as Delphi's Pos, except from the right, not left
  function  tiPosR(             pStrTarget, pStrValue : string ) : integer ;
  // Does the wildcard pPattern occur in the string pSource ?
  function tiWildcardMatch( const psSource, psPattern: String; const pbCaseSensitive: boolean = false): Boolean;
  // Extract a sub string within defined delimiters
  function  tiSubStr( const pSource, pStartDelim, pEndDelim : string ; pIndex : integer = 1 ) : string ;
  // Trunc a string to piWidth length and add '...'
  function  tiAddEllipsis( const psString : string ; piWidth : integer = 20 ) : string ;
  // Trim all characters after and including psTrim from psString
  function  tiTrimR( const psString, psTrim : string ; pbCaseSensitive : boolean = false ) : string ;
  // Trim all characters before and including psTrim from psString
  function  tiTrimL( const psString, psTrim : string ; pbCaseSensitive : boolean = false ) : string ;
  // Remove Cr & LF characters
  function  tiRemoveCrLf( const pString : string ) : string ;
  // Remove all the trailing white space characters ( #32, #10, #13 )
  // from the end of a string
  function tiTrimTrailingWhiteSpace( const pString : string ) : string ;
  // Returns true if the email address is valid
  function tiIsEMailAddressValid( const email: string): boolean;
  // Returns true if the file name is valid. Will not test if the
  // directory part of the file name exists, or is valid, will just
  // test the file name part
  function tiIsFileNameValid( const pFileName : string ) : boolean ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Directory, file and file name manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Get a tempory file name with the extension pStrExt
  function  tiGetTempFile(         const pStrExt : string ) : string ;
  // Add a trailing '/' if there is not already one
  function  tiAddTrailingSlash(    const pDir : string ) : string ;
  // Remove a trailing '/' if there is one
  function  tiRemoveTrailingSlash( const pStrDir : string ) : string ;
  // Remove a leading '\' if there is one
  function  tiRemoveLeadingSlash(  const pStrDir : string ) : string ;
  // Get the windows tempory directory
  function  tiGetTempDir   : string ;
{$IFDEF MSWINDOWS}
  // Get the windows system directory
  function  tiGetWindowsSysDir : string ;
{$ENDIF MSWINDOWS}
  // Read a file's date and size
  procedure tiReadFileDateSize( const psFileName : string ;
                                var   pDateTime  : TDateTime ;
                                var   piFileSize : integer );
  // Set a files date and time
  procedure tiSetFileDate( const psFileName : string ; const pdtFileDate : TDateTime ) ;
  // What is the path of the current EXE?
  function  tiGetEXEPath : string ;
  // Add the EXE path to a file name
  function  tiAddEXEPath(const pFileName: string): string;
  // Extract the file name part of a file name (ie, remove the path and extension)
  function  tiExtractFileNameOnly(       pStrFileName : string ) : string ;
  // Remove the extension from a filename. Similar to Delphi's ChangeFileExt( ) except the '.' is managed.
  function  tiRemoveExtension(           pStrValue : string ) : string ;
  // Change a file's extenstion. Similar to Delphi's ChangeFileExt( ) except the '.' is managed.
  function  tiSwapExt(                   const psFileName, psExt : string ) : string ;
  // Extract a file's extension. Similar to Delphi's ExtractFileExt except the '.' is not extracted.
  function  tiExtractExtension(          pStrFileName : string ) : string ;
  // Copy a file from pStrFileFrom to pStrFileTo
  procedure tiCopyFile(            const pStrFileFrom, pStrFileTo : string ) ;
  // Move a file from pStrFromFileName to pStrToFileName
  procedure tiMoveFile(                  pStrFromFileName, pStrToFileName: string);
  // Read a file's size in bytes
  function  tiGetFileSize(               pStrFileName : string ) : longInt ;
  // Remove the drive letter from a file name
  function  tiRemoveDrive(               pStrPath : string ) : string ;
  // Set a file's readonly attribute
  procedure tiSetFileReadOnly(           pStrFileTo : string ; pBoolReadOnly : boolean ) ;
  // Is a file's readonly attribute set?
  function  tiIsFileReadOnly( pStrFileTo : string ) : boolean ;
  // Copy all the directories, starting at pStrStartDir to the stringList slTree
  procedure tiDirectoryTreeToStringList( const psStartDir : string ; const pslDirList : TStringList ; pbRecurse : boolean  ) ;
  // Copy all the files, from the directory pStrStartDir, matching the wildcard pStrWildCard
  // to the stringList slResult
  procedure tiFilesToStringList(         const psStartDir,
                                         psWildCard : string ;
                                         slResult : TStringList ;
                                         const pbRecurse : boolean ) ;
  // Does a directory have any subdirectories?
  function  tiHasSubDirectory(           pStrStartDir : string ) : boolean ;
  // Write the string in psText to a file named psFileName
  procedure tiStringToFile( const psText, psFileName : string ) ;
  // Read a text file into a string
  function  tiFileToString( const pFileName : TFileName ) : string ;
  // Get the current directory
  // Extract a directory name to a certain level.
  // eg tiExtractDirToLevel( 'c:\temp\dir', 0 ) gives 'c:'
  //    tiExtractDirToLevel( 'c:\temp\dir', 1 ) gives 'c:\temp'
  //    tiExtractDirToLevel( 'c:\temp\dir', 2 ) gives 'c:\temp\dir'
  function tiExtractDirToLevel( const psFileName : TFileName ; piLevel : byte ) : TFileName ;

  // Remove a directory, and all it's owned directories and files
  function tiForceRemoveDir( const pDirName : TFileName ) : boolean ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Number manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Calculate pNum div pDenom, and return 0 if an error
  function  tiSafeDiv(   pNum, pDenom : longInt ) : longInt ; overload ;
  // Calcuate pNum / pDenom and return 0 if an error
  function  tiSafeDiv(   pNum, pDenom : real    ) : real    ; overload ;
  // Round a float to an integer using 'normal' rounding (rather than Delphi's 'Bankers rounding')
  function tiRound(X: Extended): Int64;
  // Set the precision of pValue to pPrecision decimal places
  function  tiSetPrecision( const pValue : real ; const pPrecision : integer = 3 ) : real ;
  // Convert an interger (word) to a base26 string (A..Z) characters only
  function  tiEncodeWordBase26( const pNum : Word ) : String ;
  // Convert a base26 string (A..Z characters only) to an integer (word)
  function  tiDecodeWordBase26( numstr : String ) : Word;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TDateTime manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // What is the date of the previous week day?
  function tiDateToPreviousWeekDayDate( pDTValue : TDateTime ) : TDateTime ;
  // Get the current year as an integer
  function tiYear( pDate : TDateTime = 0.0 ) : Word ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Type conversions
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Convert a string to an integer using StrToInt, default to 0 if an exception is raised
  function  tiStrToInt(           const pStrValue  : string )    : integer ;
  // Convert a string to a float using StrToFloat, default to 0 if an exception is raised
  function  tiStrToFloat(         const pStrValue  : string )    : real ;
  // Convert a date to a string using FormatDateTime with the standard date format
  function  tiDateToStr(          const pDTValue   : TDateTime ; const psFormat : string = csWinDateFormat) : string ;
  // Convert a dateTime to a string using FormatDateTime with the standard dateTime format
  function  tiDateTimeToStr(      const pDTValue   : TDateTime ) : string ;
  // Convert a time to a string using FormatDateTime with the standard time format
  function  tiTimeToStr(          const pDTValue   : TDateTime ;
                                  const psTimeFormat : string = '' ) : string ;
  // Convert an integer to a string and return '' if 0
  function  tiIntToStrHide0(      const pIntValue  : longInt )   : string ;
  // Converts an integer to a string with commas between thousands
  function  tiIntToCommaStr(      const piValue    : integer ) : string ;
  // Convert a float to a currency string and return '' if 0
  function  tiFloatToCurrencyHide0( const pRealValue : real )      : string ;
  // Convert a float to a currency string
  function  tiFloatToCurrency(      const pRealValue : real )      : string ;
  // Convert a boolean to 'True' or 'False'
  function  tiBooleanToStr(       const pBoolValue : boolean )   : string ;
  // Convert a boolean to 'True' or 'False'
  function  tiBoolToStr(          const pBoolValue : boolean )   : string ;
  // Convert a string to a boolean
  function  tiStrToBool( const psValue : string ) : boolean ;
  // Convert a float to a string in the format 9999.99
  function  tiFloatToStr(         const pRealValue : real ;
                                  const pIntPrecision : integer = 3 ) : string ;
  // Convert a float to a string in the format 9,999.99
  function  tiFloatToCommaStr(    const pRealValue : real ;
                                  const pIntPrecision : integer = 3 ) : string ;
  // Convert a float to a string in the format passed by psFloatFormat
  function  _tiFloatToStr(        const pRealValue : real ;
                                  const pIntPrecision : integer ;
                                  const psFloatFormat : string ) : string ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Win32 API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF MSWINDOWS}
  // Call Windows ShellExecute with exception handling
  function _tiShellExecute( pHwnd : integer ;
                            ppcOperation, ppcFile, ppcParameters, ppcDirectory : PChar ;
                            pIntShowCmd : integer ) : integer ;
  // Simple pascal call to ShellExecute
  function tiShellExecute( const psEXE : string ;
                           const psParameters : string = '';
                           const piWinState : integer = SW_SHOWNORMAL ) : integer ;
  // Run an EXE and wait for it to finish
  procedure tiRunEXEAndWait( pStrEXE : string ) ;
{$ENDIF MSWINDOWS}

  // Edit a file using the standard editor for this file type
  function  tiEditFile( const pStrFileName : string ) : integer ;
  // Get the currently logged on user ID
  function  tiGetUserName : string ;
  // Get the computer name
  function  tiGetComputerName : string ;

  // Bit manipulation
  // Is a particular bit set?
  function tiIsBitSet( const pVal: longint; const pBit: byte) : boolean ;
  // Convert a particular bit to a string: '1' or '0'
  function tiBitToString( const pVal: longint; const pBit: byte) : string ;
  // Convert a Int32 to a string of 0s and 1s
  function tiInt32ToBinString( const val : longInt ) : string ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Other routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Convert a variant array of variants to a string
  function  tiVariantArrayToString( pValue : oleVariant ) : string ;
  // Is a variant of a given type
  function  tiIsVariantOfType( pVariant : oleVariant ; pVarType : TVarType ) : boolean ;
  // Return a string with pCount #10 characters
  function  Lf( const pCount : Byte = 1 ) : string ;
  // Return a string with pCount #13 characters
  function  Cr( const pCount : Byte = 1 ) : string ;
  // Return a string with pCount #13+#10 characters
  function  CrLf( const pCount : Byte = 1 ) : string ;
  // Returns a string with pCount #9 characters
  function  Tab( const pCount : Byte = 1 ) : string ;
  // Returns the checksum of a string of numbers
  function tiCheckSum(const Value: string): Integer;
  // Write a string into a stream
  procedure tiStringToStream( const pStr : string ; const pStream : TStream ) ;
  // Append a string to a stream
  procedure tiAppendStringToStream( const pStr : string ; const pStream : TStream ) ;
  // Read the contents of a stream as a string
  function  tiStreamToString( const pStream : TStream ) : string ;
  // Write a string into a stream
  procedure tiFileToStream( const pFileName : string ; const pStream : TStream ) ;
  // Read the contents of a stream as a string
  function  tiStreamToFile( const pFileName : string ; const pStream : TStream ) : string ;
  // Copy one stream to another
  procedure tiCopyStream( const pStreamFrom, pStreamTo : TStream ) ;
  // Copy a TList of TPersistent's data to a TStream using CSV format
  procedure tiListToStream( pStream : TStream ;
                            pList : TList ;
                            psSepChar : string ;
                            pslColsSelected : TStringList = nil ) ;

  // Copy a TList of TPersistent's to a CSV file (Prompt the user for the file name)
  procedure tiListToCSV( pList : TList ;
                         pslColsSelected : TStringList = nil ;
                         const psFileName : string = '' ;
                         pAskToView : boolean = true ) ;

  // Copy a TList of TPersistent's to the clipboard
  procedure tiListToClipboard( pList : TList ;
                               pslColsSelected : TStringList = nil ;
                               pConfirm : boolean = true ) ;

  // Format, then re-rasie an exception
  procedure tiFmtException( e : Exception ;
                            const psMessage   : string ;
                            const psClassName : string = '' ;
                            const psMethod    : string = '' ) ; overload ;

  procedure tiFmtException( const psMessage   : string ;
                            const pA : Array of Const ;
                            const psClassName : string = ''   ;
                            const psMethod    : string = '' ) ; overload ;

  procedure tiFmtException( const psMessage   : string ;
                            const psClassName : string = '' ;
                            const psMethod    : string = '' ) ; overload ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Linux API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF LINUX}
  //: Return the width (x) and height (y) in pixels of the string s
  function tiGetTextSize(pFont: TFont; s: String): TSize;
  function GetTickCount: Longint;
{$ENDIF LINUX}

const

  // Type kinds for use with tiGetPropertyNames
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString ] ;
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 ] ;
  // Float type properties
  ctkFloat  = [ tkFloat ] ;
  // Numeric type properties
  ctkNumeric = [tkInteger, tkInt64, tkFloat];
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat ;

  // These are the leftovers
  // tkUnknown
  // tkClass, tkMethod,
  // tkEnumeration, tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

  // These are all the possibilities
  // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
  // tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
  // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);

type
  // Simple TypeKinds, as summary of the TTypeKinds available in TypInfo
  TtiTypeKind =  ( tiTKInteger, tiTKFloat , tiTKString, tiTKDateTime, tiTKBoolean, tiTKBinary ) ;
  // Convert a propert from Delphi's TTypeKind to TtiSimpleTypeKind
  // EG: Change tkInteger, tkInt64 and tkEnumeration to tkInteger
  function tiGetSimplePropType( const pPersistent : TPersistent ; const psPropName : string ) : TtiTypeKind ;
  function tiVarSimplePropType( pValue : Variant ) : TtiTypeKind ;

  // Is this a numeric property ?
  function tiIsNumericProp( pPersistent : TPersistent ; psPropName : string ) : boolean ;

  // Read a TPersistent's published properties into a TStringList
  procedure tiGetPropertyNames( pPersistent : TPersistent ;
                                pSL : TStringList ;
                                pPropFilter : TTypeKinds = ctkSimple ) ; overload ;

  procedure tiGetPropertyNames( pPersistent : TPersistentClass ;
                                pSL : TStringList ;
                                pPropFilter : TTypeKinds = ctkSimple ) ; overload ;

  // Is a property a read & write property
  function tiIsReadWriteProp( const pData : TPersistent ; const psPropName : string ) : boolean ; overload ;
  function tiIsReadWriteProp( const pData : TPersistentClass ; const psPropName : string ) : boolean ; overload ;

  function tiHasRTTI( pObject : TObject ) : boolean ; overload ;
  function tiHasRTTI( pClass : TClass ) : boolean ; overload ;


// Some global constants
const
  ciOK                = 0 ;
  ciRetry             = 1 ;
  ciAbort             = 2 ;

  cdtOneDay        = 1 ;
  cdtOneHour       = 1/24 ;
  cdtOneMinute     = 1/24/60 ;
  cdtOneSecond     = 1/24/60/60 ;
  cdtOneMiliSecond = 1/24/60/60/1000 ;

  cComma          = ',' ;
  cBackSpace      = #8 ;

  crMaxReal       = 9999999999 ;

  cNullDate       = 0.0 ;
  cNullDateTime   = 0.0 ;
  cgNullDate      = 0.0 ;
  cgNullDateTime  = 0.0 ;
  cgMinDateTime   = 0.0 ;
  cNullSQLDate    = '12/30/1899' ;
  cgdtMaxDateTime = 2958465.99998843 ; // 31/12/9999 23:59:59
  cgMaxDateTime   = cgdtMaxDateTime ;
  cMaxDateTime    = cgdtMaxDateTime ;
  cgdtMaxDate     = 2958465.0 ;
  crMaxDate       = cgdtMaxDate ;
  cMaxDate        = cgdtMaxDate ;
  cgMinDate       = 0.0 ;
  cgMaxDate       = 2958465.0 ;
  cgsComma        = ',' ;
  {$IFDEF DELPHI5} // These are defined from Delphi 5
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
  {$ENDIF}
  crZeroLimit      = 0.005 ;
  cCurrencyFormat0 = '$#,##0' ;
  cCurrencyFormat2 = '$#,##0.00' ;
  cgsNA            = 'N/A' ;

  cPI              = 3.14159265358979 ;
  cTrue            = 'TRUE' ;
  cFalse           = 'FALSE' ;

  cgExceptionMessageStart   = '* * * Message * * *'    ;
  cgExceptionMessageEnd     = '* * * End Message * * *'   ;
  cgExceptionCallStackStart = '* * * Call Stack * * *'  ;
  cgExceptionCallstackEnd   = '* * * End Call Stack * * *' ;
  cgExceptionDetailsStart   = '* * * Details * * *'    ;
  cgExceptionDetailsEnd     = '* * * End Details * * *'   ;


implementation
uses
  {$IFDEF MSWINDOWS}
  ClipBrd
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,Windows
  ,tiWin32
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QClipBrd
  ,libc
  {$ENDIF LINUX}
  ,tiRegINI
  ;

//------------------------------------------------------------------------------
function tiGetTempFile( const pStrExt : string ) : string ;
const
  cMaxPathLen = 255 ;
var
  pcTemp : array[0..cMaxPathLen] of char ;
  pcApp  : array[0..cMaxPathLen] of char ;
  pcPath : array[0..cMaxPathLen] of char ;
begin
  {$IFDEF MSWINDOWS}
  strPCopy( pcApp, copy( extractFileName( application.exeName ), 1, 3 ) ) ;
  getTempPath( cMaxPathLen, pcPath ) ;
  getTempFileName( pcPath, pcApp, 0, pcTemp ) ;
  deleteFile( pcTemp ) ; // This is using the Window deleteFile, not Delphi's
  result := strPas( pcTemp ) ;
  if pos( '.', result ) <> 0 then begin
    if pStrExt = '' then begin
      result := tiRemoveExtension( result ) ;
    end else begin
      result := copy( result, 1, pos( '.', result )) + pStrExt  ;
    end ;
  end ;
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
    {
    man tempnam
    [...]
    BUGS
       The precise meaning of `appropriate' is undefined;  it  is
       unspecified  how  accessibility  of  a directory is deter­
       mined.  Never use this function. Use tmpfile(3) instead.
    [...]

    Should we really use this?
    Alternatives would be to use tmpfile, but this creates a file.
    So maybe it would be worth checking if we ever need the name w/o a file!
  }
  Result := tempnam(nil, PChar(pStrExt));
  {$ENDIF}
end ;

//------------------------------------------------------------------------------
function tiGetTempDir : string ;
begin
  result := ExtractFilePath( tiGetTempFile( 'tmp' )) ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiGetWindowsSysDir : string ;
const
  cMaxPathLen = 255 ;
var
  pcDir : array[0..cMaxPathLen] of char ;
begin
  GetSystemDirectory( pcDir, cMaxPathLen ) ;
  result := String( pcDir ) ;
end ;
{$ENDIF MSWINDOWS}

// Seaches <sStr> and replaces <sDel> with <sIns>
// Case sensitive.
//------------------------------------------------------------------------------
function tiStrTran( pStrValue, pStrDel, pStrIns : string ) : string ;
var i : integer ;
    sToChange : string ;
begin
  result := '' ;
  sToChange := pStrValue ;
  i := pos( pStrDel, sToChange ) ;
  while i <> 0 do begin
    result := result + copy( sToChange, 1, i-1 ) + pStrIns ;
    delete( sToChange, 1, i+length( pStrDel )-1) ;
    i := pos( pStrDel, sToChange ) ;
  end ;
  result := result + sToChange ;
end ;

function tiStrTran1( aStrValue, aStrDel, aStrIns : string ) : string ;
var
  // order of declaration is significant here - earlier declared local vars
  // get assigned to registers rather than pushed to stack
  pMatch, pStrValue, pResult: PChar;
  lenDel: cardinal;
  pStrIns, pStrDel: PChar;

begin
  lenDel := Length(aStrDel);
  // max possible length = every char in pStrValue requires substitution
  SetLength(result, Length(aStrIns) * Length(aStrValue));
  pStrValue := Pointer(aStrValue);

  // empty src begets empty result...
  if pStrValue = nil then
    exit;

  pResult := Pointer(result);
  pStrIns := Pointer(aStrIns);
  pStrDel := Pointer(aStrDel);

  // Advance through pStrValue looking for pStrDel instances...
  pMatch := StrPos(pStrValue, pStrDel);

  // terminate on no match
  while pMatch <> nil do
  begin

    // Copy all chars from current pStrValue into pResult up to match point,
    // moving both pointers along as well
    while pStrValue < pMatch do
    begin
      pResult^ := pStrValue^;
      Inc(pResult);
      Inc(pStrValue);
    end;

    // Add in pStrIns and move pResult beyond it
    pResult := StrECopy(pResult, pStrIns);

    // move pStrValue beyond instance of  pStrDel
    Inc(pStrValue, lenDel);
    pMatch := StrPos(pStrValue, pStrDel);
  end;

  // no more matches  - copy remainder of pStrValue to pResult
  StrCopy(pResult, pStrValue);
  // resync (Delphi string) length of result after pchar manipulations...
  result := PChar(result);
end ;

// Seaches <sStr> and replaces <sDel> with <sIns>
// Case in-sensitive.
//------------------------------------------------------------------------------
function tiCIStrTran( pStrValue, pStrDel, pStrIns : string ) : string ;
var i : integer ;
    sToChange : string ;
begin
  result := '' ;
  sToChange := pStrValue ;
  i := pos( upperCase( pStrDel ), upperCase( sToChange )) ;
  while i <> 0 do begin
    result := result + copy( sToChange, 1, i-1 ) + pStrIns ;
    delete( sToChange, 1, i+length( pStrDel )-1) ;
    i := pos( upperCase( pStrDel ), upperCase( sToChange )) ;
  end ;
  result := result + sToChange ;
end ;

//------------------------------------------------------------------------------
function tiNumToken( const pStrValue, pStrToken : string ) : integer ;
var
  i, iCount : integer ;
  lsValue : string ;
begin

  result := 0 ;
  if pStrValue = '' then
    Exit ; //==>

  iCount := 0 ;
  lsValue := pStrValue ;
  i := pos( pStrToken, lsValue ) ;
  while i <> 0 do begin
    delete( lsValue, i, length( pStrToken )) ;
    inc( iCount ) ;
    i := pos( pStrToken, lsValue ) ;
  end ;
  result := iCount + 1 ;

end ;

//------------------------------------------------------------------------------
function tiToken( const pStrValue, pStrToken : string; const pIntNum : integer ) : string ;
var
  i, iCount, iNumToken : integer ;
  lsValue : string ;
begin

  result := '' ;

  iNumToken := tiNumToken( pStrValue, pStrToken ) ;
  if pIntNum = 1 then begin
    if pos( pStrToken, pStrValue ) = 0 then result := pStrValue
    else result := copy( pStrValue, 1, pos( pStrToken, pStrValue )-1) ;
    end
  else if (iNumToken < pIntNum-1) or (pIntNum<1) then begin
    result := '' ;
    end
  else begin

    { Remove leading blocks }
    iCount := 1 ;
    lsValue := pStrValue ;
    i := pos( pStrToken, lsValue ) ;
    while (i<>0) and (iCount<pIntNum) do begin
      delete( lsValue, 1, i + length( pStrToken ) - 1 ) ;
      inc( iCount ) ;
      i := pos( pStrToken, lsValue ) ;
    end ;

    if (i=0) and (iCount=pIntNum) then result := lsValue
    else if (i=0) and (iCount<>pIntNum) then result := ''
    else result := copy( lsValue, 1, i-1) ;

  end ;
end ;

//------------------------------------------------------------------------------
function tiSpace( pIntLen : integer ) : string ;
var i : integer ;
    sString : string ;
begin
  sString := '' ;
  for i := 1 to pIntLen do
    sString := sString + ' ' ;
  result := sString ;
end ;

//------------------------------------------------------------------------------
function tiPadR( const psValue : string; piLen : integer ) : string ;
var
  ls : string ;
begin
  ls := psValue ;
  if length( ls ) < piLen then begin
    while length( ls ) < piLen do begin
      ls := ls + ' ' ;
    end ;
  end
  else if length( ls ) > piLen then
    ls := copy( ls, 1, piLen ) ;
  result := ls ;
end ;

//------------------------------------------------------------------------------
function tiPadL( pStrValue : string; pIntLen : integer ) : string ;
begin
  if length( pStrValue ) < pIntLen then begin
    while length( pStrValue ) < pIntLen do begin
      pStrValue := ' ' + pStrValue ;
    end ;
  end
  else if length( pStrValue ) > pIntLen then
    pStrValue := copy( pStrValue, length( pStrValue )-pIntLen+1, pIntLen ) ;
  result := pStrValue ;
end ;

//------------------------------------------------------------------------------
function  tiPadC( pStrValue : string; pIntLen : integer ) : string ;
var
  liPad : integer ;
begin
  if Length( pStrValue ) = pIntLen then
  begin
    result := pStrValue ;
    Exit ; //==>
  end ;
  
  if Length( pStrValue ) + 1 = pIntLen then
  begin
    result := pStrValue + ' ';
    Exit ; //==>
  end ;

  if Length( pStrValue ) > pIntLen then
    raise exception.create( 'Can not call tiPadC when the string to be ' +
                            'padded is longer than the target length' ) ;

  liPad := ( pIntLen - length( pStrValue )) div 2 ;
  if liPad > 0 then
    result := tiSpace( liPad ) + pStrValue + tiSpace( liPad ) ;

  // To handle situations where pIntLen < length( pStrValue ) and
  // when length( pStrValue ) is an odd number
  result := tiPadR( result, pIntLen ) ;
end ;

//------------------------------------------------------------------------------
function tiPad0( pStrValue : string; pIntLen : integer ) : string ;
begin
  if length( pStrValue ) < pIntLen then begin
    while length( pStrValue ) < pIntLen do begin
      pStrValue := '0' + pStrValue ;
    end ;
  end
  else if length( pStrValue ) > pIntLen then begin
    pStrValue := copy( pStrValue, length( pStrValue )-pIntLen, pIntLen ) ;
  end ;
  result := pStrValue ;
end ;

//------------------------------------------------------------------------------
function tiRemoveLeading0( pStrValue : string ) : string ;
var i : integer ;
begin
  for i := 1 to length( pStrValue ) do begin
    if copy( pStrValue, 1, 1 ) = '0' then begin
      pStrValue := copy( pStrValue, 2, length( pStrValue ) - 1 ) ;
    end else begin
      break ;
    end ;
  end ;
  result := pStrValue ;
end ;

//------------------------------------------------------------------------------
function  tiYear( pDate : TDateTime = 0.0 ) : Word ;
var
  lD, lM : Word ;
  lDate : TDateTime ;
begin
  if pDate = 0.0 then
    lDate := Date
  else
    lDate := pDate ;
  DecodeDate( lDate, Result, lM, lD ) ;
end ;

//------------------------------------------------------------------------------
function tiMixedCase( pStrValue : string ) : string ;
var iToken : integer ;
    i : integer ;
    sBuffer : string ;
begin
  iToken := tiNumToken( pStrValue, ' ' ) ;
  result := '' ;
  pStrValue := lowerCase( pStrValue ) ;
  for i := 1 to iToken do begin
    sBuffer := tiToken( pStrValue, ' ', i ) ;
    result := tiAddTrailingValue( result, ' ', true ) ;
    result  := result +
               upperCase( copy( sBuffer, 1, 1 )) +
               copy( sBuffer, 2, length( sBuffer ) - 1 ) ;
  end ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiShellExecute( const psEXE : string ;
                         const psParameters : string = '';
                         const piWinState : integer = SW_SHOWNORMAL ) : integer ;
var
  lFileName   : array[0..255] of char ;
  lParameters : array[0..255] of char ;
  lHandle : THandle ;
begin ;

  strPCopy( lFileName,   psEXE ) ;
  strPCopy( lParameters, psParameters ) ;

  // Screen.ActiveForm.Handle is not thread safe
  //lHandle := screen.activeForm.handle ;
  lHandle := 0 ;
  result := _tiShellExecute( lHandle,
                             nil,
                             lFileName,
                             lParameters,
                             nil,
                             piWinState ) ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function _tiShellExecute( pHwnd : integer ;
                         ppcOperation, ppcFile, ppcParameters, ppcDirectory : PChar ;
                         pIntShowCmd : integer ) : integer ;
var sMessage     : string ;
begin

  result := ShellExecute( pHWnd,
                          ppcOperation,
                          ppcFile,
                          ppcParameters,
                          ppcDirectory,
                          pIntShowCmd ) ;

  { These error messages were pasted from the WINAPI help on shellExecute( ) }
  case result of
    0  : sMessage := ( 'System was out of memory, executable file was corrupt, or ' +
                       'relocations were invalid.' ) ;
    2  : sMessage := ( 'File was not found.' ) ;
    3  : sMessage := ( 'Path was not found.' ) ;
    5  : sMessage := ( 'Attempt was made to dynamically link to a task, or there ' +
                       'was a sharing or network-protection error.' ) ;
    6  : sMessage := ( 'Library required separate data segments for each task.' ) ;
    8  : sMessage := ( 'There was insufficient memory to start the application.' ) ;
    10 : sMessage := ( 'Windows version was incorrect.' ) ;
    11 : sMessage := ( 'Executable file was invalid. Either it was not a Windows ' +
                       'application or there was an error in the .EXE image.' ) ;
    12 : sMessage := ( 'Application was designed for a different operating system.' ) ;
    13 : sMessage := ( 'Application was designed for MS-DOS 4.0.' ) ;
    14 : sMessage := ( 'Type of executable file was unknown.' ) ;
    15 : sMessage := ( 'Attempt was made to load a real-mode application (developed ' +
                       'for an earlier version of Windows).' ) ;
    16 : sMessage := ( 'Attempt was made to load a second instance of an executable ' +
                       'file containing multiple data segments that were not marked ' +
                       'read-only.' ) ;
    19 : sMessage := ( 'Attempt was made to load a compressed executable file. The ' +
                       'file must be decompressed before it can be loaded.' ) ;
    20 : sMessage := ( 'Dynamic-link library (DLL) file was invalid. One of the ' +
                       'DLLs required to run this application was corrupt.' ) ;
    21 : sMessage := ( 'Application requires Windows 32-bit extensions.' ) ;
    else
      sMessage := '' ;
      { ShellExe ran OK, do nothing. }
    end ;

    if sMessage <> '' then
      tiAppError( 'Error executing external application.' + Cr +
                  'Error: ' + sMessage ) ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
function tiEditFile( const pStrFileName : string ) : integer ;
begin
  // screen.activeForm.handle,
  {$IFDEF MSWINDOWS}
  result := ShellExecute( Application.MainForm.Handle,
                          nil,
                          PChar(pStrFileName),
                          nil,
                          nil,
                          SW_SHOWNORMAL ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  { TODO -cLinux outstanding : There is no standard editor included with all
    flavours of linux. Might implement tiEditFile as a form with
    a memo control and basic edit functions. ie: Something like NotePad for Windows }
  Result := 0;    // To get rid of the compiler warning, until I implement this.
  {$ENDIF LINUX}
end ;


//------------------------------------------------------------------------------
function tiExtractFileNameOnly( pStrFileName : string ) : string ;
begin
  result := tiRemoveExtension( extractFileName( pStrFileName )) ;
end ;

//------------------------------------------------------------------------------
function tiRemoveExtension( pStrValue : string ) : string ;
var i : integer ;
begin
  i := tiPosR( '.', pStrValue ) ;
  if i <> 0 then begin
    result := copy( pStrValue, 1, i - 1 ) ;
  end else begin
    result := pStrValue ;
  end ;
end ;

//------------------------------------------------------------------------------
function  tiSwapExt( const psFileName, psExt : string ) : string ;
begin
  result := tiAddTrailingValue( tiRemoveExtension( psFileName ), '.', false ) + psExt ;
end ;

//------------------------------------------------------------------------------
function tiExtractExtension( pStrFileName : string ) : string ;
var i : integer ;
begin
  i := tiPosR( '.', pStrFileName ) ;
  if i <> 0 then begin
    result := copy( pStrFileName, i+1, length( pStrFileName ) - i ) ;
  end else begin
    result := '' ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure tiCopyFile( const pStrFileFrom, pStrFileTo : string ) ;
var
  iErrorCode : word ;
begin
{$IFDEF MSWINDOWS}
  copyFile( pChar( pStrFileFrom ), pChar( pStrFileTo ), false ) ;
  iErrorCode := getLastError( ) ;
  if iErrorCode <> 0 then begin
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
  iErrorCode := Libc.system(PChar('cp ' + pStrFileFrom + ' ' + pStrFileTo));
  if iErrorCode <> 0 then begin
{$ENDIF LINUX}
    raise exception.create( 'Unable to copy <' +
                            pStrFileFrom +
                            '> to <' +
                            pStrFileTo + '>' + #13 +
                            'System error code: ' +
                            intToStr( iErrorCode ) + #13 +
                            'System error message: ' +
                            sysErrorMessage( iErrorCode )) ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure tiMoveFile(pStrFromFileName, pStrToFileName: string);
begin
  RenameFile(pStrFromFileName, pStrToFileName);   { Rename the file }
end;

//------------------------------------------------------------------------------
function tiGetFileSize( pStrFileName : string ) : longInt ;
var f: file of Byte ;
begin
  AssignFile( f, pStrFileName );
  Reset( f );
  result := FileSize( f ) ;
  closeFile( f ) ;
end ;

//------------------------------------------------------------------------------
function tiReplicate( const pStrValue : string ; pRepCount : Word ) : string ;
var
  pResult, pValue: PChar;
  lenValue: cardinal;

begin

  if (pRepCount = 0) or (Pointer(pStrValue) = nil) then
    exit;

  lenValue := Length(pStrValue);
  SetLength(Result, lenValue * pRepCount);
  pResult := Pointer(Result);
  pValue := Pointer(pStrValue);

  while pRepCount <> 0 do
  begin
    Move(pValue^, pResult^, lenValue);
    Inc(pResult, lenValue);
    Dec(pRepCount);
  end;
end;

{
var i : Word ;
begin
  result := '' ;
  for i := 1 to pIntLength do begin ;
    result := result + pStrValue ;
  end ;
end ;


{ Check that the string contained in a TControl can be converted
  to a valid float. Pass the control as a parameter and the
  function will perform the test and retrun focus to the control
  if the test fails.

  Currently programmed to test mastEdits, some more work is necessary
  to test other controls. }
//------------------------------------------------------------------------------
{
function tiIsControlValidFloat( pControl : TControl ) : boolean ;
var sString : string ;
begin

  if (pControl is TMaskEdit ) then begin
    sString := TMaskEdit( pControl ).text ;
  end else begin
    tiAppError( 'Invalid control type passed to uIsControlValidFloar( )' ) ;
  end ;

  try
    result := true ;
  except
    tiAppError( '<' + sString + '> is not a valid number.' ) ;
    TMaskEdit( pControl ).setFocus ;
    result := false ;
  end ;

end ;
}

function _RemoveNonNumChars( pValue : string ) : string ;
begin
  result := pValue ;
  result := tiStrTran( result, ' ', '' ) ;
  result := tiStrTran( result, ',', '' ) ;
  result := tiStrTran( result, '$', '' ) ;
end ;

//------------------------------------------------------------------------------
function tiStrToInt( const pStrValue : string ) : integer ;
begin
  try
    result := strToInt( _RemoveNonNumChars( pStrValue )) ;
  except
    result := 0 ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiStrToFloat( const pStrValue : string ) : real ;
begin
  try
    result := strToFloat( _RemoveNonNumChars( pStrValue )) ;
  except
    result := 0 ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiDateToStr( const pdtValue : TDateTime ; const psFormat : string = csWinDateFormat ) : string ;
begin
  result := FormatDateTime( psFormat, pdtValue ) ;
end ;

//------------------------------------------------------------------------------
function tiDateTimeToStr( const pDTValue : TDateTime ) : string ;
begin
  result := formatDateTime( csWinDateTimeFormat, pDTValue ) ;
end ;

//------------------------------------------------------------------------------
function  tiTimeToStr( const pDTValue   : TDateTime ;
                       const psTimeFormat : string = '' ) : string ;
begin
  if psTimeFormat = '' then
    result := formatDateTime( csWinTimeFormat, pDTValue )
  else
    result := formatDateTime( psTimeFormat, pDTValue )
end;

//------------------------------------------------------------------------------
function tiSafeDiv( pNum, pDenom : longInt ) : longInt ;
begin
  if pDenom <> 0 then 
    result := pNum div pDenom
  else
    result := 0 ;
end ;

//------------------------------------------------------------------------------
function tiIntToStrHide0( const pIntValue : longInt ) : string ;
begin
  if pIntValue = 0 then begin
    result := '' ;
  end else begin
    result := intToStr( pIntValue ) ;
  end ;
end ;

//------------------------------------------------------------------------------
function  tiIntToCommaStr( const piValue : integer ) : string ;
begin
  result := tiFloatToCommaStr( piValue, 0 ) ;
end ;

//------------------------------------------------------------------------------
function tiFloatToCurrencyHide0( const pRealValue : real ) : string ;
begin
  if ( pRealValue < 0.005 ) and ( pRealValue > -0.005 ) then
    result := ''
  else
    result := tiFloatToCurrency( pRealValue ) ;
end ;

//------------------------------------------------------------------------------
function  tiFloatToCurrency( const pRealValue : real ) : string ;
begin
  try
    result := formatFloat( '$ #,##0.00', pRealValue ) ;
  except
    result := '0.00' ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiBooleanToStr( const pBoolValue : boolean ) : string ;
begin
  if pBoolValue then
    result := cTRUE
  else
    result := cFALSE ;
end ;

//------------------------------------------------------------------------------
function tiBoolToStr( const pBoolValue : boolean ) : string ;
begin
  result := tiBooleanToStr( pBoolValue ) ;
end ;

//------------------------------------------------------------------------------
function  tiStrToBool( const psValue : string ) : boolean ;
var
  ls : string ;
begin
  ls := upperCase( psValue ) ;
  if ( ls = 'TRUE' ) or
     ( ls = 'T'    ) or
     ( ls = 'YES'  ) or
     ( ls = 'Y'    ) or
     ( ls = '1'    ) then
    result := true
  else
    result := false ;
end ;

function  tiVariantArrayToString( pValue : oleVariant ) : string;
  procedure appendVariantToStringList( pStringList : TStringList ;
                                       pVariant : oleVariant ;
                                       var pIndent : integer ) ;
  var i : integer ;
      iLow : integer ;
      iHigh : integer ;
  begin
    if tiIsVariantOfType( pVariant, varArray ) then begin
      iLow  := varArrayLowBound(  pVariant, 1 ) ;
      iHigh := varArrayHighBound( pVariant, 1 ) ;
      for i := iLow to iHigh do begin
        inc( pIndent ) ;
        if i = iLow then pStringList.add( tiSpace( pIndent*3 ) + '[' ) ;
        appendVariantToStringList( pStringList, pVariant[i], pIndent ) ;
        if i = iHigh then pStringList.add( tiSpace( pIndent*3 ) + ']' ) ;
        dec( pIndent ) ;
      end ;
    end else begin
      pStringList.add( tiSpace( pIndent*3 + 1 ) + varToStr( pVariant )) ;
    end ;
  end ;

var
  lStringList : TStringList ;
  pIndent : integer ;
begin
  lStringList := TStringList.create ;
  try
    pIndent := -1 ;
    appendVariantToStringList( lStringList, pValue, pIndent ) ;
    result := lStringList.Text ;
  finally
    lStringList.free ;
  end ;
end ;

{
varEmpty	The variant is Unassigned.
varNull	The variant is Null.
VarSmallint	16-bit signed integer (type Smallint).
varInteger	32-bit signed integer (type Integer).
varSingle	Single-precision floating-point value (type Single).
varDouble	Double-precision floating-point value (type Double).
varCurrency	Currency floating-point value (type Currency).
varDate	Date and time value (type TDateTime).
varOLEStr	Reference to a dynamically allocated UNICODE string.
varDispatch	Reference to an Automation object (an IDispatch interface pointer).
varError	Operating system error code.
varBoolean	16-bit boolean (type WordBool).
varUnknown	Reference to an unknown COM object (an IUnknown interface pointer).
varByte	8-bit unsigned integer (type Byte).
varString	Reference to a dynamically allocated Pascal string (type AnsiString).
varTypeMask	Bit mask for extracting type code.
varArray	Bit indicating variant array.
varByRef	Bit indicating variant contains a reference (rather than a value).
}
//------------------------------------------------------------------------------
function tiIsVariantOfType( pVariant : oleVariant ; pVarType : TVarType ) : boolean ;
var
  xVT : TVarType;
  xVTHigh : TVarType;
//  xVTLow : TVarType;
begin
//  result := ( varType( pVariant ) and pVarType ) = pVarType ;
// Contr: VarType is varDate = 0007, pVarType is varInteger=0003.
// 0007 and 0003 = 0003. WRONG!

  xVT:=VarType(pVariant);
//  xVTLow:=xVT and varTypeMask;
  xVTHigh:=xVT and (not varTypeMask);

  // in true pVarType can be and OR of two types: varArray and varString (or others)
  // we have to recognize it.
  // there shouldn't be xVTLow because when we have array of string (normal) then
  // xVT=$2008 = $2000 (var Array) or $0008 (var String)
  // then when we asked:
  //   is $2000 (varArray)? we should receive TRUE (xVTHigh=pVarType)
  //   is $2008 (varArray of varString)? we should receive TRUE (xVT=pVarType)
  //   is $0008 (varString)? we should receive FALSE
  Result:=(xVT=pVarType) or ((xVTHigh=pVarType) and (xVTHigh<>varEmpty));
end ;

//------------------------------------------------------------------------------
function  tiAddTrailingValue( const pLine, pValue : string ; pDuplicates : boolean = true ) : string ;
begin
  if pLine = '' then
  begin
    result := pLine ;
    Exit ; //==>
  end ;

  if pDuplicates then
  begin
    result := pLine + pValue ;
    Exit ; //==>
  end ;

  if ( not SameText( Copy( pLine,
                           Length( pLine ) - Length( pValue ) + 1,
                           Length( pValue )),
                     pValue )) then
    result := pLine + pValue
  else
    result := pLine ;

end ;

//------------------------------------------------------------------------------
function  tiRemoveTrailingValue( pStrLine, pStrValue : string ) : string ;
var
  lLHS : integer ;
  lRHS : integer ;
begin
  lLHS := length( pStrLine ) - Length( pStrValue ) + 1 ;
  lRHS := Length( pStrValue ) ;

  if copy( pStrLine, lLHS, lRHS ) = pStrValue then
    result :=
      copy( pStrLine, 1, lLHS - 1 )
  else
    result := pStrLine ;
end ;

//------------------------------------------------------------------------------
function tiAddTrailingComma( pStrValue : string ) : string ;
begin
  result := tiAddTrailingValue( pStrValue, ',', true ) ;
end ;

//------------------------------------------------------------------------------
function  tiAddTrailingAnd( pStrValue : string ) : string ;
begin
  result := tiAddTrailingValue( pStrValue, ' and ', false ) ;
end ;

//------------------------------------------------------------------------------
function  tiAddTrailingOr( pStrValue : string ) : string ;
begin
  result := tiAddTrailingValue( pStrValue, ' or ', false ) ;
end ;

//------------------------------------------------------------------------------
function  tiAddTrailingSpace( pStrValue : string ) : string ;
begin
  result := tiAddTrailingValue( pStrValue, ' ', true ) ;
end ;

//------------------------------------------------------------------------------
function tiDateToPreviousWeekDayDate( pdtValue : TDateTime ) : TDateTime ;
var iDay : integer ;
begin
  result := pdtValue ;
  iDay   := dayOfWeek( result ) ;
  case iDay of
  1 : result := result - 2 ; // Sunday
  2 : result := result - 3 ; // Monday
  3 : result := result - 1 ; // Tuesday
  4 : result := result - 1 ; // Wednesday
  5 : result := result - 1 ; // Thursday
  6 : result := result - 1 ; // Friday
  7 : result := result - 1 ; // Saturday
  end ;
end ;

//------------------------------------------------------------------------------
function tiAddTrailingSlash( const pDir : string ) : string ;
begin
  result := tiAddTrailingValue( pDir, '\', false ) ;
end ;

//------------------------------------------------------------------------------
function tiRemoveTrailingSlash( const pStrDir : string ) : string ;
begin
  result := tiRemoveTrailingValue( pStrDir, '\' ) ;
end ;

//------------------------------------------------------------------------------
function tiRemoveLeadingSlash( const pStrDir : string ) : string ;
begin
  if copy( pStrDir, 1, 1 ) = '\' then begin
    result := copy( pStrDir, 2, length( pStrDir ) - 1 ) ;
  end else begin
    result := pStrDir ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiPosR( pStrTarget, pStrValue : string ) : integer ;
var i : integer ;
    iTargetLength : integer ;
begin
  i := length( pStrValue ) ;
  iTargetLength := length( pStrTarget ) ;
  while i > 0 do begin
    if copy( pStrValue, i, iTargetLength ) = pStrTarget then begin
      break ; //==>
    end ;
    dec( i ) ;
  end ;
  result := i ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiGetUserName : string ;
var userNameBuffer : array[0..255] of char ;
    sizeBuffer : DWord ;
begin
  SizeBuffer := 256 ;
  getUserName( userNameBuffer, sizeBuffer ) ;
  result := string( userNameBuffer ) ;
end ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function tiGetUserName : string ;
var
  pwrec : PPasswordRecord;
begin
  pwrec := getpwuid( getuid );
  Result := pwrec.pw_name;
end;
{$ENDIF LINUX}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiGetComputerName : string ;
var computerNameBuffer : array[0..255] of char ;
    sizeBuffer : DWord ;
begin
  SizeBuffer := 256 ;
  getComputerName( computerNameBuffer, sizeBuffer ) ;
  result := string( computerNameBuffer ) ;
end ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function tiGetComputerName : string ;
var
  uts :utsname;
begin
  uname(uts);
  Result := uts.nodename;
end ;
{$ENDIF LINUX}

//------------------------------------------------------------------------------
function tiRemoveDrive( pStrPath : string ) : string ;
var sDrive : string ;
begin
  sDrive := extractFileDrive( pStrPath ) ;
  if sDrive <> '' then begin
    result := copy( pStrPath, length( sDrive )+1, length( pStrPath ) - length( sDrive )) ;
  end else begin
    result := pStrPath ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiIsBitSet( const pVal: longint; const pBit: byte) : boolean ;
begin
  result := (pVal and (1 shl pBit)) <> 0 ;
end ;

//------------------------------------------------------------------------------
function tiBitToString( const pVal: longint; const pBit: byte): string ;
begin
  if tiIsBitSet( pVal, pBit) then
    result := '1'
  else
    result := '0' ;
end ;

//------------------------------------------------------------------------------
function tiInt32ToBinString( const val : longInt ) : string ;
var i : integer ;
begin
  result := '' ;
  for i := 31 downto 0 do begin
    result := result + tiBitToString( val, i  ) ;
  end ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure tiSetFileReadOnly( pStrFileTo : string ; pBoolReadOnly : boolean ) ;
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer ;
      lBoolReadOnly : boolean ;
begin
  lBoolReadOnly := tiIsFileReadOnly( pStrFileTo ) ;
  if lBoolReadOnly = pBoolReadOnly then exit ; //==>

  iCurrentState := tiWin32FileGetAttr( pStrFileTo ) ;
  if pBoolReadOnly then begin
    tiWin32FileSetAttr( pStrFileTo, iCurrentState or cReadOnly ) ;
  end else begin
   tiWin32FileSetAttr( pStrFileTo, iCurrentState xor cReadOnly ) ;
  end ;
end ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
procedure tiSetFileReadOnly( pStrFileTo : string ; pBoolReadOnly : boolean ) ;
var
  StatBuffer: TStatBuf ;
  lBoolReadOnly: Boolean ;
begin
  lBoolReadOnly := tiIsFileReadOnly( pStrFileTo ) ;
  if lBoolReadOnly = pBoolReadOnly then exit ; //==>

  stat( PChar(pStrFileTo), StatBuffer ) ;
  if pBoolReadOnly then begin
    chmod( PChar(pStrFileTo), StatBuffer.st_mode xor S_IWRITE ) ; // remove write access. Owner rights only.
  end else begin
    chmod( PChar(pStrFileTo), StatBuffer.st_mode or S_IWRITE ) ; // add write access.
  end ;
end ;
{$ENDIF LINUX}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiIsFileReadOnly( pStrFileTo : string ) : boolean ;
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer ;
begin
  iCurrentState := tiWin32FileGetAttr( pStrFileTo ) ;
  result := tiIsBitSet( iCurrentState, 0 ) ;
end ;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function tiIsFileReadOnly( pStrFileTo : string ) : boolean ;
var
  StatBuffer: TStatBuf;
  error: integer;
begin
  error := stat(PChar(pStrFileTo), StatBuffer);
  if error <> 0 then
    raise Exception.Create('Could not find the file <' + pStrFileTo + '>');
  { Testing for Owner rights only.
    Not sure if we should test for Group & Other aswell.
    linux permissions = owner(rwx),group(rwx),other(rwx) }
  Result :=  not (S_IWRITE = ((StatBuffer.st_mode and S_IWRITE)));
end;
{$ENDIF LINUX}

//------------------------------------------------------------------------------
function tiGetEXEPath : string ;
var
  path: array[0..MAX_PATH - 1] of char;
begin
  if IsLibrary then
    SetString(Result, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
    result := paramStr( 0 ) ;
  result := tiRemoveTrailingSlash( ExtractFilePath( Result )) ;
end;

//------------------------------------------------------------------------------
function tiAddEXEPath(const pFileName: string): string;
begin
  Result :=
    ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) +
      ExtractFileName(pFileName));
end ;

//------------------------------------------------------------------------------
procedure tiDirectoryTreeToStringList( const psStartDir : string ; const pslDirList : TStringList ; pbRecurse : boolean ) ;
  procedure _ReadDirectories( const psStartDir : string ; slTree : TStringList ; bRecurse : boolean ) ;
    procedure _AddIfDir( searchRec : TSearchRec ; sStartDir : string ; slTree : TStringList ; bRecurse : boolean ) ;
    begin
        if (( searchRec.attr and faDirectory ) > 0 ) and
           ( searchRec.name <> '.' ) and
           ( searchRec.name <> '..' ) then begin
          slTree.add( sStartDir + searchRec.name ) ;
          if bRecurse then begin
            _ReadDirectories( sStartDir + searchRec.name, slTree, bRecurse ) ;
          end ;
        end ;
    end ;
  var
    lsStartDir : string ;
    SearchRec : TSearchRec ;
  begin
    lsStartDir := tiAddTrailingSlash( psStartDir ) ;
    try
      if sysUtils.FindFirst( lsStartDir + '*.*',
                             faDirectory,
                             SearchRec ) = 0 then
      begin
        _AddIfDir( searchRec, lsStartDir, slTree, bRecurse ) ;
        while sysUtils.findNext( searchRec ) = 0 do
        begin
          _AddIfDir( searchRec, lsStartDir, slTree, bRecurse ) ;
        end ;
      end ;
    finally
      sysUtils.FindClose( SearchRec ) ;
    end ;
  end ;
var
  lCursor : TCursor ;
  lStartDir : string ;
begin

  lStartDir := tiRemoveTrailingSlash( psStartDir ) ;
  pslDirList.Clear ;

  if not DirectoryExists( lStartDir ) then
    exit ;

  pslDirList.Add( lStartDir ) ;

  lCursor := Screen.Cursor ;
  screen.cursor := crHourGlass ;
  try
    _ReadDirectories( lStartDir, pslDirList, pbRecurse ) ;
  finally
    screen.cursor := lCursor ;
  end ;

end ;

//------------------------------------------------------------------------------
{$IFDEF DELPHI6ORAVOVE} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
procedure tiFilesToStringList( const psStartDir,
                               psWildCard : string ;
                               slResult : TStringList ;
                               const pbRecurse : boolean ) ;
  // Locally visible proc
  procedure AddFile( searchRec : TSearchRec ; sStartDir, pStrWildCard : string ; slTree : TStringList ; bRecurse : boolean ) ;
  begin
    Assert( not (( searchRec.attr and faDirectory ) > 0 ), 'A directory passed, but a file expected' ) ;
    Assert(( searchRec.name <> '.'  ),                      'A directory passed, but a file expected' ) ;
    Assert(( searchRec.name <> '..' ),                    'A directory passed, but a file expected' ) ;
    slTree.add( sStartDir + searchRec.name ) ;
  end ;

var
  SearchRec : TSearchRec ;
  lsStartDir : string ;
  lslDirTree : TStringList ;
  i : integer;
  lCursor : TCursor ;
begin
  lCursor := Screen.Cursor ;
  if GetCurrentThreadID <> MainThreadID then
    screen.cursor := crHourGlass ;
  try
    slResult.Clear ;
    lsStartDir := tiAddTrailingSlash( psStartDir ) ;
    lslDirTree := TStringList.Create ;
    try
      if pbRecurse then
        tiDirectoryTreeToStringList( lsStartDir, lslDirTree, pbRecurse )
      else
        lslDirTree.Add( lsStartDir ) ;
      for i := 0 to lslDirTree.Count-1 do
      begin
        lsStartDir := tiAddTrailingSlash( lslDirTree.Strings[i] );
        {$IFDEF MSWINDOWS}
        try
          if tiWin32FindFirstFile( lsStartDir + psWildCard, SearchRec ) = 0 then
          begin
            AddFile( searchRec, lsStartDir, psWildCard, slResult, pbRecurse ) ;
            while sysUtils.findNext( searchRec ) = 0 do
            begin
              AddFile( searchRec, lsStartDir, psWildCard, slResult, pbRecurse ) ;
            end ;
          end ;
        finally
          sysUtils.FindClose( SearchRec ) ;
        end ;
        {$ENDIF MSWINDOWS}
        {$IFDEF LINUX}
        try
          if sysUtils.FindFirst( lsStartDir + psWildCard,
                                 faAnyFile-faSysFile-faDirectory,
                                 SearchRec ) = 0 then begin
            AddFile( searchRec, lsStartDir, psWildCard, slResult, pbRecurse ) ;
            while sysUtils.findNext( searchRec ) = 0 do begin
              AddFile( searchRec, lsStartDir, psWildCard, slResult, pbRecurse ) ;
            end ;
          end ;
        finally
          sysUtils.FindClose( SearchRec ) ;
        end ;
        {$ENDIF LINUX}
      end ;
    finally
      lslDirTree.Free ;
    end ;
  finally
    if GetCurrentThreadID <> MainThreadID then
      screen.cursor := lCursor ;
  end ;
end;
{$IFDEF DELPHI6ORAVOVE}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}

//------------------------------------------------------------------------------
function tiHasSubDirectory( pStrStartDir : string ) : boolean ;
var slTree : TStringList ;
begin
  slTree := TStringList.create ;
  try
    tiDirectoryTreeToStringList( pStrStartDir, slTree, false ) ;
    result := slTree.count > 1 ;
  finally
    slTree.free ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiExtractDirToLevel( const psFileName : TFileName ; piLevel : byte ) : TFileName ;
var
  i : integer ;
begin

  result := '' ;
  for i := 0 to piLevel+1 do
  begin
    if result <> '' then
      result := tiAddTrailingSlash( result ) ;
    result := result + tiToken( psFileName, '\', i ) ;
  end ;
  result := tiRemoveTrailingSlash( result ) ;

end ;

// ToDo: Must add code to handle hidden files.
function tiForceRemoveDir( const pDirName : TFileName ) : boolean ;
var
  lsl : TStringList ;
  i : integer ;
begin
  try
    lsl := TStringList.Create ;
    try
      tiFilesToStringList( pDirName, '*.*', lsl, true ) ;
      for i := 0 to lsl.Count - 1 do
      begin
        if tiIsFileReadOnly( lsl.Strings[i] ) then
          tiSetFileReadOnly( lsl.Strings[i], false ) ;
        SysUtils.DeleteFile( lsl.Strings[i] );
      end ;
      tiDirectoryTreeToStringList( pDirName, lsl, true ) ;
      for i := lsl.Count - 1 downto 0 do
        SysUtils.RemoveDir( lsl.Strings[i] );
      result := not DirectoryExists( pDirName ) ;
    finally
      lsl.Free ;
    end ;
  except
    on e:exception do
      result := false ;
  end ;
end ;


//------------------------------------------------------------------------------
function tiFloatToStr( const pRealValue : real ; const pIntPrecision : integer = 3 ) : string ;
begin
  result := _tiFloatToStr( pRealValue, pIntPrecision, '###0' ) ;
end;

//------------------------------------------------------------------------------
function  tiFloatToCommaStr( const pRealValue : real ;
                             const pIntPrecision : integer = 3 ) : string ;
begin
  result := _tiFloatToStr( pRealValue, pIntPrecision, '#,##0' ) ;
end ;

//------------------------------------------------------------------------------
function _tiFloatToStr( const pRealValue : real ;
                        const pIntPrecision : integer ;
                        const psFloatFormat : string ) : string ;
var lsFormat : string ;
begin
  lsFormat := psFloatFormat ;
  if pIntPrecision <> 0 then
    lsFormat := lsFormat + '.' + tiReplicate( '0', pIntPrecision ) ;
  try
    result := formatFloat( lsFormat, pRealValue ) ;
  except
    on e:exception do
      raise exception.Create( 'Unable to format floating point number. ' + Cr( 2 ) +
                              'Called in tiFloatToStr( ) ' +
                              'Format mask <' + lsFormat + '>' + Cr +
                              'Error message: ' + e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
function  tiSafeDiv( pNum, pDenom : real ) : real ;
begin
  if pDenom <> 0 then begin
    result := pNum / pDenom ;
  end else begin
    result := 0 ;
  end ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure tiRunEXEAndWait( pStrEXE : string ) ;
begin
  tiWin32RunEXEAndWait(pStrEXE) ;
end ;
{$ENDIF MSWINDOWS}

{
From the HELP

In Delphi, the Round function rounds a real-type value to an integer-type value.

X is a real-type expression. Round returns an Int64 value that is the value of
X rounded to the nearest whole number. If X is exactly halfway between two whole
numbers, the result is always the even number. This method of rounding is often
called "Banker’s Rounding".

If the rounded value of X is not within the Int64 range, a runtime error is
generated, which can be handled using the EInvalidOp exception.

Note:	The behavior of Round can be affected by the Set8087CW procedure or
SetRoundMode function.}
function tiRound(X: Extended): Int64;
// Rounds a number "normally": if the fractional
// part is >= 0.5 the number is rounded up (see RoundUp)
// Otherwise, if the fractional part is < 0.5, the
// number is rounded down (see RoundDn).
//   RoundN(3.5) = 4     RoundN(-3.5) = -4
//   RoundN(3.1) = 3     RoundN(-3.1) = -3
begin
  Result := Trunc(Int(X) + Int(Frac(X) * 2));
end;

//------------------------------------------------------------------------------
function tiSetPrecision( const pValue : real ; const pPrecision : integer = 3 ) : real ;

var
  lMultiplier : integer ;
begin
  if pPrecision >= 0 then
  begin
    lMultiplier := trunc( power( 10, pPrecision )) ;
    result := tiRound( pValue * lMultiplier ) / lMultiplier ;
//     result := StrToFloat( tiFloatToStr(pValue, pPrecision));
  end else
  begin
    lMultiplier := trunc( power( 10, pPrecision * -1 )) ;
    result := tiRound( pValue / lMultiplier ) * lMultiplier ;
  end ;
end ;

// Get the decimal part of a real number
//------------------------------------------------------------------------------
{
Q:  How do I find one string inside another with wildcards?

A:  There are many times when you need to compare two strings, but want to
use wild cards in the match - all last names that begin with 'St', etc.  The
following is a piece of code I got from Sean Stanley in Tallahassee Florida
in C.  I translated it into Delphi an am uploading it here for all to use. I
have not tested it extensivly, but the original function has been tested
quite thoughly. }
//------------------------------------------------------------------------------
function tiWildcardMatch( const psSource, psPattern: String; const pbCaseSensitive: boolean = false): Boolean;

  function MatchPatternStr(const psElement, psPattern: String): Boolean;
  var PatternChar: Char;
  begin
    if (psPattern = '*') then Result := True
    else if (psElement = EmptyStr) then Result := (psPattern = EmptyStr)
    else begin
      if (Copy(psPattern, 1, 1) <> EmptyStr) then PatternChar := Copy(psPattern, 1, 1)[1]
      else PatternChar := #0;

      case PatternChar of
      '*': begin
        if MatchPatternStr(psElement, Copy(psPattern, 2, Length(psPattern))) then Result := True
        else Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), psPattern);
      end;//'*'
      '?': Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(psPattern, 2, Length(psPattern)));
      else
        if Copy(psElement, 1, 1) = Copy(psPattern, 1, 1) then
          Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(psPattern, 2, Length(psPattern)))
        else Result := False;
      end;//case
    end;//else
  end;

begin
  if pbCaseSensitive then
    Result := MatchPatternStr( psSource, psPattern )
  else
    Result := MatchPatternStr( UpperCase( psSource ), UpperCase( psPattern )) ;
end;

//------------------------------------------------------------------------------
{ TODO : 
This will work:
  tiSubStr( 'my <d>string</d>', '<d>', '</d>', ) ;
but this will not:
  tiSubStr( 'my <u>long</e><d>string</e>', '<d>', '</e>', ) ; }
function  tiSubStr( const pSource, pStartDelim, pEndDelim : string ; pIndex : integer = 1 ) : string ;
var
  liStart : integer ;
  liEnd   : integer ;
begin

  Assert( pIndex = 1, 'Under development, pIndex not yet in use' ) ;

  result := '' ;

  liStart := Pos( pStartDelim, pSource ) ;
  if liStart <> 0 then
    liStart := liStart + length( pStartDelim ) ;

  liEnd := Pos( pEndDelim, pSource ) ;
  if liEnd <> 0 then
    liEnd := liEnd - 1 ;

  if ( liStart = 0 ) or ( liEnd = 0 ) then
    Exit ; //==>

  result := Copy( pSource, liStart, liEnd - liStart + 1 ) ;

end ;

//------------------------------------------------------------------------------
procedure tiReadFileDateSize( const psFileName : string ;
                              var   pDateTime  : TDateTime ;
                              var   piFileSize : integer );
var
  lFileStream : TFileStream ;
begin
  // Read the file date
  pDateTime := FileDateToDateTime( FileAge( psFileName )) ;
  // Read the file size
  try
    lFileStream := TFileStream.Create( psFileName, fmOpenRead or fmShareDenyNone ) ;
    try
      piFileSize := lFileStream.Size ;
    finally
      lFileStream.Free;
    end;
  except
    on e:exception do
      raise Exception.Create( 'Unable to read file size. Error: ' +
                e.message ) ;
  end ;
end;

//------------------------------------------------------------------------------
procedure tiSetFileDate( const psFileName : string ; const pdtFileDate : TDateTime ) ;
var
  lFileHandle : Integer ;
  lFileDate   : Integer ;
  {$IFDEF LINUX}
  lError      : Integer ;
  {$ENDIF LINUX}
begin
  lFileDate   := DateTimeToFileDate( pdtFileDate ) ;
  {$IFDEF MSWINDOWS}
  lFileHandle := FileOpen( psFileName, fmOpenWrite or fmShareDenyNone);
  try
    if lFileHandle > 0 then
      FileSetDate( lFileHandle, lFileDate )
    else
      raise exception.Create( 'Unable to set file date on <' +
                              psFileName ) ;
  finally
    FileClose( lFileHandle ) ;
  end ;
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
    lError := FileSetDate(psFileName, lFileDate);
    if lError <> 0 then
      raise Exception.Create('Unable to set file date on <' + psFileName);
  {$ENDIF LINUX}
end ;

//------------------------------------------------------------------------------
function  Cr( const pCount : Byte = 1 ) : string ;
begin
  result := tiReplicate( #13, pCount ) ;
end ;

//------------------------------------------------------------------------------
function  Lf( const pCount : Byte = 1 ) : string ;
begin
  result := tiReplicate( #10, pCount ) ;
end ;

//------------------------------------------------------------------------------
function CrLf( const pCount : Byte = 1 ) : string ;
begin
  result := tiReplicate( #13 + #10, pCount ) ;
end;

function  Tab( const pCount : Byte = 1 ) : string ;
begin
  result := tiReplicate( #9, pCount ) ;
end ;

//------------------------------------------------------------------------------
{
procedure tiSortList( pData : TList ; pDoCompare : TSortCompare ) ;
  procedure _SortList( pData : TList ;
                       pDoCompare : TSortCompare );
  var
    i, j  : integer ;
    lTemp : TObject ;
    liComp : integer ;
  begin
    for i := pData.Count - 1 downto 0 do
      for j := 0 to pData.Count - 2 do begin
        pDoCompare( pData.Items[j], pData.Items[j + 1], liComp ) ;
        if liComp > 0 then begin
          lTemp := pData.Items[j] ;
          pData.Items[j] := pData.Items[j + 1] ;
          pData.Items[j + 1] := lTemp ;
        end;
      end ;
  end ;

begin
  _SortList( pData, pDoCompare  ) ;
end ;
}

procedure tiStringToStream( const pStr : string ; const pStream : TStream ) ;
var
  lBuffer : PChar ;
  lLen : integer ;
begin
  lBuffer := PChar( pStr ) ;
  lLen := length( pStr ) ;
  pStream.Size := 0 ;
  pStream.write( lBuffer^, lLen ) ;
  pStream.Position := 0 ;
end;

procedure tiAppendStringToStream( const pStr : string ; const pStream : TStream ) ;
var
  lPC : PChar ;
begin
  Assert( pStream <> nil, 'Stream unassigned.' ) ;
  pStream.Position := pStream.Size ;
  lPC := PChar( pStr ) ;
  pStream.WriteBuffer( lPC^, length( pStr )) ;
end ;

function  tiStreamToString( const pStream : TStream ) : string ;
var
  lPos : integer ;
begin
  lPos := pStream.Position ;
  pStream.Position := 0 ;
  SetLength(Result,  pStream.Size);
  pStream.Read( Result[1], pStream.Size ) ;
  pStream.Position := lPos ;
end;

procedure tiFileToStream( const pFileName : string ; const pStream : TStream ) ;
var
  lStream : TMemoryStream ;
  lPosition : integer ;
begin
  lPosition := pStream.Position ;
  lStream := TMemoryStream.Create ;
  try
    lStream.LoadFromStream(pStream);
    lStream.SaveToFile(pFileName);
  finally
    lStream.Free;
  end;
  pStream.Position := lPosition ;
end ;

function  tiStreamToFile( const pFileName : string ; const pStream : TStream ) : string ;
var
  lStream : TMemoryStream ;
begin
  lStream := TMemoryStream.Create ;
  try
    pStream.Size := 0 ;
    lStream.LoadFromFile(pFileName);
    lStream.SaveToStream(pStream);
  finally
    lStream.Free;
  end;
  pStream.Position := 0 ;
end;

procedure tiCopyStream( const pStreamFrom, pStreamTo : TStream ) ;
begin
  pStreamFrom.Position := 0 ;
  pStreamTo.Size := 0 ;
  pStreamTo.CopyFrom(pStreamFrom, pStreamFrom.Size);
  pStreamTo.Position := 0 ;
  pStreamFrom.Position := 0 ;
end;

//------------------------------------------------------------------------------
procedure tiListToStream( pStream : TStream ;
                          pList : TList ;
                          psSepChar : string ;
                          pslColsSelected : TStringList = nil ) ;
  procedure Write( pStream : TStream ; psText : string ) ;
  var
    lpcText : PChar ;
  begin
    lpcText := PChar( psText ) ;
    pStream.WriteBuffer( lpcText^, length( lpcText )) ;
  end ;
var
  i, j       : integer ;
  lslFields  : TStringList ;
  lsValue    : string ;
  lFieldName : string ;
  pData      : TPersistent ;
  lLine      : string ;
begin

  lslFields  := TStringList.Create ;

  try

    // If pslColsSelected was not assigned, then read all the prop names from
    // the first entry in the list.
    if pslColsSelected = nil then
      tiGetPropertyNames( ( TObject(pList.Items[0])  as TPersistent ),
                          lslFields )
    // If the pslColsSelected param was assigned, then use these as the
    // cols to display.
    else
      lslFields.Assign( pslColsSelected ) ;

    // Write column headings
    for i := 0 to lslFields.Count - 1 do begin
      write( pStream, lslFields.Strings[i] ) ;
      if i < lslFields.Count - 1 then
        write( pStream, psSepChar )
      else
        write( pStream, CrLf ) ;
    end ;

    // Write the data
    for i := 0 to pList.Count - 1 do
    begin
      pData := ( TObject( pList.Items[i]) as TPersistent ) ;
      lLine := '' ;
      for j := 0 to lslFields.Count - 1 do
      begin
        if lLine <> '' then
          lLine := lLine + psSepChar ;
        lFieldName := lslFields.Strings[j] ;
        if GetPropInfo(pData,lFieldName).PropType^.Name = 'TDateTime' then
          lsValue := tiDateTimeToStr(GetPropValue(pData,lFieldName))
        else
          lsValue := GetPropValue( pData, lFieldName, true ) ;
        lLine := lLine + lsValue ;
      end ;
      if i <> 0 then
        lLine := CrLf + lLine ;
      Write( pStream, lLine )
    end ;
  finally
    lslFields.Free ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure tiListToCSV( pList : TList ;
                       pslColsSelected : TStringList = nil ;
                       const psFileName : string = '' ;
                       pAskToView : boolean = true ) ;
var
  lSaveDialog : TSaveDialog ;
  lsFileName : string ;
  lStream    : TFileStream ;
begin
  lsFileName := psFileName ;

  // A file name was not passed, so ask the user.
  if lsFileName = '' then begin
    lSaveDialog := TSaveDialog.Create( nil ) ;
    try
      lSaveDialog.DefaultExt := 'CSV' ;
      {$IFDEF MSWINDOWS}
      lSaveDialog.FileName := gReg.ReadString( 'tiDSToCSV', 'FileName', '' ) ;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      lSaveDialog.FileName := gINI.ReadString( 'tiDSToCSV', 'FileName', '' ) ;
      {$ENDIF LINUX}
      lSaveDialog.Filter := 'CSV Files|*.CSV|All files|' + AllFilesWildCard ;
      if lSaveDialog.Execute then begin
        lsFileName := lSaveDialog.FileName ;
        {$IFDEF MSWINDOWS}
        gReg.WriteString( 'tiDSToCSV', 'FileName', lsFileName ) ;
        {$ENDIF MSWINDOWS}
        {$IFDEF LINUX}
        gINI.WriteString( 'tiDSToCSV', 'FileName', lsFileName ) ;
        {$ENDIF LINUX}
      end else begin
        exit ; //==>
      end ;
    finally
      lSaveDialog.Free ;
    end ;
  end ;

  if lsFileName = '' then
    raise exception.create( 'Invalid file name <' + lsFileName + '>' ) ;

    lStream := TFileStream.Create( lsFileName, fmCreate or fmShareDenyNone ) ;
    try
      tiListToStream( lStream, pList, ',', pslColsSelected ) ;
    finally
      lStream.Free ;
    end ;

  if pAskToView and
     tiAppConfirmation( 'Do you want to view ' + lsFileName + '?' ) then
    tiEditFile( lsFileName ) ;
end ;

//------------------------------------------------------------------------------
procedure tiListToClipboard( pList : TList ;
                             pslColsSelected : TStringList = nil ;
                             pConfirm : boolean = true ) ;
var
  lStream    : TStringStream ;
//  lBuffer    : PChar ;
begin
  lStream := TStringStream.Create( '' ) ;
  try
    tiListToStream( lStream, pList, Tab, pslColsSelected ) ;
//    GetMem( lBuffer, lStream.Size ) ;
//    try
      lStream.Position := 0 ;
//      lStream.ReadBuffer( lBuffer^, lStream.Size ) ;
//      Clipboard.Clear ;
//      Clipboard.SetTextBuf( lBuffer ) ;
      Clipboard.AsText := lStream.DataString ;
//    finally
  //    FreeMem( lBuffer ) ;
//    end ;
  finally
    lStream.Free ;
  end ;

  if pConfirm then
    tiAppMessage( 'Data has been copied to the clipboard.' ) ;

end ;

//------------------------------------------------------------------------------
procedure tiStringToFile( const psText, psFileName : string ) ;
var
  lStream : TFileStream ;
  lpcText  : PChar ;
begin
  lStream := TFileStream.Create( psFileName, fmCreate  ) ;
  try
    lpcText := PChar( psText ) ;
    lStream.WriteBuffer( lpcText^, length( psText )) ;
  finally
    lStream.Free ;
  end ;
end ;

//------------------------------------------------------------------------------
function  tiFileToString( const pFileName : TFileName ) : string ;
var
  lFileStream   : TFileStream ;
begin
// // A PChar needs to be one more than the string size
// GetMem( lBuffer, lFileStream.Size+1 ) ;
// // Set the null terminator in last memory location
// IBuffer[lFileStream.Size]=Char(0);

  result := '' ;
  lFileStream := TFileStream.Create( pFileName,
                                     fmOpenRead or fmShareDenyNone ) ;
  try
    SetLength(Result,  lFileStream.Size);
    lFileStream.Read( Result[1], lFileStream.Size ) ;
  finally
    lFileStream.Free ;
  end ;
  
end ;

//------------------------------------------------------------------------------
{
function  tiFileToString( const pFileName : TFileName ) : string ;
var
  lFileStream : TFileStream ;
  lBuffer   : PChar ;
begin
  result := '' ;
  lFileStream := TFileStream.Create( pFileName,
                                     fmOpenReadWrite or fmShareCompat ) ;
  try
    GetMem( lBuffer, lFileStream.Size ) ;
    try
      lFileStream.Read( lBuffer^, lFileStream.Size ) ;
      Result := String( lBuffer ) ;
    finally
      FreeMem( lBuffer ) ;
    end ;
  finally
    lFileStream.Free ;
  end ;
end ;
}
//------------------------------------------------------------------------------
procedure tiGetPropertyNames( pPersistent : TPersistent ; pSL : TStringList ;
                              pPropFilter : TTypeKinds = ctkSimple ) ;
begin
  Assert( pPersistent <> nil, 'pPersistent not assigned.' ) ;
  tiGetPropertyNames( TPersistentClass( pPersistent.ClassType ),
                      pSL,
                      pPropFilter ) ;
end ;

procedure tiGetPropertyNames( pPersistent : TPersistentClass ;
                              pSL : TStringList ;
                              pPropFilter : TTypeKinds = ctkSimple ) ;
var
  lCount : integer ;
  lSize  : integer ;
  lList  : PPropList ;
  i : integer ;
  lPropFilter : TTypeKinds ;
begin

  Assert( pSL <> nil, 'pSL not assigned.' ) ;
  lPropFilter := pPropFilter ;

  pSL.Clear ;
  {$IFDEF DELPHI6ORABOVE}
  lCount := GetPropList(pPersistent.ClassInfo, lPropFilter, nil, false);
  {$ELSE}
  lCount := GetPropList(pPersistent.ClassInfo, lPropFilter, nil);
  {$ENDIF}
  lSize := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
    {$IFDEF DELPHI6ORABOVE}
    GetPropList(pPersistent.ClassInfo, lPropFilter, lList, false);
    {$ELSE}
    GetPropList(pPersistent.ClassInfo, lPropFilter, lList);
    {$ENDIF}
    for i := 0 to lcount - 1 do
      psl.add( lList[i].Name ) ;
  finally
    FreeMem( lList, lSize ) ;
  end ;
end ;

//------------------------------------------------------------------------------
function tiIsReadWriteProp( const pData : TPersistent ; const psPropName : string ) : boolean ;
//var
//  lPropInfo : PPropInfo ;
begin
  result :=tiIsReadWriteProp( TPersistentClass(pData.ClassType), psPropName ) ;
//  Assert( pData <> nil, 'pData not assigned' ) ;
//  Assert( IsPublishedProp( pData, psPropName ), psPropName + ' not a published property on ' + pData.ClassName ) ;
//  try
//    lPropInfo := GetPropInfo( pData, psPropName ) ;
//    result    := ( lPropInfo.GetProc <> nil ) and ( lPropInfo.SetProc <> nil ) ;
//  except
//    on e:exception do
//      raise exception.CreateFmt( 'Error calling tiIsReadWriteProp with class: %s and property %s',
//                                 [pData.ClassName, psPropName]);
//  end ;
end;

//------------------------------------------------------------------------------
function tiIsReadWriteProp( const pData : TPersistentClass ; const psPropName : string ) : boolean ;
var
  lPropInfo : PPropInfo ;
begin
  Assert( pData <> nil, 'pData not assigned' ) ;
  Assert( IsPublishedProp( pData, psPropName ), psPropName + ' not a published property on ' + pData.ClassName ) ;
  try
    lPropInfo := GetPropInfo( pData, psPropName ) ;
    result    := ( lPropInfo.GetProc <> nil ) and ( lPropInfo.SetProc <> nil ) ;
  except
    on e:exception do
      raise exception.CreateFmt( 'Error calling tiIsReadWriteProp with class: %s and property %s',
                                 [pData.ClassName, psPropName]);
  end ;
end;

//  TTypeKind = ( tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
//                tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
//                tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);
//------------------------------------------------------------------------------
function tiGetSimplePropType( const pPersistent : TPersistent ; const psPropName : string ) : TtiTypeKind ;
var
  lPropType : TTypeKind ;
  lPropTypeName : string ;
begin

  Assert( pPersistent <> nil, 'pPersistent is nil' ) ;

  lPropTypeName := GetPropInfo(pPersistent, psPropName).PropType^.Name ;

  // Check for a TDateTime
  if SameText( lPropTypeName, 'TDateTime' ) then
  begin
    result := tiTKDateTime ;
    Exit ; //==>
  end ;

  // Check for a Boolean
  if SameText( lPropTypeName, 'Boolean' ) then
  begin
    result := tiTKBoolean ;
    Exit ; //==>
  end ;

  try
    lPropType := PropType( pPersistent, psPropName ) ;
  except
    on e:exception do
      raise exception.create( 'Error in tiGetSimpleTypeKind ' + Cr +
                              'Property name: ' + psPropName + Cr +
                              'Message: ' + e.message ) ;
  end ;

  // ToDo: Detection of stream properties could be better
  if ( lPropType = tkClass ) and
     (( SameText( 'TStream', lPropTypeName )) or
      ( SameText( 'TMemoryStream', lPropTypeName )) or
      ( SameText( 'TFileStream', lPropTypeName )) or
      ( SameText( 'TStringStream', lPropTypeName ))) then
  begin
    result := tiTKBinary ;
    Exit ; //==>
  end ;

  case lPropType of
  tkInteger,
  tkInt64,
  tkEnumeration : result := tiTKInteger ;

  tkFloat       : result := tiTKFloat ;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  tkWString     : result := tiTKString ;

  else
    raise exception.create( 'Invalid property type passed to ' +
                            'tiGetSimplePropType. ClassName <' +
                            pPersistent.ClassName +
                            '> Property name <' +
                            psPropName + '>' ) ;
  end ;

end;

function tiVarSimplePropType( pValue : Variant ) : TtiTypeKind ;
begin
{
varEmpty        The variant is Unassigned.
varNull	        The variant is Null.
VarSmallint     16-bit signed integer (type Smallint).
varInteger      32-bit signed integer (type Integer).
varSingle       Single-precision floating-point value (type Single).
varDouble       Double-precision floating-point value (type Double).
varCurrency     Currency floating-point value (type Currency).
varDate         Date and time value (type TDateTime).
varOLEStr       Reference to a dynamically allocated UNICODE string.
varDispatch     Reference to an Automation object (an IDispatch interface pointer).
varError        Operating system error code.
varBoolean      16-bit boolean (type WordBool).
varUnknown      Reference to an unknown COM object (an IUnknown interface pointer).
varByte         8-bit unsigned integer (type Byte).
varString       Reference to a dynamically allocated Pascal string (type AnsiString).
varTypeMask     Bit mask for extracting type code.
varArray        Bit indicating variant array.
varByRef        Bit indicating variant contains a reference (rather than a value).
}

  if tiIsVariantOfType( pValue, varSmallint ) or
     tiIsVariantOfType( pValue, varInteger ) or
     tiIsVariantOfType( pValue, varByte ) then
    Result := tiTKInteger
  else if tiIsVariantOfType( pValue, varSingle ) or
          tiIsVariantOfType( pValue, varDouble ) or
          tiIsVariantOfType( pValue, varCurrency ) then
    Result := tiTKFloat
  else if tiIsVariantOfType( pValue, varString ) or
          tiIsVariantOfType( pValue, varOLEStr ) then
    Result := tiTKString
  else if tiIsVariantOfType( pValue, varDate ) then
    Result := tiTKDateTime
  else if tiIsVariantOfType( pValue, varBoolean ) then
    Result := tiTKBoolean
  else
  begin
    tiFmtException( 'Invalid varianat type', 'tiUtils', 'tiVarSimplePropType' ) ;
    Result := tiTKInteger ; // Just to shut the compiler up. Won't get here.
  end ;


end;

//------------------------------------------------------------------------------
function tiIsNumericProp( pPersistent : TPersistent ; psPropName : string ) : boolean ;
var
  lPropType : TTypeKind ;
begin
  try
    lPropType := PropType( pPersistent, psPropName ) ;
  except
    on e:exception do
      raise exception.create( 'Error in tiGetSimpleTypeKind ' +
                              'Message: ' + e.message ) ;
  end ;
  result := lPropType in [ tkInteger, tkInt64,tkEnumeration, tkFloat ] ;
end;

//------------------------------------------------------------------------------
function tiHasRTTI( pObject : TObject ) : boolean ;
var
  lClass : TClass ;
begin
  lClass := pObject.ClassType ;
  result := tiHasRTTI( lClass ) ;
end ;

//------------------------------------------------------------------------------
function tiHasRTTI( pClass : TClass ) : boolean ;
var
  lClassInfo : Pointer ;
begin
  lClassInfo := pClass.ClassInfo ;
  result := lClassInfo <> nil ;
end ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiMessageDlg
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
const
  cuiBorder    = 16 ;
  cuiBtnBorder = 8 ;
  cuiBtnHeight = 25 ;
  cuiBtnWidth  = 75 ;
  cuiImageWidth = 32 ;

procedure _FormatAndRaiseException( const pMessage, pCallStack : string ) ;
var
  lsException : string ;
begin
  lsException :=
    cgExceptionMessageStart    + Cr( 1 ) +
    pMessage                   + Cr( 1 ) +
    cgExceptionMessageEnd      + Cr( 2 ) +
    cgExceptionCallStackStart  + Cr( 1 ) +
    pCallStack         + Cr( 1 ) +
    cgExceptionCallStackEnd;
  raise exception.create( lsException ) ;
end;

// -----------------------------------------------------------------------------
procedure tiFmtException( e : Exception ;
                          const psMessage   : string ;
                          const psClassName : string = '' ;
                          const psMethod    : string = '' ) ;
var
  lsException : string ;
  lsMessage   : string ;
  lsCallStack : string ;
begin
  lsException := e.message ;
  if ( Pos( cgExceptionMessageStart,   lsException ) <> 0 ) and
     ( Pos( cgExceptionMessageEnd,     lsException ) <> 0 ) and
     ( Pos( cgExceptionCallStackStart, lsException ) <> 0 ) and
     ( Pos( cgExceptionCallStackEnd,   lsException ) <> 0 ) then
  begin
    lsMessage   := tiSubStr( lsException, cgExceptionMessageStart,   cgExceptionMessageEnd ) ;
    lsCallStack := tiSubStr( lsException, cgExceptionCallStackStart, cgExceptionCallStackEnd ) ;
    lsMessage   := Trim( psMessage ) + Cr + Trim(lsMessage) ;
    lsCallStack := psClassName + '.' + psMethod + Cr + Trim( lsCallStack ) ;
  end
  else
  begin
    lsMessage   := lsException + Cr(2) + psMessage ;
    lsCallStack := psClassName + '.' + psMethod ;
  end ;

  _FormatAndRaiseException( lsMessage, lsCallStack ) ;

end ;

// -----------------------------------------------------------------------------
procedure tiFmtException( const psMessage   : string ;
                          const pA : Array of Const ;
                          const psClassName : string = ''   ;
                          const psMethod    : string = '' ) ; overload ;
begin
  _FormatAndRaiseException( Format( psMessage, pA ), psClassName + '.' + psMethod ) ;
end ;

// -----------------------------------------------------------------------------
procedure tiFmtException( const psMessage   : string ;
                          const psClassName : string = '' ;
                          const psMethod    : string = '' ) ;
begin
  _FormatAndRaiseException( psMessage, psClassName + '.' + psMethod ) ;
end ;

//------------------------------------------------------------------------------
function  tiAddEllipsis( const psString : string ; piWidth : integer = 20 ) : string ;
var
  lLen : integer ;
begin

  lLen := Length( psString ) ;
  if lLen <= piWidth then
    result := psString
  else if ( lLen > piWidth ) then
    result := Copy( psString, 1, piWidth - 3 ) + '...'
  else
    result := Copy( psString, 1, lLen - 3 ) + '...'

end ;

//------------------------------------------------------------------------------
function  tiTrimR( const psString, psTrim : string ; pbCaseSensitive : boolean = false ) : string ;
var
  li : integer ;
begin
  if pbCaseSensitive then
    li := pos( psTrim, psString )
  else
    li := pos( UpperCase( psTrim ), UpperCase( psString )) ;

  if li <> 0 then
    result := Copy( psString, 1, li - 1 )
  else
    result := psString ;
end ;

//------------------------------------------------------------------------------
function  tiTrimL( const psString, psTrim : string ; pbCaseSensitive : boolean = false ) : string ;
var
  li : integer ;
begin
  if pbCaseSensitive then
    li := pos( psTrim, psString )
  else
    li := pos( UpperCase( psTrim ), UpperCase( psString )) ;

  if li <> 0 then
  begin
    li := li+Length( psTrim ) ;
    result := Copy( psString, li, Length( psString )-li+1 )
  end
  else
    result := psString ;
end ;

function  tiRemoveCrLf( const pString : string ) : string ;
begin
  result := tiStrTran( pString, Lf, '' ) ;
  result := tiStrTran( result,  Cr, ' ' ) ;
end ;

function tiTrimTrailingWhiteSpace( const pString : string ) : string ;
const
  cWhiteSpace = [ #13, #10, #32 ] ;
var
  i : integer ;
  lTruncChar : integer ;
begin
  lTruncChar := Length( pString ) ;
  for i := Length( pString ) downto 1 do
  begin
    if pString[i] in cWhiteSpace then
      Dec( lTruncChar )
    else
      Break ; //==>
  end ;
  result := Copy( pString, 1, lTruncChar ) ;
end ;

//------------------------------------------------------------------------------
// Returns true if the email address is valid
// Author/Autor: Ernesto D'Spirito <edspirito@latiumsoftware.com>
// Accompanying article at http://www.howtodothings.com/showarticle.asp?article=297
function tiIsEMailAddressValid(const email: string): boolean;
const
  // Valid characters in an "atom"
  atom_chars = [#33..#255] - ['(', ')', '<', '>', '@', ',', ';', ':',
                              '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars = [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters = ['A'..'Z', 'a'..'z'];
  letters_digits = ['0'..'9', 'A'..'Z', 'a'..'z'];
  subdomain_chars = ['-', '0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  i, n, subdomains: integer;
  c: char;
begin
  State := STATE_BEGIN;
  n := Length(email);
  i := 1;
  subdomains := 1;
  while (i <= n) do begin
    c := email[i];
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_ATOM:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c = '\' then
        State := STATE_QCHAR
      else if c = '"' then
        State := STATE_QUOTE
      else if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters then
        State := STATE_SUBDOMAIN
      else
        break;
    STATE_SUBDOMAIN:
      if c = '.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else if c = '-' then
        State := STATE_HYPHEN
      else if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN
      else if c <> '-' then
        break;
    end;
    inc(i);
  end;
  if i <= n then
    Result := False
  else
    Result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;

function tiIsFileNameValid( const pFileName : string ) : boolean ;
var
  lFileName : string ;
  i : integer ;
const
  ExcludedChars = [ '\', '/', ':', '*', '?', '"', '<', '>', '|' ] ;
begin
  lFileName := ExtractFileName( pFileName ) ;
  result :=
    ( Length( lFileName ) <= 255 ) and
    ( Length( lFileName ) > 0 ) ;
  if not result then
    Exit ; //==>

  // From the NT help
  //A filename can contain up to 255 characters, including spaces.
  // But, it cannot contain any of the following characters:
  // \ / : * ? " < > |
  for i := 1 to Length( lFileName ) do
    if  lFileName[i] in ExcludedChars then
    begin
      result := false ;
      Exit ; //==>
    end ;
end ;

function tiCheckSum(const Value: string): Integer;
// http://www.scalabium.com/faq/dct0129.htm
// The basic algorithm:
//
// add the values of the digits in the odd positions (1, 3, 5...)
// multiply this result by 3
// add the values of the digits in the even positions (2, 4, 6...)
// sum the results of steps 2 and 3
// the check digit is the smallest number which,
// when added to the result in step 4, produces a multiple of 10.
// Small example. Assume the source data is 08137919805
//
// 1. 0+1+7+1+8+5=22
// 2. 22*3=66
// 3. 8+3+9+9+0=29
// 4. 66+29=95
// 5. 95+??=100 where ?? is a 5 (our checksum)
var
  i, intOdd, intEven: Integer;
begin
  {add all odd seq numbers}
  intOdd := 0;
  i := 1;
  while (i <= Length(Value)) do
  begin
    Inc(intOdd, StrToIntDef(Value[i], 0));
    Inc(i, 2);
  end;

  {add all even seq numbers}
  intEven := 0;
  i := 2;
  while (i <= Length(Value)) do
  begin
    Inc(intEven, StrToIntDef(Value[i], 0));
    Inc(i, 2);
  end;

  Result := 3*intOdd + intEven;
  {modulus by 10 to get}
  Result := Result mod 10;
  if Result <> 0 then
    Result := 10 - Result
end;

{$IFDEF LINUX}
function GetTickCount: Longint;
var
  dummy: TTimes;
begin
  Result := times(dummy);
end;
function tiGetTextSize(pFont: TFont; s: String): TSize;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width       := 50;
    Bmp.Height      := 50;
    Bmp.Canvas.Font := pFont;
    Result.cx       := Bmp.Canvas.TextWidth( s );
    Result.cy       := Bmp.Canvas.TextHeight( s );
  finally
    Bmp.Free;
  end;
end;

{$ENDIF LINUX}

const
  cBase = 26 ;
  cZeroChar = 'A';

function tiEncodeWordBase26( const pNum : Word ) : String ;
var
  lNum : Int64 ;
  lPos : byte ;
begin
  if pNum = 0 then
  begin
    Result := cZeroChar;
    Exit ; //==>
  end;

  Result := '     '; // If the param is changed from Word, this lenght must be changed.
  lPos := 5;
  lNum := pNum;
  While lNum > 0 Do
  Begin
    Result[lPos] := Chr( Ord( cZeroChar ) + lNum Mod cBase);
    Dec(lPos) ;
    lNum := lNum div cBase;
  End;
  Result := Copy( Result, lPos+1, 5 - lPos ) ;
end ;

function  tiDecodeWordBase26( numstr : String ) : Word;
var
  i: Integer;
begin
  Result := 0;
  If Length(numStr) = 0 Then
    Exit; //==>
  i:= 1;
  While i <= Length( numStr ) Do
  Begin
    If (numstr[i] < cZeroChar) or
       ((Ord( numStr[i] ) - Ord( cZeroChar )) >= cBase )
    Then
      raise EConvertError.CreateFmt( cErrorDecodeNumError, [numstr, cBase]);
    Result := Result * cBase + Ord( numStr[i] ) - Ord( cZeroChar );
    Inc( i );
  End;
end ;


initialization
  // Set Windows internal date and time format strings
//  ShortDateFormat := csWinDateFormat ;
//  ShortTimeFormat := csWinTimeFormat ;

end.



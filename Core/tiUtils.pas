unit tiUtils;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiVisitor 
  ,tiObject
  ,SysUtils
  ,Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  ,shellAPI
  ,Messages
  {$ENDIF MSWINDOWS}
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,Buttons
  ,ExtCtrls
  ,Math
  ,TypInfo
  {$IFDEF FPC}
  ,LCLType
  {$ENDIF}
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,Contnrs
  ,tiConstants
  ;


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
  function  tiStrTran(         pStrValue, pStrDel, pStrIns : string) : string;
  function  tiStrTran1(aStrValue, aStrDel, aStrIns : string) : string;
  // Scan the string pStrValue, and replace any characters pStrDel with pStrIns (Case insensitive)
  function  tiCIStrTran(       pStrValue, pStrDel, pStrIns : string) : string;
  // Count the number of blocks of text in pStrValue seperated by pStrToken
  function  tiNumToken(        const pStrValue, pStrToken : string) : integer;
  // Extract the pIntNum(th) block of text in pStrValue, seperated by pStrToken
  function  tiToken(           const pStrValue, pStrToken : string; const pIntNum : integer) : string;
  // Return a string of spaces pIntLen long
  function  tiSpace(           pIntLen : integer) : string;
  // Pad pStrValue with spaces on the RHS to make it pIntLen long
  function  tiPadR(const psValue : string; piLen : integer) : string;
  // Pad pStrValue with spaces on the LHS to make it pIntLen long
  function  tiPadL(            pStrValue : string; pIntLen : integer) : string;
  // Pad pStrValue with spaces on both sides to make it pIntLen long
  function  tiPadC(            pStrValue : string; pIntLen : integer) : string;
  // Pad pStrValue with zeros on the LHS to make it pIntLen long
  function  tiPad0(            pStrValue : string; pIntLen : integer) : string;
  // Remove any leading zeros from a string
  function  tiRemoveLeading0(  pStrValue : string) : string;
  // Convert a string into mized case using a simple algorithm
  // Note: There is a more complete version of this (that takes care of works like
  //       McCrae or King Charles IV
  //       It will also work with characters other than #32 to sep words
  function  tiMixedCase(       pStrValue : string) : string;
  // Replicate the string pStrValue to return a string pIntLength long
  function  tiReplicate(       const pStrValue : string; pRepCount : Word) : string;
  // If pStrLine is not '', add the trailing value pStrValue
  function  tiAddTrailingValue(const pLine, pValue : string; pDuplicates : boolean = true) : string;
  // If the last character of pStrLine is pStrValue, then remove it
  function  tiRemoveTrailingValue(pStrLine, pStrValue : string) : string;
  // If pStrValue is not '', add a trailing ', '
  function  tiAddTrailingComma(pStrValue : string) : string;
  // If pStrValue is not '', add a trailing ' and '
  function  tiAddTrailingAnd(  pStrValue : string) : string;
  // If pStrValue is not '', add a trailing ' or '
  function  tiAddTrailingOr(   pStrValue : string) : string;
  // If pStr is not '', add a trailing ' '
  function  tiAddTrailingSpace(pStrValue : string) : string;
  // Return the first position of pStrValue in pStrTarget from the right.
  // Same as Delphi's Pos, except from the right, not left
  function  tiPosR(            pStrTarget, pStrValue : string) : integer;
  // Does the wildcard pPattern occur in the string pSource ?
  function tiWildcardMatch(const psSource, psPattern: String; const pbCaseSensitive: boolean = false): Boolean;
  // Extract a sub string within defined delimiters
  function  tiSubStr(const pSource, pStartDelim, pEndDelim : string; pIndex : integer = 1) : string;
  // Trunc a string to piWidth length and add '...'
  function  tiAddEllipsis(const psString : string; piWidth : integer = 20) : string;
  // Trim all characters after and including psTrim from psString
  function  tiTrimR(const psString, psTrim : string; pbCaseSensitive : boolean = false) : string;
  // Trim all characters before and including psTrim from psString
  function  tiTrimL(const psString, psTrim : string; pbCaseSensitive : boolean = false) : string;
  // Remove Cr & LF characters
  function  tiRemoveCrLf(const pString : string) : string;
  // Remove all the trailing white space characters (#32, #10, #13)
  // from the end of a string
  function tiTrimTrailingWhiteSpace(const pString : string) : string;
  // Returns true if the email address is valid
  function tiIsEMailAddressValid(const email: string): boolean;
  // Returns true if the file name is valid. Will not test if the
  // directory part of the file name exists, or is valid, will just
  // test the file name part
  function tiIsFileNameValid(const pFileName : string) : boolean;
  {: Replacement for Delphi's StrPos, but much faster. Not for FPC though!}
  function tiStrPos(const AString, ASubString: PChar): PChar;
  {: Normalize the string by replacing all repeated Spaces, Tabs and NewLines
    chars with a single space. It's so easy with Regular Expression! :( }
  function tiNormalizeStr(const pString: string): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Directory, file and file name manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Get a tempory file name with the extension pStrExt
  function  tiGetTempFile(        const pStrExt : string) : string;
  // Add a trailing slash ('\' or '/' based on platform) if there is not already one
  function  tiAddTrailingSlash(   const pDir : string) : string;
  // Remove a trailing slash ('\' or '/' based on platform) if there is one
  function  tiRemoveTrailingSlash(const pStrDir : string) : string;
  // Remove a leading slash ('\' or '/' based on platform) if there is one
  function  tiRemoveLeadingSlash( const pStrDir : string) : string;
  // Get the systems tempory directory
  function  tiGetTempDir   : string;
{$IFDEF MSWINDOWS}
  // Get the windows system directory
  function  tiGetWindowsSysDir : string;
{$ENDIF MSWINDOWS}
  {: Read a file's date with correction for the file's locale}
  function  tiReadFileDate(const AFileName : string): TDateTime;
  {: Read a file's size}
  function  tiReadFileSize(const AFileName : string): DWord;
  {: Read a file's date and size}
  procedure tiReadFileDateSize(const psFileName : string;
                                var   pDateTime  : TDateTime;
                                var   piFileSize : integer);
  // Set a files date and time
  procedure tiSetFileDate(const psFileName : string; const pdtFileDate : TDateTime);
  // What is the path of the current EXE?
  function  tiGetEXEPath : string;
  // Add the EXE path to a file name
  function  tiAddEXEPath(const pFileName: string): string;
  // Extract the file name part of a file name (ie, remove the path and extension)
  function  tiExtractFileNameOnly(      pStrFileName : string) : string;
  // Remove the extension from a filename. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiRemoveExtension(          pStrValue : string) : string;
  // Change a file's extenstion. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiSwapExt(                  const psFileName, psExt : string) : string;
  // Extract a file's extension. Similar to Delphi's ExtractFileExt except the '.' is not extracted.
  function  tiExtractExtension(         pStrFileName : string) : string;
  // Copy a file from pStrFileFrom to pStrFileTo
  procedure tiCopyFile(           const pStrFileFrom, pStrFileTo : string);
  // Move a file from pStrFromFileName to pStrToFileName
  procedure tiMoveFile(                 pStrFromFileName, pStrToFileName: string);
  // Read a file's size in bytes
  function  tiGetFileSize(              pStrFileName : string) : longInt;
  // Remove the drive letter from a file name
  function  tiRemoveDrive(              pStrPath : string) : string;
  // Removed part of a directory
  function  tiRemoveDirectory(          const pFileName: string; const pToRemove: string): string;
  // Set a file's readonly attribute
  procedure tiSetFileReadOnly(          pStrFileTo : string; pBoolReadOnly : boolean);
  // Is a file's readonly attribute set?
  function  tiIsFileReadOnly(pStrFileTo : string) : boolean;
  // Copy all the directories, starting at pStrStartDir to the stringList slTree
  procedure tiDirectoryTreeToStringList(const psStartDir : string; const pslDirList : TStringList; pbRecurse : boolean );
  // Copy all the files, from the directory pStrStartDir, matching the wildcard pStrWildCard
  // to the stringList slResult
  procedure tiFilesToStringList(        const psStartDir,
                                         psWildCard : string;
                                         slResult : TStringList;
                                         const pbRecurse : boolean);
  // Copy one directory tree to another
  procedure tiXCopy(const pSource, pTarget: string);

  {: Delete all the files that match AWildCard found in ADirectory}
  procedure tiDeleteFiles(const ADirectory, AWildCard: string);

  // Does a directory have any subdirectories?
  function  tiHasSubDirectory(pStrStartDir : string) : boolean;
  // Write the string in psText to a file named psFileName
  procedure tiStringToFile(const psText, psFileName : string);
  // Read a text file into a string
  function  tiFileToString(const pFileName : TFileName) : string;
  // Get the current directory
  // Extract a directory name to a certain level.
  // eg tiExtractDirToLevel('c:\temp\dir', 0) gives 'c:'
  //    tiExtractDirToLevel('c:\temp\dir', 1) gives 'c:\temp'
  //    tiExtractDirToLevel('c:\temp\dir', 2) gives 'c:\temp\dir'
  function tiExtractDirToLevel(const psFileName : TFileName; piLevel : byte) : TFileName;
  // Same as Delphi's ForceDirectory, but will raise an exception if create fails
  procedure tiForceDirectories(const pDirName : TFileName);
  // Remove a directory, and all it's owned directories and files
  function tiForceRemoveDir(const pDirName : TFileName) : boolean;
  // Join two path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const pLeftSide, pRightSide: string): string; overload;
  // Join multiple path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const pElements: array of string): string; overload;
  // Fixes the path separator for the *Unix platform. See implementation for more details.
  function tiFixPathDelim(const pText: string): string;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Number manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Type TtiDecimalRoundingCtrl =    {The defined rounding methods}
     (drNone,    {No rounding.}
      drHalfEven,{Round to nearest or to even whole number. (a.k.a Bankers) }
      drHalfPos, {Round to nearest or toward positive.}
      drHalfNeg, {Round to nearest or toward negative.}
      drHalfDown,{Round to nearest or toward zero.}
      drHalfUp,  {Round to nearest or away from zero.}
      drRndNeg,  {Round toward negative.                    (a.k.a. Floor) }
      drRndPos,  {Round toward positive.                    (a.k.a. Ceil) }
      drRndDown, {Round toward zero.                        (a.k.a. Trunc) }
      drRndUp);  {Round away from zero.}

  // Set the precision of pValue to pPrecision decimal places
  function  tiSetPrecision(const AValue : Extended; const APrecision : integer = 3) : real;
  { The following functions have a two times "epsilon" error built in for the
    single, double, and extended argument respectively: }
  Function tiDecimalRoundSgl(AValue: single;   ANDFD: Integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundDbl(AValue: double;   ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundExt(AValue: extended; ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;

  // Calculate pNum div pDenom, and return 0 if an error
  function  tiSafeDiv(pNum, pDenom: longInt): longInt; overload;
  // Calcuate pNum / pDenom and return 0 if an error
  function  tiSafeDiv(pNum, pDenom: Extended): Extended; overload;
  // Round a float to an integer
  function  tiRound(const AValue : Extended) : Int64;
  // Convert an interger (word) to a base26 string (A..Z) characters only
  function  tiEncodeWordBase26(const pNum : Word) : String;
  // Convert a base26 string (A..Z characters only) to an integer (word)
  function  tiDecodeWordBase26(numstr : String) : Word;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TDateTime manipulation
  // * Also see the VCL unit DateUtils
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // What is the date of the previous week day?
  function tiDateToPreviousWeekDayDate(pDTValue : TDateTime) : TDateTime;
  // Get the current year as an integer
  function tiYear(pDate : TDateTime = 0.0) : Word;
  // Is a date within a given range?
  function tiDateWithinRange(const pDate, pFrom, pTo: TDateTime): Boolean;
  {: Convert a year to the date of the beginning of the financial year.
     2005 will return 01/07/2004}
  function tiYearToStartAusFinancialYear(AFinancialYear: Word): TDateTime;
  {: Convert a year to the date of the end of the financial year
     2005 will return 30/06/2005}
  function tiYearToEndAusFinancialYear(AFinancialYear: Word): TDateTime;
  {: Converts a year to a financial year string
     tiYearToEndAusFinancialYear(2005) will return 2004 - 2005}
  function tiAusFinancialYearToString(AFinancialYear: Word): string;
  {: Converts a date to its financial year
     tiDateToAusFinancialYear(EncodeDate(2005/03/01)) will return 2005}
  function tiDateToAusFinancialYear(ADate: TDateTime): Word;

  function tiDateTimeAsXMLString(const pDateTime: TDateTime): string;
  function tiXMLStringToDateTime(const pValue : string) : TDateTime;
  
  function tiDateTimeAsIntlDateStor(const pDateTime: TDateTime): string;
  function tiDateTimeAsIntlDateDisp(const pDateTime: TDateTime): string;
  function tiIntlDateStorAsDateTime(const pValue: string): TDateTime;
  function tiIntlDateDispAsDateTime(const pValue: string): TDateTime;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Type conversions
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Convert a string to an integer using StrToInt, default to 0 if an exception is raised
  function  tiStrToInt(          const pStrValue  : string)    : integer;
  // Convert a string to a float using StrToFloat, default to 0 if an exception is raised
  function  tiStrToFloat(        const pStrValue  : string)    : Extended;
  // Convert a date to a string using FormatDateTime with the standard date format
  function  tiDateToStr(         const pDTValue   : TDateTime; const psFormat : string = csWinDateFormat) : string;
  // Convert a dateTime to a string using FormatDateTime with the standard dateTime format
  function  tiDateTimeToStr(     const pDTValue   : TDateTime) : string;
  // Convert a time to a string using FormatDateTime with the standard time format
  function  tiTimeToStr(         const pDTValue   : TDateTime;
                                  const psTimeFormat : string = '') : string;
  // Convert an integer to a string and return '' if 0
  function  tiIntToStrHide0(     const pIntValue  : longInt)   : string;
  // Converts an integer to a string with commas between thousands
  function  tiIntToCommaStr(     const piValue    : integer) : string;
  // Convert a float to a currency string and return '' if 0
  function  tiFloatToCurrencyHide0(const pRealValue : Extended)      : string;
  // Convert a float to a currency string
  function  tiFloatToCurrency(     const pRealValue : Extended)      : string;

  // ToDo: Tidy these up. String may be T/F, Y/N, True/False, TRUE/FALSE, 1/0, etc.
  // Convert a boolean to 'True' or 'False'
  function  tiBooleanToStr(      const pBoolValue : boolean)   : string;
  // Convert a boolean to 'True' or 'False'
  function  tiBoolToStr(         const pBoolValue : boolean)   : string;
  // Convert a string to a boolean
  function  tiStrToBool(const psValue : string) : boolean;

  // Convert a float to a string in the format 9999.99
  function  tiFloatToStr(        const pRealValue : Extended;
                                  const pIntPrecision : integer = 3) : string;
  // Convert a float to a string in the format 9,999.99
  function  tiFloatToCommaStr(   const pRealValue : Extended;
                                  const pIntPrecision : integer = 3) : string;
  // Convert a float to a string in the format passed by psFloatFormat
  function  _tiFloatToStr(       const pRealValue : Extended;
                                  const pIntPrecision : integer;
                                  const psFloatFormat : string) : string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Win32 API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF MSWINDOWS}
  // Call Windows ShellExecute with exception handling
  function _tiShellExecute(pHwnd : integer;
                            ppcOperation, ppcFile, ppcParameters, ppcDirectory : PChar;
                            pIntShowCmd : integer) : integer;
  // Simple pascal call to ShellExecute
  function tiShellExecute(const psEXE : string;
                           const psParameters : string = '';
                           const piWinState : integer = SW_SHOWNORMAL) : integer;
{$ENDIF MSWINDOWS}
  // Run an EXE and wait for it to finish
  procedure tiRunEXEAndWait(pStrEXE : string);

  // Edit a file using the standard editor for this file type
  function  tiEditFile(const pStrFileName : string) : integer;
  // Get the currently logged on user ID
  function  tiGetUserName : string;
  // Get the computer name
  function  tiGetComputerName : string;

  // Bit manipulation
  // Is a particular bit set?
  function tiIsBitSet(const pVal: longint; const pBit: byte) : boolean;
  // Convert a particular bit to a string: '1' or '0'
  function tiBitToString(const pVal: longint; const pBit: byte) : string;
  // Convert a Int32 to a string of 0s and 1s
  function tiInt32ToBinString(const val : longInt) : string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Mouse cursor routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  function tiAutoWaitCursor: IUnknown;
  function tiAutoCursor(ACursor: TCursor = crHourglass): IUnknown;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Graphics routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  {: This class prevents control flicker using a brute-force method.
     The passed <i>AControlToMask</i> is hidden behind a topmost screen snapshot of the control.
     The snapshot is removed when the instance is freed. }
{$IFDEF MSWINDOWS}
type
  TtiBruteForceNoFlicker = class(TCustomControl)
  private
    FMaskControl: TControl;
    FControlSnapshot: TBitmap;

    procedure ScreenShot(bm: TBitmap; pLeft, pTop, pWidth, pHeight: Integer; Window: HWND);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AControlToMask: TControl); reintroduce;
    destructor Destroy; override;
  end;
{$ENDIF MSWINDOWS}

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Other routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  {: Compare two float values for equality, using a given number of significant digits.
     Ref: http://www.adtmag.com/joop/crarticle.asp?ID=396
     @param( FEPS Determines the significant digits. @br
                 FEPS value of 0.00001 means 5 significant digits. @br
                 FEPS value of 0.01 means 2 significant digits.) }
  function tiIsNearEnough(N, D: Double;  FEPS: Double = 0.00001): Boolean;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue Value to return if ACondition is true.
     @param AResultFalse Value to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Real): Real; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue Value to return if ACondition is true.
     @param AResultFalse Value to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Integer): Integer; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue Value to return if ACondition is true.
     @param AResultFalse Value to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: String): String; overload;

  // Convert a variant array of variants to a string
  function  tiVariantArrayToString(pValue : oleVariant) : string;
  // Is a variant of a given type
  function  tiIsVariantOfType(pVariant : Variant; pVarType : TVarType) : boolean;
  // Return a string with pCount #10 characters
  function  Lf(const pCount : Byte = 1) : string;
  // Return a string with pCount #13 characters
  function  Cr(const pCount : Byte = 1) : string;
  // Return a string with pCount #13+#10 characters
  function  CrLf(const pCount : Byte = 1) : string;
  // Returns a string with pCount #9 characters
  function  Tab(const pCount : Byte = 1) : string;
  // Returns the checksum of a string of numbers
  function tiCheckSum(const Value: string): Integer;
  {: Write a string into a stream overwriting all contents}
  procedure tiStringToStream(const pStr : string; const pStream : TStream);
  {: Append a string to a stream}
  procedure tiAppendStringToStream(const pStr : string; const pStream : TStream);
  {: Insert a string to a stream}
  procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
  {: Read the entire contents of a stream as a string}
  function  tiStreamToString(const pStream : TStream) : string; overload;
  {: Read part of the contents of a stream as a string}
  function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string; overload;
  // Write a string into a stream
  procedure tiFileToStream(const pFileName : string; const pStream : TStream);
  // Read the contents of a stream as a string
  function  tiStreamToFile(const pFileName : string; const pStream : TStream) : string;
  // Copy one stream to another
  procedure tiCopyStream(const pStreamFrom, pStreamTo : TStream);

  {:Copy a TtiObjectList of TtiObject(s) data to a TStream using CSV format}
  procedure tiListToStream(AStream: TStream;
                           AList: TtiObjectList;
                           AFieldDelim: string;
                           ARowDelim: string;
                           AColsSelected: TStringList); overload;

  procedure tiListToStream(AStream : TStream;
                           AList : TtiObjectList); overload;

  // Copy a TList of TtiBaseObject's to a CSV file (Prompt the user for the file name)
  procedure tiListToCSV(AList: TtiObjectList;
                         const AFileName: string;
                         AColsSelected: TStringList); overload;

  procedure tiListToCSV(AList: TtiObjectList;
                         const AFileName: string); overload;

  // Copy a TList of TtiBaseObject's to the clipboard
  procedure tiListToClipboard(AList: TtiObjectList;
                              AColsSelected: TStringList); overload;

  procedure tiListToClipboard(AList: TtiObjectList); overload;

  // Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean; overload;
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream):boolean; overload;

  function tiHasRTTI(pObject : TObject) : boolean; overload;
  function tiHasRTTI(pClass : TClass) : boolean; overload;

  {: Writes "Press <Enter> to continue and waits for CrLf in a console application.}
  procedure tiConsoleAppPause;
  
  {: Platform neutral function to return a GUID string. }
  function  tiCreateGUIDString: string;
  {: Platform neutral function to return the systems TickCount. }
  function  tiGetTickCount: Cardinal;

type

  TtiIntegerListItem = class(TtiBaseObject)
  private
    FValue: Int64;
  public
    property Value: Int64 Read FValue Write FValue;
  end;

  TtiIntegerList = class(TtiBaseObject)
  private
    FList: TObjectList;
    function    GetItems(i: Integer): Int64;
    procedure   SetItems(i: Integer; const Value: Int64);
    function    GetCount: Integer;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Add(AValue: Int64);
    function    IndexOf(AValue: Int64): Integer;
    function    Remove(AValue: Int64): Integer;
    property    Items[i: Integer]: Int64 Read GetItems Write SetItems;
    property    Count: Integer Read GetCount;
  end;

//  // These functions where added in Delphi 6, and found their way into the framework
//  // Added here for D5 support
//  {$ifndef Delphi6OrAbove}
//     function IsNan(const AValue: Extended): Boolean;
//     function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
//     function CompareValue(const A, B: Int64): Integer; overload;
//     function StrToFloatDef(const S: string; const Default: Extended): Extended;
//  {$endif}

implementation
uses
   tiRegINI
  ,tiExcept
  ,ClipBrd
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI5}
    ,FileCtrl
    {$ENDIF}
  ,tiWin32
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ,unix
  ,baseunix
  ,tiLinux
  {$ENDIF UNIX}
  {$IFDEF FPC}
  ,Process
  ,StrUtils   // used for DelSpace1
  {$ENDIF}
  ;

//{$ifndef Delphi6OrAbove}
//const
//  FuzzFactor = 1000;
//  ExtendedResolution = 1E-19 * FuzzFactor;
//
//function IsNan(const AValue: Extended): Boolean;
//type
//  TExtented = packed record
//    Mantissa: Int64;
//    Exponent: Word;
//  end;
//  PExtended = ^TExtented;
//begin
//  Result := ((PExtended(@AValue)^.Exponent and $7FFF)  = $7FFF) and
//            ((PExtended(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0);
//end;
//
//function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
//begin
//  if Epsilon = 0 then
//    Epsilon := Max(Min(Abs(A), Abs(B)) * ExtendedResolution, ExtendedResolution);
//  if A > B then
//    Result := (A - B) <= Epsilon
//  else
//    Result := (B - A) <= Epsilon;
//end;
//
//function CompareValue(const A, B: Int64): Integer;
//begin
//  if A = B then
//    Result := 0
//  else if A < B then
//    Result := -1
//  else
//    Result := 1;
//end;
//
//function StrToFloatDef(const S: string; const Default: Extended): Extended;
//begin
//  if not TextToFloat(PChar(S), Result, fvExtended) then
//    Result := Default;
//end;
//
//{$endif}

type
  TAutoCursor = class(TInterfacedObject)
  private
  public
    constructor Create(ANewCursor: TCursor);
    destructor Destroy; override;
  end;


var
  uCursorStack: TList;


function tiGetTempFile(const pStrExt : string) : string;
{$IFNDEF FPC}
const
  cMaxPathLen = 255;
var
  pcTemp : array[0..cMaxPathLen] of char;
  pcApp  : array[0..cMaxPathLen] of char;
  pcPath : array[0..cMaxPathLen] of char;
{$ENDIF FPC}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetTempFileName('', '');    // prefix of TMP is default
  if pStrExt = '' then
    Result := tiRemoveExtension(Result)
  else
    Result := Copy(Result, 1, Pos('.', Result)) + pStrExt;
  {$ELSE}
  strPCopy(pcApp, copy(extractFileName(application.exeName), 1, 3));
  getTempPath(cMaxPathLen, pcPath);
  getTempFileName(pcPath, pcApp, 0, pcTemp);
  deleteFile(pcTemp); // This is using the Window deleteFile, not Delphi's
  result := strPas(pcTemp);
  if pos('.', result) <> 0 then begin
    if pStrExt = '' then begin
      result := tiRemoveExtension(result);
    end else begin
      result := copy(result, 1, pos('.', result)) + pStrExt ;
    end;
  end;
  {$ENDIF}
end;


function tiGetTempDir : string;
begin
  {$IFDEF FPC}
  result := GetTempDir;
  {$ELSE}
  result := ExtractFilePath(tiGetTempFile('tmp'));
  {$ENDIF}
end;


{$IFDEF MSWINDOWS}
function tiGetWindowsSysDir : string;
const
  cMaxPathLen = 255;
var
  pcDir : array[0..cMaxPathLen] of char;
begin
  GetSystemDirectory(pcDir, cMaxPathLen);
  result := String(pcDir);
end;
{$ENDIF MSWINDOWS}


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case sensitive.
function tiStrTran(pStrValue, pStrDel, pStrIns : string) : string;
var
  i : integer;
  sToChange : string;
begin
  result := '';
  sToChange := pStrValue;
  i := pos(pStrDel, sToChange);
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + pStrIns;
    delete(sToChange, 1, i+length(pStrDel)-1);
    i := pos(pStrDel, sToChange);
  end;
  result := result + sToChange;
end;


function tiStrTran1(aStrValue, aStrDel, aStrIns : string) : string;
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
end;


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case in-sensitive.
function tiCIStrTran(pStrValue, pStrDel, pStrIns : string) : string;
var
  i: integer;
  sToChange: string;
begin
  Result := '';
  sToChange := pStrValue;
  i := pos(upperCase(pStrDel), upperCase(sToChange));
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + pStrIns;
    delete(sToChange, 1, i+length(pStrDel)-1);
    i := pos(upperCase(pStrDel), upperCase(sToChange));
  end;
  Result := Result + sToChange;
end;


function tiNumToken(const pStrValue, pStrToken : string) : integer;
var
  i, iCount : integer;
  lsValue : string;
begin
  Result := 0;
  if pStrValue = '' then
    Exit; //==>

  iCount := 0;
  lsValue := pStrValue;
  i := pos(pStrToken, lsValue);
  while i <> 0 do begin
    delete(lsValue, i, length(pStrToken));
    inc(iCount);
    i := pos(pStrToken, lsValue);
  end;
  Result := iCount + 1;
end;


function tiToken(const pStrValue, pStrToken : string; const pIntNum : integer) : string;
var
  i, iCount, iNumToken : integer;
  lsValue : string;
begin
  result := '';

  iNumToken := tiNumToken(pStrValue, pStrToken);
  if pIntNum = 1 then begin
    if pos(pStrToken, pStrValue) = 0 then result := pStrValue
    else result := copy(pStrValue, 1, pos(pStrToken, pStrValue)-1);
    end
  else if (iNumToken < pIntNum-1) or (pIntNum<1) then begin
    result := '';
    end
  else begin

    { Remove leading blocks }
    iCount := 1;
    lsValue := pStrValue;
    i := pos(pStrToken, lsValue);
    while (i<>0) and (iCount<pIntNum) do begin
      delete(lsValue, 1, i + length(pStrToken) - 1);
      inc(iCount);
      i := pos(pStrToken, lsValue);
    end;

    if (i=0) and (iCount=pIntNum) then result := lsValue
    else if (i=0) and (iCount<>pIntNum) then
      result := ''
    else
      result := copy(lsValue, 1, i-1);
  end;
end;


function tiSpace(pIntLen : integer) : string;
var i : integer;
    sString : string;
begin
  sString := '';
  for i := 1 to pIntLen do
    sString := sString + ' ';
  result := sString;
end;


function tiPadR(const psValue : string; piLen : integer) : string;
var
  ls : string;
begin
  ls := psValue;
  if length(ls) < piLen then begin
    while length(ls) < piLen do begin
      ls := ls + ' ';
    end;
  end
  else if length(ls) > piLen then
    ls := copy(ls, 1, piLen);
  result := ls;
end;


function tiPadL(pStrValue : string; pIntLen : integer) : string;
begin
  if length(pStrValue) < pIntLen then begin
    while length(pStrValue) < pIntLen do begin
      pStrValue := ' ' + pStrValue;
    end;
  end
  else if length(pStrValue) > pIntLen then
    pStrValue := copy(pStrValue, length(pStrValue)-pIntLen+1, pIntLen);
  result := pStrValue;
end;


function  tiPadC(pStrValue : string; pIntLen : integer) : string;
var
  liPad : integer;
begin
  if Length(pStrValue) = pIntLen then
  begin
    result := pStrValue;
    Exit; //==>
  end;
  
  if Length(pStrValue) + 1 = pIntLen then
  begin
    result := pStrValue + ' ';
    Exit; //==>
  end;

  if Length(pStrValue) > pIntLen then
    raise exception.Create('Can not call tiPadC when the string to be ' +
                            'padded is longer than the target length');

  liPad := (pIntLen - length(pStrValue)) div 2;
  if liPad > 0 then
    result := tiSpace(liPad) + pStrValue + tiSpace(liPad);

  // To handle situations where pIntLen < length(pStrValue) and
  // when length(pStrValue) is an odd number
  result := tiPadR(result, pIntLen);
end;


function tiPad0(pStrValue : string; pIntLen : integer) : string;
begin
  if length(pStrValue) < pIntLen then begin
    while length(pStrValue) < pIntLen do begin
      pStrValue := '0' + pStrValue;
    end;
  end
  else if length(pStrValue) > pIntLen then begin
    pStrValue := copy(pStrValue, length(pStrValue)-pIntLen, pIntLen);
  end;
  result := pStrValue;
end;


function tiRemoveLeading0(pStrValue : string) : string;
var i : integer;
begin
  for i := 1 to length(pStrValue) do begin
    if copy(pStrValue, 1, 1) = '0' then begin
      pStrValue := copy(pStrValue, 2, length(pStrValue) - 1);
    end else begin
      break;
    end;
  end;
  result := pStrValue;
end;


function  tiYear(pDate : TDateTime = 0.0) : Word;
var
  lD, lM : Word;
  lDate : TDateTime;
begin
  if pDate = 0.0 then
    lDate := Date
  else
    lDate := pDate;
  DecodeDate(lDate, Result, lM, lD);
end;


function tiDateWithinRange(const pDate, pFrom, pTo: TDateTime): Boolean;
var
  lDate: TDateTime;
begin
  lDate := Trunc(pDate);
  Result := (lDate >= Trunc(pFrom)) and (lDate <= Trunc(pTo));
end;


function tiYearToStartAusFinancialYear(AFinancialYear: Word): TDateTime;
begin
  Result := EncodeDate(AFinancialYear - 1, 07, 1);
end;


function tiYearToEndAusFinancialYear(AFinancialYear: Word): TDateTime;
begin
  Result := EncodeDate(AFinancialYear, 06, 30);
end;


function tiAusFinancialYearToString(AFinancialYear: Word): string;
begin
  Result := IntToStr(AFinancialYear-1) + '-' + IntToStr(AFinancialYear);
end;


function tiDateToAusFinancialYear(ADate: TDateTime): Word;
var
  lY, lM, lD: Word;
begin
  DecodeDate(ADate, lY, lM, lD);
  if lM <= 6 then
    Result := lY
  else
    Result := lY + 1;
end;


function tiMixedCase(pStrValue : string) : string;
var iToken : integer;
    i : integer;
    sBuffer : string;
begin
  iToken := tiNumToken(pStrValue, ' ');
  result := '';
  pStrValue := lowerCase(pStrValue);
  for i := 1 to iToken do begin
    sBuffer := tiToken(pStrValue, ' ', i);
    result := tiAddTrailingValue(result, ' ', true);
    result  := result +
               upperCase(copy(sBuffer, 1, 1)) +
               copy(sBuffer, 2, length(sBuffer) - 1);
  end;
end;


{$IFDEF MSWINDOWS}
function tiShellExecute(const psEXE : string;
                         const psParameters : string = '';
                         const piWinState : integer = SW_SHOWNORMAL) : integer;
var
  lFileName   : array[0..255] of char;
  lParameters : array[0..255] of char;
  lHandle : THandle;
begin;

  strPCopy(lFileName,   psEXE);
  strPCopy(lParameters, psParameters);

  // Screen.ActiveForm.Handle is not thread safe
  //lHandle := screen.activeForm.handle;
  lHandle := 0;
  result := _tiShellExecute(lHandle,
                             nil,
                             lFileName,
                             lParameters,
                             nil,
                             piWinState);
end;
{$ENDIF MSWINDOWS}


{$IFDEF MSWINDOWS}
function _tiShellExecute(pHwnd : integer;
                         ppcOperation, ppcFile, ppcParameters, ppcDirectory : PChar;
                         pIntShowCmd : integer) : integer;
var sMessage     : string;
begin

  result := ShellExecute(pHWnd,
                          ppcOperation,
                          ppcFile,
                          ppcParameters,
                          ppcDirectory,
                          pIntShowCmd);

  { These error messages were pasted from the WINAPI help on shellExecute() }
  case result of
    0  : sMessage := ('System was out of memory, executable file was corrupt, or ' +
                       'relocations were invalid.');
    2  : sMessage := ('File was not found.');
    3  : sMessage := ('Path was not found.');
    5  : sMessage := ('Attempt was made to dynamically link to a task, or there ' +
                       'was a sharing or network-protection error.');
    6  : sMessage := ('Library required separate data segments for each task.');
    8  : sMessage := ('There was insufficient memory to start the application.');
    10 : sMessage := ('Windows version was incorrect.');
    11 : sMessage := ('Executable file was invalid. Either it was not a Windows ' +
                       'application or there was an error in the .EXE image.');
    12 : sMessage := ('Application was designed for a different operating system.');
    13 : sMessage := ('Application was designed for MS-DOS 4.0.');
    14 : sMessage := ('Type of executable file was unknown.');
    15 : sMessage := ('Attempt was made to load a real-mode application (developed ' +
                       'for an earlier version of Windows).');
    16 : sMessage := ('Attempt was made to load a second instance of an executable ' +
                       'file containing multiple data segments that were not marked ' +
                       'read-only.');
    19 : sMessage := ('Attempt was made to load a compressed executable file. The ' +
                       'file must be decompressed before it can be loaded.');
    20 : sMessage := ('Dynamic-link library (DLL) file was invalid. One of the ' +
                       'DLLs required to run this application was corrupt.');
    21 : sMessage := ('Application requires Windows 32-bit extensions.');
    else
      sMessage := '';
      { ShellExe ran OK, do nothing. }
    end;

    if sMessage <> '' then
      raise Exception.Create('Error executing external application.' + Cr +
                              'Error: ' + sMessage);
end;
{$ENDIF MSWINDOWS}


function tiEditFile(const pStrFileName : string) : integer;
begin
  // screen.activeForm.handle,
  {$IFDEF MSWINDOWS}
  result := ShellExecute(Application.MainForm.Handle,
                          nil,
                          PChar(pStrFileName),
                          nil,
                          nil,
                          SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  { TODO: There is no standard editor included with all
    flavours of linux. Might implement tiEditFile as a form with
    a memo control and basic edit functions. ie: Something like NotePad for Windows }
  Result := 0;    // To get rid of the compiler warning, until I implement this.
  {$ENDIF LINUX}
end;


function tiExtractFileNameOnly(pStrFileName : string) : string;
begin
  result := tiRemoveExtension(extractFileName(pStrFileName));
end;


function tiRemoveExtension(pStrValue : string) : string;
var i : integer;
begin
  i := tiPosR('.', pStrValue);
  if i <> 0 then begin
    result := copy(pStrValue, 1, i - 1);
  end else begin
    result := pStrValue;
  end;
end;


function  tiSwapExt(const psFileName, psExt : string) : string;
begin
  result := tiAddTrailingValue(tiRemoveExtension(psFileName), '.', false) + psExt;
end;


function tiExtractExtension(pStrFileName : string) : string;
var i : integer;
begin
  i := tiPosR('.', pStrFileName);
  if i <> 0 then begin
    result := copy(pStrFileName, i+1, length(pStrFileName) - i);
  end else begin
    result := '';
  end;
end;


procedure tiCopyFile(const pStrFileFrom, pStrFileTo : string);
var
  iErrorCode : Longword;
  {$IFDEF FPC}
    function fpcCopyFile(Org, Dest:string): boolean;
    var
      Source, Target:TFileStream;
    begin
      Result := false;
      try
        try
          Source := TFileStream.Create(Org, fmShareDenyNone or fmOpenRead);
          try
            Target := TFileStream.Create(Dest, fmOpenWrite or fmCreate);
            Target.CopyFrom(Source, Source.Size);
            Result := true;
          finally
            Target.Free;
          end;
        finally
          Source.Free;
        end;
      except
      end;
    end;
  {$ENDIF}
begin
{$IFNDEF FPC}
  copyFile(pChar(pStrFileFrom), pChar(pStrFileTo), false);
  iErrorCode := getLastError();
{$ELSE}
  if fpcCopyFile(pStrFileFrom, pStrFileTo) then
    iErrorCode := 0
  else
    iErrorCode := GetLastOSError;
{$ENDIF}

  if iErrorCode <> 0 then begin
    raise exception.Create('Unable to copy <' +
                            pStrFileFrom +
                            '> to <' +
                            pStrFileTo + '>' + #13 +
                            'System error code: ' +
                            intToStr(iErrorCode) + #13 +
                            'System error message: ' +
                            sysErrorMessage(iErrorCode));
  end;
end;


procedure tiMoveFile(pStrFromFileName, pStrToFileName: string);
begin
  RenameFile(pStrFromFileName, pStrToFileName);   { Rename the file }
end;


function tiGetFileSize(pStrFileName : string) : longInt;
var f: file of Byte;
begin
  AssignFile(f, pStrFileName);
  Reset(f);
  result := FileSize(f);
  closeFile(f);
end;


function tiReplicate(const pStrValue : string; pRepCount : Word) : string;
var
  pResult, pValue: PChar;
  lenValue: cardinal;
begin
  if (pRepCount = 0) or (Pointer(pStrValue) = nil) then
    exit;

  lenValue  := Length(pStrValue);
  SetLength(Result, lenValue * pRepCount);
  pResult   := Pointer(Result);
  pValue    := Pointer(pStrValue);

  while pRepCount <> 0 do
  begin
    Move(pValue^, pResult^, lenValue);
    Inc(pResult, lenValue);
    Dec(pRepCount);
  end;
end;


{ Check that the string contained in a TControl can be converted
  to a valid float. Pass the control as a parameter and the
  function will perform the test and retrun focus to the control
  if the test fails.

  Currently programmed to test mastEdits, some more work is necessary
  to test other controls. }
{
function tiIsControlValidFloat(pControl : TControl) : boolean;
var sString : string;
begin

  if (pControl is TMaskEdit) then begin
    sString := TMaskEdit(pControl).text;
  end else begin
    tiAppError('Invalid control type passed to uIsControlValidFloar()');
  end;

  try
    result := true;
  except
    tiAppError('<' + sString + '> is not a valid number.');
    TMaskEdit(pControl).setFocus;
    result := false;
  end;

end;
}


function _RemoveNonNumChars(pValue : string) : string;
begin
  result := pValue;
  result := tiStrTran(result, ' ', '');
  result := tiStrTran(result, ',', '');
  result := tiStrTran(result, '$', '');
end;


function tiStrToInt(const pStrValue : string) : integer;
begin
  try
    result := strToInt(_RemoveNonNumChars(pStrValue));
  except
    result := 0;
  end;
end;


function tiStrToFloat(const pStrValue : string) : Extended;
begin
  try
    result := strToFloat(_RemoveNonNumChars(pStrValue));
  except
    result := 0;
  end;
end;


function tiDateToStr(const pdtValue : TDateTime; const psFormat : string = csWinDateFormat) : string;
begin
  result := FormatDateTime(psFormat, pdtValue);
end;


function tiDateTimeToStr(const pDTValue : TDateTime) : string;
begin
  result := FormatDateTime(csWinDateTimeFormat, pDTValue);
end;


function  tiTimeToStr(const pDTValue   : TDateTime;
                       const psTimeFormat : string = '') : string;
begin
  if psTimeFormat = '' then
    result := FormatDateTime(csWinTimeFormat, pDTValue)
  else
    result := FormatDateTime(psTimeFormat, pDTValue)
end;


function tiSafeDiv(pNum, pDenom: longInt): longInt;
begin
  if pDenom <> 0 then 
    result := pNum div pDenom
  else
    result := 0;
end;


function tiIntToStrHide0(const pIntValue : longInt) : string;
begin
  if pIntValue = 0 then begin
    result := '';
  end else begin
    result := intToStr(pIntValue);
  end;
end;


function  tiIntToCommaStr(const piValue : integer) : string;
begin
  result := tiFloatToCommaStr(piValue, 0);
end;


function tiFloatToCurrencyHide0(const pRealValue: Extended): string;
begin
  if (pRealValue < 0.005) and (pRealValue > -0.005) then
    result := ''
  else
    result := tiFloatToCurrency(pRealValue);
end;


function tiFloatToCurrency(const pRealValue: Extended): string;
begin
  try
    result := FormatFloat('$ #,##0.00', pRealValue);
  except
    result := '0.00';
  end;
end;


function tiBooleanToStr(const pBoolValue : boolean) : string;
begin
  if pBoolValue then
    result := cTrueDB
  else
    result := cFalseDB;
end;


function tiBoolToStr(const pBoolValue : boolean) : string;
begin
  result := tiBooleanToStr(pBoolValue);
end;


function  tiStrToBool(const psValue : string) : boolean;
var
  ls : string;
begin
  ls := upperCase(psValue);
  if (ls = 'TRUE') or
     (ls = 'T'   ) or
     (ls = 'YES' ) or
     (ls = 'Y'   ) or
     (ls = '1'   ) then
    result := true
  else
    result := false;
end;


function tiIsNearEnough(N, D: Double;  FEPS: Double): Boolean;
begin
  if D = 0.0 then
    Result := (N > -feps) and (N < +feps)
  else if N = 0.0 then
    Result := (D > -feps) and (D < +feps)
  else
    Result := (((1-feps) < (N/D)) and ((N/D) < (1+feps)));
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Real): Real;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Integer): Integer;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: String): String;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function  tiVariantArrayToString(pValue : oleVariant) : string;
  procedure appendVariantToStringList(pStringList : TStringList;
                                       pVariant : oleVariant;
                                       var pIndent : integer);
  var i : integer;
      iLow : integer;
      iHigh : integer;
  begin
    if tiIsVariantOfType(pVariant, varArray) then begin
      iLow  := varArrayLowBound( pVariant, 1);
      iHigh := varArrayHighBound(pVariant, 1);
      for i := iLow to iHigh do begin
        inc(pIndent);
        if i = iLow then pStringList.add(tiSpace(pIndent*3) + '[');
        appendVariantToStringList(pStringList, pVariant[i], pIndent);
        if i = iHigh then pStringList.add(tiSpace(pIndent*3) + ']');
        dec(pIndent);
      end;
    end else begin
      pStringList.add(tiSpace(pIndent*3 + 1) + varToStr(pVariant));
    end;
  end;

var
  lStringList : TStringList;
  pIndent : integer;
begin
  lStringList := TStringList.Create;
  try
    pIndent := -1;
    appendVariantToStringList(lStringList, pValue, pIndent);
    result := lStringList.Text;
  finally
    lStringList.free;
  end;
end;

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
function tiIsVariantOfType(pVariant : Variant; pVarType : TVarType) : boolean;
var
  xVT : TVarType;
  xVTHigh : TVarType;
//  xVTLow : TVarType;
begin
//  result := (varType(pVariant) and pVarType) = pVarType;
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
end;


function  tiAddTrailingValue(const pLine, pValue : string; pDuplicates : boolean = true) : string;
begin
  if pLine = '' then
  begin
    result := pLine;
    Exit; //==>
  end;

  if pDuplicates then
  begin
    result := pLine + pValue;
    Exit; //==>
  end;

  if (not SameText(Copy(pLine,
                           Length(pLine) - Length(pValue) + 1,
                           Length(pValue)),
                     pValue)) then
    result := pLine + pValue
  else
    result := pLine;

end;


function  tiRemoveTrailingValue(pStrLine, pStrValue : string) : string;
var
  lLHS : integer;
  lRHS : integer;
begin
  lLHS := length(pStrLine) - Length(pStrValue) + 1;
  lRHS := Length(pStrValue);

  if copy(pStrLine, lLHS, lRHS) = pStrValue then
    result :=
      copy(pStrLine, 1, lLHS - 1)
  else
    result := pStrLine;
end;


function tiAddTrailingComma(pStrValue : string) : string;
begin
  result := tiAddTrailingValue(pStrValue, ',', true);
end;


function  tiAddTrailingAnd(pStrValue : string) : string;
begin
  result := tiAddTrailingValue(pStrValue, ' and ', false);
end;


function  tiAddTrailingOr(pStrValue : string) : string;
begin
  result := tiAddTrailingValue(pStrValue, ' or ', false);
end;


function  tiAddTrailingSpace(pStrValue : string) : string;
begin
  result := tiAddTrailingValue(pStrValue, ' ', true);
end;


function tiDateToPreviousWeekDayDate(pdtValue : TDateTime) : TDateTime;
var iDay : integer;
begin
  result := pdtValue;
  iDay   := dayOfWeek(result);
  case iDay of
  1 : result := result - 2; // Sunday
  2 : result := result - 3; // Monday
  3 : result := result - 1; // Tuesday
  4 : result := result - 1; // Wednesday
  5 : result := result - 1; // Thursday
  6 : result := result - 1; // Friday
  7 : result := result - 1; // Saturday
  end;
end;


function tiAddTrailingSlash(const pDir : string) : string;
begin
  result := tiAddTrailingValue(pDir, PathDelim, false);
end;


function tiRemoveTrailingSlash(const pStrDir : string) : string;
begin
  result := tiRemoveTrailingValue(tiFixPathDelim(pStrDir), PathDelim);
end;


function tiRemoveLeadingSlash(const pStrDir: string): string;
var
  lStr: string;
begin
  lStr := tiFixPathDelim(pStrDir);
  if copy(lStr, 1, 1) = PathDelim then begin
    result := copy(lStr, 2, length(lStr) - 1);
  end else begin
    result := lStr;
  end;
end;


function tiPosR(pStrTarget, pStrValue : string) : integer;
var i : integer;
    iTargetLength : integer;
begin
  i := length(pStrValue);
  iTargetLength := length(pStrTarget);
  while i > 0 do begin
    if copy(pStrValue, i, iTargetLength) = pStrTarget then begin
      break; //==>
    end;
    dec(i);
  end;
  result := i;
end;


function tiGetUserName : string;
begin
  {$IFDEF MSWINDOWS}
    Result := tiWin32GetUserName;
  {$ENDIF}
  {$IFDEF LINUX}
    Result := tiLinuxGetUserName;
  {$ENDIF}
end;


function tiGetComputerName : string;
begin
  {$IFDEF MSWINDOWS}
  Result := tiWin32GetComputerName;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := tiLinuxGetComputerName;
  {$ENDIF LINUX}
end;


function tiRemoveDrive(pStrPath : string) : string;
var
  sDrive : string;
begin
  sDrive := extractFileDrive(pStrPath);
  if sDrive <> '' then begin
    result := copy(pStrPath, length(sDrive)+1, length(pStrPath) - length(sDrive));
  end else begin
    result := pStrPath;
  end;
end;


function  tiRemoveDirectory(const pFileName: string; const pToRemove: string): string;
var
  lToRemove: string;
  lFileName: string;
begin
  lToRemove := UpperCase(tiRemoveTrailingSlash(pToRemove));
  lFileName := UpperCase(pFileName);
  if Pos(lToRemove, lFileName) <> 1 then
    raise EtiOPFProgrammerException.CreateFmt(cErrorRemoveDirectory, [pToRemove, lFileName]);
  Result := Copy(pFileName, Length(lToRemove)+1, Length(lFileName));
  while Copy(Result, 1, 1) = PathDelim do
    Result := Copy(Result, 2, Length(Result));
end;


function tiIsBitSet(const pVal: longint; const pBit: byte) : boolean;
begin
  result := (pVal and (1 shl pBit)) <> 0;
end;


function tiBitToString(const pVal: longint; const pBit: byte): string;
begin
  if tiIsBitSet(pVal, pBit) then
    result := '1'
  else
    result := '0';
end;


function tiInt32ToBinString(const val : longInt) : string;
var i : integer;
begin
  result := '';
  for i := 31 downto 0 do begin
    result := result + tiBitToString(val, i );
  end;
end;


{$IFDEF MSWINDOWS}
procedure tiSetFileReadOnly(pStrFileTo : string; pBoolReadOnly : boolean);
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
      lBoolReadOnly : boolean;
begin
  lBoolReadOnly := tiIsFileReadOnly(pStrFileTo);
  if lBoolReadOnly = pBoolReadOnly then exit; //==>

  iCurrentState := tiWin32FileGetAttr(pStrFileTo);
  if pBoolReadOnly then begin
    tiWin32FileSetAttr(pStrFileTo, iCurrentState or cReadOnly);
  end else begin
   tiWin32FileSetAttr(pStrFileTo, iCurrentState xor cReadOnly);
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ Works on Owner rights only. Unix has 3 sets of rights: Owner, Group and Global }
procedure tiSetFileReadOnly(pStrFileTo : string; pBoolReadOnly : boolean);
var
  lAttr: LongInt;
  Info: Stat;
begin
  lAttr := FileGetAttr(pStrFileTo);
  if lAttr <> -1 then
  begin
    if FpStat(pStrFileTo, Info) < 0 then
      raise Exception.Create('Unable to set ReadOnly attribute');

    if pBoolReadOnly then
      FpChmod(pStrFileTo, Info.st_mode xor S_IWUSR)   // remove write access. Owner rights only.
    else
      FpChmod(pStrFileTo, Info.st_mode or S_IWUSR);   // add write access.
  end
  else
  begin
    raise Exception.Create('Unable to set ReadOnly attribute');
  end
end;
{$ENDIF LINUX}


{$IFDEF MSWINDOWS}
function tiIsFileReadOnly(pStrFileTo : string) : boolean;
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
begin
  iCurrentState := tiWin32FileGetAttr(pStrFileTo);
  result := tiIsBitSet(iCurrentState, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function tiIsFileReadOnly(pStrFileTo : string) : boolean;
begin
  Result := FileIsReadOnly(pStrFileTo);
end;
{$ENDIF LINUX}


function tiGetEXEPath : string;
var
  path: array[0..MAX_PATH - 1] of char;
begin
  {$IFDEF MSWINDOWS}
  if IsLibrary then
    SetString(Result, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
  {$ENDIF}
    result := paramStr(0);
  result := tiRemoveTrailingSlash(ExtractFilePath(Result));
end;


function tiAddEXEPath(const pFileName: string): string;
begin
  Result :=
    ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) +
      ExtractFileName(pFileName));
end;


procedure tiDirectoryTreeToStringList(const psStartDir : string; const pslDirList : TStringList; pbRecurse : boolean);
  procedure _ReadDirectories(const psStartDir : string; slTree : TStringList; bRecurse : boolean);
    procedure _AddIfDir(searchRec : TSearchRec; sStartDir : string; slTree : TStringList; bRecurse : boolean);
    begin
        if ((searchRec.attr and faDirectory) > 0) and
           (searchRec.name <> '.') and
           (searchRec.name <> '..') then begin
          slTree.add(sStartDir + searchRec.name);
          if bRecurse then begin
            _ReadDirectories(sStartDir + searchRec.name, slTree, bRecurse);
          end;
        end;
    end;
  var
    lsStartDir : string;
    SearchRec : TSearchRec;
  begin
    lsStartDir := tiAddTrailingSlash(psStartDir);
    try
      if sysUtils.FindFirst(lsStartDir + AllFilesWildCard,
                             faDirectory,
                             SearchRec) = 0 then
      begin
        _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        while sysUtils.findNext(searchRec) = 0 do
        begin
          _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        end;
      end;
    finally
      sysUtils.FindClose(SearchRec);
    end;
  end;
var
  lStartDir : string;
begin

  lStartDir := tiRemoveTrailingSlash(psStartDir);
  pslDirList.Clear;

  if not DirectoryExists(lStartDir) then
    exit;

  pslDirList.Add(lStartDir);
  _ReadDirectories(lStartDir, pslDirList, pbRecurse);

end;


{$IFDEF DELPHI6ORAVOVE} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
procedure tiFilesToStringList(const psStartDir,
                               psWildCard : string;
                               slResult : TStringList;
                               const pbRecurse : boolean);
  // Locally visible proc
  procedure AddFile(searchRec : TSearchRec; sStartDir, pStrWildCard : string; slTree : TStringList; bRecurse : boolean);
  begin
    Assert(not ((searchRec.attr and faDirectory) > 0), 'A directory passed, but a file expected');
    Assert((searchRec.name <> '.' ),                      'A directory passed, but a file expected');
    Assert((searchRec.name <> '..'),                    'A directory passed, but a file expected');
    slTree.add(sStartDir + searchRec.name);
  end;

var
  SearchRec : TSearchRec;
  lsStartDir : string;
  lslDirTree : TStringList;
  i : integer;
begin
  slResult.Clear;
  lsStartDir := tiAddTrailingSlash(psStartDir);
  lslDirTree := TStringList.Create;
  try
    if pbRecurse then
      tiDirectoryTreeToStringList(lsStartDir, lslDirTree, pbRecurse)
    else
      lslDirTree.Add(lsStartDir);
    for i := 0 to lslDirTree.Count-1 do
    begin
      lsStartDir := tiAddTrailingSlash(lslDirTree.Strings[i]);
      {$IFDEF MSWINDOWS}
      try
        if tiWin32FindFirstFile(lsStartDir + psWildCard, SearchRec) = 0 then
        begin
          AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          while sysUtils.findNext(searchRec) = 0 do
          begin
            AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          end;
        end;
      finally
        sysUtils.FindClose(SearchRec);
      end;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      try
        if sysUtils.FindFirst(lsStartDir + psWildCard,
                               faAnyFile-faSysFile-faDirectory,
                               SearchRec) = 0 then begin
          AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          while sysUtils.findNext(searchRec) = 0 do begin
            AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          end;
        end;
      finally
        sysUtils.FindClose(SearchRec);
      end;
      {$ENDIF LINUX}
    end;
  finally
    lslDirTree.Free;
  end;
end;
{$IFDEF DELPHI6ORAVOVE}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}


procedure tiXCopy(const pSource, pTarget: string);
var
  lFiles: TStringList;
  i: Integer;
  lErrors: string;
  lSource: string;
  lTarget: string;
begin
  lFiles := TStringList.Create;
  try
    tiForceDirectories(pTarget);
    tiFilesToStringList(pSource, AllFilesWildCard, lFiles, true);
    for i := 0 to lFiles.Count - 1 do
    begin
      lSource := lFiles.Strings[i];
      lTarget := tiAddTrailingSlash(pTarget) + tiRemoveDirectory(lSource, pSource);
      tiForceDirectories(ExtractFileDir(lTarget));
      try
        tiCopyFile(lSource, lTarget);
      except
        on e:Exception do
        begin
          if lErrors <> '' then lErrors := lErrors + Cr;
          lErrors := lErrors + e.message;
        end;
      end;
    end;
    if lErrors <> '' then
      raise EtiOPFFileSystemException.Create(lErrors);
  finally
    lFiles.Free;
  end;
end;


procedure tiDeleteFiles(const ADirectory, AWildCard: string);
var
  Lsl: TStringList;
  i : Integer;
begin
  Lsl:= TStringList.Create;
  try
    tiFilesToStringList(ADirectory, AWildCard, Lsl, false);
    for i := 0 to Lsl.Count - 1 do
      if not SysUtils.DeleteFile(Lsl.Strings[i]) then
        raise EtiOPFFileSystemException.CreateFmt(cErrorCanNotDeleteFile, [Lsl.Strings[i]]);
  finally
    Lsl.Free;
  end;
end;


function tiHasSubDirectory(pStrStartDir : string) : boolean;
var
  slTree : TStringList;
begin
  slTree := TStringList.Create;
  try
    tiDirectoryTreeToStringList(tiFixPathDelim(pStrStartDir), slTree, false);
    result := slTree.count > 1;
  finally
    slTree.free;
  end;
end;


function tiExtractDirToLevel(const psFileName : TFileName; piLevel : byte) : TFileName;
var
  i : integer;
begin
  result := '';
  for i := 0 to piLevel+1 do
  begin
    if result <> '' then
      result := tiAddTrailingSlash(result);
    result := result + tiToken(psFileName, PathDelim, i);
  end;
  result := tiRemoveTrailingSlash(result);
  {$IFDEF UNIX}
  result := PathDelim + result;
  {$ENDIF}
end;


procedure tiForceDirectories(const pDirName : TFileName);
var
  lDirName: string;
begin
  if Pos('.', pDirName) <> 0 then
    lDirName := ExtractFilePath(ExpandFileName(pDirName))
  else
    lDirName := pDirName;
  if not ForceDirectories(lDirName) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorUnableToCreateDirectory, [lDirName]);
end;


// ToDo: Must add code to handle hidden files.
function tiForceRemoveDir(const pDirName : TFileName) : boolean;
var
  lsl : TStringList;
  i : integer;
begin
  try
    lsl := TStringList.Create;
    try
      tiFilesToStringList(pDirName, AllFilesWildCard, lsl, true);
      for i := 0 to lsl.Count - 1 do
      begin
        if tiIsFileReadOnly(lsl.Strings[i]) then
          tiSetFileReadOnly(lsl.Strings[i], false);
        SysUtils.DeleteFile(lsl.Strings[i]);
      end;
      tiDirectoryTreeToStringList(pDirName, lsl, true);
      for i := lsl.Count - 1 downto 0 do
        SysUtils.RemoveDir(lsl.Strings[i]);
      result := not DirectoryExists(pDirName);
    finally
      lsl.Free;
    end;
  except
    on e:exception do
      result := false;
  end;
end;


function tiJoinPath(const pLeftSide, pRightSide: string): string;
var
  lLeftSide, lRightSide: string;
begin
  lLeftSide   := tiFixPathDelim(pLeftSide);
  lRightSide  := tiFixPathDelim(pRightSide);
  Result := tiRemoveTrailingSlash(tiAddTrailingSlash(lLeftSide) + lRightSide);
end;


function tiJoinPath(const pElements: array of string): string;
var
  I: Integer;
begin
  if Length(pElements) > 0 then
  begin
    Result := tiRemoveTrailingSlash(pElements[0]);
    for I := 1 to High(pElements) do
      Result := tiJoinPath(Result, pElements[I]);
  end
  else
    Result := '';
end;


{ This allows us to always use a \ as a path separator. For Win32 it will
  do nothing, but for *Unix it will replace all \'s with /'s.  Now we don't have
  to have so many IFDEFs in the Unit Tests! }
function tiFixPathDelim(const pText: string): string;
begin
  {$IFDEF MSWINDOWS}
  result := pText;
  {$ENDIF}
  {$IFDEF UNIX}
  result := SetDirSeparators(pText);
  {$ENDIF}
end;


function tiFloatToStr(const pRealValue : Extended;
    const pIntPrecision : integer = 3) : string;
begin
  result := _tiFloatToStr(pRealValue, pIntPrecision, '###0');
end;


function  tiFloatToCommaStr(const pRealValue: Extended;
    const pIntPrecision: integer = 3): string;
begin
  result := _tiFloatToStr(pRealValue, pIntPrecision, '#,##0');
end;


function _tiFloatToStr(const pRealValue: Extended; const pIntPrecision: integer;
    const psFloatFormat: string): string;
var
  lsFormat: string;
begin
  lsFormat := psFloatFormat;
  if pIntPrecision <> 0 then
    lsFormat := lsFormat + '.' + tiReplicate('0', pIntPrecision);
  try
    result := formatFloat(lsFormat, tiDecimalRoundDbl(pRealValue, pIntPrecision));
  except
    on e:exception do
      raise exception.Create('Unable to format floating point number. ' + Cr(2) +
                              'Called in tiFloatToStr() ' +
                              'Format mask <' + lsFormat + '>' + Cr +
                              'Error message: ' + e.message);
  end;
end;


function  tiSafeDiv(pNum, pDenom : Extended) : Extended;
begin
  if pDenom <> 0 then begin
    result := pNum / pDenom;
  end else begin
    result := 0;
  end;
end;


procedure tiRunEXEAndWait(pStrEXE : string);
begin
{$IFDEF MSWINDOWS}
  tiWin32RunEXEAndWait(pStrEXE);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  tiLinuxRunEXEAndWait(pStrEXE);
{$ENDIF}
end;


function  tiRound(const AValue : Extended) : Int64;
begin
  Result := Trunc(AVAlue);
  if AValue - Result >= 0.5 then
    Inc(Result);
end;


// Get the decimal part of a real number
{
Q:  How do I find one string inside another with wildcards?

A:  There are many times when you need to compare two strings, but want to
use wild cards in the match - all last names that begin with 'St', etc.  The
following is a piece of code I got from Sean Stanley in Tallahassee Florida
in C.  I translated it into Delphi an am uploading it here for all to use. I
have not tested it extensivly, but the original function has been tested
quite thoughly. }
function tiWildcardMatch(const psSource, psPattern: String; const pbCaseSensitive: boolean = false): Boolean;

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
    Result := MatchPatternStr(psSource, psPattern)
  else
    Result := MatchPatternStr(UpperCase(psSource), UpperCase(psPattern));
end;


{ TODO : 
This will work:
  tiSubStr('my <d>string</d>', '<d>', '</d>',);
but this will not:
  tiSubStr('my <u>long</e><d>string</e>', '<d>', '</e>',); }
function  tiSubStr(const pSource, pStartDelim, pEndDelim : string; pIndex : integer = 1) : string;
var
  liStart : integer;
  liEnd   : integer;
begin
  Assert(pIndex = 1, 'Under development, pIndex not yet in use');

  result := '';

  liStart := Pos(pStartDelim, pSource);
  if liStart <> 0 then
    liStart := liStart + length(pStartDelim);

  liEnd := Pos(pEndDelim, pSource);
  if liEnd <> 0 then
    liEnd := liEnd - 1;

  if (liStart = 0) or (liEnd = 0) then
    Exit; //==>

  result := Copy(pSource, liStart, liEnd - liStart + 1);
end;


function tiReadFileDate(const AFileName : string): TDateTime;
begin
  Result := FileDateToDateTime(FileAge(AFileName));
end;


function tiReadFileSize(const AFileName : string): DWord;
var
  LFileStream : TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LFileStream.Size;
  finally
    LFileStream.Free;
  end;
end;


procedure tiReadFileDateSize(const psFileName: string; var pDateTime: TDateTime;
    var piFileSize: integer);
begin
  pDateTime   := tiReadFileDate(psFileName);
  piFileSize  := tiReadFileSize(psFileName);
end;


procedure tiSetFileDate(const psFileName : string; const pdtFileDate : TDateTime);
var
  lFileDate   : Integer;
  {$IFDEF MSWINDOWS}
  lFileHandle : Integer;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  lError      : Integer;
  {$ENDIF LINUX}
begin
  lFileDate   := DateTimeToFileDate(pdtFileDate);
  {$IFDEF MSWINDOWS}
  lFileHandle := FileOpen(psFileName, fmOpenWrite or fmShareDenyNone);
  try
    if lFileHandle > 0 then
      FileSetDate(lFileHandle, lFileDate)
    else
      raise exception.Create('Unable to set file date on <' +
                              psFileName);
  finally
    FileClose(lFileHandle);
  end;
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
    lError := FileSetDate(psFileName, lFileDate);
    if lError <> 0 then
      raise Exception.Create('Unable to set file date on <' + psFileName);
  {$ENDIF LINUX}
end;


function  Cr(const pCount : Byte = 1) : string;
begin
  result := tiReplicate(#13, pCount);
end;


function  Lf(const pCount : Byte = 1) : string;
begin
  result := tiReplicate(#10, pCount);
end;


function CrLf(const pCount : Byte = 1) : string;
begin
  result := tiReplicate(#13 + #10, pCount);
end;


function  Tab(const pCount : Byte = 1) : string;
begin
  result := tiReplicate(#9, pCount);
end;


{
procedure tiSortList(pData : TList; pDoCompare : TSortCompare);
  procedure _SortList(pData : TList;
                       pDoCompare : TSortCompare);
  var
    i, j  : integer;
    lTemp : TObject;
    liComp : integer;
  begin
    for i := pData.Count - 1 downto 0 do
      for j := 0 to pData.Count - 2 do begin
        pDoCompare(pData.Items[j], pData.Items[j + 1], liComp);
        if liComp > 0 then begin
          lTemp := pData.Items[j];
          pData.Items[j] := pData.Items[j + 1];
          pData.Items[j + 1] := lTemp;
        end;
      end;
  end;

begin
  _SortList(pData, pDoCompare );
end;
}


procedure tiStringToStream(const pStr : string; const pStream : TStream);
var
  lBuffer : PChar;
  lLen : integer;
begin
  lBuffer := PChar(pStr);
  lLen := length(pStr);
  pStream.Size := 0;
  pStream.write(lBuffer^, lLen);
  pStream.Position := 0;
end;


procedure tiAppendStringToStream(const pStr : string; const pStream : TStream);
var
  lPC : PChar;
begin
  Assert(pStream <> nil, 'Stream unassigned.');
  pStream.Position := pStream.Size;
  lPC := PChar(pStr);
  pStream.WriteBuffer(lPC^, length(pStr));
end;


procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
var
  LRHLength: Longword;
  LRHPChar: PChar;
  lSize: Longword;
begin
  Assert(AStream <> nil, 'Stream unassigned.');
  Assert(APos <= AStream.Size, 'Pos > AStream.Size');
  lSize := AStream.Size;
  Assert(APos <= lSize, 'Pos > AStream.Size');

  // Copy the RH portion to a string
  LRHLength:= AStream.Size - APos;
  AStream.Position:= APos;

  GetMem(LRHPChar, LRHLength);
  try
    AStream.Read(LRHPChar^, LRHLength);

    // Chop the RH portion off
    AStream.Size:= APos+1;
    AStream.Position:= APos;

    // Append the new string + the RH portion
    AStream.WriteBuffer(PChar(AStr)^, Length(AStr));
    AStream.WriteBuffer(LRHPChar^, LRHLength);
  finally
    FreeMem(LRHPChar);
  end;
end;


function  tiStreamToString(const pStream : TStream) : string;
var
  lPos : integer;
begin
  lPos := pStream.Position;
  pStream.Position := 0;
  SetLength(Result,  pStream.Size);
  pStream.Read(Result[1], pStream.Size);
  pStream.Position := lPos;
end;


function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string;
var
  LPos: Longword;
  LEnd: Longword;
  LSize: Longword;
  LEndExt: Extended;
begin
  Assert(AStart<=AEnd, 'AStart>AEnd');

  { AStream.Size could be zero which causes a range error. }
  LEndExt := Min(AEnd, AStream.Size-1);
  if LEndExt < 0 then
    LEnd := 0
  else
    LEnd := trunc(LEndExt);

  LSize := LEnd - AStart + 1;

  if (AStart >= AStream.Size) or
     (LSize = 0) then     // LSize may never be less than 0, it is a LongWord
  begin
    Result:= '';
    Exit; //==>
  end;

  LPos := AStream.Position;
  try
    AStream.Position := AStart;
    SetLength(Result,  LSize);
    AStream.Read(Result[1], LSize);
  finally
    AStream.Position:= LPos;
  end;

end;

procedure tiFileToStream(const pFileName : string; const pStream : TStream);
var
  lStream : TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    lStream.LoadFromFile(pFileName);
    tiCopyStream(lStream, pStream);
  finally
    lStream.Free;
  end;
end;


function  tiStreamToFile(const pFileName : string; const pStream : TStream) : string;
var
  lStream : TMemoryStream;
  lPosition: Integer;
begin
  lPosition := pStream.Position;
  try
    lStream := TMemoryStream.Create;
    try
      pStream.Position := 0;
      lStream.LoadFromStream(pStream);
      lStream.Position := 0;
      lStream.SaveToFile(pFileName);
    finally
      lStream.Free;
    end;
  finally
    pStream.Position := lPosition;
  end;
end;


procedure tiCopyStream(const pStreamFrom, pStreamTo : TStream);
begin
  pStreamFrom.Position := 0;
  pStreamTo.Size := 0;
  pStreamTo.CopyFrom(pStreamFrom, pStreamFrom.Size);
  pStreamTo.Position := 0;
  pStreamFrom.Position := 0;
end;


procedure tiListToStream(AStream : TStream;
                         AList : TtiObjectList;
                         AFieldDelim : string;
                         ARowDelim: string;
                         AColsSelected : TStringList);
var
  i, j       : integer;
  lsValue    : string;
  lFieldName : string;
  pData      : TtiBaseObject;
  lLine      : string;
  lPropType: TTypeKind;
begin
  // Write column headings
  for i := 0 to AColsSelected.Count - 1 do begin
    tiAppendStringToStream(AColsSelected.Strings[i], AStream);
    if i < AColsSelected.Count - 1 then
      tiAppendStringToStream(AFieldDelim, AStream)
    else
      tiAppendStringToStream(ARowDelim, AStream);
  end;

  // Write the data
  for i := 0 to AList.Count - 1 do
  begin
    pData := (TObject(AList.Items[i]) as TtiBaseObject);
    lLine := '';
    for j := 0 to AColsSelected.Count - 1 do
    begin
      if lLine <> '' then
        lLine := lLine + AFieldDelim;
      lFieldName := AColsSelected.Strings[j];
      if GetPropInfo(pData,lFieldName)^.PropType^.Name = 'TDateTime' then
        lsValue := tiDateTimeToStr(GetPropValue(pData,lFieldName))
      else
      begin
        lPropType := TypInfo.PropType(pData, lFieldName);
        case lPropType of
          tkChar        : lsValue := IntToStr(GetOrdProp(pData, lFieldName));
          tkWChar       : lsValue := IntToStr(GetOrdProp(pData, lFieldName));
          tkString      : lsValue := GetStrProp(pData, lFieldName);
          tkLString     : lsValue := GetStrProp(pData, lFieldName);
          tkWString     : lsValue := GetWideStrProp(pData, lFieldName);
          {$IFDEF FPC}
          tkAString     : lsValue := GetStrProp(pData, lFieldName);
          {$ENDIF}
          tkInteger     : lsValue := IntToStr(GetInt64Prop(pData, lFieldName));
          tkInt64       : lsValue := IntToStr(GetInt64Prop(pData, lFieldName));
          tkFloat       : lsValue := FloatToStr(GetFloatProp(pData, lFieldName));
          tkEnumeration : lsValue := IntToStr(GetOrdProp(pData, lFieldName));
          {$IFDEF FPC}
          tkBool        : lsValue := IntToStr(GetInt64Prop(pData, lFieldName));
          {$ENDIF}
        end;
      end;
      lLine := lLine + lsValue;
    end;
    if i <> 0 then
      lLine := ARowDelim + lLine;
    tiAppendStringToStream(lLine, AStream)
  end;
end;


procedure tiListToStream(AStream : TStream; AList : TtiObjectList);
var
  lFields  : TStringList;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields := TStringList.Create;
  try
    tiGetPropertyNames(AList.Items[0], lFields);
    tiListToStream(AStream, AList, ',', CrLf, lFields);
  finally
    lFields.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList);
var
  lStream    : TFileStream;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');

  lStream := TFileStream.Create(AFileName, fmCreate);
  try
    tiListToStream(lStream, AList, #9, CrLf, AColsSelected);
  finally
    lStream.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList; const AFileName: string);
var
  lStream    : TFileStream;
  lFields: TStringList;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields:= TStringList.Create;
  try
    lStream := TFileStream.Create(AFileName, fmCreate);
    try
      tiGetPropertyNames(AList.Items[0], lFields);
      tiListToStream(lStream, AList, #9, CrLf, lFields);
    finally
      lStream.Free;
    end;
  finally
    lFields.Free;
  end;
end;


procedure tiListToClipboard(AList: TtiObjectList; AColsSelected: TStringList);
var
  lStream: TStringStream;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');
  lStream := TStringStream.Create('');
  try
    tiListToStream(lStream, AList, Tab, CrLf, AColsSelected);
    ClipBoard.AsText := lStream.DataString;
  finally
    lStream.Free;
  end;
end;


procedure tiListToClipboard(AList: TtiObjectList);
var
  lFields  : TStringList;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields := TStringList.Create;
  try
    tiGetPropertyNames(AList.Items[0], lFields);
    tiListToClipboard(AList, lFields);
  finally
    lFields.Free;
  end;
end;


procedure tiStringToFile(const psText, psFileName : string);
var
  lStream : TFileStream;
  lpcText  : PChar;
begin
  lStream := TFileStream.Create(psFileName, fmCreate or fmShareCompat);
  try
    lpcText := PChar(psText);
    lStream.WriteBuffer(lpcText^, length(psText));
  finally
    lStream.Free;
  end;
end;


function  tiFileToString(const pFileName : TFileName) : string;
var
  lFileStream: TFileStream;
begin
  result := '';
  lFileStream := TFileStream.Create(pFileName,
                                     fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result,  lFileStream.Size);
    lFileStream.Read(Result[1], lFileStream.Size);
  finally
    lFileStream.Free;
  end;
end;


function tiHasRTTI(pObject : TObject) : boolean;
var
  lClass : TClass;
begin
  lClass := pObject.ClassType;
  result := tiHasRTTI(lClass);
end;


function tiHasRTTI(pClass : TClass) : boolean;
var
  lClassInfo : Pointer;
begin
  lClassInfo := pClass.ClassInfo;
  result := lClassInfo <> nil;
end;


function  tiAddEllipsis(const psString : string; piWidth : integer = 20) : string;
var
  lLen : integer;
begin

  lLen := Length(psString);
  if lLen <= piWidth then
    result := psString
  else if (lLen > piWidth) then
    result := Copy(psString, 1, piWidth - 3) + '...'
  else
    result := Copy(psString, 1, lLen - 3) + '...'

end;


function  tiTrimR(const psString, psTrim : string; pbCaseSensitive : boolean = false) : string;
var
  li : integer;
begin
  if pbCaseSensitive then
    li := pos(psTrim, psString)
  else
    li := pos(UpperCase(psTrim), UpperCase(psString));

  if li <> 0 then
    result := Copy(psString, 1, li - 1)
  else
    result := psString;
end;


function  tiTrimL(const psString, psTrim : string; pbCaseSensitive : boolean = false) : string;
var
  li : integer;
begin
  if pbCaseSensitive then
    li := pos(psTrim, psString)
  else
    li := pos(UpperCase(psTrim), UpperCase(psString));

  if li <> 0 then
  begin
    li := li+Length(psTrim);
    result := Copy(psString, li, Length(psString)-li+1)
  end
  else
    result := psString;
end;


function  tiRemoveCrLf(const pString : string) : string;
begin
  result := tiStrTran(pString, Lf, '');
  result := tiStrTran(result,  Cr, ' ');
end;


function tiTrimTrailingWhiteSpace(const pString : string) : string;
const
  cWhiteSpace = [ #13, #10, #32 ];
var
  i : integer;
  lTruncChar : integer;
begin
  lTruncChar := Length(pString);
  for i := Length(pString) downto 1 do
  begin
    if pString[i] in cWhiteSpace then
      Dec(lTruncChar)
    else
      Break; //==>
  end;
  result := Copy(pString, 1, lTruncChar);
end;

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


function tiIsFileNameValid(const pFileName : string) : boolean;
var
  lFileName : string;
  i : integer;
const
  ExcludedChars = [ '\', '/', ':', '*', '?', '"', '<', '>', '|' ];
begin
  lFileName := ExtractFileName(pFileName);
  result :=
    (Length(lFileName) <= 255) and
    (Length(lFileName) > 0);
  if not result then
    Exit; //==>

  // From the NT help
  //A filename can contain up to 255 characters, including spaces.
  // But, it cannot contain any of the following characters:
  // \ / : * ? " < > |
  for i := 1 to Length(lFileName) do
    if  lFileName[i] in ExcludedChars then
    begin
      result := false;
      Exit; //==>
    end;
end;


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


function tiEncodeWordBase26(const pNum : Word) : String;
var
  lNum : Int64;
  lPos : byte;
begin
  if pNum = 0 then
  begin
    Result := cZeroChar;
    Exit; //==>
  end;

  Result := '     '; // If the param is changed from Word, this lenght must be changed.
  lPos := 5;
  lNum := pNum;
  While lNum > 0 Do
  Begin
    Result[lPos] := Chr(Ord(cZeroChar) + lNum Mod cBase);
    Dec(lPos);
    lNum := lNum div cBase;
  End;
  Result := Copy(Result, lPos+1, 5 - lPos);
end;


function  tiDecodeWordBase26(numstr : String) : Word;
var
  i: Integer;
begin
  Result := 0;
  If Length(numStr) = 0 Then
    Exit; //==>
  i:= 1;
  While i <= Length(numStr) Do
  Begin
    If (numstr[i] < cZeroChar) or
       ((Ord(numStr[i]) - Ord(cZeroChar)) >= cBase)
    Then
      raise EConvertError.CreateFmt(cErrorDecodeNumError, [numstr, cBase]);
    Result := Result * cBase + Ord(numStr[i]) - Ord(cZeroChar);
    Inc(i);
  End;
end;


// Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean;
var
  LByte1, LByte2 : byte;
begin
  LByte1 := 0;    // stop the compiler from complaining
  LByte2 := 0;
  result := true;
  if AStream1.Size <> AStream2.Size then
  begin
    result := false;
    VMessage := 'Streams have different sizes ('+inttostr(AStream1.Size)+'/'+inttostr(AStream2.Size)+')';
    Exit; //==>
  end;

  if AStream1.Position <> AStream2.Position then
  begin
    result := false;
    VMessage := 'Streams have different positions ('+IntToStr(AStream1.Position)+'/'+IntToStr(AStream2.Position)+')';
    Exit; //==>
  end;

  while (AStream1.Size - AStream1.Position > 0) do
  begin
    AStream1.Read(LByte1, 1);
    AStream2.Read(LByte2, 1);
    if LByte1 <> LByte2 then
    begin
      result := false;
      VMessage := 'Streams Differ at position '+inttostr(AStream1.Position)+' of '+inttostr(AStream1.Size)+': '+inttostr(LByte1)+'/'+inttostr(LByte2);
      Exit; //==>
    end;
  end;
end;


function tiTestStreamsIdentical(AStream1, AStream2 : TStream):Boolean;
var
  ls: string;
begin
  Result := tiTestStreamsIdentical(AStream1, AStream2, ls);
end;

function CursorStack: TList;
begin
  if not Assigned(uCursorStack) then
    uCursorStack := TList.Create;
  Result := uCursorStack;
end;


{ TAutoCursor }

constructor TAutoCursor.Create(ANewCursor: TCursor);
begin
  inherited Create;
  // push
  CursorStack.Add(@(Screen.Cursor));
  Screen.Cursor := ANewCursor;
end;

destructor TAutoCursor.Destroy;
begin
  // pop
  Screen.Cursor := TCursor(CursorStack.Last);
  CursorStack.Delete(uCursorStack.Count-1);
  inherited;
end;

function tiAutoWaitCursor: IUnknown;
begin
  if GetCurrentThreadId = MainThreadId then
    Result := TAutoCursor.Create(crHourglass)
  else
    Result := nil;
end;

function tiAutoCursor(ACursor: TCursor = crHourglass): IUnknown;
begin
  if GetCurrentThreadId = MainThreadId then
    Result := TAutoCursor.Create(ACursor);
end;

{ TtiBruteForceNoFlicker }
{$IFDEF MSWINDOWS}
constructor TtiBruteForceNoFlicker.Create(AControlToMask: TControl);
var
  ScreenPt: TPoint;
begin
  inherited Create(nil);
  FMaskControl := AControlToMask;
  BoundsRect := FMaskControl.BoundsRect;

  ScreenPt := FMaskControl.ClientToScreen(Classes.Point(0,0));

  FControlSnapshot := TBitmap.Create;

  ScreenShot(FControlSnapshot, ScreenPt.X, ScreenPt.Y, Width, Height, HWND_DESKTOP);

  Parent := FMaskControl.Parent;

  Update;
end;

procedure TtiBruteForceNoFlicker.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := WS_CHILD or WS_CLIPSIBLINGS;
  Params.ExStyle := WS_EX_TOPMOST;
end;

destructor TtiBruteForceNoFlicker.Destroy;
begin
  FreeAndNil(FControlSnapshot);
  inherited;
end;

procedure TtiBruteForceNoFlicker.Paint;
begin
end;

// From Jedi JCL JCLGraphics.pas
procedure TtiBruteForceNoFlicker.ScreenShot(bm: TBitmap; pLeft, pTop, pWidth, pHeight: Integer; Window: HWND);
var
  WinDC: HDC;
  Pal: TMaxLogPalette;
begin
  bm.Width  := pWidth;
  bm.Height := pHeight;

  // Get the HDC of the window...
  WinDC := GetDC(Window);
  try
    if WinDC = 0 then
      raise Exception.Create('No DeviceContext For Window');

    // Palette-device?
    if (GetDeviceCaps(WinDC, RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
    begin
      FillChar(Pal, SizeOf(TMaxLogPalette), #0);  // fill the structure with zeros
      Pal.palVersion := $300;                     // fill in the palette version

      // grab the system palette entries...
      Pal.palNumEntries := GetSystemPaletteEntries(WinDC, 0, 256, Pal.palPalEntry);
      if Pal.PalNumEntries <> 0 then
        {$IFDEF FPC}
        bm.Palette := CreatePalette(LPLOGPALETTE(@Pal)^);
        {$else}
        bm.Palette := CreatePalette(PLogPalette(@Pal)^);
        {$endif}
    end;

    // copy from the screen to our bitmap...
    BitBlt(bm.Canvas.Handle, 0, 0, pWidth, pHeight, WinDC, pLeft, pTop, SRCCOPY);
  finally
    ReleaseDC(Window, WinDC);        // finally, relase the DC of the window
  end;
end;

procedure TtiBruteForceNoFlicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  BitBlt(
    Message.DC,
    0,0, Width,Height,
    FControlSnapshot.Canvas.Handle,
    0,0,
    SRCCOPY);
end;
{$ENDIF MSWINDOWS}

function tiStrPos(const AString, ASubString: PChar): PChar;
  {
  From Paul Spain. paul@xpro.com.au
  You may remember during the XML optimisation project that there was a
  method where I couldn't get any performance benefit over a string-based
  method that was using quite a few Copy() calls? What, it's not just like
  yesterday for you?;-)

  Well, I finally figured out why last night after trying to optimise
  StringReplace(). It turns out that the RTL implementation of StrPos() is
  a dog. Written in assembler, but a dog nonetheless. If you find any
  calls to StrPos() in the XML code, you can crank some more from it using
  this implementation:
 }
var
 sub, str: PChar;
begin
  {$IFDEF FPC}
  { Free Pascal's implementation of StrPos is even faster than the tiStrPos
    version, so we rather call the Free Pascal one. }
  result := StrPos(AString, ASubString);
  {$ELSE}
  if (ASubString <> nil) and (ASubString^ <> #0) and (AString <> nil) then
  begin
     str := AString;
     while str^ <> #0 do
     begin
       sub := ASubString;
       // look for start of sub in str or end of str
       while (str^ <> sub^) and (str^ <> #0) do
         Inc(str);
       // check end of str
       if str^ <> #0 then
       begin
         // remember start of sub in str
         Result := str;
         // look for end of sub in str or end of sub or end of str
         while (str^ = sub^) and (str^ <> #0) do
         begin
           Inc(str);
           Inc(sub);
         end;
         // success if sub^ = #0
         if sub^ = #0 then
           exit;
       end;
     end;
   end;
   Result := nil;
  {$ENDIF}
end;


function tiNormalizeStr(const pString: string): string;
var
  s: string;

  {$IFNDEF FPC}
  function DelSpace1(const S: string): string;
  var
    i: Integer;
  begin
    Result:=S;
    for i:=Length(Result) downto 2 do
      if (Result[i]=' ') and (Result[I-1]=' ') then
        Delete(Result,I,1);
  end;
  {$ENDIF}

begin
  s := Trim(pString);
  s := tiStrTran(s, #9, ' ');   // replace Tabs
  s := tiStrTran(s, #10, ' ');  // replace LF
  s := tiStrTran(s, #13, ' ');  // replace CR
  Result := DelSpace1(s);
end;


function tiDateTimeAsXMLString(const pDateTime: TDateTime): string;
  function _IntToStr(pValue, pSize : integer) : string;
  begin
    result := IntToStr(pValue);
    result := tiPad0(result, pSize);
  end;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
begin
    DecodeDate(pDateTime, lY, lM, lD);
    DecodeTime(pDateTime, lH, lMi, lS, lMs);
    Result :=
      _IntToStr(lD, 2) + '/' +
      _IntToStr(lM, 2) + '/' +
      _IntToStr(lY, 4) + ' ' +
      _IntToStr(lH, 2) + ':' +
      _IntToStr(lMi, 2) + ':' +
      _IntToStr(lS, 2) + ':' +
      _IntToStr(lMs, 3);

  { What about using the ISO 8601 standard!  See cISODateTimeFmt1 in
    tiConstants.
    Also, why not use FormatDateTime()? }
end;

function tiXMLStringToDateTime(const pValue : string) : TDateTime;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
  lStr : string;
begin
  if pValue = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

  try
    //          1         2
    // 12345678901234567890123
    // YYYY/MM/DD HH:MM:SS:NNN
    // DD/MM/YYYY HH:MM:SS:NNN
    lD  := StrToInt(Copy(pValue, 1, 2));
    lM  := StrToInt(Copy(pValue, 4, 2));
    lY  := StrToInt(Copy(pValue, 7, 4));
//    if Length(pValue) >= 12 then
//    begin
      lStr := Copy(pValue, 12, 2); lH  := StrToIntDef(lStr, 0);
      lStr := Copy(pValue, 15, 2); lMi := StrToIntDef(lStr, 0);
      lStr := Copy(pValue, 18, 2); lS  := StrToIntDef(lStr, 0);
      lStr := Copy(pValue, 21, 3); lMs := StrToIntDef(lStr, 0);
      result :=
        EncodeDate(lY, lM, lD) +
        EncodeTime(lH, lMi, lS, lMs);
//    end else
//      result :=
//        EncodeDate(lY, lM, lD);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorXMLStringToDate, [pValue, e.message]);
  end;
end ;


function tiDateTimeAsIntlDateStor(const pDateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeStor, pDateTime);
  if Pos('18991230', Result) = 1 then
    Result := tiStrTran(Result, '18991230', '00000000');
end;


function tiDateTimeAsIntlDateDisp(const pDateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeDisp, pDateTime);
  if Pos('1899-12-30', Result) = 1 then
    Result := tiStrTran(Result, '1899-12-30', '0000-00-00');
end;


function tiIntlDateStorAsDateTime(const pValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word ;
begin
  if Trim(pValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;
  
    //          1         2
    // 12345678901234567890123
    // yyyymmddThhmmss
  lY  := StrToInt(Copy(pValue, 1, 4));
  lM  := StrToInt(Copy(pValue, 5, 2));
  lD  := StrToInt(Copy(pValue, 7, 2));
  lH  := StrToInt(Copy(pValue, 10, 2));
  lMi := StrToInt(Copy(pValue, 12, 2));
  lS  := StrToInt(Copy(pValue, 14, 2));
  
  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;


function tiIntlDateDispAsDateTime(const pValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word ;
begin
  if Trim(pValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;
  
    //          1         2
    // 12345678901234567890123
    // yyyy-mm-dd hh:mm:ss
  lY  := StrToInt(Copy(pValue, 1, 4));
  lM  := StrToInt(Copy(pValue, 6, 2));
  lD  := StrToInt(Copy(pValue, 9, 2));
  lH  := StrToInt(Copy(pValue, 12, 2));
  lMi := StrToInt(Copy(pValue, 15, 2));
  lS  := StrToInt(Copy(pValue, 18, 2));
  
  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;


procedure tiConsoleAppPause;
begin
  WriteLn('');
  WriteLn('Press <Enter> to continue...');
  ReadLn;
end;


function tiCreateGUIDString: string;
{$IFDEF FPC}
var
  lGUID: TGUID;
{$ENDIF}
begin
{$IFDEF FPC}
  CreateGUID(lGUID);
  Result := GUIDToString(lGUID);
{$ELSE}
  Result := tiWin32CoCreateGUID;
{$ENDIF}
end;


function tiGetTickCount: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := tiWin32GetTickCount;
{$ENDIF}
{$IFDEF LINUX}
  Result := tiLinuxGetTickCount;
{$ENDIF}
end;


{ TtiIntegerList }

procedure TtiIntegerList.Add(AValue: Int64);
var
  L: TtiIntegerListItem;
begin
  L:= TtiIntegerListItem.Create;
  L.Value:= AValue;
  FList.Add(L);
end;


constructor TtiIntegerList.Create;
begin
  inherited Create;
  FList:= TObjectList.Create(True);
end;


destructor TtiIntegerList.Destroy;
begin
  FList.Free;
  inherited;
end;


function TtiIntegerList.GetCount: Integer;
begin
  Result := FList.Count;
end;


function TtiIntegerList.GetItems(i: Integer): Int64;
begin
  Result := (FList.Items[i] as TtiIntegerListItem).Value;
end;


function TtiIntegerList.IndexOf(AValue: Int64): Integer;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    if (FList.Items[i] as TtiIntegerListItem).Value = AValue then
    begin
      Result:= i;
      Exit; //==>
    end;
  Result:= -1;
end;


function TtiIntegerList.Remove(AValue: Int64): Integer;
var
  LIndex: Integer;
begin
  LIndex:= IndexOf(AValue);
  if LIndex = -1 then
    raise EtiOPFDataException.CreateFmt(
      'Attempt to remove an integer from %s that does not exist: "%d"',
      [ClassName, AValue]);
  FList.Delete(LIndex);
  Result:= LIndex;
end;


procedure TtiIntegerList.SetItems(i: Integer; const Value: Int64);
begin
  (FList.Items[i] as TtiIntegerListItem).Value := Value;
end;


(* *****************************************************************************

 ROUTINES FOR ROUNDING IEEE-754 FLOATS TO SPECIFIED NUMBER OF DECIMAL FRACTIONS

  These routines round input values to fit as closely as possible to an
  output number with desired number of decimal fraction digits.

  Because, in general, numbers with decimal fractions cannot be exactly
  represented in IEEE-754 floating binary point variables, error limits
  are used to determine if the input numbers are intended to represent an
  exact decimal fraction rather than a nearby value.   Thus an error limit
  will be taken into account when deciding that a number input such as
  1.295, which internally is represented 1.29499 99999 …, really should
  be considered exactly 1.295 and that 0.29999 99999 ... really should
  be interpreted as 0.3 when applying the rounding rules.

  Some terminology:
    "NDFD" is used for Number of Decimal Fraction Digits.  If NDFD is
        negative, then the inputs will be rounded so that there are zeros
        on the left of the decimal point.  I.E. if NDFD = -3, then the
        output will be rounded to an integral multiple of a thousand.
    "MaxRelError" designates the maximum relative error to be allowed in the
        input values when deciding they are supposed to represent exact
        decimal fractions (as mentioned above).
    "Ctrl" determines the type of rounding to be done.  Nine kinds of
        rounding (plus no rounding) are defined.  They include almost
        every kind of rounding known.  See the definition of
        tDecimalRoundingCtrl below for the specific types.

 Rev. 2002.01.14 by John Herbster to add overloaded IsNaN() functions to
     solve problem with invalid operation when loading values for test.
 Rev. 2001.11.24 by John Herbster to add KnownErrorLimit, SafetyFactor,
     and MaxRelErrXxx constants. Also corrected DecimalRoundingCtrlStrs
     and comments per feedback from Marjan Venema.
 Pgm. 2001.09.19 by John Herbster.  Please send feedback and suggestions
     to author at herb-sci@swbell.net.

 Dedicated to the participants in the borland.public.delphi.objectpascal
     forum.  With special thanks to Brad White for his proofing and
     suggestions.

  http://cc.borland.com/Item.aspx?id=21909

***************************************************************************** *)
{ The following "epsilon" values are representative of the resolution of the
  floating point numbers divided by the number being represented.
  These constants are supplied to the rounding routines to determine how much
  correction should be allowed for the natural errors in representing
  decimal fractions.  Using 2 times or higher multiples of these values may
  be advisable if the data have been massaged through arithmetic
  calculations. }
Const
  SglEps = 1.1920928955e-07;
  DblEps = 2.2204460493e-16;
  ExtEps = 1.0842021725e-19;
{ These epsilon values are smallest for which "1 + epsilon <> 1".
      For "1 - epsilon <> 1", divide the following values by 2. }

  KnownErrorLimit = 1.234375;
  SafetyFactor = 2;
  MaxRelErrSgl = SglEps * KnownErrorLimit * SafetyFactor;
  MaxRelErrDbl = DblEps * KnownErrorLimit * SafetyFactor;
  MaxRelErrExt = ExtEps * KnownErrorLimit * SafetyFactor;
{ If MaxRelErr < XxxEps * KnownErrorLimit then errors can occur. }

{ The following DecimalRound function is for doing the best possible job of
  rounding floating binary point numbers to the specified NDFD.  MaxRelError
  is the maximum relative error that will be allowed when determining the
  cut points for applying the rounding rules. }
Function tiDecimalRound(Value: extended; NDFD: integer; MaxRelErr: double;
                         Ctrl: TtiDecimalRoundingCtrl = drHalfEven): extended;
var i64, j64: Int64; j: integer; m, ScaledVal, ScaledErr: extended;
begin

  If IsNaN(Value) or (Ctrl = drNone)
    then begin Result := Value; EXIT end;

{ Compute 10^NDFD and scale the Value and MaxError: }
  m := 1; For j := 1 to abs(NDFD) do m := m*10;
  If NDFD >= 0
    then begin
      ScaledVal := Value * m;
      ScaledErr := abs(MaxRelErr*Value) * m;
      end
    else begin
      ScaledVal := Value / m;
      ScaledErr := abs(MaxRelErr*Value) / m;
      end;

{ Do the diferent basic types separately: }
  Case Ctrl of
    drHalfEven: begin
      i64 := round((ScaledVal - ScaledErr));
      j64 := round((ScaledVal + ScaledErr));
      if Odd(i64)
        then i64 := j64;
      end;
    drHalfDown:  {Round to nearest or toward zero.}
      i64 := round((abs(ScaledVal) - ScaledErr));
    drHalfUp:    {Round to nearest or away from zero.}
      i64 := round((abs(ScaledVal) + ScaledErr));
    drHalfPos:   {Round to nearest or toward positive.}
      i64 := round((ScaledVal + ScaledErr));
    drHalfNeg:   {Round to nearest or toward negative.}
      i64 := round((ScaledVal - ScaledErr));
    drRndNeg:    {Truncate toward negative. (a.k.a. Floor)}
      i64 := round((ScaledVal + (ScaledErr - 1/2)));
    drRndPos:    {Truncate toward positive. (a.k.a. Ceil)}
      i64 := round((ScaledVal - (ScaledErr - 1/2)));       {}
    drRndDown:   {Truncate toward zero (a.k.a. Trunc).}
      i64 := round((abs(ScaledVal) + (ScaledErr - 1/2)));
    drRndUp:     {Truncate away from zero.}
      i64 := round((abs(ScaledVal) - (ScaledErr - 1/2)));
    else i64 := round(ScaledVal);
    end{cases};

{ Finally convert back to the right order: }
  If NDFD >= 0
    then Result := i64 / m
    else Result := i64 * m;

  If (Ctrl in [drHalfDown,drHalfUp,drRndDown,drRndUp]) and
     (Value < 0)
    then Result := -Result;

end{DecimalRound};

{ The following functions have a two times "epsilon" error built in for the
  single, double, and extended argument respectively: }
Function tiDecimalRoundSgl(AValue: single; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrSgl, ACtrl);
end;

Function tiDecimalRoundDbl(AValue: double; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrDbl, ACtrl);
end;

Function tiDecimalRoundExt(AValue: extended; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrExt, ACtrl);
end;

function tiSetPrecision(const AValue : Extended; const APrecision : integer = 3) : real;
begin
  Result:= tiDecimalRoundDbl(AValue, APrecision);
end;


initialization
  // Set Windows internal date and time format strings
//  ShortDateFormat := csWinDateFormat;
//  ShortTimeFormat := csWinTimeFormat;
finalization
  FreeAndNil(uCursorStack);
end.

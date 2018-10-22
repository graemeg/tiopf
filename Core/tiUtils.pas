unit tiUtils;

{$I tiDefines.inc}

interface

uses
   tiBaseObject
  ,tiConstants
  ,SysUtils
  ,Classes
  ,Types
  ,Math
{$IFDEF IOS}
  ,System.Generics.Defaults
  ,Generics.Collections
{$ELSE}
  ,Contnrs
{$ENDIF IOS}
  ,Variants
  {$IFDEF MSWINDOWS}
  ,Windows       // Graeme: This must appear before SyncObjs for Free Pascal!
  ,shellAPI
  {$ENDIF MSWINDOWS}
  ,SyncObjs
 ;

{$IFDEF VER130}
type
  TVarType = Word;
{$ENDIF}

type
{$IFNDEF FPC}
// So Delphi can be FPC compatible
  TThreadID = THandle;
  PtrInt = integer;  // we must watch this when Delphi gets 64bit support
  PtrUInt = cardinal;
{$ENDIF}

  ItiTokens = interface(IInterface)
    ['{9E3F0CF7-1430-4A8B-B204-80AD03A4F567}']
    function Count: integer;
    function GetToken(const AIndex: integer): string;
    // AText is a collection of Tokens separated by ASeparator
    procedure SetText(const AText, ASeparator: string);
    // note: AIndex is 1-based index of AIndex-th Token in AText
    property Tokens[const AIndex: integer]: string read GetToken; default;
  end;

  function CreateTiTokens(const AText: string = ''; const ASeparator: string = ','): ItiTokens;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * String manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    { ToDo 5 -cUtils: Convert params to const }
  // Scan the string AValue, and replace any characters ADel with AIns (Case sensitive)
  function  tiStrTran(        AValue, ADel, AIns : string): string;
  function  tiStrTran1(AValue, ADel, AIns : string): string;
  // Scan the string AValue, and replace any characters ADel with AIns (Case insensitive)
  function  tiCIStrTran(      AValue, ADel, AIns : string): string;
  // Count the number of blocks of text (tokens) in AValue separated by ASeparator
  function tiNumToken(const AValue, ASeparator : string): integer;
  // Extract the 1-based ATokenIndex(th) block of text (token) in AValue, seperated by ASeparator
  function  tiToken(const AValue, ASeparator : string; const ATokenIndex : integer): string;
  // Return a string of spaces ALen long
  function  tiSpace(          ALen : integer): string;

  // Pad AValue with spaces on the RHS to make it ALen long
  function  tiPadR(AValue: string; const ALen: integer; const APadChar: Char = ' '): string;
  // Pad AValue with specified character on the LHS to make it ALen long
  function  tiPadL(AValue: string; const ALen: integer; const APadChar: Char = ' '): string;
  // Pad AValue with spaces on both sides to make it ALen long
  function  tiPadC(AValue: string; const ALen : integer; const APadChar: Char = ' '): string;
  // Pad AValue with zeros on the LHS to make it ALen long
  function  tiPad0(const AValue: string; const ALen : integer): string;

  // Remove any leading zeros from a string
  function  tiRemoveLeading0( AValue : string): string;
  // Convert a string into mized case using a simple algorithm
  // Note: There is a more complete version of this (that takes care of works like
  //       McCrae or King Charles IV
  //       It will also work with characters other than #32 to sep words
  function  tiMixedCase(      AValue : string): string;
  // Replicate the string AValue to return a string pIntLength long
  function  tiReplicate(       const AValue : string; ARepCount : Word): string;
  // If ALine is not '', add the trailing value AValue
  function  tiAddTrailingValue(const ALine, AValue : string; ADuplicates : boolean = true): string;
  // If the last character of ALine is AValue, then remove it
  function  tiRemoveTrailingValue(ALine, AValue : string): string;
  // If AValue is not '', add a trailing ', '
  function  tiAddTrailingComma(AValue : string): string;
  // If AValue is not '', add a trailing ' and '
  function  tiAddTrailingAnd( AValue : string): string;
  // If AValue is not '', add a trailing ' or '
  function  tiAddTrailingOr(  AValue : string): string;
  // If AStr is not '', add a trailing ' '
  function  tiAddTrailingSpace(AValue : string): string;
  // Return the first position of AValue in ATarget from the right.
  // Same as Delphi's Pos, except from the right, not left
  function  tiPosR(           ATarget, AValue : string): integer;
  // Does the wildcard pPattern occur in the string ASource ?
  function tiWildcardMatch(const ASource, APattern: String; const ACaseSensitive: boolean = false): Boolean;
  // Extract a sub string within defined delimiters
  function  tiSubStr(const ASource, AStartDelim, AEndDelim : string; AIndex : integer = 1): string;
  // Trunc a string to AWidth length and add '...'
  function  tiAddEllipsis(const AString : string; AWidth : integer = 20): string;
  {: Trim all leading and trailing instances of AString.
     ToDo: Overload tiTrim with
           tiTrim(const AString: string; const ATrim: string); overload;
           tiTrim(const AString: string; const ATrim: array of string); overload;
     }
  function  tiTrim(const AString: string; ATrim: char): string;
  // Trim all characters after and including ATrim from AString
  function  tiTrimR(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
  // Trim all characters before and including ATrim from AString
  function  tiTrimL(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
  // Remove all the trailing white space characters (#32, #10, #13)
  // from the end of a string
  function tiTrimTrailingWhiteSpace(const AString : string): string;
  // Remove Cr & LF characters
  function  tiRemoveCrLf(const AString : string): string;
  // Returns true if the email address is valid
  function tiIsEMailAddressValid(const AValue: string): boolean;
  // Returns true if the character is valid for a file name.
  function tiIsFileNameCharValid(const AFileNameChar : char): boolean;
  // Returns true if the file name is valid. Will not test if the
  // directory part of the file name exists, or is valid, will just
  // test the file name part
  function tiIsFileNameValid(const AFileName : string): boolean;
  // Substitute the invalid characters in the given filename with the given
  // character. If the substitute character is not a valid filename character
  // then an exception is raised.
  function tiConvertToValidFileName(const AFileName : string;
                                    const ASubstituteChar: char): string;
  {: Replacement for Delphi's StrPos, but much faster. Not for FPC though!}
  function tiStrPos(const AString, ASubString: PChar): PChar;
  {: Normalize the string by replacing all repeated Spaces, Tabs and NewLines
    chars with a single space. It's so easy with Regular Expression!:(}
  function tiNormalizeStr(const AString: string): string;
  {: Encode a URI, replacing certain known characters with %HH values }
  function tiURIEncode(const AString: string): string;
  {: Decode a URI, replacing certain known %HH values with characters }
  function tiURIDecode(const AString: string): string;
  {: Decode a URI, replacing all %HH values with characters }
  function tiURIDecodeAll(const AString: string): string;
  {: Encode a string for display in HTML, replacing reserved characters with &xyz; as appropriate }
  function tiHTMLEncode(const AString: string): string;
  {: Decode &xyz; reserved characters in a HTML string. }
  function tiHTMLDecode(const AString: string): string;
  {: Add the given prefix and suffix to the given string, optionally only if
    they are not already present }
  function tiEnclose(const AString: string;
                     const APrefix: string;
                     const ASuffix: string = '';
                     const AEncloseIfPresent: Boolean = false): string;
  {: Enclose the given string in double quotes if they are not already present }
  function tiQuote(const AString: string): string;
  {: Insert a separator string into a string at each column position. }
  function tiAddSeparators(const AString: string; const AColumnWidth: Integer; const ASeparator: string): string;
  {: Wrap a string at a column position by inserting newline separators as
     typically used in the OS. }
  function tiWrap(const AString: string; const AColumnWidth: Integer): string;
  function tiStripNonAlphaCharacters(const AString: string): string;
  function tiStripNonNumericCharacters(const AString: string): string;
  function tiReplaceFileNameReservedChars(const AString: string;
                                          const AReplaceWith: string;
                                          const AReplaceDot: Boolean = false;
                                          const AReplaceSlashes: Boolean = false;
                                          const AReplaceColons: Boolean = false): string;
  {: Format a data size in bytes (note that this uses kibi/mebi/gibi/... magnitudes.
     AValueFormat is a floating point format string. e.g. %f, %.0f, %g }
  function tiFormatDataSize(const AValue: Int64; const AValueFormat: string = '%.0f'): string;
  {: Remove sequence of digits from start of string. }
  function tiStripIntPrefix(const AString : string): string;
  {: Append value onto existing string. If the existing string is not empty
     add the separator first }
  function tiAppendStr(const AString: string; const AValue: string; const ASeparator: string = ' '): string;
  {: Return the plural form of the given singular word }
  function tiPluralize(const ASingularWord: string; const AMakePlural: boolean = true): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Directory, file and file name manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  // Get a temporary file name with the extension AFileNameExtension
  function  tiGetTempFile(const AFileNameExtension : string): string;
  // Add a trailing slash ('\' or '/' based on platform) if there is not already one
  function  tiAddTrailingSlash(  const AValue : string): string;
  // Remove a trailing slash ('\' or '/' based on platform) if there is one
  function  tiRemoveTrailingSlash(const AValue : string): string;
  // Remove a leading slash ('\' or '/' based on platform) if there is one
  function  tiRemoveLeadingSlash(const AValue : string): string;
  // Get the systems temporary directory
  function  tiGetTempDir: string;
{$IFDEF MSWINDOWS}
  // Get the windows system directory
  function  tiGetWindowsSysDir : string;
{$ENDIF MSWINDOWS}
  {: Read a file's date with correction for the file's locale}
  function  tiReadFileDate(const AFileName : string): TDateTime;
  {: Read a file's size}
  function  tiReadFileSize(const AFileName : string): DWord;
  {: Read a file's date and size}
  procedure tiReadFileDateSize(const AFileName : string;
                                var   ADateTime : TDateTime;
                                var   AFileSize : integer);
  // Set a files date and time
  procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
  // Convert the given date time to the value that would be used in the filesystem
  // There is usually a difference in precision (e.g. Windows rounds up to 2 sec)
  function tiDateTimeToFileDateTime(const ADateTime: TDateTime): TDateTime;
  // Get the full path and filename of the exe or dll, Win32 and .NET
  function tiGetModuleFileName: string;
  // What is the path of the current EXE?
  function  tiGetEXEPath : string;
  // Add the EXE path to a file name
  function  tiAddEXEPath(const AFileName: string): string;
  // Extract the file name part of a file name (ie, remove the path and extension)
  function  tiExtractFileNameOnly(     AValue : string): string;
  // Remove the extension from a filename. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiRemoveExtension(AValue: string): string;
  // Change a file's extenstion. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiSwapExt(                 const AFileName, AExt : string): string;
  // Extract a file's extension. Similar to Delphi's ExtractFileExt except the '.' is not extracted.
  function  tiExtractExtension(AValue: string): string;
  // Copy a file from AFileFrom to AFileTo
  procedure tiCopyFile(          const AFrom, ATo : string);
  // Move a file from AFromFileName to AToFileName
  function  tiMoveFile(          const AFrom, ATo: string): Boolean;
  // Read a file's size in bytes
  function  tiGetFileSize(             AValue : string): longInt;
  // Remove the drive letter from a file name
  function  tiRemoveDrive(             AValue : string): string;
  // Removed part of a directory
  function  tiRemoveDirectory(const AFileName: string; const AToRemove: string): string;
  // Set a file's readonly attribute
  procedure tiSetFileReadOnly(AFileName: string; AReadOnly: boolean);
  // Is a file's readonly attribute set?
  function  tiIsFileReadOnly(AValue: string): boolean;
  // Copy all the directories, starting at pStrStartDir to the stringList slTree
  procedure tiDirectoryTreeToStringList(const AStartDir: string; const ADirList: TStringList; ARecurse: boolean);
  // Copy all the files, from the directory pStrStartDir, matching the wildcard pStrWildCard
  // to the stringList slResult
  procedure tiFilesToStringList(const AStartDir, AWildCard: string; const AResults : TStringList; const ARecurse : boolean);
  // Copy one directory tree to another
  procedure tiXCopy(const ASource, ATarget: string);
  {: Delete all the files that match AWildCard found in ADirectory}
  procedure tiDeleteFiles(const ADirectory, AWildCard: string);
  {: Delete all the files that match AWildCard found in ADirectory where file date modified is < CurrentDate - ADaysOld}
  procedure tiDeleteOldFiles(const ADirectory, AWildCard: string;
                             const ADaysOld: Integer; const ARecurseDirectories: Boolean;
                             const ADeleteEmptyDirectories: Boolean);
  {: Delete a file, but without BDS2006 function inlining warning}
  function tiDeleteFile(const AFileName: string): boolean;
  // Does a directory have any subdirectories?
  function  tiHasSubDirectory(AStartDir : string): boolean;

{$IFNDEF IOS}
  // Write the string in AText to a file named AFileName
  procedure tiStringToFile(const AText : string; const AFileName: string);
  // Append the string in AText to a file named AFileName
  procedure tiAppendStringToFile(const AText : string; const AFileName: string);
  // Read a text file into a string
  function  tiFileToString(const AFileName : TFileName): string;
{$ENDIF IOS}

  // Get the current directory
  // Extract a directory name to a certain level.
  // eg tiExtractDirToLevel('c:\temp\dir', 0) gives 'c:'
  //    tiExtractDirToLevel('c:\temp\dir', 1) gives 'c:\temp'
  //    tiExtractDirToLevel('c:\temp\dir', 2) gives 'c:\temp\dir'
  function tiExtractDirToLevel(const AFileName : TFileName; ALevel : byte): TFileName;
  {: Same as Delphi's ForceDirectory, but will raise an exception if create fails
     If there is a period in AValue, then AValue is assumed to be a file name and
     ExtractFilePath() will be called and the result of this call will be used as
     the directory to create. This will cause unexpected results if there is a
     period in the directory name.}
  procedure tiForceDirectories(const AValue : TFileName);
  {: Same as Delphi's ForceDirectory, but will raise an exception if create fails.}
  procedure tiForceDirectories1(const AValue : TFileName);
  // Remove a directory, and all it's owned directories and files
  function tiForceRemoveDir(const AValue : TFileName): boolean;
  // Join two path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const ALeftSide, ARightSide: string): string; overload;
  // Join multiple path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const AElements: array of string): string; overload;
  // Fixes the path separator for the *Unix platform. See implementation for more details.
  function tiFixPathDelim(const AText: string): string;
  {: Get the directory name where we can store application config files.
     This directory will be private to the currently logged on user.
     This just returns a name, it doesn't create the directory or checks if we have write access to it.}
  function tiGetAppDataDirPrivate: string;
  {: Get the directory name where we can store application config files.
     This directory will be shared between all users.
     This just returns a name, it doesn't create the directory or checks if we have write access to it.}
  function tiGetAppDataDirPublic: string;
  // Platform neutral function to return application name without GUI requirement
  function tiApplicationName: string;
  // If it is a local file then get the full path.
  function tiExpandURI(const AURI: string): string;
  // Check for file based URI.
  function tiIsFileURI(const AURI: string): Boolean;
  // Checks if a filename can be created. Puts user friendly error into AErrorMsg and returns false if there is an error.
  function tiCheckFileCanBeCreated(const AFileName: string; var AErrorMsg: string): boolean;
  // Find the position of the file extension.
  function tiLocateExtension(aValue: TFileName): integer;
  function tiGetCommonAppDataDir(const AAppDataSubDir: string): string;
  function tiGetUserLocalAppDataDir(const AAppDataSubDir: string): string;
  function tiGetCurrentUserPersonalDir: string;
  function tiRemoveCurrentUserPersonalDirPrefix(const AFileName: string): string;

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

  {: Set the precision of AValue to pPrecision decimal places}
  function  tiSetPrecision(const AValue : Extended; const APrecision : integer = 3): Extended;
  { The following functions have a two times "epsilon" error built in for the
    single, double, and extended argument respectively: }
  Function tiDecimalRoundSgl(AValue: single;   ANDFD: Integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundDbl(AValue: double;   ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundExt(AValue: extended; ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;

  // Calculate pNum div ADenom, and return 0 if an error
  function  tiSafeDiv(ANum, ADenom: longInt): longInt; overload;
  // Calcuate pNum / ADenom and return 0 if an error
  function  tiSafeDiv(ANum, ADenom: Extended): Extended; overload;
  // Round a float to an integer
  function  tiRound(const AValue : Extended): Int64;
  // Convert an interger (word) to a base26 string (A..Z) characters only
  function  tiEncodeWordBase26(const AValue : Word): String;
  // Convert a base26 string (A..Z characters only) to an integer (word)
  function  tiDecodeWordBase26(AValue : String): Word;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TDateTime manipulation
  // * Also see the VCL unit DateUtils
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  type TtiTimeInterval =
     (titiDay,
      titiHour,
      titiMinute,
      titiSecond,
      titiMillisecond);

  // What is the date of the previous week day?
  function tiDateToPreviousWeekDayDate(AValue : TDateTime): TDateTime;
  // Get the current year as an integer
  function tiYear(AValue : TDateTime = 0.0): Word;
  // Is a date within a given range?
  function tiDateWithinRange(const ADate, AFrom, ATo: TDateTime): Boolean;
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
  {: Count the number of days in an Australian financial year
     (allowing for leap years)}
  function tiAusFinancialYearDayCount(const AYear: Word): Word;

  {: Round a date time to the previous whole minute}
  function tiRoundDateToPreviousMinute(const ADateTime: TDateTime): TDateTime;
  {: Round a date time to the next whole minute (not rounded if already a whole minute) }
  function tiRoundDateToNextMinute(const ADateTime: TDateTime): TDateTime;
  {: Round a date time to the nearest whole minute }
  function tiRoundDateToNearestMinute(const ADateTime: TDateTime): TDateTime;
  {: Increment a date time to the next specified interval}
  function tiNextInterval(const ADateTime: TDateTime;
      const AInterval: TtiTimeInterval): TDateTime;
  {: Decode a date time to the nearest millisecond. }
  procedure tiDecodeDateTimeToNearestMilliSecond(const ADateTime: TDateTime;
    out ADays: Integer; out AHours: Word; out AMins: Word; out ASecs: Word;
    out AMSecs: Word);
  {: Round a date time to the nearest millisecond. This helps to overcome precision
     problems when adding or comparing date-times. }
  function tiRoundDateTimeToNearestMilliSecond(const ADateTime: TDateTime): TDateTime;
  {: Round a date time to the nearest interval given in TDateTime format. }
  function tiRoundDateTimeToNearestInterval(const ADateTime: TDateTime;
    const AInterval: TDateTime): TDateTime;
  {: Compare date-time with millisecond rounding to avoid floating point
     comparison issues. }
  function tiCompareDateTimeToMilliSecond(const AFirstDateTime: TDateTime;
    const ASecondDateTime: TDateTime): TValueRelationship;
  {: Compare date-time with rounding to the nearest interval given in TDateTime
     format to avoid floating point comparison issues. }
  function tiCompareDateTimeToInterval(const AFirstDateTime: TDateTime;
    const ASecondDateTime: TDateTime; const AInterval: TDateTime): TValueRelationship;
  {: Is the given time within the given time range. Millisecond precision. }
  function tiIncludesTime(const ADateTime: TDateTime;
    const AStartDateTime: TDateTime; const AEndDateTime: TDateTime): boolean;
  {: Do the two given time ranges overlap. Millisecond precision. }
  function tiOverlapsTime(const AStartDateTimeA: TDateTime;
    const AEndDateTimeA: TDateTime; const AStartDateTimeB: TDateTime;
    const AEndDateTimeB: TDateTime): boolean;
  {: Convert a date to the week number}
  function tiWeekNumber(const ADate: TDateTime): Byte;

  function tiDateTimeAsXMLString(const ADateTime: TDateTime): string;
  function tiDateAsXMLString(const ADateTime: TDateTime): string;
  function tiXMLStringToDateTime(const AValue : string): TDateTime;

  function tiDateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
  function tiDateTimeAsIntlDateDisp(const ADateTime: TDateTime): string;
  function tiDateAsIntlDateDisp(const ADateTime: TDateTime): string;
  function tiIntlDateStorAsDateTime(const AValue: string): TDateTime;
  function tiIntlDateDispAsDateTime(const AValue: string): TDateTime;
  {$IFDEF MSWINDOWS}
  {: Offset (in days or a fraction thereof) to convert current local time to GMT.}
  function tiGMTOffset: TDateTime;
  {: Offset (in days or a fraction thereof) to convert GMT to current local time.}
  function tiOffsetFromGMT: TDateTime;
  function tiTimeToGMT(const ALocalTime: TDateTime; const AOffsetFromGMTHours: Integer): TDateTime;
  function tiLocalTimeToGMT(const ALocalTime: TDateTime): TDateTime;
  function tiGMTToTime(const AGMTTime: TDateTime; const AOffsetFromGMTHours: Integer): TDateTime;
  function tiGMTToLocalTime(const AGMTTime: TDateTime): TDateTime;
  {$ENDIF}

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Type conversions
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  // Convert a string to an integer using StrToInt, default to 0 if an exception is raised
  function  tiStrToInt(         const AValue : string)   : integer;
  // Convert the numeric prefix of a string to an integer using StrToInt, default to 0 if an exception is raised
  function  tiStrToIntPrefix(   const AValue : string)   : integer;
  // Convert a string to a float using StrToFloat, default to 0 if an exception is raised
  function  tiStrToFloat(       const AValue : string)   : Extended;
  // Convert a date to a string using FormatDateTime with the standard date format
  function  tiDateToStr(        const pDTValue  : TDateTime; const psFormat : string = csWinDateFormat): string;
  // Convert a dateTime to a string using FormatDateTime with the standard dateTime format
  function  tiDateTimeToStr(    const ADateTime  : TDateTime): string;
  // Convert a time to a string using FormatDateTime with the standard time format
  function  tiTimeToStr(        const ADateTime  : TDateTime;
                                  const ATimeFormat : string = ''): string;
  // Convert an integer to a string and return '' if 0
  function  tiIntToStrHide0(    const AValue : Int64)  : string;
  // Converts an integer to a string with commas between thousands
  function  tiIntToCommaStr(    const AValue   : Int64): string;
  // Convert a float to a currency string and return '' if 0
  function  tiFloatToCurrencyHide0(const AValue : Extended)     : string;
  // Convert a float to a currency string
  function  tiFloatToCurrency(    const AValue : Extended)     : string;

  // ToDo: Tidy these up. String may be T/F, Y/N, True/False, TRUE/FALSE, 1/0, etc.
  // Convert a boolean to 'True' or 'False'
  function  tiBooleanToStr(     const AValue : boolean)  : string;
  // Convert a boolean to 'True' or 'False'
  function  tiBoolToStr(        const AValue : boolean)  : string;
  // Convert a boolean to Y or N
  function  tiBoolToYNString( const AValue : boolean)  : string;
  // Convert a string to a boolean
  function  tiStrToBool(const AValue : string): boolean;

  // Convert a float to a string in the format 9999.99
  function  tiFloatToStr(       const AValue : Extended;
                                  const APrecision : integer = 3): string;
  // Convert a float to a string in the format 9,999.99
  function  tiFloatToCommaStr(  const AValue : Extended;
                                  const APrecision : integer = 3): string;
  // Convert a float to a string in the format passed by AFormat
  function  _tiFloatToStr(      const AValue : Extended;
                                  const APrecision : integer;
                                  const AFormat : string): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Win32 API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{$IFDEF MSWINDOWS}
  // Call Windows ShellExecute with exception handling
  function _tiShellExecute(AHwnd : integer;
                            AOpperation, AFile, AParameters, ADirectory : PChar;
                            AShowCmd : integer): integer;
  // Simple pascal call to ShellExecute
  function tiShellExecute(const AEXE : string;
                           const AParameters : string = '';
                           const AWinState : integer = SW_SHOWNORMAL;
                           const AOperation: string = ''): integer;
  procedure tiOpenFile(const AFileName: string);
{$ENDIF MSWINDOWS}
  // Run an EXE and wait for it to finish
  procedure tiRunEXEAndWait(const AEXE : string; const AParams: string = '';
      AInheritParentStartInfo: boolean = true);

  // Get the currently logged on user ID
  function  tiGetUserName : string;
  // Get the computer name
  function  tiGetComputerName : string;

  // Bit manipulation
  // Is a particular bit set?
  function tiIsBitSet(const AValue: longint; const ABit: byte): boolean;
  // Convert a particular bit to a string: '1' or '0'
  function tiBitToString(const AValue: longint; const ABit: byte): string;
  // Convert a Int32 to a string of 0s and 1s
  function tiInt32ToBinString(const AValue : longInt): string;
  // Display characters in the string as hex values, each with optional prefix.
  function tiStrToHex(const AString: string; const AHexPrefix: string = ''): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Other routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  {: Compare two float values for equality, using a given number of significant digits.
     Ref: http://www.adtmag.com/joop/crarticle.asp?ID=396
     @param(FEPS Determines the significant digits. @br
                 FEPS value of 0.00001 means 5 significant digits. @br
                 FEPS value of 0.01 means 2 significant digits.)
     Note: This test is based on significant figures, not number of decimal places. }
  function tiIsNearEnough(N, D: Double;  FEPS: Double = 0.00001): Boolean; overload;
  {: Compare two float values for equality, using a given number of decimal places. }
  function tiIsNearEnoughDecimalPlaces(const AValue1, AValue2: Double; ADecimalPlaces: Integer = 2): Boolean; overload;
  {: Compare two TDateTimes. Returns True if they are within 0.5 sec}
  function tiIsDateTimeNearEnough(const AVal1: TDateTime; const AVal2: TDateTime;
      const AProximity: TDateTime = cdtOneSecond / 2): Boolean;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Extended): Extended; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Integer): Integer; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: String): String; overload;

  // Convert a variant array of variants to a string
  function  tiVariantArrayToString(AValue: Variant): string;
  // Is a variant of a given type
  function  tiIsVariantOfType(AVariant: Variant; AVarType: TVarType): boolean;
  {: Return the given vairant as a string. If Null then return the default. }
  function tiVariantAsStringDef(const AVariant: Variant; const ADefault: string = ''): string;
  // Return a string with ACount #10 characters
  function  Lf(const ACount : Byte = 1): string;
  // Return a string with ACount #13 characters
  function  Cr(const ACount : Byte = 1): string;
  // Return a string with ACount #13+#10 characters
  function  CrLf(const ACount : Byte = 1): string;
  // Return a string with ACount line endings (OS dependent) characters
  function  tiLineEnd(const ACount : Byte = 1): string;
  // Returns a string with ACount #9 characters
  function  Tab(const ACount : Byte = 1): string;
  // Returns the checksum of a string of numbers
  function tiCheckSum(const AValue: string): Integer;

{ $IFNDEF IOS}
  {: Write a string into a stream overwriting all contents}
  procedure tiStringToStream(const AStr : string; const AStream : TStream);
  {: Append a string to a stream}
  procedure tiAppendStringToStream(const AStr : string; const AStream : TStream);
  {: Insert a string to a stream}
  procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
  {: Read the entire contents of a stream as a string}
  function  tiStreamToString(const AStream : TStream): string; overload;
  {: Read part of the contents of a stream as a string}
  function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string; overload;
{ $ENDIF IOS}

  // Read a file into a stream
  procedure tiFileToStream(const AFileName : string; const AStream : TStream);
  // Save the contents of a stream to a file
  procedure tiStreamToFile(const AFileName : string; const AStream : TStream);
  // Copy one stream to another
  procedure tiCopyStream(const AStreamFrom, AStreamTo : TStream);

  // Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean; overload;
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream):boolean; overload;

  function tiTestFilesIdentical(const AFileName1, AFileName2: string): boolean;

  function tiHasRTTI(AObject : TObject): boolean; overload;
  function tiHasRTTI(AClass : TClass): boolean; overload;

  {: Writes "Press <Enter> to continue and waits for line input in a console application.}
  procedure tiConsoleAppPause;

  {: Platform neutral function to return a GUID string. }
  function tiCreateGUIDString: string;
  {: Platform neutral function to return the systems TickCount. }
  function tiGetTickCount: Cardinal;
  { Take an object, and a string class name and return true if the object
    is or descends from the string class name. }
  function tiIsClassOfType(pData: TObject; pClassName: string): boolean;

type

  TtiIntegerListItem = class(TtiBaseObject)
  private
    FValue: Int64;
  public
    constructor CreateCustom(AValue: Int64);
    property Value: Int64 Read FValue Write FValue;
  end;

  TtiIntegerList = class(TtiBaseObject)
  private
{$IFDEF IOS}
    FList: TObjectList<TtiIntegerListItem>;
{$ELSE}
    FList: TObjectList;
{$ENDIF IOS}
    function    GetItems(i: Integer): Int64;
    procedure   SetItems(i: Integer; const AValue: Int64);
    function    GetCount: Integer;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Add(AValue: Int64);
    function    IndexOf(AValue: Int64): Integer;
    function    Remove(AValue: Int64): Integer;
    property    Items[i: Integer]: Int64 Read GetItems Write SetItems; default;
    property    Count: Integer Read GetCount;
    procedure   Sort;
  end;

  TtiUniqueIntegerList = class(TtiIntegerList)
  public
    procedure   Add(AValue: Int64);
  end;

  TtiRealListItem = class(TtiBaseObject)
  private
    FValue: Extended;
  public
    constructor CreateCustom(AValue: Extended);
    property    Value: Extended Read FValue Write FValue;
  end;

  TtiRealList = class(TtiBaseObject)
  private
{$IFDEF IOS}
    FList: TObjectList<TtiRealListItem>;
{$ELSE}
    FList: TObjectList;
{$ENDIF IOS}
    function    GetItems(i: Integer): Extended;
    procedure   SetItems(i: Integer; const AValue: Extended);
    function    GetCount: Integer;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Add(AValue: Extended);
    property    Items[i: Integer]: Extended Read GetItems Write SetItems; default;
    property    Count: Integer Read GetCount;
  end;


  {: Provides similar functionality to the VCL's TMultiReadSingleWriteSynchronizer,
     except that a Read can not be promoted to a write. The VCL's version reports
     a leak under DUnit2 and this has been fixed in the tiOPF version.}
  TtiMultiReadExclusiveWriteSynchronizer = class(TtiBaseObject)
  private
    FReadCount: Cardinal;
    FWriting: Boolean;
    FCritSect: TCriticalSection;
    function LockForWrite: boolean;
    function LockForRead: boolean;
  protected
    function CanLockForWrite: boolean; virtual;
    function CanLockForRead: boolean; virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BeginRead;
    procedure   EndRead;
    procedure   BeginWrite;
    procedure   EndWrite;
  end;

{$IFDEF IOS}
// cf http://docwiki.embarcadero.com/CodeExamples/XE4/en/ShortStringToString_(Delphi)
// NB These two ShortString methods have not been tested

type
  EtiShortStringConvertError = class(Exception)
  end;

function ShortStringToString(AValue: array of Byte): String;
procedure StringToShortString(const AFromString: String; var AReturnValue);
{$ENDIF IOS}

implementation
uses
   tiExcept
  {$IFDEF MSWINDOWS}
  ,tiWin32
    {$IFDEF FPC}
    ,winsysut
    {$ENDIF}
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  {$IFNDEF MACOS}
  ,unix
  ,baseunix
  ,tiUnix
  {$ENDIF MACOS}
  {$ENDIF UNIX}
  {$IFDEF FPC}
  ,Process
  {$ENDIF}
  {$IFDEF MACOS}
  ,System.IOUtils
  {$ENDIF MACOS}
  ,StrUtils   // used for DelSpace1 and tiEnclose
  ;

{$I tiUtils_Impl.inc}


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case sensitive.
function tiStrTran(AValue, ADel, AIns : string): string;
var
  i : integer;
  sToChange : string;
begin
  result := '';
  sToChange := AValue;
  i := pos(ADel, sToChange);
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + AIns;
    delete(sToChange, 1, i+length(ADel)-1);
    i := pos(ADel, sToChange);
  end;
  result := result + sToChange;
end;


function tiStrTran1(AValue, ADel, AIns : string): string;
var
  // order of declaration is significant here - earlier declared local vars
  // get assigned to registers rather than pushed to stack
  LMatch, LValue, LResult: PChar;
  LLen: cardinal;
  LIns, LDel: PChar;
begin
  LLen := Length(ADel);
  // max possible length = every char in LValue requires substitution
  SetLength(result, Length(AIns) * Length(AValue));
  LValue := Pointer(AValue);

  // empty src begets empty result...
  if LValue = nil then
    exit;

  LResult := Pointer(result);
  LIns := Pointer(AIns);
  LDel := Pointer(ADel);

  // Advance through LValue looking for LDel instances...
  LMatch := StrPos(LValue, LDel);

  // terminate on no match
  while LMatch <> nil do
  begin

    // Copy all chars from current LValue into LResult up to match point,
    // moving both pointers along as well
    while LValue < LMatch do
    begin
      LResult^:= LValue^;
      Inc(LResult);
      Inc(LValue);
    end;

    // Add in LIns and move LResult beyond it
    LResult := StrECopy(LResult, LIns);

    // move LValue beyond instance of  LDel
    Inc(LValue, LLen);
    LMatch := StrPos(LValue, LDel);
  end;

  // no more matches  - copy remainder of LValue to LResult
  StrCopy(LResult, LValue);
  // resync (Delphi string) length of result after pchar manipulations...
  result := PChar(result);
end;


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case in-sensitive.
function tiCIStrTran(AValue, ADel, AIns : string): string;
var
  i: integer;
  sToChange: string;
begin
  Result := '';
  sToChange := AValue;
  i := pos(upperCase(ADel), upperCase(sToChange));
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + AIns;
    delete(sToChange, 1, i+length(ADel)-1);
    i := pos(upperCase(ADel), upperCase(sToChange));
  end;
  Result := Result + sToChange;
end;

function tiNumToken(const AValue, ASeparator : string): integer;
var
  LCurrent, LCurrSeparator: PChar;
begin
  // Token index is 1-based
  Result := 1; //(implicit value) unless set otherwise

  // Check for special cases
  if (Length(AValue) = 0) then exit(0); //==>
  if Length(ASeparator) = 0 then exit; //==>

  LCurrent := Pointer(AValue);

  while LCurrent^ <> #0 do
  begin
    LCurrSeparator := Pointer(ASeparator);
    // look for start of sub in str or end of str
    while (LCurrent^ <> LCurrSeparator^) and (LCurrent^ <> #0) do
      Inc(LCurrent);
    // check end of str
    if LCurrent^ <> #0 then
    begin
      // look for end of sub in str or end of sub or end of str
      while (LCurrent^ = LCurrSeparator^) and (LCurrent^ <> #0) do
      begin
        Inc(LCurrent);
        Inc(LCurrSeparator);
      end;
      // success if sub^ = #0
      if LCurrSeparator^ = #0 then
        Inc(Result);
    end;
  end;
end;

function tiToken(const AValue, ASeparator: string; const ATokenIndex : integer): string;
var
  LCurrent: PChar;
  LRemainingTokens: integer;
  LPrevTokenStart: PChar;
  LFoundNextToken: boolean;

  function SeekNextTokenStart: boolean;
  var
    LSeparator: PChar;
  begin
    LSeparator := Pointer(ASeparator);

    while (LCurrent^ <> #0) and (LSeparator^ <> #0) do
    begin
      // reset to start of separator
      LSeparator := Pointer(ASeparator);

      //seek next start of separator
      while (LCurrent^ <> #0) and (LCurrent^ <> LSeparator^) do
        Inc(LCurrent);

      //skip to end of separator
      while (LCurrent^ <> #0) and (LSeparator^ <> #0)
        and (LCurrent^ = LSeparator^) do
      begin
        Inc(LCurrent);
        Inc(LSeparator);
      end;

    end;
    // at token start if separator chars exhausted
    Result := (LSeparator^ = #0);
  end;

begin
  // Token index is 1-based
  Result := ''; //(implicit value) unless set otherwise

  // Check for special cases
  if (Length(AValue) = 0) or (ATokenIndex < 1) then exit;
  if Length(ASeparator) = 0 then exit(AValue);

  LCurrent := Pointer(AValue);
  LRemainingTokens := ATokenIndex;

  repeat
    Dec(LRemainingTokens);
    LPrevTokenStart := LCurrent;
    LFoundNextToken := SeekNextTokenStart;
  until not (LFoundNextToken and (LRemainingTokens > 0));

  if (LRemainingTokens = 0) then
    if LFoundNextToken then
      SetString(Result, LPrevTokenStart,
        LCurrent - LPrevTokenStart - Length(ASeparator))
    else
      SetString(Result, LPrevTokenStart,
        LCurrent - LPrevTokenStart);
end;

function tiSpace(ALen : integer): string;
var i : integer;
    sString : string;
begin
  sString := '';
  for i := 1 to ALen do
    sString := sString + ' ';
  result := sString;
end;


function tiPadR(AValue: string; const ALen: integer; const APadChar: Char = ' '): string;
var
  ls : string;
begin
  ls := AValue;
  if length(ls) < ALen then begin
    while length(ls) < ALen do begin
      ls := ls + APadChar;
    end;
  end
  else if length(ls) > ALen then
    ls := copy(ls, 1, ALen);
  result := ls;
end;


function tiPadL(AValue: string; const ALen: integer; const APadChar: Char = ' '): string;
begin
  if length(AValue) < ALen then begin
    while length(AValue) < ALen do begin
      AValue := APadChar + AValue;
    end;
  end
  else if length(AValue) > ALen then
    AValue := copy(AValue, length(AValue)-ALen+1, ALen);
  result := AValue;
end;


function  tiPadC(AValue: string; const ALen : integer; const APadChar: Char = ' '): string;
var
  liPad : integer;
begin
  if Length(AValue) = ALen then
  begin
    result := AValue;
    Exit; //==>
  end;
  
  if Length(AValue) + 1 = ALen then
  begin
    result := AValue + APadChar;
    Exit; //==>
  end;

  if Length(AValue) > ALen then
    raise exception.Create('Can not call tiPadC when the string to be ' +
                            'padded is longer than the target length');

  liPad := (ALen - length(AValue)) div 2;
  if liPad > 0 then
    result := DupeString(APadChar, liPad) + AValue + DupeString(APadChar, liPad);

  // To handle situations where ALen < length(AValue) and
  // when length(AValue) is an odd number
  result := tiPadR(result, ALen, APadChar);
end;


function tiPad0(const AValue: string; const ALen: integer): string;
var
  ls: string;
begin
  ls := AValue;
  if Length(ls) < ALen then
  begin
    while Length(ls) < ALen do
    begin
      ls := '0' + ls;
    end;
  end
  else if Length(ls) > ALen then
  begin
    // This is a somewhat bizarre - if ALen < Length(AValue) - trim from
    // the right. However, this is how it's used to set the
    // decimal part of the result in TtiFieldCurrency.GetAsString
    // Ideally, tiPad0('123', 2) would raise an exception.
    ls := Copy(ls, Length(ls)-ALen, ALen);
  end;
  Result := ls;
end;


function tiRemoveLeading0(AValue : string): string;
var i : integer;
begin
  for i := 1 to length(AValue) do begin
    if copy(AValue, 1, 1) = '0' then begin
      AValue := copy(AValue, 2, length(AValue) - 1);
    end else begin
      break;
    end;
  end;
  result := AValue;
end;


function  tiYear(AValue : TDateTime = 0.0): Word;
var
  lD, lM : Word;
  lDate : TDateTime;
begin
  if AValue = 0.0 then
    lDate := Date
  else
    lDate := AValue;
  DecodeDate(lDate, Result, lM, lD);
end;


function tiDateWithinRange(const ADate, AFrom, ATo: TDateTime): Boolean;
var
  lDate: TDateTime;
begin
  lDate := Trunc(ADate);
  Result := (lDate >= Trunc(AFrom)) and (lDate <= Trunc(ATo));
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

function tiAusFinancialYearDayCount(const AYear: Word): Word;
var
  LStart: TDateTime;
  LEnd: TDateTime;
begin
  LStart:= EncodeDate(AYear-1, 07, 01);
  LEnd:= EncodeDate(AYear, 07, 01);
  result:= Trunc(LEnd-LStart);
end;

function tiRoundDateToPreviousMinute(const ADateTime: TDateTime): TDateTime;
var
  LDate: TDateTime;
  LH: Word;
  LM: Word;
  LS: Word;
  LMS: Word;
begin
  LDate:= Trunc(ADateTime);
  DecodeTime(ADateTime, LH, LM, LS, LMS);
  Result:= LDate +
      ((LH * MinsPerHour * SecsPerMin * MSecsPerSec +
        LM * SecsPerMin * MSecsPerSec) / MSecsPerDay); // Allow for values out of range
end;

function tiRoundDateToNextMinute(const ADateTime: TDateTime): TDateTime;
var
  LDate: TDateTime;
  LH: Word;
  LM: Word;
  LS: Word;
  LMS: Word;
begin
  LDate:= Trunc(ADateTime);
  DecodeTime(ADateTime, LH, LM, LS, LMS);
  // Round up a minute unless within a millisecond past the minute
  if (LS <> 0) or (LMS <> 0) then
    Inc(LM);
  // Calculate absolute minute
  // Note: Whatever the outcome above we need to drop sub-milliseconds. ie. If
  // LS and LMS are both 0, ADateTime could still be say 200 microseconds past
  // the minute which we need to drop.
  Result := LDate +
    ((LH * MinsPerHour * SecsPerMin * MSecsPerSec +
      LM * SecsPerMin * MSecsPerSec) / MSecsPerDay); // Allow for values out of range
end;

function tiRoundDateToNearestMinute(const ADateTime: TDateTime): TDateTime;
const
  CHalfMinuteOfDay = 1/24/60/2;
begin
  result := tiRoundDateToPreviousMinute(ADateTime + CHalfMinuteOfDay);
end;

function tiNextInterval(const ADateTime: TDateTime;
  const AInterval: TtiTimeInterval): TDateTime;
var
  LH: Word;
  LM: Word;
  LS: Word;
  LMS: Word;
begin
  result := Trunc(ADateTime);
  if AInterval = titiDay then
    result := result + 1
  else
  begin
    DecodeTime(ADateTime, LH, LM, LS, LMS);
    case AInterval of
      titiHour:
        begin
          Inc(LH);
          LM := 0;
          LS := 0;
          LMS := 0;
        end;
      titiMinute:
        begin
          Inc(LM);
          LS := 0;
          LMS := 0;
        end;
      titiSecond:
        begin
          Inc(LS);
          LMS := 0;
        end;
      titiMillisecond:
        Inc(LMS);
    end;
    result := result + EncodeTime(LH, LM, LS, LMS);
  end;
end;

procedure tiDecodeDateTimeToNearestMilliSecond(const ADateTime: TDateTime;
  out ADays: Integer; out AHours: Word; out AMins: Word; out ASecs: Word;
  out AMSecs: Word);
var
  LRemainder: TDateTime;
const
  cHalfMSec: Extended = 1/24/60/60/1000/2;
begin
  LRemainder := ADateTime + cHalfMSec;
  ADays := Trunc(LRemainder);
  LRemainder := (LRemainder - ADays) * 24;
  AHours := Trunc(LRemainder);
  LRemainder := (LRemainder - AHours) * 60;
  AMins := Trunc(LRemainder);
  LRemainder := (LRemainder - AMins) * 60;
  ASecs := Trunc(LRemainder);
  LRemainder := (LRemainder - ASecs) * 1000;
  AMSecs := Trunc(LRemainder);
end;

function tiRoundDateTimeToNearestMilliSecond(const ADateTime: TDateTime): TDateTime;
var
  LDays: Integer;
  LHours: Word;
  LMins: Word;
  LSecs: Word;
  LMSecs: Word;
begin
  tiDecodeDateTimeToNearestMilliSecond(ADateTime, LDays, LHours, LMins, LSecs, LMSecs);
  result := LDays +
      ((LHours * MinsPerHour * SecsPerMin * MSecsPerSec +
        LMins * SecsPerMin * MSecsPerSec +
        LSecs * MSecsPerSec +
        LMSecs) / MSecsPerDay); // Allow for values out of range
end;

function tiRoundDateTimeToNearestInterval(const ADateTime: TDateTime;
  const AInterval: TDateTime): TDateTime;
begin
  if AInterval <> 0 then
    Result := Round(ADateTime/AInterval) * AInterval
  else
    Result := ADateTime;
end;

function tiCompareDateTimeToMilliSecond(const AFirstDateTime: TDateTime;
  const ASecondDateTime: TDateTime): TValueRelationship;
var
  LFirstDateTime: TDateTime;
  LSecondDateTime: TDateTime;
begin
  // Calculate the two date times to the nearest millisecond.
  LFirstDateTime := tiRoundDateTimeToNearestMilliSecond(AFirstDateTime);
  LSecondDateTime := tiRoundDateTimeToNearestMilliSecond(ASecondDateTime);
  // Values are rounded and encoded identically so just compare values.
  // Do not use CompareDateTime as it can return the wrong result.
  if LFirstDateTime < LSecondDateTime then
    result := LessThanValue
  else if LFirstDateTime > LSecondDateTime then
    result := GreaterThanValue
  else
    result := EqualsValue;
end;

function tiCompareDateTimeToInterval(const AFirstDateTime: TDateTime;
  const ASecondDateTime: TDateTime; const AInterval: TDateTime): TValueRelationship;
var
  LFirstDateTime: TDateTime;
  LSecondDateTime: TDateTime;
begin
  // Calculate the two date times to the nearest interval.
  LFirstDateTime := tiRoundDateTimeToNearestInterval(AFirstDateTime, AInterval);
  LSecondDateTime := tiRoundDateTimeToNearestInterval(ASecondDateTime, AInterval);
  // Values are rounded and encoded identically so just compare values.
  // Do not use CompareDateTime as it can return the wrong result.
  if LFirstDateTime < LSecondDateTime then
    result := LessThanValue
  else if LFirstDateTime > LSecondDateTime then
    result := GreaterThanValue
  else
    result := EqualsValue;
end;

function tiIncludesTime(const ADateTime: TDateTime;
  const AStartDateTime: TDateTime; const AEndDateTime: TDateTime): boolean;
begin
  // Millisecond precision - allows for slight differences in numerical values
  // to be treated as the same time.
  result :=
      (tiCompareDateTimeToMilliSecond(ADateTime, AStartDateTime) >= 0) and
      (tiCompareDateTimeToMilliSecond(ADateTime, AEndDateTime) < 0);
end;

function tiOverlapsTime(const AStartDateTimeA: TDateTime;
  const AEndDateTimeA: TDateTime; const AStartDateTimeB: TDateTime;
  const AEndDateTimeB: TDateTime): boolean;
begin
  // Millisecond precision - allows for slight differences in numerical values
  // to be treated as the same time.
  result :=
      (tiCompareDateTimeToMilliSecond(AStartDateTimeB, AEndDateTimeA) < 0) and
      (tiCompareDateTimeToMilliSecond(AEndDateTimeB, AStartDateTimeA) > 0);
end;

function tiWeekNumber(const ADate: TDateTime): Byte;
var
  LD, LM, LY: Word;
  LDayCount: Word;
  LFirstDayOfYear: TDateTime;
begin
  DecodeDate(ADate, LY, LM, LD);
  LFirstDayOfYear:= EncodeDate(LY, 1, 1);
  LDayCount:= Trunc(ADate) - Trunc(LFirstDayOfYear);
  Result:= (LDayCount div 7 ) + 1;
end;

function tiMixedCase(AValue : string): string;
var iToken : integer;
    i : integer;
    sBuffer : string;
begin
  iToken := tiNumToken(AValue, ' ');
  result := '';
  AValue := lowerCase(AValue);
  for i := 1 to iToken do begin
    sBuffer := tiToken(AValue, ' ', i);
    result := tiAddTrailingValue(result, ' ', true);
    result := result +
               upperCase(copy(sBuffer, 1, 1)) +
               copy(sBuffer, 2, length(sBuffer) - 1);
  end;
end;


function tiExtractFileNameOnly(AValue : string): string;
begin
  result := tiRemoveExtension(extractFileName(AValue));
end;


function tiRemoveExtension(AValue: string): string;
var
  i: integer;
  s: string;
begin
  s := tiFixPathDelim(AValue);
  i := tiLocateExtension(s);
  if i <> 0 then
    result := copy(s, 1, i - 1)
  else
    result := s;
end;

function tiSwapExt(const AFileName, AExt : string): string;
begin
  result := tiAddTrailingValue(tiRemoveExtension(AFileName), '.', false) + AExt;
end;

function tiExtractExtension(AValue: string): string;
var
  i: integer;
  s: string;
begin
  s := tiFixPathDelim(AValue);
  i := tiLocateExtension(s);
  if i <> 0 then
    result := copy(s, i+1, length(s) - i)
  else
    result := '';
end;


{ Linux behavior is different to Windows if the ATo file exists.
  Under Linux RenameFile silently removes the ATo file and then continues
  with the rename. }
function tiMoveFile(const AFrom, ATo: string): boolean;
begin
  result := RenameFile(AFrom, ATo);
end;


function tiGetFileSize(AValue : string): longInt;
var f: file of Byte;
begin
  AssignFile(f, AValue);
  Reset(f);
  result := FileSize(f);
  closeFile(f);
end;


function tiReplicate(const AValue : string; ARepCount : Word): string;
(*var
  LResult, LValue: PChar;
  LLen: cardinal;*)
begin
  Result:= DupeString(AValue, ARepCount);

(* if (ARepCount = 0) or (Pointer(AValue) = nil) then
    exit;

  LLen := Length(AValue);
  SetLength(Result, LLen * ARepCount);
  LResult  := Pointer(Result);
  LValue   := Pointer(AValue);

  while ARepCount <> 0 do
  begin
    Move(LValue^, LResult^, LLen);
    Inc(LResult, LLen);
    Dec(ARepCount);
  end;   *)
end;


{ Check that the string contained in a TControl can be converted
  to a valid float. Pass the control as a parameter and the
  function will perform the test and retrun focus to the control
  if the test fails.

  Currently programmed to test mastEdits, some more work is necessary
  to test other controls. }
{
function tiIsControlValidFloat(pControl : TControl): boolean;
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


function _RemoveNonNumChars(AValue : string): string;
begin
  result := AValue;
  result := tiStrTran(result, ' ', '');
  result := tiStrTran(result, ',', '');
  result := tiStrTran(result, '$', '');
end;


function tiStrToInt(const AValue : string): integer;
begin
  try
    result := strToInt(_RemoveNonNumChars(AValue));
  except
    result := 0;
  end;
end;


function tiStrToIntPrefix(const AValue : string): integer;
var
  s: string;
  i: Integer;
begin
  s := AValue;
  for i := 1 to Length(s) do
    if (s[i] < '0') or (s[i] > '9') then
    begin
      s := Copy(s, 1, i - 1);
      break;
    end;

  try
    result := StrToInt(s);
  except
    result := 0;
  end;
end;


function tiStrToFloat(const AValue : string): Extended;
begin
  try
    result := strToFloat(_RemoveNonNumChars(AValue));
  except
    result := 0;
  end;
end;


function tiDateToStr(const pdtValue : TDateTime; const psFormat : string = csWinDateFormat): string;
begin
  result := FormatDateTime(psFormat, pdtValue);
end;


function tiDateTimeToStr(const ADateTime : TDateTime): string;
begin
  result := FormatDateTime(csWinDateTimeFormat, ADateTime);
end;


function  tiTimeToStr(const ADateTime  : TDateTime;
                       const ATimeFormat : string = ''): string;
begin
  if ATimeFormat = '' then
    result := FormatDateTime(csWinTimeFormat, ADateTime)
  else
    result := FormatDateTime(ATimeFormat, ADateTime)
end;


function tiSafeDiv(ANum, ADenom: longInt): longInt;
begin
  if ADenom <> 0 then 
    result := ANum div ADenom
  else
    result := 0;
end;


function tiIntToStrHide0(const AValue : Int64): string;
begin
  if AValue = 0 then begin
    result := '';
  end else begin
    result := intToStr(AValue);
  end;
end;


function  tiIntToCommaStr(const AValue : Int64): string;
begin
  result := tiFloatToCommaStr(AValue, 0);
end;


function tiFloatToCurrencyHide0(const AValue: Extended): string;
begin
  if (AValue < 0.005) and (AValue > -0.005) then
    result := ''
  else
    result := tiFloatToCurrency(AValue);
end;


function tiFloatToCurrency(const AValue: Extended): string;
var
  {$IF Defined(DELPHI2009) or Defined(DELPHI2010)}
    FormatSettings: TFormatSettings;
  {$IFEND}
  e: Extended;
begin
  try
    {$IF Defined(DELPHI2009) or Defined(DELPHI2010)}
      GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
    {$IFEND}

    // FPC's FormatFloat uses Bankers Rounding which differs from Delphi.
    // Setting the precision resolves this issue.
    e := tiSetPrecision(AValue, 2);
    result := FormatFloat(FormatSettings.CurrencyString + ' #,##0.00', e);
  except
    result := FormatSettings.CurrencyString + ' 0' + FormatSettings.DecimalSeparator + '00';
  end;
end;


function tiBooleanToStr(const AValue : boolean): string;
begin
  if AValue then
    result := cTrueDB
  else
    result := cFalseDB;
end;


function tiBoolToStr(const AValue : boolean): string;
begin
  result := tiBooleanToStr(AValue);
end;

function  tiBoolToYNString( const AValue : boolean)  : string;
begin
  if AValue then
    result := 'Y'
  else
    result := 'N';
end;

function  tiStrToBool(const AValue : string): boolean;
var
  ls : string;
begin
  ls := upperCase(AValue);
  if (ls = 'TRUE') or
     (ls = 'T'  ) or
     (ls = 'YES') or
     (ls = 'Y'  ) or
     (ls = '1'  ) then
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


function tiIsNearEnoughDecimalPlaces(const AValue1, AValue2: Double; ADecimalPlaces: Integer): Boolean;
begin
  result := SimpleRoundTo(AValue1, -ADecimalPlaces) =
      SimpleRoundTo(AValue2, -ADecimalPlaces);
end;


function tiIsDateTimeNearEnough(const AVal1: TDateTime; const AVal2: TDateTime;
  const AProximity: TDateTime): Boolean;
begin
  result:= Abs(AVal1-AVal2) <= AProximity;
end;

function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Extended): Extended;
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


function  tiVariantArrayToString(AValue: Variant): string;
  //---------
  procedure appendVariantToStringList(pStringList: TStringList;
      pVariant: Variant; var pIndent: integer);
  var
    i: integer;
    iLow: integer;
    iHigh: integer;
  begin
    if VarIsArray(pVariant) then
    begin
      iLow := varArrayLowBound(pVariant, 1);
      iHigh := varArrayHighBound(pVariant, 1);
      for i := iLow to iHigh do
      begin
        inc(pIndent);

        if i = iLow then
          pStringList.add(tiSpace(pIndent*3) + '[');

        appendVariantToStringList(pStringList, pVariant[i], pIndent);

        if i = iHigh then
          pStringList.add(tiSpace(pIndent*3) + ']');

        dec(pIndent);
      end;
    end
    else
    begin
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
    appendVariantToStringList(lStringList, AValue, pIndent);
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
  varUString	Reference to a dynamically allocated unicode Pascal string (type String).
}
function tiIsVariantOfType(AVariant : Variant; AVarType : TVarType): boolean;
var
  xVT : TVarType;
  xVTHigh : TVarType;
//  xVTLow : TVarType;
begin
//  result := (varType(pVariant) and pVarType) = pVarType;
// Contr: VarType is varDate = 0007, pVarType is varInteger=0003.
// 0007 and 0003 = 0003. WRONG!

  xVT:=VarType(AVariant);
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
  Result:=(xVT=AVarType) or ((xVTHigh=AVarType) and (xVTHigh<>varEmpty));
end;


function tiVariantAsStringDef(const AVariant: Variant; const ADefault: string): string;
begin
  if AVariant = Null then
    result := ADefault
  else
    result := AVariant;
end;


function  tiAddTrailingValue(const ALine, AValue : string; ADuplicates : boolean = true): string;
begin
  if ALine = '' then
  begin
    result := ALine;
    Exit; //==>
  end;

  if ADuplicates then
  begin
    result := ALine + AValue;
    Exit; //==>
  end;

  if (not SameText(Copy(ALine,
                           Length(ALine) - Length(AValue) + 1,
                           Length(AValue)),
                     AValue)) then
    result := ALine + AValue
  else
    result := ALine;

end;


function  tiRemoveTrailingValue(ALine, AValue : string): string;
var
  lLHS : integer;
  lRHS : integer;
begin
  lLHS := length(ALine) - Length(AValue) + 1;
  lRHS := Length(AValue);

  if copy(ALine, lLHS, lRHS) = AValue then
    result :=
      copy(ALine, 1, lLHS - 1)
  else
    result := ALine;
end;


function tiAddTrailingComma(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ',', true);
end;


function  tiAddTrailingAnd(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' and ', false);
end;


function  tiAddTrailingOr(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' or ', false);
end;


function  tiAddTrailingSpace(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' ', true);
end;


function tiDateToPreviousWeekDayDate(AValue : TDateTime): TDateTime;
var iDay : integer;
begin
  result := AValue;
  iDay  := dayOfWeek(result);
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


function tiAddTrailingSlash(const AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, PathDelim, false);
end;


function tiRemoveTrailingSlash(const AValue : string): string;
begin
  result := tiRemoveTrailingValue(tiFixPathDelim(AValue), PathDelim);
end;


function tiRemoveLeadingSlash(const AValue: string): string;
var
  lStr: string;
begin
  lStr := tiFixPathDelim(AValue);
  if copy(lStr, 1, 1) = PathDelim then begin
    result := copy(lStr, 2, length(lStr) - 1);
  end else begin
    result := lStr;
  end;
end;


function tiPosR(ATarget, AValue : string): integer;
var i : integer;
    iTargetLength : integer;
begin
  i := length(AValue);
  iTargetLength := length(ATarget);
  while i > 0 do begin
    if copy(AValue, i, iTargetLength) = ATarget then begin
      break; //==>
    end;
    dec(i);
  end;
  result := i;
end;



function tiRemoveDrive(AValue : string): string;
var
  sDrive : string;
begin
  sDrive := extractFileDrive(AValue);
  if sDrive <> '' then begin
    result := copy(AValue, length(sDrive)+1, length(AValue) - length(sDrive));
  end else begin
    result := AValue;
  end;
end;


function  tiRemoveDirectory(const AFileName: string; const AToRemove: string): string;
var
  lToRemove: string;
  lFileName: string;
begin
  lToRemove := UpperCase(tiRemoveTrailingSlash(AToRemove));
  lFileName := UpperCase(AFileName);
  if Pos(lToRemove, lFileName) <> 1 then
    raise EtiOPFProgrammerException.CreateFmt(cErrorRemoveDirectory, [AToRemove, lFileName]);
  Result := Copy(AFileName, Length(lToRemove)+1, Length(lFileName));
  while Copy(Result, 1, 1) = PathDelim do
    Result := Copy(Result, 2, Length(Result));
end;


function tiIsBitSet(const AValue: longint; const ABit: byte): boolean;
begin
  result := (AValue and (1 shl ABit)) <> 0;
end;


function tiBitToString(const AValue: longint; const ABit: byte): string;
begin
  if tiIsBitSet(AValue, ABit) then
    result := '1'
  else
    result := '0';
end;


function tiInt32ToBinString(const AValue : longInt): string;
var i : integer;
begin
  result := '';
  for i := 31 downto 0 do begin
    result := result + tiBitToString(AValue, i);
  end;
end;


function tiStrToHex(const AString: string; const AHexPrefix: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
    Result := Result + AHexPrefix + IntToHex(Ord(AString[I]), SizeOf(Char) * 2);
end;


function tiAddEXEPath(const AFileName: string): string;
begin
  Result :=
    ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) +
      ExtractFileName(AFileName));
end;


procedure tiDirectoryTreeToStringList(const AStartDir: string;
    const ADirList: TStringList; ARecurse: boolean);
  //-----------
  procedure _ReadDirectories(const psStartDir: string; slTree: TStringList;
      bRecurse: boolean);
    //-----------
    procedure _AddIfDir(searchRec: TSearchRec; sStartDir: string;
        slTree: TStringList; bRecurse: boolean);
    begin
        if ((searchRec.Attr and faDirectory) <> 0) and
           (searchRec.name <> '.') and
           (searchRec.name <> '..') then
        begin
          slTree.add(sStartDir + searchRec.name);
          if bRecurse then begin
            _ReadDirectories(sStartDir + searchRec.name, slTree, bRecurse);
          end;
        end;
    end;
  var
    lsStartDir: string;
    SearchRec: TSearchRec;
  begin
    lsStartDir := tiAddTrailingSlash(psStartDir);
    try
      if SysUtils.FindFirst(lsStartDir + AllFilesWildCard, faAnyFile, SearchRec) = 0 then
      begin
        _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        while SysUtils.FindNext(searchRec) = 0 do
        begin
          _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        end;
      end;
    finally
      sysUtils.FindClose(SearchRec);
    end;
  end;
var
  lStartDir: string;
  oldSorted: boolean;
begin
  lStartDir := tiRemoveTrailingSlash(AStartDir);
  ADirList.Clear;
  oldSorted := ADirList.Sorted; // store original value
  ADirList.Sorted := True;

  if not DirectoryExists(lStartDir) then
    Exit; //==>

  ADirList.Add(lStartDir);
  _ReadDirectories(lStartDir, ADirList, ARecurse);
  ADirList.Sorted := oldSorted; // restore original value
end;


{$IFDEF DELPHI6ORAVOVE} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
procedure tiFilesToStringList(const AStartDir, AWildCard : string;
                               const AResults : TStringList;
                               const ARecurse : boolean);
  // Locally visible proc
  procedure AddFile(SearchRec: TSearchRec; sStartDir, pStrWildCard: string; slTree: TStringList; bRecurse: boolean);
  begin
    // We only want files, not directories
    if (SearchRec.Attr and faDirectory) <> 0 then
      Exit; //==>
    slTree.Add(sStartDir + SearchRec.Name);
  end;

var
  SearchRec: TSearchRec;
  lsStartDir: string;
  lslDirTree: TStringList;
  i: integer;
begin
  AResults.Clear;
  AResults.Sorted := True;
  lsStartDir := tiAddTrailingSlash(AStartDir);
  lslDirTree := TStringList.Create;
  try
    if ARecurse then
      tiDirectoryTreeToStringList(lsStartDir, lslDirTree, ARecurse)
    else
      lslDirTree.Add(lsStartDir);
    for i := 0 to lslDirTree.Count-1 do
    begin
      lsStartDir := tiAddTrailingSlash(lslDirTree.Strings[i]);
      try
        {$IFDEF MSWINDOWS}
        if tiWin32FindFirstFile(lsStartDir + AWildCard, SearchRec) = 0 then
        {$ENDIF MSWINDOWS}
        {$IFDEF UNIX}
        { FPC under Linux has some bug in <= v2.2.5 so faAnyFile is all we
          can use instead of the following:  faAnyFile-faSysFile-faDirectory, }
        if SysUtils.FindFirst(lsStartDir + AWildCard, faAnyFile, SearchRec) = 0 then
        {$ENDIF UNIX}
        begin
          repeat
            AddFile(SearchRec, lsStartDir, AWildCard, AResults, ARecurse);
          until SysUtils.FindNext(SearchRec) <> 0;
        end;
      finally
        SysUtils.FindClose(SearchRec);
      end;
    end;
  finally
    lslDirTree.Free;
  end;
end;
{$IFDEF DELPHI6ORAVOVE}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}


procedure tiXCopy(const ASource, ATarget: string);
var
  lFiles: TStringList;
  i: Integer;
  lErrors: string;
  lSource: string;
  lTarget: string;
begin
  lFiles := TStringList.Create;
  try
    tiForceDirectories(ATarget);
    tiFilesToStringList(ASource, AllFilesWildCard, lFiles, true);
    for i := 0 to lFiles.Count - 1 do
    begin
      lSource := lFiles.Strings[i];
      lTarget := tiAddTrailingSlash(ATarget) + tiRemoveDirectory(lSource, ASource);
      tiForceDirectories1(ExtractFileDir(lTarget));
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

procedure tiDeleteOldFiles(const ADirectory, AWildCard: string;
  const ADaysOld: Integer; const ARecurseDirectories: Boolean;
  const ADeleteEmptyDirectories: Boolean);
var
  Lsl: TStringList;
  i : Integer;
  LFileModifiedDate: TDateTime;
begin
  Lsl:= TStringList.Create;
  try
    tiFilesToStringList(ADirectory, AWildCard, Lsl, ARecurseDirectories);
    for i := 0 to Lsl.Count - 1 do
    begin
      LFileModifiedDate := Trunc(tiReadFileDate(Lsl.Strings[i]));
      if (LFileModifiedDate < Date - ADaysOld) and
         (not tiIsFileReadOnly(lsl.Strings[i])) then
        if not SysUtils.DeleteFile(Lsl.Strings[i]) then
          raise EtiOPFFileSystemException.CreateFmt(cErrorCanNotDeleteFile, [Lsl.Strings[i]]);
    end;
    Lsl.Clear;

    if ADeleteEmptyDirectories then
    begin
      tiDirectoryTreeToStringList(ADirectory, lsl, ARecurseDirectories);
      for i := lsl.Count - 1 downto 0 do
        SysUtils.RemoveDir(lsl.Strings[i]);
    end;
  finally
    Lsl.Free;
  end;
end;

{$HINTS OFF}
function tiDeleteFile(const AFileName: string): boolean;
begin
  result:= SysUtils.DeleteFile(AFileName);
end;
{$HINTS ON}

function tiHasSubDirectory(AStartDir : string): boolean;
var
  slTree : TStringList;
begin
  slTree := TStringList.Create;
  try
    tiDirectoryTreeToStringList(tiFixPathDelim(AStartDir), slTree, false);
    result := slTree.count > 1;
  finally
    slTree.free;
  end;
end;


function tiExtractDirToLevel(const AFileName : TFileName; ALevel : byte): TFileName;
var
  i : integer;
begin
  result := '';
  for i := 0 to ALevel+1 do
  begin
    if result <> '' then
      result := tiAddTrailingSlash(result);
    result := result + tiToken(AFileName, PathDelim, i);
  end;
  result := tiRemoveTrailingSlash(result);
  {$IFDEF UNIX}
  result := PathDelim + result;
  {$ENDIF}
end;

procedure tiForceDirectories(const AValue : TFileName);
var
  lDirName: string;
  s: string;
begin
  s := tiFixPathDelim(AValue);
  if tiLocateExtension (s) <> 0 then
    lDirName := ExtractFilePath(ExpandFileName(s))
  else
    lDirName := s;
  if not ForceDirectories(lDirName) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorUnableToCreateDirectory, [lDirName]);
end;

procedure tiForceDirectories1(const AValue : TFileName);
begin
  if not ForceDirectories(AValue) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorUnableToCreateDirectory, [AValue]);
end;

// ToDo: Must add code to handle hidden files.
function tiForceRemoveDir(const AValue : TFileName): boolean;
var
  lsl : TStringList;
  i : integer;
begin
  try
    lsl := TStringList.Create;
    try
      tiFilesToStringList(AValue, AllFilesWildCard, lsl, true);
      for i := 0 to lsl.Count - 1 do
      begin
        if tiIsFileReadOnly(lsl.Strings[i]) then
          tiSetFileReadOnly(lsl.Strings[i], false);
        SysUtils.DeleteFile(lsl.Strings[i]);
      end;
      tiDirectoryTreeToStringList(AValue, lsl, true);
      for i := lsl.Count - 1 downto 0 do
        SysUtils.RemoveDir(lsl.Strings[i]);
      result := not DirectoryExists(AValue);
    finally
      lsl.Free;
    end;
  except
    on e:exception do
      result := false;
  end;
end;


function tiJoinPath(const ALeftSide, ARightSide: string): string;
var
  lLeftSide, lRightSide: string;
begin
  lLeftSide  := tiFixPathDelim(ALeftSide);
  lRightSide := tiFixPathDelim(ARightSide);
  Result := tiRemoveTrailingSlash(tiAddTrailingSlash(lLeftSide) + lRightSide);
end;


function tiJoinPath(const AElements: array of string): string;
var
  I: Integer;
begin
  if Length(AElements) > 0 then
  begin
    Result := tiRemoveTrailingSlash(AElements[0]);
    for I := 1 to High(AElements) do
      Result := tiJoinPath(Result, AElements[I]);
  end
  else
    Result := '';
end;


function tiFloatToStr(const AValue : Extended;
    const APrecision : integer = 3): string;
begin
  result := _tiFloatToStr(AValue, APrecision, '###0');
end;


function  tiFloatToCommaStr(const AValue: Extended;
    const APrecision: integer = 3): string;
begin
  result := _tiFloatToStr(AValue, APrecision, '#,##0');
end;


function _tiFloatToStr(const AValue: Extended; const APrecision: integer;
    const AFormat: string): string;
var
  lsFormat: string;
begin
  lsFormat := AFormat;
  if APrecision <> 0 then
    lsFormat := lsFormat + '.' + tiReplicate('0', APrecision);
  try
    result := formatFloat(lsFormat, tiDecimalRoundDbl(AValue, APrecision));
  except
    on e:exception do
      raise exception.Create('Unable to format floating point number. ' + Cr(2) +
                              'Called in tiFloatToStr() ' +
                              'Format mask <' + lsFormat + '>' + Cr +
                              'Error message: ' + e.message);
  end;
end;


function  tiSafeDiv(ANum, ADenom : Extended): Extended;
begin
  if ADenom <> 0 then begin
    result := ANum / ADenom;
  end else begin
    result := 0;
  end;
end;

function  tiRound(const AValue : Extended): Int64;
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
function tiWildcardMatch(const ASource, APattern: String; const ACaseSensitive: boolean = false): Boolean;

  function MatchPatternStr(const psElement, APattern: String): Boolean;
  var PatternChar: Char;
  begin
    if (APattern = '*') then Result := True
    else if (psElement = EmptyStr) then Result := (APattern = EmptyStr)
    else begin
      if (Copy(APattern, 1, 1) <> EmptyStr) then PatternChar := Copy(APattern, 1, 1)[1]
      else PatternChar := #0;

      case PatternChar of
      '*': begin
        if MatchPatternStr(psElement, Copy(APattern, 2, Length(APattern))) then Result := True
        else Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), APattern);
      end;//'*'
      '?': Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(APattern, 2, Length(APattern)));
      else
        if Copy(psElement, 1, 1) = Copy(APattern, 1, 1) then
          Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(APattern, 2, Length(APattern)))
        else Result := False;
      end;//case
    end;//else
  end;

begin
  if ACaseSensitive then
    Result := MatchPatternStr(ASource, APattern)
  else
    Result := MatchPatternStr(UpperCase(ASource), UpperCase(APattern));
end;


{ TODO : 
This will work:
  tiSubStr('my <d>string</d>', '<d>', '</d>',);
but this will not:
  tiSubStr('my <u>long</e><d>string</e>', '<d>', '</e>',); }
function  tiSubStr(const ASource, AStartDelim, AEndDelim : string; AIndex : integer = 1): string;
var
  liStart : integer;
  liEnd  : integer;
  i: integer;
begin
  liStart := 0;
  if AIndex < 1 then
    AIndex := 1;

  for i := 1 to AIndex do
    liStart := PosEx(AStartDelim, ASource, liStart+1);

  result := '';

  if liStart <> 0 then
    liStart := liStart + Length(AStartDelim);

  liEnd := PosEx(AEndDelim, ASource, liStart);
  if liEnd <> 0 then
    liEnd := liEnd - 1;

  if (liStart = 0) or (liEnd = 0) then
    Exit; //==>

  result := Copy(ASource, liStart, liEnd - liStart + 1);
end;


function tiReadFileDate(const AFileName : string): TDateTime;
begin
  if not FileAge(AFileName, Result) then
    result:= 0;
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


procedure tiReadFileDateSize(const AFileName: string; var ADateTime: TDateTime;
    var AFileSize: integer);
begin
  ADateTime  := tiReadFileDate(AFileName);
  AFileSize := tiReadFileSize(AFileName);
end;


function tiDateTimeToFileDateTime(const ADateTime: TDateTime): TDateTime;
begin
  result := FileDateToDateTime(DateTimeToFileDate(ADateTime));
end;


function  Cr(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#13, ACount);
end;

{ Unix EOL character }
function  Lf(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#10, ACount);
end;


{ Windows EOL character }
function CrLf(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#13 + #10, ACount);
end;


{ OS dependent line ending character(s) }
function  tiLineEnd(const ACount : Byte = 1): string;
begin
  result := tiReplicate(cLineEnding, ACount);
end;


function  Tab(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#9, ACount);
end;


{
procedure tiSortList(AData : TList; pDoCompare : TSortCompare);
  procedure _SortList(AData : TList;
                       pDoCompare : TSortCompare);
  var
    i, j : integer;
    lTemp : TObject;
    liComp : integer;
  begin
    for i := AData.Count - 1 downto 0 do
      for j := 0 to AData.Count - 2 do begin
        pDoCompare(AData.Items[j], AData.Items[j + 1], liComp);
        if liComp > 0 then begin
          lTemp := AData.Items[j];
          AData.Items[j]:= AData.Items[j + 1];
          AData.Items[j + 1]:= lTemp;
        end;
      end;
  end;

begin
  _SortList(AData, pDoCompare);
end;
}

{$IFDEF IOS}
type
  AnsiString = Array of Byte;
{$ENDIF IOS}

{ $IFNDEF IOS}
procedure tiStringToStream(const AStr : string; const AStream : TStream);
var
  LAnsiStr: AnsiString;
  lBuffer : Pointer;
  lLen : integer;
begin
  LAnsiStr := AnsiString(AStr);
  lBuffer := Pointer(LAnsiStr);
  lLen := length(LAnsiStr);
  AStream.Size := 0;
  AStream.write(lBuffer^, lLen);
  AStream.Position := 0;
end;


procedure tiAppendStringToStream(const AStr : string; const AStream : TStream);
var
  LAnsiStr: AnsiString;
  lPC : Pointer;
begin
  Assert(AStream <> nil, 'Stream unassigned.');
  LAnsiStr := AnsiString(AStr);
  AStream.Position := AStream.Size;
  lPC := Pointer(LAnsiStr);
  AStream.WriteBuffer(lPC^, length(LAnsiStr));
end;


procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
var
  LAnsiStr: AnsiString;
  LRHLength: Longword;
  LRHPChar: PChar;
begin
  Assert(AStream <> nil, 'Stream unassigned.');
  Assert(APos <= AStream.Size, 'Pos > AStream.Size');
  Assert(APos <= AStream.Size, 'Pos > AStream.Size');

  LAnsiStr := AnsiString(AStr);

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
{$IFDEF IOS}
    AStream.WriteBuffer(LAnsiStr, Length(LAnsiStr));
{$ELSE}
    AStream.WriteBuffer(PAnsiChar(LAnsiStr)^, Length(LAnsiStr));
{$ENDIF IOS}
    AStream.WriteBuffer(LRHPChar^, LRHLength);
  finally
    FreeMem(LRHPChar);
  end;
end;


function  tiStreamToString(const AStream : TStream): string;
var
  LAnsiStr: AnsiString;
  lPos : integer;
begin
  lPos := AStream.Position;
  AStream.Position := 0;
  SetLength(LAnsiStr,  AStream.Size);
  AStream.Read(LAnsiStr[1], AStream.Size);
  AStream.Position := lPos;
  Result := string(LAnsiStr);
end;


function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string;
var
  LAnsiStr: AnsiString;
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
    SetLength(LAnsiStr, LSize);
    AStream.Read(LAnsiStr[1], LSize);
  finally
    AStream.Position:= LPos;
  end;

  Result := string(LAnsiStr);
end;
{ $ENDIF IOS}

procedure tiFileToStream(const AFileName : string; const AStream : TStream);
var
  lStream : TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    lStream.LoadFromFile(AFileName);
    tiCopyStream(lStream, AStream);
  finally
    lStream.Free;
  end;
end;


procedure tiStreamToFile(const AFileName : string; const AStream : TStream);
var
  lStream : TMemoryStream;
  lPosition: Integer;
begin
  lPosition := AStream.Position;
  try
    lStream := TMemoryStream.Create;
    try
      AStream.Position := 0;
      lStream.LoadFromStream(AStream);
      lStream.Position := 0;
      lStream.SaveToFile(AFileName);
    finally
      lStream.Free;
    end;
  finally
    AStream.Position := lPosition;
  end;
end;


procedure tiCopyStream(const AStreamFrom, AStreamTo : TStream);
begin
  AStreamFrom.Position := 0;
  AStreamTo.Size := 0;
  AStreamTo.CopyFrom(AStreamFrom, AStreamFrom.Size);
  AStreamTo.Position := 0;
  AStreamFrom.Position := 0;
end;


{$IFNDEF IOS}
procedure tiStringToFile(const AText : string; const AFileName: string);
var
  LAnsiStr: AnsiString;
  lStream : TFileStream;
  lpcText : PAnsiChar;
begin
  lStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    LAnsiStr := AnsiString(AText);
    lpcText := PAnsiChar(LAnsiStr);
    lStream.WriteBuffer(lpcText^, length(LAnsiStr));
  finally
    lStream.Free;
  end;
end;

procedure tiAppendStringToFile(const AText : string; const AFileName: string);
var
  LAnsiStr: AnsiString;
  lStream : TFileStream;
  lpcText : PAnsiChar;
begin
  if not FileExists(AFileName) then
    tiStringToFile(AText, AFileName)
  else begin
    lStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareExclusive);
    try
      LAnsiStr := AnsiString(AText);
      lpcText := PAnsiChar(LAnsiStr);
      LStream.Position:= LStream.Size;
      lStream.WriteBuffer(lpcText^, length(LAnsiStr));
    finally
      lStream.Free;
    end;
  end;
end;

function  tiFileToString(const AFileName : TFileName): string;
var
  LAnsiStr: AnsiString;
  lFileStream: TFileStream;
begin
  lFileStream := TFileStream.Create(AFileName,
                                     fmOpenRead or fmShareDenyNone);
  try
    SetLength(LAnsiStr,  lFileStream.Size);
    lFileStream.Read(LAnsiStr[1], lFileStream.Size);
  finally
    lFileStream.Free;
  end;

  result := string(LAnsiStr);
end;
{$ENDIF IOS}


function tiHasRTTI(AObject : TObject): boolean;
var
  lClass : TClass;
begin
  lClass := AObject.ClassType;
  result := tiHasRTTI(lClass);
end;


function tiHasRTTI(AClass : TClass): boolean;
var
  lClassInfo : Pointer;
begin
  lClassInfo := AClass.ClassInfo;
  result := lClassInfo <> nil;
end;


function  tiAddEllipsis(const AString : string; AWidth : integer = 20): string;
var
  lLen : integer;
begin
  lLen := Length(AString);
  if lLen <= AWidth then
    result := AString
  else if (lLen > AWidth) then
    result := TrimRight(Copy(AString, 1, AWidth - 3)) + '...'
  else
    result := TrimRight(Copy(AString, 1, lLen - 3)) + '...'
end;


function  tiTrim(const AString: string; ATrim: char): string;
var
  i, LLength: Integer;
begin
  LLength := Length(AString);
  i := 1;
  while (i <= LLength) and (AString[i] = ATrim) do Inc(i);
  if i > LLength then Result := '' else
  begin
    while AString[LLength] = ATrim do Dec(LLength);
    Result := Copy(AString, i, LLength - i + 1);
  end;
end;

function  tiTrimR(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
var
  li : integer;
begin
  if ACaseSensitive then
    li := pos(ATrim, AString)
  else
    li := pos(UpperCase(ATrim), UpperCase(AString));

  if li <> 0 then
    result := Copy(AString, 1, li - 1)
  else
    result := AString;
end;


function  tiTrimL(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
var
  li : integer;
begin
  if ACaseSensitive then
    li := pos(ATrim, AString)
  else
    li := pos(UpperCase(ATrim), UpperCase(AString));

  if li <> 0 then
  begin
    li := li+Length(ATrim);
    result := Copy(AString, li, Length(AString)-li+1)
  end
  else
    result := AString;
end;


function  tiRemoveCrLf(const AString : string): string;
begin
  result := tiStrTran(AString, Lf, '');
  result := tiStrTran(result,  Cr, ' ');
end;


function tiTrimTrailingWhiteSpace(const AString : string): string;
const
  cWhiteSpace = [ #13, #10, #32 ];
var
  i : integer;
  lTruncChar : integer;
begin
  lTruncChar := Length(AString);
  for i := Length(AString) downto 1 do
  begin
    if CharInSet(AString[i], cWhiteSpace) then
      Dec(lTruncChar)
    else
      Break; //==>
  end;
  result := Copy(AString, 1, lTruncChar);
end;

// Returns true if the email address is valid
// Author/Autor: Ernesto D'Spirito <edspirito@latiumsoftware.com>
// Accompanying article at http://www.howtodothings.com/showarticle.asp?article=297
function tiIsEMailAddressValid(const AValue: string): boolean;
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
  n := Length(AValue);
  i := 1;
  subdomains := 1;
  while (i <= n) do begin
    c := AValue[i];
    case State of
    STATE_BEGIN:
      if CharInSet(c, atom_chars) then
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
      else if not CharInSet(c, atom_chars) then
        break;
    STATE_QTEXT:
      if c = '\' then
        State := STATE_QCHAR
      else if c = '"' then
        State := STATE_QUOTE
      else if not CharInSet(c, quoted_string_chars) then
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
      if CharInSet(c, atom_chars) then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if CharInSet(c, letters) then
        State := STATE_SUBDOMAIN
      else
        break;
    STATE_SUBDOMAIN:
      if c = '.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else if c = '-' then
        State := STATE_HYPHEN
      else if not CharInSet(c, letters_digits) then
        break;
    STATE_HYPHEN:
      if CharInSet(c, letters_digits) then
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


// From:
// Naming Files, Paths, and Namespaces
// http://msdn.microsoft.com/en-us/library/aa365247.aspx?ppud=4
function tiIsFileNameCharValid(const AFileNameChar : char): boolean;
const
  CExcludedChars = [ '\', '/', ':', '*', '?', '"', '<', '>', '|' ];
begin
  result := not CharInSet(AFileNameChar, CExcludedChars);
end;


// From:
// Naming Files, Paths, and Namespaces
// http://msdn.microsoft.com/en-us/library/aa365247.aspx?ppud=4
function tiIsFileNameValid(const AFileName : string): boolean;
var
  LFileName: string;
  LLastChar: Char;
  i: integer;
const
  CReservedFileName: array [1..4] of string = ('CON','NUL','PRN','AUX');
  CReservedFileNameNumbered: array [1..2] of string = ('COM','LPT');
  CDeviceNamespace = '\\.\';
begin
  LFileName := ExtractFileName(AFileName);
  result :=
    (Length(LFileName) <= 255) and
    (Length(LFileName) > 0);
  if not result then
    Exit; //==>
  LLastChar := LFileName[Length(LFileName)];

  // Path cannot contain a device reference
  if Pos(CDeviceNamespace, AFileName) > 0 then
  begin
    result := false;
    Exit; //==>
  end;

  // File name must contain valid characters
  for i := 1 to Length(LFileName) do
    if not tiIsFileNameCharValid(LFileName[i]) then
    begin
      result := false;
      Exit; //==>
    end;

  // File name cannot end with a period or space
  if (LLastChar = '.') or (LLastChar = ' ') then
  begin
    result := false;
    Exit; //==>
  end;

  // File name cannot be a device name
  for i := Low(CReservedFileName) to High(CReservedFileName) do
    if SameText(CReservedFileName[i], LFileName) then
    begin
      result := false;
      Exit; //==>
    end;

  // File name cannot be a numbered device name alias
  if (LLastChar >= '1') and (LLastChar <= '9') then
  begin
    LFileName := Copy(LFileName, 1, Length(LFileName) - 1);
    for i := Low(CReservedFileNameNumbered) to High(CReservedFileNameNumbered) do
      if SameText(CReservedFileNameNumbered[i], LFileName) then
      begin
        result := false;
        Exit; //==>
      end;
  end;
end;


function tiConvertToValidFileName(const AFileName : string;
  const ASubstituteChar: char): string;
var
  lFileName : string;
  i : integer;
begin
  if not tiIsFileNameCharValid(ASubstituteChar) then
    raise Exception.CreateFmt('Invalid substitute character: %s', [ASubstituteChar]);

  lFileName := Copy(ExtractFileName(AFileName), 1, 255);
  for i := 1 to Length(lFileName) do
    if not tiIsFileNameCharValid(lFileName[i]) then
      lFileName[i] := ASubstituteChar;
  result := ExtractFilePath(AFileName) + lFileName;
end;


function tiCheckSum(const AValue: string): Integer;
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
  while (i <= Length(AValue)) do
  begin
    Inc(intOdd, StrToIntDef(AValue[i], 0));
    Inc(i, 2);
  end;

  {add all even seq numbers}
  intEven := 0;
  i := 2;
  while (i <= Length(AValue)) do
  begin
    Inc(intEven, StrToIntDef(AValue[i], 0));
    Inc(i, 2);
  end;

  Result := 3*intOdd + intEven;
  {modulus by 10 to get}
  Result := Result mod 10;
  if Result <> 0 then
    Result := 10 - Result
end;


function tiEncodeWordBase26(const AValue : Word): String;
var
  lNum : Int64;
  lPos : byte;
begin
  if AValue = 0 then
  begin
    Result := cZeroChar;
    Exit; //==>
  end;

  Result := '     '; // If the param is changed from Word, this lenght must be changed.
  lPos := 5;
  lNum := AValue;
  While lNum > 0 Do
  Begin
    Result[lPos]:= Chr(Ord(cZeroChar) + lNum Mod cBase);
    Dec(lPos);
    lNum := lNum div cBase;
  End;
  Result := Copy(Result, lPos+1, 5 - lPos);
end;


function  tiDecodeWordBase26(AValue : String): Word;
var
  i: Integer;
begin
  Result := 0;
  If Length(AValue) = 0 Then
    Exit; //==>
  i:= 1;
  While i <= Length(AValue) Do
  Begin
    If (AValue[i] < cZeroChar) or
       ((Ord(AValue[i]) - Ord(cZeroChar)) >= cBase)
    Then
      raise EConvertError.CreateFmt(cErrorDecodeNumError, [AValue, cBase]);
    Result := Result * cBase + Ord(AValue[i]) - Ord(cZeroChar);
    Inc(i);
  End;
end;


// Originally cloned from IdSoapTestingUtils.pas (IndySoap) by
// Grahame Grieve & Andrew Cumming.
// Modified to increase speed.
function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean;
const
  CStreamCompareBufferSize = 1000;
var
  LBytes1, LBytes2: array[0..CStreamCompareBufferSize-1] of byte;
  LBytesRead1, LBytesRead2: Integer;
  LPos1, LPos2: Int64;
  I: Integer;
  LDiffIndex: Integer;
  LDiffPosition: Int64;
begin
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

  LPos1 := AStream1.Position;
  LPos2 := AStream2.Position;
  try
    // Compare contents. Buffered reads for increased speed.
    while (AStream1.Size - AStream1.Position > 0) do
    begin
      LBytesRead1 := AStream1.Read(LBytes1, CStreamCompareBufferSize);
      LBytesRead2 := AStream2.Read(LBytes2, CStreamCompareBufferSize);
      if LBytesRead1 <> LBytesRead2 then
      begin
        // This should never happen.
        result := false;
        VMessage := 'Streams have different sizes ('+inttostr(AStream1.Size)+'/'+inttostr(AStream2.Size)+')';
        Exit; //==>
      end;

      if not CompareMem(@LBytes1, @LBytes2, LBytesRead1) then
      begin
        result := false;
        // Find position where they differ
        LDiffIndex := AStream1.Position;
        for I := 0 to LBytesRead1 - 1 do
          if LBytes1[I] <> LBytes2[I] then
          begin
            LDiffIndex := I;
            Break;
          end;
        LDiffPosition := AStream1.Position - LBytesRead1 + LDiffIndex;
        VMessage := 'Streams Differ at position ' +
            inttostr(LDiffPosition) + ' of ' +
            inttostr(AStream1.Size) + ': ' +
            inttostr(LBytes1[LDiffIndex]) + '/' +
            inttostr(LBytes2[LDiffIndex]);
        Exit; //==>
      end;
    end;
  finally
    AStream1.Position := LPos1;
    AStream2.Position := LPos2;
  end;
end;


function tiTestStreamsIdentical(AStream1, AStream2 : TStream):Boolean;
var
  ls: string;
begin
  ls := '';
  Result := tiTestStreamsIdentical(AStream1, AStream2, ls);
end;

function tiTestFilesIdentical(const AFileName1, AFileName2: string): boolean;
var
  LStream1: TStream;
  LStream2: TStream;
begin
  LStream1 := nil;
  LStream2 := nil;
  try
    LStream1 := TMemoryStream.Create;
    LStream2 := TMemoryStream.Create;
    tiFileToStream(AFileName1, LStream1);
    tiFileToStream(AFileName2, LStream2);
    result := tiTestStreamsIdentical(LStream1, LStream2);
  finally
    LStream2.Free;
    LStream1.Free;
  end;
end;

function tiNormalizeStr(const AString: string): string;
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
  s := Trim(AString);
  s := tiStrTran(s, #9, ' ');   // replace Tabs
  s := tiStrTran(s, #10, ' ');  // replace LF
  s := tiStrTran(s, #13, ' ');  // replace CR
  Result := DelSpace1(s);
end;

function tiURIEncode(const AString: string): string;
var
  I: Integer;
const
  UnsafeChars = ['*', '#', '%', '<', '>', '+', ' '];
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    if CharInSet(AString[I], UnsafeChars) or (AString[I] < #32) or (AString[I] >= #$80) then
      Result := Result + '%' + IntToHex(Ord(AString[I]), 2)
    else
      Result := Result + AString[I];
  end;
end;

function tiURIDecode(const AString: string): string;
const
  UnsafeChars: Array [0..6] of Char = ('*', '#', '%', '<', '>', '+', ' ');

  function _ContainsEncodedChars(const AValue: string): Boolean;
  var
    I: Integer;
    LCharCode: Integer;
    LEncodedCharString: string;
  begin
    for I := Low(UnsafeChars) to High(UnsafeChars) do
    begin
      LCharCode := Ord(UnsafeChars[I]);
      LEncodedCharString := '%' + IntToHex(LCharCode, 2);
      Result := AnsiContainsStr(AValue, LEncodedCharString);
      if Result then
        exit; //-->
    end;
  Result := false;
  end;
var
  I: Integer;
  LCharCode: Integer;
  LEncodedCharString: string;
  LResult: string;
begin
  LResult := AString;

  while _ContainsEncodedChars(LResult) do
    for I := Low(UnsafeChars) to High(UnsafeChars) do
    begin
      LCharCode := Ord(UnsafeChars[I]);
      LEncodedCharString := '%' + IntToHex(LCharCode, 2);
      LResult := tiStrTran(LResult, LEncodedCharString, UnsafeChars[I]);
    end;
  Result := LResult;
end;

function tiURIDecodeAll(const AString: string): string;
var
  i: Integer;
  j: Integer;
  LCharValue: Integer;
  LChar: Char;
begin
  SetLength(result, Length(AString));

  i := 1;
  j := 1;
  while i <= Length(AString) - 2 do
  begin
    if (AString[i] = '%') and
        TryStrToInt('$' + Copy(AString, i+1, 2), LCharValue) then
    begin
      LChar := Char(LCharValue);
      Inc(i, 2);
    end
    else
      LChar := AString[i];

    result[j] := LChar;
    Inc(i);
    Inc(j);
  end;

  while i <= Length(AString) do
  begin
    result[j] := AString[i];
    Inc(i);
    Inc(j);
  end;

  SetLength(result, j - 1);
end;

function tiHTMLEncode(const AString: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AString) do
  begin
    C := AString[I];
    case C of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
    else
      Result := Result + C;
    end;
  end;
end;

function tiHTMLDecode(const AString: string): string;
begin
  Result := AString;
  Result := tiStrTran(Result, '&lt;', '<');
  Result := tiStrTran(Result, '&gt;', '>');
  Result := tiStrTran(Result, '&quot;', '"');
  Result := tiStrTran(Result, '&apos;', '''');
  Result := tiStrTran(Result, '&cr;', #13);
  Result := tiStrTran(Result, '&lf;', #10);
  Result := tiStrTran(Result, '&amp;', '&');
end;

function tiEnclose(
    const AString: string;
    const APrefix: string;
    const ASuffix: string;
    const AEncloseIfPresent: Boolean): string;
var
  LSuffix: string;
begin
  Result := '';
  if ASuffix = '' then
    LSuffix := APrefix
  else
    LSuffix := ASuffix;
  if AEncloseIfPresent or (LeftStr(AString, Length(APrefix)) <> APrefix) then
    Result := Result + APrefix;
  Result := Result + AString;
  if AEncloseIfPresent or (RightStr(AString, Length(LSuffix)) <> LSuffix) then
    Result := Result + LSuffix;
end;

function tiQuote(const AString: string): string;
begin
  Result := tiEnclose(AString, '"');
end;

function tiAddSeparators(const AString: string; const AColumnWidth: Integer; const ASeparator: string): string;
var
  I: Integer;
begin
  Assert(AColumnWidth > 0, 'AColumnWidth must be > 0');
  Result := '';
  I := 1;
  while I < Length(AString) do
  begin
    Result := Result + MidStr(AString, I, AColumnWidth);
    if I + AColumnWidth <= Length(AString) then
      Result := Result + ASeparator;
    Inc(I, AColumnWidth);
  end;
end;

function tiWrap(const AString: string; const AColumnWidth: Integer): string;
begin
  Result := tiAddSeparators(AString, AColumnWidth, tiLineEnd);
end;

function tiStripNonAlphaCharacters(const AString: string): string;
var
  i: integer;
  LResult: string;
begin
  LResult := '';
  for i := 1 to Length(AString) do
    if isCharAlpha(AString[i]) then
      LResult := LResult + AString[i];
  Result := LResult;
end;

function tiStripNonNumericCharacters(const AString: string): string;
var
  i: integer;
  LResult: string;
begin
  LResult := '';
  for i := 1 to Length(AString) do
    if CharInSet(AString[i], ['0'..'9']) then
      LResult := LResult + AString[i];
  Result := LResult;
end;

function tiReplaceFileNameReservedChars(
    const AString: string; const AReplaceWith: string;
    const AReplaceDot: Boolean = false; const AReplaceSlashes: Boolean = false;
    const AReplaceColons: Boolean = false): string;
var
  i: integer;
  LResult: string;
begin
  // TODO: Different behaviour for non-Windows OSs
  LResult := '';
  for i := 1 to Length(AString) do
  begin
    if CharInSet(AString[i], ['*', '?', '<', '>', '"', '[', ']', ';', '|', '=', ',']) or
        (AReplaceDot and CharInSet(AString[i], ['.'])) or
        (AReplaceSlashes and CharInSet(AString[i], ['/', '\'])) or
        (AReplaceColons and CharInSet(AString[i], [':'])) then
    begin
      LResult := LResult + AReplaceWith;
    end else
      LResult := LResult + AString[i];
  end;
  Result := LResult;
end;

function tiFormatDataSize(const AValue: Int64; const AValueFormat: string): string;
const
  // NOTE: These use the old 1024 base conventions.
  // These are now known as kibibyte, mebibyte, gibibyte, etc.
  // See: http://en.wikipedia.org/wiki/Megabyte
  K = Int64(1024); // kB (KiB)
  M = K * K; // MB (MiB)
  G = K * M; // GB (GiB)
  T = K * G; // TB (TiB)
begin
  if AValue < K then Result := Format('%d bytes', [AValue])
  else if AValue < M then Result := Format(AValueFormat + ' kB', [AValue / K])
  else if AValue < G then Result := Format(AValueFormat + ' MB', [AValue / M])
  else if AValue < T then Result := Format(AValueFormat + ' GB', [AValue / G])
  else Result := Format(AValueFormat + ' TB', [AValue / T]);
end;

function tiStripIntPrefix(const AString : string): string;
var
  i: Integer;
begin
  for i := 1 to Length(AString) do
    if (AString[i] < '0') or (AString[i] > '9') then
    begin
      result := Copy(AString, i, Length(AString));
      Exit; //==>
    end;
  result := '';
end;

function tiAppendStr(const AString: string; const AValue: string;
  const ASeparator: string = ' '): string;
begin
  result := AString;
  if AValue <> '' then
  begin
    if result <> '' then
      result := result + ASeparator;
    result := result + AValue;
  end;
end;

// This is a partial implementation, for English only. There are many
// rules and exceptions. See:
// http://www.oxforddictionaries.com/words/plurals-of-nouns
function tiPluralize(const ASingularWord: string;
  const AMakePlural: boolean): string;
var
  LWord: string;
  LLast: Char;
  LLastTwo: string;
  LSecondLast: Char;
  LUseUpper: boolean;

  function _IsVowel(const AChar: Char): boolean;
  begin
    result := CharInSet(AChar, ['a', 'e', 'i', 'o', 'u']);
  end;

  function _ReplaceLast(const ACount: Integer; const AWith: string): string;
  begin
    result := Copy(ASingularWord, 1, Length(ASingularWord) - ACount);
    if LUseUpper then
      result := result + UpperCase(AWith)
    else
      result := result + AWith;
  end;

  function _Add(const AAdd: string): string;
  begin
    if LUseUpper then
      result := ASingularWord + UpperCase(AAdd)
    else
      result := ASingularWord + AAdd;
  end;

begin
  if (not AMakePlural) or (ASingularWord = '') then
  begin
    result := ASingularWord;
    Exit; //==>
  end;

  LUseUpper := ASingularWord[Length(ASingularWord)] =
      UpperCase(ASingularWord[Length(ASingularWord)]);
  LWord := LowerCase(Trim(ASingularWord));
  LLast := LWord[Length(LWord)];
  if Length(LWord) > 1 then
  begin
    LLastTwo := Copy(LWord, Length(LWord) - 1, 2);
    LSecondLast := LWord[Length(LWord)-1];
  end
  else
  begin
    LLastTwo := LWord;
    LSecondLast := #0;
  end;

  // Common rules
  // If it ends in -is, change -is to -es
  if LLastTwo = 'is' then
    result := _ReplaceLast(2, 'es')
  // If it ends in -s,z,x,ch,sh,ss, add -es
  else if MatchStr(LLast, ['s','z','x']) or MatchStr(LLastTwo, ['ch','sh','ss']) then
    result := _Add('es')
  // If it ends in a vowel and -y, add -s
  // If it ends in a constonant and -y, change -y to -ies
  else if LLast = 'y' then
  begin
    if _IsVowel(LSecondLast) then
      result := _Add('s')
    else
      result := _ReplaceLast(1, 'ies');
  end
  // Common word exceptions
  else if LWord = 'zero' then
    result := _Add('es')
  else if LWord = 'man' then
    result := _ReplaceLast(2, 'en')
  else if LWord = 'woman' then
    result := _ReplaceLast(2, 'en')
  else if LWord = 'child' then
    result := _Add('ren')
  // Default, true for most words
  else
    result := _Add('s');
end;

function tiDateTimeAsXMLString(const ADateTime: TDateTime): string;
  function _IntToStr(AValue, pSize : integer): string;
  begin
    result := IntToStr(AValue);
    result := tiPad0(result, pSize);
  end;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
begin
  DecodeDate(ADateTime, lY, lM, lD);
  DecodeTime(ADateTime, lH, lMi, lS, lMs);
  {$IFDEF DATEFORMAT_YYYYMMDD}
  Result :=
    _IntToStr(lY, 4) + '-' +    // NB '-' separator deliberate
    _IntToStr(lM, 2) + '-' +
    _IntToStr(lD, 2) + ' ' +
    _IntToStr(lH, 2) + ':' +
    _IntToStr(lMi, 2) + ':' +
    _IntToStr(lS, 2) + ':' +
    _IntToStr(lMs, 3);
  {$ELSE}
  Result :=
    _IntToStr(lD, 2) + '/' +
    _IntToStr(lM, 2) + '/' +
    _IntToStr(lY, 4) + ' ' +
    _IntToStr(lH, 2) + ':' +
    _IntToStr(lMi, 2) + ':' +
    _IntToStr(lS, 2) + ':' +
    _IntToStr(lMs, 3);
  {$ENDIF}

  { What about using the ISO 8601 standard!  See cISODateTimeFmt1 in
    tiConstants.
    Also, why not use FormatDateTime()? }
end;

function tiDateAsXMLString(const ADateTime: TDateTime): string;
  function _IntToStr(AValue, pSize : integer): string;
  begin
    result := IntToStr(AValue);
    result := tiPad0(result, pSize);
  end;
var
  lY, lM, lD: Word;
begin
  DecodeDate(ADateTime, lY, lM, lD);
  {$IFDEF DATEFORMAT_YYYYMMDD}
  Result :=
    _IntToStr(lY, 4) + '-' +    // NB '-' separator deliberate
    _IntToStr(lM, 2) + '-' +
    _IntToStr(lD, 2);
  {$ELSE}
  Result :=
    _IntToStr(lD, 2) + '/' +
    _IntToStr(lM, 2) + '/' +
    _IntToStr(lY, 4);
  {$ENDIF}
end;

function tiXMLStringToDateTime(const AValue : string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
begin
  if AValue = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

  try
    //          1         2
    // 12345678901234567890123
    // YYYY/MM/DD HH:MM:SS:NN   (legacy format)
    // DD/MM/YYYY HH:MM:SS:NN   (current format - see function above)
    if  (Length(AValue) > 3)
    and (AValue[3] <> '/') then // assume lecacy format
    begin
      lY := StrToInt(Copy(AValue, 1, 4));
      lM := StrToInt(Copy(AValue, 6, 2));
      lD := StrToInt(Copy(AValue, 9, 2));
    end
    else
    begin
      lD := StrToInt(Copy(AValue, 1, 2));
      lM := StrToInt(Copy(AValue, 4, 2));
      lY := StrToInt(Copy(AValue, 7, 4));
    end;

    lH := StrToIntDef(Copy(AValue, 12, 2), 0);
    lMi := StrToIntDef(Copy(AValue, 15, 2), 0);
    lS := StrToIntDef(Copy(AValue, 18, 2), 0);
    lMs := StrToIntDef(Copy(AValue, 21, 3), 0);
    result :=
      EncodeDate(lY, lM, lD) +
      EncodeTime(lH, lMi, lS, lMs);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorXMLStringToDate, [AValue, e.message]);
  end;
end;


function tiDateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeStor, ADateTime);
  if Pos('18991230', Result) = 1 then
    Result := tiStrTran(Result, '18991230', '00000000');
end;


function tiDateTimeAsIntlDateDisp(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeDisp, ADateTime);
  if Pos('1899-12-30', Result) = 1 then
    Result := tiStrTran(Result, '1899-12-30', '0000-00-00');
end;

function tiDateAsIntlDateDisp(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(CIntlDateDisp, ADateTime);
  if Pos('1899-12-30', Result) = 1 then
    Result := tiStrTran(Result, '1899-12-30', '0000-00-00');
end;

function tiIntlDateStorAsDateTime(const AValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word;
begin
  if Trim(AValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;
  
    //          1         2
    // 12345678901234567890123
    // yyyymmddThhnnss
  lY := StrToInt(Copy(AValue, 1, 4));
  lM := StrToInt(Copy(AValue, 5, 2));
  lD := StrToInt(Copy(AValue, 7, 2));
  lH := StrToInt(Copy(AValue, 10, 2));
  lMi := StrToInt(Copy(AValue, 12, 2));
  lS := StrToInt(Copy(AValue, 14, 2));
  
  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;


function tiIntlDateDispAsDateTime(const AValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word;
begin
  if Trim(AValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

    //          1         2
    // 12345678901234567890123
    // yyyy-mm-dd hh:nn:ss
  lY := StrToInt(Copy(AValue, 1, 4));
  lM := StrToInt(Copy(AValue, 6, 2));
  lD := StrToInt(Copy(AValue, 9, 2));
  lH := StrToInt(Copy(AValue, 12, 2));
  lMi := StrToInt(Copy(AValue, 15, 2));
  lS := StrToInt(Copy(AValue, 18, 2));

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

function tiIsClassOfType(pData: TObject; pClassName: string): boolean;
var
  lsClassName: string;
  lClass: TClass;
begin
  lsClassName := upperCase(pClassName);
  lClass := pData.ClassType;
  result := false;
  while (not result) and
    (lClass <> nil) do
  begin
    if UpperCase(lClass.ClassName) = lsClassName then
    begin
      result := true;
      break; //==>
    end;
    lClass := lClass.ClassParent;
  end;
end;

function tiExpandURI(const AURI: string): string;
begin
  // If it is a local file then get the full path.
  if (Pos('://', AURI) = 0) and (LeftStr(AURI, 2) <> '\\') then
    Result := ExpandFileName(AURI)
  else
    Result := AURI;
end;

// Check for file based URI:
// It is a file if it does not have :// (e.g. C:\xxx or \\xxx) or is file://xxx
function tiIsFileURI(const AURI: string): Boolean;
var
  LURI: string;
begin
  LURI := LowerCase(AURI);
  Result := ((Pos('://', LURI) = 0) or (Pos('file://', LURI) = 1));
end;

function tiCheckFileCanBeCreated(const AFileName: string; var AErrorMsg: string): boolean;

  function _CheckValidPathStart(const AFilePath: string): string;
  var
    LValid: Boolean;
    LUNCFileNameStart: string;
  begin
    //blank path
    LValid := AFilePath <> '';

    if LValid then
    begin
      //no drive entered and not UNC filename
      LUNCFileNameStart := PathDelim+PathDelim;
      if Pos(LUNCFileNameStart, AFilePath) = 0 then
      begin
        LValid := (Pos(DriveDelim, AFilePath) <> 0) and
                  (Pos(DriveDelim, AFilePath) < Pos(PathDelim, AFilePath));
      end
      else
      //UNC filename
        LValid := PosEx(PathDelim, AFilePath, 3) <> 0;
    end;

    if LValid then
      result := ''
    else
      result := CNoPathEntered;
  end;

  procedure _DeconstructPath(var APathList: TStringList;
    AFileName: string);
  var
    LDelimPos: integer;
    LPathSubString: string;
    LUNCFileNameStart: string;
  begin
    Assert(Assigned(APathList));
    APathList.Clear;

    //If path starts with '\\' (ie. It's a UNC filename) then start looking for
    //instances of PathDelim at Pos=3.
    LUNCFileNameStart := PathDelim+PathDelim;
    if Pos(LUNCFileNameStart, AFileName) = 0 then
      LDelimPos := PosEx(PathDelim, AFileName, 1)
    else
      LDelimPos := PosEx(PathDelim, AFileName, 3);

    while LDelimPos <> 0 do
    begin
      LPathSubString := LeftStr(AFileName, LDelimPos - 1);
      LPathSubString := tiAddTrailingSlash(LPathSubString);
      APathList.Add(LPathSubString);
      LDelimPos := PosEx(PathDelim, AFileName, LDelimPos + 1);
    end;
  end;

  function _CheckDrive(const AFilePath: string): string;
  var
    LUNCFileNameStart: string;
    LFileDrive: string;
  begin
    result := '';
    LUNCFileNameStart := PathDelim+PathDelim;
    if Pos(LUNCFileNameStart, AFilePath) <> 1 then
    begin
      LFileDrive := LeftStr(AFilePath, Pos(DriveDelim, AFilePath));
      if not DirectoryExists(LFileDrive) then
        result := Format(CInvalidDrive, [LFileDrive]);
    end;
  end;

  function _CheckDirectories(const APathList: TStringList;
             var ADirsCreatedByCheck: TStringList): string;
  var
    i: integer;
  begin
    result := '';
    for i := 0 to APathList.Count - 1 do
    begin
      if not DirectoryExists(APathList.Strings[i]) then
      begin
        if ForceDirectories(APathList.Strings[i]) then
          ADirsCreatedByCheck.Add(APathList.Strings[i])
        else
          if i = APathList.Count - 1 then
            result := Format(CInvalidDirName, [APathList.Strings[i]]);
      end;
      end;
    end;

{$IFDEF IOS}
  function _CheckFileName(const AFileName: string): string;
  var
    lStream : TFileStream;
  begin
    if tiIsFileNameValid(AFileName) then
    begin
      try
        lStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
      except
        on e: exception do
          result := Format(CFileAccessDenied, [AFileName]);
      end;
      lStream.Free;
    end else
      result := Format(CInvalidFileName, [AFileName]);
  end;

{$ELSE}

  function _CheckFileName(const AFileName: string): string;
  begin
    if tiIsFileNameValid(AFileName) then
    begin
      try
        tiStringToFile('', AFileName);
      except
        on e: exception do
          result := Format(CFileAccessDenied, [AFileName]);
      end;
    end else
      result := Format(CInvalidFileName, [AFileName]);
  end;
{$ENDIF IOS}


  procedure _CleanupDirsCreatedByCheck(const ADirsCreatedByCheck: TStringList);
  var
    i: integer;
  begin
  for i := ADirsCreatedByCheck.Count - 1 downto 0 do
    tiForceRemoveDir(ADirsCreatedByCheck.Strings[i]);
  end;

var
  i: Integer;
  LPathString: string;
  LPathList: TStringList;
  LDirsCreatedByCheck: TStringList;
  LFileCreatedByCheck: Boolean;
  LFileName: string;
  LFilePath: string;
begin
  AErrorMsg := '';
  LFileName := AFileName;
  if FileExists(LFileName) and not tiIsFileReadOnly(LFileName) then
  begin
    result := true;
    exit;
  end else
  begin

    LFilePath := ExtractFilePath(LFileName);

    AErrorMsg := _CheckValidPathStart(LFilePath);
    if (AErrorMsg = '') then
    begin

      LFileCreatedByCheck := not FileExists(LFileName);

      AErrorMsg := _CheckDrive(LFilePath);

      if (AErrorMsg = '') then
      begin
        LPathList := TStringList.Create;
        try
          _DeconstructPath(LPathList, LFilePath);

          //Directory names ending in a space are invalid
          for i := 0 to LPathList.Count - 1 do
          begin
            LPathString := tiRemoveTrailingSlash(LPathList.Strings[i]);
            if LPathString[Length(LPathString)] = ' ' then
            AErrorMsg := CDirsEndingInSpace;
          end;

          if (AErrorMsg = '') then
          begin
            LDirsCreatedByCheck := TStringList.Create;
            try
              AErrorMsg := _CheckDirectories(LPathList, LDirsCreatedByCheck);

              if AErrorMsg = '' then
                AErrorMsg := _CheckFileName(LFileName);

              if LFileCreatedByCheck then
                tiDeleteFile(LFileName);

              _CleanupDirsCreatedByCheck(LDirsCreatedByCheck);

            finally
              LDirsCreatedByCheck.Free;
            end;
          end;
        finally
          LPathList.Free;
        end;
      end;
    end;
  end;
  result := AErrorMsg = '';
end;

// Remove path prefix to my documents from given dir.
// e.g.
// Input : C:\Documents and Settings\User\My Documents\Path\File.ext
// Output: My Documents\Path\File.ext
function tiRemoveCurrentUserPersonalDirPrefix(const AFileName: string): string;
var
  LDocumentsDir: string;
  LPos: integer;
begin
  // Get My Documents directory without last sub-dir so that it is
  // preserved.
  LDocumentsDir := tiRemoveTrailingSlash(tiGetCurrentUserPersonalDir);
  LPos := tiPosR(PathDelim, LDocumentsDir);
  if LPos <> 0 then
    LDocumentsDir := Copy(LDocumentsDir, 1, LPos);

  if Copy(AFileName, 1, Length(LDocumentsDir)) = LDocumentsDir then
    result := Copy(AFileName, Length(LDocumentsDir) + 1, Length(AFileName))
  else
    result := AFileName;
end;

function tiLocateExtension(aValue: TFileName): integer;
var
  i: integer;
  Done: boolean;
begin
  result := 0;
  Done := false;
  i := Length(AValue);
  while not Done and (Result = 0) and (i > 0) do
    if (AValue[i] = PathDelim) then
      Done := true
    else if (AValue[i] = '.') then
      result := i
    else
      Dec(i);
end;

{ TtiIntegerList }

procedure TtiIntegerList.Add(AValue: Int64);
begin
  FList.Add(TtiIntegerListItem.CreateCustom(AValue));
end;


procedure TtiIntegerList.Clear;
begin
  FList.Clear;
end;


constructor TtiIntegerList.Create;
begin
  inherited Create;
{$IFDEF IOS}
  FList:= TObjectList<TtiIntegerListItem>.Create(True);
{$ELSE}
  FList:= TObjectList.Create(True);
{$ENDIF IOS}
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


procedure TtiIntegerList.SetItems(i: Integer; const AValue: Int64);
begin
  (FList.Items[i] as TtiIntegerListItem).Value := AValue;
end;


function _DoSort(Item1, Item2: Pointer): Integer;
begin
  result:=
    CompareValue(
      TtiIntegerListItem(Item1).Value,
      TtiIntegerListItem(Item2).Value);
end;

procedure TtiIntegerList.Sort;
begin
{$IFDEF IOS}
  FList.Sort(TComparer<TtiIntegerListItem>.Construct(
   function (const L, R: TtiIntegerListItem): integer
   begin
     result := _DoSort(L, R);
   end
   ));
{$ELSE}
  FList.Sort(_DoSort);
{$ENDIF IOS}
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
  1.295, which internally is represented 1.29499 99999
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
function tiDecimalRound(
  AValue: extended;
  NDFD: integer;
  MaxRelErr: extended;
  Ctrl: TtiDecimalRoundingCtrl = drHalfEven): extended;
var
  i64, j64: Int64;
  j: integer;
  m, ScaledVal, ScaledErr: extended;
begin

  If IsNaN(AValue) or (Ctrl = drNone)
    then begin Result := AValue; EXIT end;

{ Compute 10^NDFD and scale the AValue and MaxError: }
  m := 1; For j := 1 to abs(NDFD) do m := m*10;
  If NDFD >= 0
    then begin
      ScaledVal := AValue * m;
      ScaledErr := abs(MaxRelErr*AValue) * m;
      end
    else begin
      ScaledVal := AValue / m;
      ScaledErr := abs(MaxRelErr*AValue) / m;
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
     (AValue < 0)
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

function tiSetPrecision(const AValue : Extended; const APrecision : integer = 3): Extended;
begin
  Result:= tiDecimalRoundExt(AValue, APrecision);
end;

function TtiMultiReadExclusiveWriteSynchronizer.LockForRead: boolean;
begin
  FCritSect.Enter;
  try
    result:= CanLockForRead;
    if Result then
      Inc(FReadCount);
  finally
    FCritSect.Leave;
  end;
end;

function TtiMultiReadExclusiveWriteSynchronizer.LockForWrite: boolean;
begin
  FCritSect.Enter;
  try
    result:= CanLockForWrite;
    if Result then
      FWriting:= True;
  finally
    FCritSect.Leave;
  end;
end;

function TtiMultiReadExclusiveWriteSynchronizer.CanLockForRead: boolean;
begin
  result:= not FWriting;
end;

function TtiMultiReadExclusiveWriteSynchronizer.CanLockForWrite: boolean;
begin
  result:= (FReadCount = 0) and not FWriting;
end;

constructor TtiMultiReadExclusiveWriteSynchronizer.Create;
begin
  inherited Create;
  FCritSect:= TCriticalSection.Create;
end;

destructor TtiMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  BeginWrite;
  FCritSect.Free;
  inherited Destroy;
end;

procedure TtiMultiReadExclusiveWriteSynchronizer.BeginWrite;
begin
  while not LockForWrite do
    Sleep(100);
end;

procedure TtiMultiReadExclusiveWriteSynchronizer.EndWrite;
begin
  FCritSect.Enter;
  try
    FWriting:= False;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiMultiReadExclusiveWriteSynchronizer.BeginRead;
begin
  while not LockForRead do
    Sleep(100);
end;

procedure TtiMultiReadExclusiveWriteSynchronizer.EndRead;
begin
  FCritSect.Enter;
  try
    Dec(FReadCount);
  finally
    FCritSect.Leave;
  end;
end;


//ItiTokens = interface
//    ['{9E3F0CF7-1430-4A8B-B204-80AD03A4F567}']
//    function Count: integer;
//    function GetToken(const AIndex: integer): string;
//    procedure SetText(const AText, ASeparator: string);
//    property Tokens[const AIndex: integer]: string read GetToken; default;
//  end;

type
  TtiTokens = class(TInterfacedObject, ItiTokens)
  private
    FTokens: TStrings;
  protected
    function Count: integer;
    function GetToken(const AIndex: integer): string;
    procedure SetText(const AText, ASeparator: string);
  public
    constructor Create(const AText: string = ''; const ASeparator: string = ',');
    destructor Destroy; override;
  end;

  function CreateTiTokens(const AText, ASeparator: string): ItiTokens;
  begin
    Result := TtiTokens.Create(AText, ASeparator);
  end;

{ TtiTokens }

function TtiTokens.Count: integer;
begin
  Result := FTokens.Count;
end;

constructor TtiTokens.Create(const AText, ASeparator: string);
begin
  inherited Create;
  FTokens := TStringList.Create;
  SetText(AText, ASeparator);
end;

destructor TtiTokens.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TtiTokens.GetToken(const AIndex: integer): string;
begin
  if (AIndex < 1) or (AIndex > Count) then
    raise ERangeError.Create('AIndex outside valid range')
  else
  begin
    Result := FTokens[AIndex - 1];
  end;
end;

procedure TtiTokens.SetText(const AText, ASeparator: string);
var
  idxSeparator : integer;
  LText : string;
  lLenSeparator: integer;
begin
  FTokens.Clear;

  if AText = '' then
    Exit; //==>

  LText := AText;
  lLenSeparator := Length(ASeparator);
  idxSeparator := pos(ASeparator, LText);

  while idxSeparator <> 0 do begin
    FTokens.Add(Copy(LText, 1, idxSeparator - 1));
    delete(LText, 1, idxSeparator - 1 + lLenSeparator);
    idxSeparator := pos(ASeparator, LText);
  end;

  if Length(LText) > 0 then
    FTokens.Add(LText);

end;

{ TtiUniqueIntegerList }

procedure TtiUniqueIntegerList.Add(AValue: Int64);
begin
  if IndexOf(AValue) = -1 then
    inherited Add(AValue);
end;

{ TtiRealList }

procedure TtiRealList.Add(AValue: Extended);
begin
  FList.Add(TtiRealListItem.CreateCustom(AValue));
end;

constructor TtiRealList.Create;
begin
  inherited Create;
{$IFDEF IOS}
  FList:= TObjectList<TtiRealListItem>.Create(True);
{$ELSE}
  FList:= TObjectList.Create(True);
{$ENDIF IOS}
end;

destructor TtiRealList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiRealList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TtiRealList.GetItems(i: Integer): Extended;
begin
  Result := (FList.Items[i] as TtiRealListItem).Value;
end;

procedure TtiRealList.SetItems(i: Integer; const AValue: Extended);
begin
  (FList.Items[i] as TtiRealListItem).Value := AValue;
end;

{$IFDEF IOS}
function ShortStringToString(AValue: array of Byte): String;
var
  B: TBytes;
  L: Byte;
begin
  Result := '';
  L := AValue[0];
  SetLength(B, L);
  Move(AValue[1], B[0], L);
  Result := TEncoding.Ansi.GetString(B);
end;

procedure StringToShortString(const AFromString: String; var AReturnValue);
var
  L: Integer;
  P: PByte;
  B: TBytes;
begin
  L := Length(AFromString);
  if L > 255 then
    raise EtiShortStringConvertError.Create('Strings longer than 255 characters cannot be converted');
  SetLength(B, L);
  P := @AReturnValue;
  P^ := L;
  Inc(P);
  B := TEncoding.Ansi.GetBytes(AFromString);
  Move(B[0], P^, L);
end;

{$ENDIF IOS}

{ TtiRealListItem }

constructor TtiRealListItem.CreateCustom(AValue: Extended);
begin
  Create;
  FValue := AValue;
end;

{ TtiIntegerListItem }

constructor TtiIntegerListItem.CreateCustom(AValue: Int64);
begin
  Create;
  FValue := AValue;
end;


end.
